library(tidyverse)
library(phenometR)
library(broom)
library(kableExtra)


# The average SPEI for the scan site using months Jan-June
spi_data = read_csv('./data/scan_spei_and_weather.csv') %>%
  filter(month %in% 1:6) %>%
  group_by(year) %>%
  summarise(spei = mean(spei)) %>%
  ungroup()

phenophase_info = get_phenophase_metadata() %>%
  select(phenophase, phenophase_desc)

scan_plant_locations = read_csv('data/scan_plants.csv') %>%
  janitor::clean_names() %>%
  mutate(plant_id = paste0('SC',plant_id)) %>%
  select(soilprofile, plant_id)

phenophase_data = read_csv('data/PHENOMET_SCAN_DATA.csv') %>%
  janitor::clean_names() %>%
  filter(spp_code %in% c('FLCE','PRGL','SPAI')) %>%
  filter(site_code %in% c('SC')) %>%
  left_join(scan_plant_locations, by='plant_id') %>%
  left_join(phenophase_info, by='phenophase')

#-------------------------------
# event things like timing of leaf out, full fl, etc.
event_phenophase_codes =  c('DS_01','DS_02','DS_07','DS_08','DS_09','DS_10','DS_11','DS_12','GR_02','GR_04','GR_05','GR_06','GR_07','GR_09')

event_phenophases = phenophase_data %>%
  filter(phenophase %in% event_phenophase_codes) %>%
  filter(status %in% c(0,1)) 

# diagnostic plot of individual phenophases over time
event_phenophases %>%
  filter(phenophase=='GR_04') %>%
  ggplot(aes(x=doy, y=status,color=plant_id)) + 
  geom_jitter(width=0, height=0.05) + 
  facet_grid(plant_id~year) +
  theme(legend.position = 'none')

# Calculate yearly onset for each individual and each phenophase
onset_timing = event_phenophases %>%
  mutate(status = case_when(
    phenophase=='GR_07' & doy<100 ~ 0,            # Suppress various fruit,leaf phenophases which persist into spring
    phenophase=='DS_02' & doy<50  ~ 0,
    phenophase=='DS_11' & doy<150 ~ 0,
    phenophase=='DS_10' & doy<150 ~ 0,
    TRUE ~ status
  )) %>%
  group_by(plant_id, spp_code, phenophase, phenophase_desc, site_code, soilprofile, year) %>%
  summarise(onset_doy = min(doy[status==1]),
            had_onset = any(status==1),
            n_visits = n()) %>%
  ungroup() %>%
  filter(had_onset, n_visits>=40) %>%   # only individuals with at least 40 visits per year
  left_join(spi_data, by='year')        # Bring in SPEI data for each year. 

# Figure: onset doy over time for all phenophases
ggplot(onset_timing, aes(x=year, y=onset_doy)) + 
  geom_jitter(aes(color=soilprofile),width=0.05, height=0, size=2) +
  #geom_smooth(aes(color=soilprofile), method='lm', se=T) + 
  scale_x_continuous(breaks=seq(2010,2020,2)) + 
  facet_wrap(spp_code~str_wrap(paste(phenophase,phenophase_desc),25), scales='free') +
  theme_bw(12)

# Figure: onset doy versus drought index
ggplot(onset_timing,aes(x=spei, y=onset_doy, color=soilprofile)) + 
  geom_jitter(aes(color=soilprofile),width=0.05, height=0, size=2) +
  geom_smooth(method='lm') +
  geom_vline(xintercept = 0) + 
  facet_wrap(spp_code~str_wrap(paste(phenophase,phenophase_desc),25), scales='free') +
  labs(x='Average SPEI for Jan-June (negative = drier, positive = wetter)',
       y='Onset DOY') +
  theme_bw(12)


#-----------------
# Correlation analysis for present/absent phenophases
#
# first fit model for each species/phenophase/profile
# fitting models with group_by outlined here: https://broom.tidymodels.org/articles/broom_and_dplyr.html
onset_models = onset_timing %>%
  group_by(spp_code, phenophase, phenophase_desc, soilprofile) %>%
  nest() %>%
  mutate(model = map(data, ~lm(onset_doy~spei, data=.))) %>%
  ungroup()

#----------------------------------
# Figure: slope coefficient for onset~spei
# extract the model coefficient using broom package and plot with CI.
onset_models %>%
  mutate(t = map(model, broom::tidy)) %>%
  unnest(t) %>%
  filter(term == 'spei') %>% 
  ggplot(aes(x=estimate, y=fct_rev(str_wrap(paste(phenophase,phenophase_desc),25)), color=soilprofile)) + 
  geom_point(position = position_dodge(width=0.25), size=3) + 
  geom_errorbarh(aes(xmin = estimate - std.error*1.96, xmax = estimate + std.error*1.96),
                 height=0, size=1.5, position = position_dodge(width=0.25)) + 
  #scale_y_discrete(expand = c(0,0,0,0)) + 
  geom_vline(xintercept = 0) +
  facet_wrap(spp_code~., scales='free_y', ncol=3) +
  labs(x='Slope of SPEI - Onset relationship',y='',
       title='Drought sensitivity for phenophase onset') +
  theme_bw(12)

# A table of model statics, R^2, p-value, and AIC
onset_models %>%
  mutate(t = map(model, broom::glance)) %>%
  unnest(t) %>%
  select(spp_code, soilprofile, phenophase, phenophase_desc, r.squared, p.value, AIC) %>%
  kable(format='simple')

#-----------------
# Correlation analysis for count phenophases
#--------------------
# counts like # flowers, # fruit
count_phenophase_codes = c('DS_207','DS_208','DS_210','DS_211', 'GR_204','GR_205','GR_206','GR_207')

count_phenophases = phenophase_data %>%
  filter(spp_code %in% c('FLCE','PRGL','SPAI')) %>%
  filter(phenophase %in% count_phenophase_codes) %>%
  filter(status>=0)  %>%
  filter(!(year %in% c(2016,2017))) # dropping these years for counts cause they are wayyyy overestimated. 

# the maximum amount counted for each individual, phenophase, and calendar year.
max_annual_count = count_phenophases %>%
  group_by(plant_id,spp_code,phenophase, phenophase_desc, site_code, soilprofile, year) %>%
  summarise(max_count = max(status),
            n_visits = n()) %>%
  ungroup() %>%
  filter(n_visits>40) %>%
  left_join(spi_data, by='year')

# counts over time for all phenophases
ggplot(max_annual_count, aes(x=year, y=max_count, color=soilprofile)) + 
  geom_jitter(width=0.05, height=0) +
  scale_x_continuous(breaks=seq(2010,2020,2)) + 
  facet_wrap(spp_code~str_wrap(paste(phenophase,phenophase_desc),30), scales='free') +
  labs(x='', y='Maximum annual count') +
  theme_bw(12)

# counts over time for all phenophases
ggplot(max_annual_count, aes(x=spei, y=max_count, color=soilprofile)) + 
  geom_jitter(width=0.05, height=0) +
  geom_smooth(method='lm', se=T) + 
  facet_wrap(spp_code~str_wrap(paste(phenophase,phenophase_desc),30), scales='free') +
  labs(x='SPEI for Jan-June (negative = drier, positive = wetter)',
       y='Maximum annual count') +
  theme_bw(12)

# Linear models for max_count~spei for each species,phenophase,soil profile
count_models = max_annual_count %>%
  group_by(spp_code, phenophase, phenophase_desc, soilprofile) %>%
  nest() %>%
  mutate(model = map(data, ~lm(max_count~spei, data=.))) %>%
  #unnest(t) %>%
  ungroup()

# extract the slope coefficient and CI and plot
count_models %>%
  mutate(t = map(model, broom::tidy)) %>%
  unnest(t) %>%
  filter(term == 'spei') %>% 
  ggplot(aes(x=estimate, y=fct_rev(str_wrap(paste(phenophase,phenophase_desc),30)), color=soilprofile)) + 
  geom_point(position = position_dodge(width=0.25), size=3) + 
  geom_errorbarh(aes(xmin = estimate - std.error*1.96, xmax = estimate + std.error*1.96),
                 height=0, size=1.5, position = position_dodge(width=0.25)) + 
  geom_vline(xintercept = 0) +
  facet_wrap(spp_code~., scales='free', ncol=3) +
  labs(x='Slope of SPEI - Maximum Count relationship', y='', 
       title='Drought sensitivity for count phenophases') +
  theme_bw(15)

# A table of model statics, R^2, p-value, and AIC
count_models %>%
  mutate(t = map(model, broom::glance)) %>%
  unnest(t) %>%
  select(spp_code, soilprofile, phenophase, phenophase_desc, r.squared, p.value, AIC) %>%
  kable(format='simple')
