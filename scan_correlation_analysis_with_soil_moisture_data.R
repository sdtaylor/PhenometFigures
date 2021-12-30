library(tidyverse)
library(phenometR)
library(broom)
library(kableExtra)
library(zoo)

# Correlate onset dates with lagged change in soil moisture, using the three species with individuals in both soil types at SCAN.

soil_moisture_monthly_average = read_csv('./data/scan_monthly_soil_moisture.csv')

# Which soil depths to use, of -2, -4, -8, -20, -40
depths_to_use = c(-2,-4)
# How long of a lag to use. This is offset by 1 month. So for an onset in June with 2 months of lag, the % change
# in moisture for April & May will be used. With 3 months of lag March,April, & May is used. 
months_of_lag = 2

# Calculate average % change in soil moisture of the prior two months.
soil_moisture_monthly_average = soil_moisture_monthly_average %>%
  filter(depth %in% depths_to_use) %>%
  group_by(soilprofile, month_date) %>%
  summarise(avg_soil_moisture_change = mean(avg_soil_moisture_change, na.rm=T)) %>%
  ungroup() %>%
  group_by(soilprofile) %>%
  arrange(month_date) %>%
  mutate(avg_soil_moisture_change_with_lag = zoo::rollmean(avg_soil_moisture_change, k = months_of_lag, align = 'right', fill=NA)) %>%
  ungroup() %>%
  mutate(month_date = month_date + lubridate::days(30)) %>% # offset the soil moisture by 1 month so there is a lag
  mutate(month = lubridate::month(month_date),              # eg. For an onset in June, the soil moisture change will be the average for April & May
         year  = lubridate::year(month_date)) %>%
  select(soilprofile, year, month, avg_soil_moisture_change_with_lag)

# Bring in phenomet observations.
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
  mutate(onset_date = as.Date(paste(year,onset_doy,sep='-'),format='%Y-%j')) %>%
  mutate(onset_month = lubridate::month(onset_date)) %>%
  left_join(soil_moisture_monthly_average, by=c('year','onset_month'='month','soilprofile')) # Bring in soil moisture data for each year. 

# Figure: onset doy over time for all phenophases
ggplot(onset_timing, aes(x=year, y=onset_doy)) + 
  geom_jitter(aes(color=soilprofile),width=0.05, height=0, size=2) +
  #geom_smooth(aes(color=soilprofile), method='lm', se=T) + 
  scale_x_continuous(breaks=seq(2010,2020,2)) + 
  facet_wrap(spp_code~str_wrap(paste(phenophase,phenophase_desc),25), scales='free') +
  theme_bw(12)

# Figure: onset doy versus soil moisture change
ggplot(onset_timing,aes(x=avg_soil_moisture_change_with_lag, y=onset_doy, color=soilprofile)) + 
  geom_jitter(aes(color=soilprofile),width=0.05, height=0, size=2) +
  geom_smooth(method='lm') +
  geom_vline(xintercept = 0) + 
  facet_wrap(spp_code~str_wrap(paste(phenophase,phenophase_desc),25), scales='free') +
  labs(x='Average % Soil moisture change for prior 2 months',
       y='Onset DOY') +
  theme_bw(12)


#-----------------
# Correlation analysis for present/absent phenophases onset
#
# first fit model for each species/phenophase/profile
# fitting models with group_by outlined here: https://broom.tidymodels.org/articles/broom_and_dplyr.html
onset_models = onset_timing %>%
  group_by(spp_code, phenophase, phenophase_desc, soilprofile) %>%
  nest() %>%
  mutate(model = map(data, ~lm(onset_doy~avg_soil_moisture_change_with_lag, data=.))) %>%
  ungroup()

#----------------------------------
# Figure: slope coefficient for onset~soil_moisture
# extract the model coefficient using broom package and plot with CI.
onset_models %>%
  mutate(t = map(model, broom::tidy)) %>%
  unnest(t) %>%
  filter(term == 'avg_soil_moisture_change_with_lag') %>% 
  ggplot(aes(x=estimate, y=fct_rev(str_wrap(paste(phenophase,phenophase_desc),25)), color=soilprofile)) + 
  geom_point(position = position_dodge(width=0.25), size=3) + 
  geom_errorbarh(aes(xmin = estimate - std.error*1.96, xmax = estimate + std.error*1.96),
                 height=0, size=1.5, position = position_dodge(width=0.25)) + 
  #scale_y_discrete(expand = c(0,0,0,0)) + 
  geom_vline(xintercept = 0) +
  facet_wrap(spp_code~., scales='free_y', ncol=3) +
  labs(x='Slope of Soil Moisture Change - Onset relationship',y='',
       title='Soil moisture sensitivity for phenophase onset') +
  theme_bw(12)

# A table of model statics, R^2, p-value, and AIC
onset_models %>%
  mutate(t = map(model, broom::glance)) %>%
  unnest(t) %>%
  select(spp_code, soilprofile, phenophase, phenophase_desc, r.squared, p.value, AIC) %>%
  kable(format='simple')
