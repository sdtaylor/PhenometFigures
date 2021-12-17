library(tidyverse)
library(zoo)
library(patchwork)
library(cowplot)

#---------------------
# timeseries figure of IBP gcc, soil moisture, and phenomet obs
# faceted by year.
#----------------------

gcc = read_csv('data/phenocam/ibp_GR_1000_3day.csv', skip=24) %>%
  select(year, date, doy, gcc = smooth_gcc_90) %>%
  mutate(var='gcc_grass') %>%
  mutate(gcc_1st_derivative = c(NA,diff(gcc))) %>%
  filter(date <= lubridate::today())


start_date = min(gcc$date)
end_date   = max(gcc$date)

precip = read_csv('data/weather_data.csv') %>%
  #filter(date <= '2021-06-30') %>% # july 2021 was really wet and not in data yet
  filter(date >= start_date, date<=end_date) %>%
  mutate(week =  lubridate::week(date) * 7,   # need a date for plotting, so label each as the last of the 7 days. These 7 day periods do *not* alighn with sun-sat, for that use lubridate::epiweek
         year  = lubridate::year(date)) %>%
  mutate(week = ifelse(week>365, 365, week)) %>%
  group_by(year, week) %>%
  summarise(weekly_precip = sum(precip, na.rm=T)) %>%
  ungroup() %>%
  #mutate(date = as.Date(paste(year,month,'01',sep='-')))
  mutate(date = as.Date(paste(year,week,sep='-'), format='%Y-%j'))

soil_moisture = read_csv('data/soil_data.csv') %>%
  group_by(site_code, year, date, doy) %>%
  summarise(soil_moisture = mean(soil_moisture), n=n()) %>%
  ungroup() %>%
  mutate(depth='all') %>%
  as_tibble()

observed_greenness = read_csv('data/phenomet_datadump_GR_12Aug2021.csv') %>%
  phenometR::add_percent_cover_column() %>%
  rename_all(tolower) %>%
  filter(site_code=='GI', spp_code=='BOER', phenophase=='GR_202') %>%
  filter(status != -99) %>%
  group_by(site_code, spp_code, year, date, doy, phenophase) %>%
  summarise(mean_observed_greenness = mean(percent_cover),
            n_individuals = n()) %>%
  ungroup()

#--------------------------

solstice_dates = tibble(date=as.Date(paste(2013:2021,'06-21', sep = '-')))

num_days_above_0 = function(values){
  # Count the number of consecutive days values is >0, 
  # reseting whenever a 0 is encountered. 
  # Via https://stackoverflow.com/a/32502162/6615512
  
  # the ave() part does not work when NA's are present, so
  # take out and re-insert afterward.
  is_na = is.na(values)
  values[is_na] = 0
  values = values > 0
  values = ave(values, cumsum(values==0), FUN = cumsum)
  values[is_na] = NA
  return(values)
}

# rolling avg soil moisture  vars
soil_moisture = soil_moisture %>%
  arrange(depth, date) %>%
  group_by(site_code, depth) %>%
  mutate(prior_30_days_PAW = zoo::rollmean(plant_avail_water, k=30, align='right', fill=NA),
         n_days_with_PAW = num_days_above_0(plant_avail_water)) %>%
  ungroup()



#--------------------------
soil_figure = ggplot(soil_moisture, aes(x=date, y=soil_moisture, color=as.factor(depth))) + 
  geom_line(size=1.5, linetype='solid') +
  #geom_line(size=1.5, color='blue',aes(y=soil_moisture_PAW)) + 
  #geom_segment(data=avg_solstice_soil_moisture, size=2,
  #             aes(x=x_begin,xend=x_end,y=avg_soil_moisture,yend=avg_soil_moisture)) + 
  scale_x_date(date_breaks = '6 month', limits=c(start_date, end_date)) + 
  theme_bw() 

precip_figure = ggplot(precip, aes(x=date, y=weekly_precip)) + 
  geom_col(fill='deepskyblue', width=5) + 
  scale_x_date(date_breaks = '6 month', limits=c(start_date, end_date)) + 
  theme_bw() 

gcc_figure = ggplot(gcc, aes(x=date, y=gcc)) + 
  geom_line(color='seagreen', size=1.5) + 
  geom_vline(data=solstice_dates,aes(xintercept=date), color='red') + 
  scale_x_date(date_breaks = '6 month') + 
  theme_bw() 

precip_figure + soil_figure  + gcc_figure + plot_layout(ncol=1)

#---------------------------

scale_0_1 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

gcc_soil_moisture_combined = gcc %>%
  #filter(doy %in% 60:305) %>% # solstice to sep 1
  left_join(select(soil_moisture, date, soil_moisture, plant_avail_water,prior_30_days_PAW,n_days_with_PAW, depth), by='date') %>%
  left_join(select(observed_greenness, date, mean_observed_greenness, phenophase), by='date') %>%
  arrange(date) %>%
  mutate(gcc_scaled = scale_0_1(gcc), 
         soil_moisture_scaled = scale_0_1(soil_moisture),
         plant_avail_water = scale_0_1(plant_avail_water),
         prior_30_days_PAW = scale_0_1(prior_30_days_PAW),
         mean_observed_greenness_scaled = scale_0_1(mean_observed_greenness))

# max_gcc_doy = gcc_soil_moisture_combined %>%
#   group_by(year) %>%
#   slice_max(gcc) %>%
#   ungroup()
# 
# ggplot(gcc_soil_moisture_combined, aes(x=soil_moisture, y=gcc_1st_derivative, color=doy)) + 
#   geom_path() + 
#   geom_point() +
#   geom_point(data=max_gcc_doy, color='red') + 
#   scale_color_viridis_c() + 
#   facet_wrap(~year)

base_plot = ggplot(gcc_soil_moisture_combined, aes(x=doy)) + 
  geom_line(aes(y=gcc_scaled), color='yellowgreen', size=1.5) +
  geom_line(aes(y=prior_30_days_PAW, color=depth), size=1.5) +
  geom_point(aes(y=mean_observed_greenness_scaled), color='green4', size=2) +
  geom_vline(xintercept = 172, color='red') + 
  #scale_color_viridis_c() + 
  facet_wrap(~year) +
  theme_bw(20) +
  labs(y='')

legend_text = paste('IBP grass dynamics',
                    '',
                    'green line - GCC (scaled 0-1)',
                    'green points - avg BOER green % cover (GR_202)',
                    'dark blue line - soil moisture (avg 10,20,30cm, scaled 0-1)',
                    'light blue line - plant avail. water (avg 10,20,30cm, scaled 0-1)',
                    'red lines - summer solstice (june 21)',
                    sep='\n')

ggdraw(base_plot) +
  draw_text(legend_text, x=0.7, y=0.2, size=14, hjust = 0)



gcc_soil_moisture_combined %>%
  filter(doy >= 100 & doy <= 300) %>%
ggplot(aes(y=gcc_scaled, x=prior_30_days_PAW)) + 
  geom_point(aes(color=date)) +
  scale_color_viridis_c() + 
  geom_smooth(method='lm') + 
  facet_wrap(~depth)


