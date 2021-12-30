library(tidyverse)
library(cowplot)

#---------------------
# timeseries figure of IBP phenocam gcc, soil moisture, and phenomet obs for BOE
# faceted by year.
# Data is downloaded and compiled in:
#    get_gcc_and_phenomet_data.R
#    get_weather_and_soil_data.R

#----------------------

gcc = read_csv('data/phenocam/ibp_GR_1000_3day.csv', skip=24) %>%
  select(year, date, doy, gcc = smooth_gcc_90) %>%
  mutate(var='gcc_grass') %>%
  mutate(gcc_1st_derivative = c(NA,diff(gcc))) %>%
  filter(date <= lubridate::today()) %>%
  filter(year>=2014) # not much gcc or soil data before 2014


start_date = min(gcc$date)
end_date   = max(gcc$date)

# Read the  precip data and calculate the total for each week. 
precip = read_csv('data/ibp_weather_data.csv') %>%
  filter(date >= start_date, date<=end_date) %>%
  mutate(week =  lubridate::week(date) * 7,   # need a date for plotting, so label each as the last of the 7 days. These 7 day periods do *not* align with sun-sat, for that use lubridate::epiweek
         year  = lubridate::year(date)) %>%
  mutate(week = ifelse(week>365, 365, week)) %>%
  group_by(year, week) %>%
  summarise(weekly_precip = sum(precip, na.rm=T)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year,week,sep='-'), format='%Y-%j'))

# Read soil moisture data and get the average among all three depths.
soil_moisture = read_csv('data/ibp_soil_data.csv') %>%
  group_by(site_code, year, date, doy) %>%
  summarise(soil_moisture = mean(soil_moisture), n=n()) %>%
  ungroup() %>%
  mutate(depth='all') %>%
  as_tibble()

# Read the phenomet data and filter to BOER, GR_202 at GI.
# calculate the mean percent_green at the site for each
# observation date. 
observed_greenness = read_csv('data/PHENOMET_PG_DATA.csv') %>%
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

# A function to scale values from 0-1 based on their relative min/max.
scale_0_1 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

# Combine all 3 variables into a single data.frame and apply the
# scaling to each for plotting.
gcc_soil_moisture_combined = gcc %>%
  left_join(select(soil_moisture, date, soil_moisture, depth), by='date') %>%
  left_join(select(observed_greenness, date, mean_observed_greenness, phenophase), by='date') %>%
  arrange(date) %>%
  mutate(gcc_scaled = scale_0_1(gcc), 
         soil_moisture_scaled = scale_0_1(soil_moisture),
         mean_observed_greenness_scaled = scale_0_1(mean_observed_greenness))

base_plot = ggplot(gcc_soil_moisture_combined, aes(x=doy)) + 
  geom_line(aes(y=gcc_scaled), color='yellowgreen', size=1.5) +
  geom_line(aes(y=soil_moisture_scaled), color='blue', size=1.5) +
  geom_point(aes(y=mean_observed_greenness_scaled), color='green4', size=2) +
  geom_vline(xintercept = 172, color='red') + 
  scale_x_continuous(limits = c(50,300)) +    # constrain to growing season, mid-Feb - late Oct.
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

# ggdraw from the cowplot package to combine the plot and custom legend text.
ggdraw(base_plot) +
  draw_text(legend_text, x=0.7, y=0.2, size=14, hjust = 0)



