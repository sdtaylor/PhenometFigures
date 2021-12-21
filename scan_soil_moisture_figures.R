library(tidyverse)
library(janitor)
library(patchwork)

#-----------------------------------------------
# Read in yearly data files downloaded from https://wcc.sc.egov.usda.gov/nwcc/site?sitenum=2168

read_scan_datafile = function(datafile_path){
  # Read the downloaded files, clean up column names, and pivot so
  # columns are site_id, date, measurement, value
 read_csv(datafile_path, skip = 4) %>%
  janitor::clean_names() %>%
  select(-time) %>% # time column not needed for daily data
  pivot_longer(-c('site_id','date'), names_to = 'measurement', values_to='value')
}

all_scan_files = list.files('./scan/data/',pattern = '2168_ALL_YEAR*', full.names = T)

scan_data = purrr::map_df(all_scan_files, read_scan_datafile)

#-----------------------------------------------
# Pull soil moisture measurements from data. Note column names were changed using janitor::clean_names()
# and pivoted into a new column called 'measurement'.

scan_soil_moisture = scan_data %>%
  filter(value != -99.9) %>%
  filter(str_detect(measurement,'sms_*')) %>%
  separate(measurement,  # soil moist variables are like: sms_i_1_8_pct_loam, sms_i_{profile_number)_{depth}_pct_loam
           c('drop1','drop2','profile','depth','drop3','drop4'), 
           convert=TRUE,
           sep='_',) %>%  
  select(-starts_with('drop')) %>%
  mutate(depth = depth * -1)

# monthly average for all years.
soil_moisture_monthly_average = scan_soil_moisture %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         month_date = lubridate::floor_date(date, unit='month') + lubridate::days(14)) %>% # Have a column for the 15th of the respective month for easier plotting
  group_by(profile, depth, year, month, month_date) %>%
  summarise(avg_soil_moisture = mean(value), n_days=n()) %>%
  ungroup() %>%
  filter(n_days >= 20) # only have monthly averages with > 20 days of data

#---------------------------------
# scan precipitation
scan_precip_monthly_average = scan_data %>%
  filter(value != -99.9) %>%
  filter(value >= 0) %>%   # some neg numbers besides -99.9 in the precip data.
  filter(measurement=='prcp_d_1_in') %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         month_date = lubridate::floor_date(date, unit='month') + lubridate::days(14)) %>%
  group_by(year, month, month_date) %>%
  summarise(monthly_precip = sum(value),
            n_days = n()) %>%
  ungroup()

# calculate precip annomoly every month (unit still in inches)
scan_precip_monthly_average = scan_precip_monthly_average %>%
  group_by(month) %>%
  mutate(mean_monthly_precip = mean(monthly_precip)) %>%     # the ~11 year per month average.
  ungroup() %>%
  mutate(monthly_annomoly = monthly_precip - mean_monthly_precip)

precip_fig1 = ggplot(scan_precip_monthly_average, aes(x=month_date, y=monthly_precip)) +
  geom_col(fill='darkblue') +
  scale_x_date(date_breaks = '6 month', limits = as.Date(c('2009-01-01','2021-12-31'))) +
  theme_bw() + 
  theme(axis.text = element_text(color='black'),
        axis.text.x = element_blank(),
        legend.position = 'none') +
  labs(y='monthly precip (in)', x='')

precip_fig2 = ggplot(scan_precip_monthly_average, aes(x=month_date, y=monthly_annomoly)) +
  geom_col(aes(fill=monthly_annomoly<0)) +
  scale_fill_manual(values=c('blue','red')) + 
  scale_x_date(date_breaks = '6 month', limits = as.Date(c('2009-01-01','2021-12-31'))) +
  theme_bw() + 
  theme(axis.text = element_text(color='black'),
        axis.text.x = element_text(angle = -45, hjust=0),
        legend.position = 'none') +
  labs(y='monthly precip\nannomoly (in)', x='')

#----------------------------------------------
# soil moisture as a heatmap/timeseries

get_profile_soil_moisture_figure = function(this_profile=1){
  soil_moisture_monthly_average %>%
    filter(profile==this_profile) %>%
  ggplot(aes(x=month_date, y=as.factor(depth), fill=avg_soil_moisture)) + 
    geom_tile(height=0.8, width=25) + 
    #scale_fill_gradient2(low='red', mid='white', high='blue',midpoint=15) + 
    scale_fill_viridis_c(option='D') + 
    scale_x_date(date_breaks = '6 month', limits = as.Date(c('2009-01-01','2021-12-31'))) + 
    facet_wrap(~profile, scale='free', ncol=1, strip.position = 'right', labeller = label_both) +
    theme_bw() + 
    theme(axis.text = element_text(color='black'),
          axis.text.x = element_text(angle = -45, hjust=0)) +
    labs(fill='soil moisture %', y='Depth', x='')
}

soil_moisture_fig1 = get_profile_soil_moisture_figure(1)
soil_moisture_fig2 = get_profile_soil_moisture_figure(2)
soil_moisture_fig3 = get_profile_soil_moisture_figure(3)


# all figures together
precip_fig1 + precip_fig2 + soil_moisture_fig1 + soil_moisture_fig2 + soil_moisture_fig3 + plot_layout(ncol=1)

