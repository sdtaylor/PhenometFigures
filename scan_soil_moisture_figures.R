library(tidyverse)
library(janitor)


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
# and pivoted to values into a new column.

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
monthly_average = scan_soil_moisture %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         month_date = lubridate::floor_date(date, unit='month') + lubridate::days(14)) %>% # Have a column for the 15th of the respective month for easier plotting
  group_by(profile, depth, year, month, month_date) %>%
  summarise(avg_soil_moisture = mean(value), n_days=n()) %>%
  ungroup() %>%
  filter(n_days >= 20) # only have monthly averages with > 20 days of data

# soil moisture as a heatmap/timeseries
ggplot(monthly_average, aes(x=month_date, y=as.factor(depth), fill=avg_soil_moisture)) + 
  geom_tile(height=0.8, width=30) + 
  scale_fill_gradient2(low='red', mid='white', high='blue',midpoint=10) + 
  scale_x_date(date_breaks = '6 month') + 
  facet_wrap(~profile, scale='free') +
  theme_bw() + 
  theme(axis.text = element_text(color='black'),
        axis.text.x = element_text(angle = -45, hjust=0))

# soil moisture as
monthly_average %>%
  mutate(depth = fct_rev(as_factor(depth))) %>% # make depth have the shallowest value on top of the facet
ggplot(aes(x=month, y=avg_soil_moisture, color=year, group=year)) + 
  geom_line() + 
  scale_color_viridis_c(breaks=2010:2020) + 
  facet_grid(depth~profile, labeller = label_both)
