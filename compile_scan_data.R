library(tidyverse)
library(janitor)
library(SPEI)

#-----------------------------------------------
# Read in yearly data files downloaded from https://wcc.sc.egov.usda.gov/nwcc/site?sitenum=2168
# and make two files for soil moisture and air temp/precip/spei

read_scan_datafile = function(datafile_path){
  # Read the downloaded files, clean up column names, and pivot so
  # columns are site_id, date, measurement, value
  read_csv(datafile_path, skip = 4) %>%
    janitor::clean_names() %>%
    select(-time) %>% # time column not needed for daily data
    pivot_longer(-c('site_id','date'), names_to = 'measurement', values_to='value')
}

all_scan_files = list.files('./data/scan_downloaded_data/',pattern = '2168_ALL_YEAR*', full.names = T)

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

all_months = tibble(month_date = seq(as.Date('2010-01-15'), as.Date('2021-12-15'), by='1 month')) 

# calculate change in % soil moisture
soil_moisture_monthly_average = soil_moisture_monthly_average %>%
  full_join(all_months, by=c('month_date')) %>%    # A bit cumbersome. These 3 lines introduce NA's where
  complete(profile, depth, month_date) %>%         # there is missing values for each profile and depth
  mutate(year = lubridate::year(month_date), month=lubridate::month(month_date)) %>%
  filter((!is.na(profile)) & (!is.na(depth))) %>%
  group_by(profile, depth) %>%          # change in soil % for each profile/depth

  arrange(month_date) %>%
  mutate(avg_soil_moisture_change = avg_soil_moisture - lag(avg_soil_moisture)) %>%
  ungroup()

#---------------------------------
# calculate SPEI from the precip and tmin/tmax and the SPEI pakage. 
scan_monthly_precip = scan_data %>%
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

# inches to mm
scan_monthly_precip$monthly_precip = scan_monthly_precip$monthly_precip * 25.4

scan_monthly_temperature = scan_data %>%
  filter(value != -99.9) %>%
  filter(measurement %in% c('tmax_d_1_deg_c','tmin_d_1_deg_c')) %>%
  separate(measurement, c('measurement'), sep='_', extra='drop') %>% # make values either tmax or tmin
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         month_date = lubridate::floor_date(date, unit='month') + lubridate::days(14)) %>%
  group_by(year, month, month_date, measurement) %>%
  summarise(monthly_avg = mean(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = 'measurement', values_from = 'monthly_avg') %>%
  add_row(year=2020, month=9, month_date=as.Date('2020-09-15'), tmin=NA, tmax=NA) %>%   # Missing temp data for 2020 Sep/Oct, so need NA
  add_row(year=2020, month=10, month_date=as.Date('2020-10-15'), tmin=NA, tmax=NA) %>%  # values for SPEI to work correctly
  arrange(month_date)

# SPEI assumes data are monthly values, with the first values in Jan, which Scan data is (starting Jan 2010)
pet = SPEI::hargreaves(Tmin = scan_monthly_temperature$tmin, 
                       Tmax = scan_monthly_temperature$tmax,
                       lat = 32.5566, na.rm=T)
scan_monthly_temperature$pet = as.vector(pet)

scan_spei_and_weather_data = scan_monthly_temperature %>%
  left_join(scan_monthly_precip, by=c('year','month','month_date')) %>%
  mutate(climatic_balance = monthly_precip - pet)

spei = SPEI::spei(scan_spei_and_weather_data$climatic_balance, scale=1, na.rm=T)
scan_spei_and_weather_data$spei = as.vector(spei$fitted)


#--------------------------------
write_csv(soil_moisture_monthly_average, './data/scan_monthly_soil_moisture.csv')
write_csv(scan_spei_and_weather_data, './data/scan_spei_and_weather.csv')
