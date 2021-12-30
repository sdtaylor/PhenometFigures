library(tidyverse)

#-------------------------
# Get the latest drought data from https://www.ncdc.noaa.gov/temp-and-precip/drought/nadm/indices 
# for the Las Cruces NMSU station and put into a nice format
#---------------------------

nmsu_station = 'USC00298535'

spi_12month_link = 'https://www.ncdc.noaa.gov/monitoring-content/temp-and-precip/drought/nadm/indices/spi/data/12mon-spi-us-stn-lower48.txt'
spi_6month_link  = 'https://www.ncdc.noaa.gov/monitoring-content/temp-and-precip/drought/nadm/indices/spi/data/06mon-spi-us-stn-lower48.txt'
spi_3month_link  = 'https://www.ncdc.noaa.gov/monitoring-content/temp-and-precip/drought/nadm/indices/spi/data/03mon-spi-us-stn-lower48.txt'


process_drought_file = function(upstream_link, stations, variable, timeframe){
  spi_colnames = c('station_id','element_id','year', month.abb)
  
  read_table(upstream_link, col_names = spi_colnames) %>%
    filter(station_id %in% stations) %>%
    pivot_longer(month.abb, names_to='month',values_to='spi') %>%
    mutate(spi = as.numeric(spi)) %>%
    mutate(variable=variable, timeframe=timeframe)  %>%
    mutate(date = as.Date(paste(year,month,'01',sep='-'), format='%Y-%b-%d')) 
}

#--------------------------------
# from 6 month SPI:
#      June = Jan-jun
first = process_drought_file(spi_6month_link, stations = nmsu_station, variable = 'spi',timeframe = '6month')  %>%
  filter(month == 'Jun') %>%
  mutate(months = case_when(
    month == 'Jun' ~ 'JFMAMJ',
  )) %>% 
  select(station_id, year, months, timeframe, spi) 

#-----------------------------------
# from 3 month SPI:
#      June = April/May/June
#      Mar  = Jan/Feb/Mar

second = bind_rows(process_drought_file(spi_3month_link, stations = nmsu_station, variable = 'spi',timeframe = '3month')) %>%
  filter(month %in% c('Jun','Mar')) %>%
  mutate(months = case_when(
    month == 'Jun' ~ 'AMJ',
    month == 'Mar' ~ 'JFM',
  )) %>%
  select(station_id, year, months, timeframe, spi) 

#-------------------------------------
# Combine the 3 and 6 month SPI time series, save it, and make a simple figure.
both = first %>%
  bind_rows(second) %>%
  mutate(spi = ifelse(spi==-99.99, NA, spi))

write_csv(both, './data/nmsu_spi.csv')

# Both 3 and 6 month SPI
ggplot(both, aes(x=year, y=spi, color=months)) + 
  geom_line() + 
  facet_wrap(~timeframe, ncol=1)

# Only the 6 month SPI
both %>%
  filter(timeframe == '6month') %>%
  ggplot(aes(x=year, y=spi)) +
  geom_line(size=2) +
  geom_point(size=4) +
  geom_hline(yintercept=0) +
  scale_x_continuous(limits = c(2000,2020), breaks=seq(2000,2020,2)) +
  theme_bw(20) +
  labs(x='', y='Jan-Jun SPI')



