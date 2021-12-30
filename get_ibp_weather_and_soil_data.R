library(tidyverse)

#---------------------------
# Download site data from the Jornada EDI repo
#--------------------------

#----------------------------
# Code from the EDI data portal to automatically read/parse from their links
#----------------------------
soil_data_from_url = function(url){

  dt1 <-read.csv(url,header=F 
                 ,skip=4
                 ,sep=","  
                 ,quot='"' 
                 , col.names=c(
                   "Date",     
                   "Year",     
                   "YearDay",     
                   "Hours",     
                   "RECORD",     
                   "Flag_RECORD",     
                   "Sitename",     
                   "VWC_Avg_605_10cm",     
                   "Flag_VWC_Avg_605_10cm",     
                   "EC_Avg_605_10cm",     
                   "Flag_EC_Avg_605_10cm",     
                   "Soil_Temp_Avg_605_10cm",     
                   "Flag_Soil_Temp_Avg_605_10cm",     
                   "P_Avg_605_10cm",     
                   "Flag_P_Avg_605_10cm",     
                   "Period_Avg_605_10cm",     
                   "Flag_Period_Avg_605_10cm",     
                   "Voltage_Ratio_Avg_605_10cm",     
                   "Flag_Voltage_Ratio_Avg_605_10cm",     
                   "VWC_Avg_605_20cm",     
                   "Flag_VWC_Avg_605_20cm",     
                   "EC_Avg_605_20cm",     
                   "Flag_EC_Avg_605_20cm",     
                   "Soil_Temp_Avg_605_20cm",     
                   "Flag_Soil_Temp_Avg_605_20cm",     
                   "P_Avg_605_20cm",     
                   "Flag_P_Avg_605_20cm",     
                   "Period_Avg_605_20cm",     
                   "Flag_Period_Avg_605_20cm",     
                   "Voltage_Ratio_Avg_605_20cm",     
                   "Flag_Voltage_Ratio_Avg_605_20cm",     
                   "VWC_Avg_605_30cm",     
                   "Flag_VWC_Avg_605_30cm",     
                   "EC_Avg_605_30cm",     
                   "Flag_EC_Avg_605_30cm",     
                   "Soil_Temp_Avg_605_30cm",     
                   "Flag_Soil_Temp_Avg_605_30cm",     
                   "P_Avg_605_30cm",     
                   "Flag_P_Avg_605_30cm",     
                   "Period_Avg_605_30cm",     
                   "Flag_Period_Avg_605_30cm",     
                   "Voltage_Ratio_Avg_605_30cm",     
                   "Flag_Voltage_Ratio_Avg_605_30cm",     
                   "VWC_Avg_606_10cm",     
                   "Flag_VWC_Avg_606_10cm",     
                   "EC_Avg_606_10cm",     
                   "Flag_EC_Avg_606_10cm",     
                   "Soil_Temp_Avg_606_10cm",     
                   "Flag_Soil_Temp_Avg_606_10cm",     
                   "P_Avg_606_10cm",     
                   "Flag_P_Avg_606_10cm",     
                   "Period_Avg_606_10cm",     
                   "Flag_Period_Avg_606_10cm",     
                   "Voltage_Ratio_Avg_606_10cm",     
                   "Flag_Voltage_Ratio_Avg_606_10cm",     
                   "VWC_Avg_606_20cm",     
                   "Flag_VWC_Avg_606_20cm",     
                   "EC_Avg_606_20cm",     
                   "Flag_EC_Avg_606_20cm",     
                   "Soil_Temp_Avg_606_20cm",     
                   "Flag_Soil_Temp_Avg_606_20cm",     
                   "P_Avg_606_20cm",     
                   "Flag_P_Avg_606_20cm",     
                   "Period_Avg_606_20cm",     
                   "Flag_Period_Avg_606_20cm",     
                   "Voltage_Ratio_Avg_606_20cm",     
                   "Flag_Voltage_Ratio_Avg_606_20cm",     
                   "VWC_Avg_606_30cm",     
                   "Flag_VWC_Avg_606_30cm",     
                   "EC_Avg_606_30cm",     
                   "Flag_EC_Avg_606_30cm",     
                   "Soil_Temp_Avg_606_30cm",     
                   "Flag_Soil_Temp_Avg_606_30cm",     
                   "P_Avg_606_30cm",     
                   "Flag_P_Avg_606_30cm",     
                   "Period_Avg_606_30cm",     
                   "Flag_Period_Avg_606_30cm",     
                   "Voltage_Ratio_Avg_606_30cm",     
                   "Flag_Voltage_Ratio_Avg_606_30cm",     
                   "VwcCorr_Avg_605_10cm",     
                   "Flag_VwcCorr_Avg_605_10cm",     
                   "VwcCorr_Avg_605_20cm",     
                   "Flag_VwcCorr_Avg_605_20cm",     
                   "VwcCorr_Avg_605_30cm",     
                   "Flag_VwcCorr_Avg_605_30cm",     
                   "VwcCorr_Avg_606_10cm",     
                   "Flag_VwcCorr_Avg_606_10cm",     
                   "VwcCorr_Avg_606_20cm",     
                   "Flag_VwcCorr_Avg_606_20cm",     
                   "VwcCorr_Avg_606_30cm",     
                   "Flag_VwcCorr_Avg_606_30cm"    ), check.names=TRUE)
  
  return(dt1)
}




met_data_from_url = function(url){

    dt1 <-read.csv(url,header=F 
                 ,skip=4
                 ,sep=","  
                 ,quot='"' 
                 , col.names=c(
                   "Date",     
                   "Year",     
                   "YearDay",     
                   "Hours",     
                   "RECORD",     
                   "Flag_RECORD",     
                   "Sitename",     
                   "Batt_V_Min",     
                   "Flag_Batt_V_Min",     
                   "Batt_V_TMn",     
                   "Ptemp_C_Avg",     
                   "Flag_Ptemp_C_Avg",     
                   "Air_TempC_Avg",     
                   "Flag_Air_TempC_Avg",     
                   "Air_TempC_Max",     
                   "Flag_Air_TempC_Max",     
                   "Air_TempC_Min",     
                   "Flag_Air_TempC_Min",     
                   "Relative_Humidity_Avg",     
                   "Flag_Relative_Humidity_Avg",     
                   "Relative_Humidity_Max",     
                   "Flag_Relative_Humidity_Max",     
                   "Relative_Humidity_Min",     
                   "Flag_Relative_Humidity_Min",     
                   "Ppt_mm_Tot",     
                   "Flag_Ppt_mm_Tot",     
                   "Ws_Mean_300cm",     
                   "Flag_Ws_Mean_300cm",     
                   "Ws_mean_Resultant_300cm",     
                   "Flag_Ws_mean_Resultant_300cm",     
                   "WinDir_mean_Resultant",     
                   "Flag_WinDir_mean_Resultant",     
                   "WinDir_Std_Dev",     
                   "Flag_WinDir_Std_Dev",     
                   "WS_ms_75cm_Avg",     
                   "Flag_WS_ms_75cm_Avg",     
                   "WS_ms_150cm_Avg",     
                   "Flag_WS_ms_150cm_Avg",     
                   "WS_ms_300cm_Avg",     
                   "Flag_WS_ms_300cm_Avg",     
                   "WS_ms_75cm_Max",     
                   "Flag_WS_ms_75cm_Max",     
                   "WS_ms_150cm_Max",     
                   "Flag_WS_ms_150cm_Max",     
                   "WS_ms_300cm_Max",     
                   "Flag_WS_ms_300cm_Max",     
                   "WS_ms_75cm_Min",     
                   "Flag_WS_ms_75cm_Min",     
                   "WS_ms_150cm_Min",     
                   "Flag_WS_ms_150cm_Min",     
                   "WS_ms_300cm_Min",     
                   "Flag_WS_ms_300cm_Min"    ), check.names=TRUE)

  return(dt1)
}

#----------------------------
# Precip and temp data
# Official QA'd version as of 13 Nov 2021
# knb-lter-jrn.210437050.23
ibp_met_link  = "https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210437050/23/128f244ea3df3d72701cb9aa4cdc8be6" 

ibp = met_data_from_url(ibp_met_link)
ibp$site_code = 'GI'

ibp %>%
 # bind_rows(p9) %>%
#  bind_rows(no) %>%
  janitor::clean_names() %>% 
  select(site_code, date, year, doy = year_day, temp = air_temp_c_avg, temp_flag = flag_air_temp_c_avg, precip = ppt_mm_tot, precip_flag = flag_ppt_mm_tot) %>%
  write_csv('data/ibp_weather_data.csv')


#--------------
# Soil data
# Official QA'd version as of 13 Nov 2021
# knb-lter-jrn.210437095.24

ibp_soil_link  = 'https://pasta.lternet.edu/package/data/eml/knb-lter-jrn/210437095/24/10b8283a3b005fa1c9620f4d76373cbb'

ibp = soil_data_from_url(ibp_soil_link)
ibp$site_code = 'GI'

ibp %>%
  janitor::clean_names() %>% 
  select(site_code, date, year, doy=year_day, starts_with('vwc_avg_')) %>% 
  pivot_longer(starts_with('vwc_avg'), names_to='variable', values_to = 'value') %>%
  separate('variable', sep='_', into=c('variable','stat','probe_id','depth')) %>%
  group_by(site_code, date, year, doy, stat, depth) %>%
  summarise(soil_moisture = mean(value)) %>%
  ungroup() %>%
  write_csv('data/ibp_soil_data.csv')
