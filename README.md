


`compile_scan_data.R` - this takes all the downloaded data from the scan site (in the folder `data/scan_downloaded_data`) and makes a monthly temperature,precip, and spei file (`./data/scan_spei_and_weather.csv`) and monthly soil moisture file  (`./data/scan_monthly_soil_moisture.csv`).

`get_gcc_and_phenomet_data.R` - this downloads all the ibp phenocam gcc data to `./data/phenocam/`, all perennial grass phenomet observations to `./data/PHENOMET_PG_DATA.csv`, and all scan site phenomet observations to `./data/PHENOMET_SCAN_DATA.csv`.

`get_ibp_weather_and_soil_data.R` - this downloads the soil moisture, precip, and temperature data for the IBP NPP site from the EDI page.  

`scan_soil_moisture_figures.R` - this uses the monthly spei and soil moisture data to make a figure showing all variables from 2010 to present, with soil moisture as a heatmap of % monthly change.

`IBP_BOER_soil_moisture_data_exploration.R` - this uses the phenocam IBP gcc data, ibp BOER observations, and LTER soil moisture data to make a nice time series for every year, where every variable is scaled 0-1.

`scan_correlation_analysis_with_spei_data.R` - this does the correlation analysis using onset dates and monthly SPEI for the different soil profiles at the SCAN site. 

`scan_correlation_analysis_with_soil_moisture_data.R` - this does the correlation analysis using onset dates and % change in soil moisture for the different soil profiles at the SCAN site. 

`download_and_plot_NMSU_SPI.R` - this downloads SPI data for the NMSU weather station and makes a timeseries plot from the year 2000 to present. 
