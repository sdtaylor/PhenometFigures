library(tidyverse)
library(patchwork)

#----------------------------------------------

scan_spei_and_weather_data = read_csv('./scan/scan_spei_and_weather.csv')
soil_moisture_monthly_average = read_csv('./scan/scan_monthly_soil_moisture.csv')


solstice_dates = tibble(solstice = as.Date(paste(2010:2021, '-06-21', sep = '')))

#----------------------------------------------
spei_fig = ggplot(scan_spei_and_weather_data, aes(x=month_date, y=spei)) +
  geom_col(aes(fill=spei<0)) +
  geom_vline(data=solstice_dates, aes(xintercept=solstice), color='gold') + 
  scale_fill_manual(values=c('blue','red')) + 
  scale_x_date(date_breaks = '6 month', limits = as.Date(c('2009-01-01','2021-12-31'))) +
  theme_bw() + 
  theme(axis.text = element_text(color='black'),
        axis.text.x = element_text(angle = -45, hjust=0),
        legend.position = 'none') +
  labs(y='Monthly SPEI', x='')

#----------------------------------------------
# soil moisture as a heatmap/timeseries

get_profile_soil_moisture_figure = function(this_profile=1){
  soil_moisture_monthly_average %>%
    filter(profile==this_profile) %>%
  ggplot(aes(x=month_date, y=as.factor(depth), fill=avg_soil_moisture_change)) + 
    geom_tile(height=0.8, width=25) + 
    geom_vline(data=solstice_dates, aes(xintercept=solstice), color='gold') + 
    scale_fill_gradient2(low='firebrick', mid='lightyellow', high='darkgreen',limits=c(-10,10),midpoint=0, na.value='white') + 
    #scale_fill_viridis_c(option='D', na.value='white') + 
    scale_x_date(date_breaks = '6 month', limits = as.Date(c('2009-01-01','2021-12-31'))) + 
    facet_wrap(~profile, scale='free', ncol=1, strip.position = 'right', labeller = label_both) +
    theme_bw() + 
    theme(axis.text = element_text(color='black'),
          axis.text.x = element_text(angle = -45, hjust=0)) +
    labs(fill='Monthly change in\nsoil moisture %', y='Depth', x='')
}

soil_moisture_fig1 = get_profile_soil_moisture_figure(1)
soil_moisture_fig2 = get_profile_soil_moisture_figure(2)
soil_moisture_fig3 = get_profile_soil_moisture_figure(3)


# all figures together
main_figure1 = spei_fig + soil_moisture_fig1 + soil_moisture_fig2 + soil_moisture_fig3 + plot_layout(ncol=1)

ggsave('./scan_soil_moisture_and_spei_timeseries.png', main_figure1, dpi=300, width=30, height=25, units='cm')


#----------------------------------------------
# scatter splot of spei x soil moisture

all_data = soil_moisture_monthly_average %>%
  left_join(scan_spei_and_weather_data, by=c('year','month','month_date')) %>%
  mutate(depth = fct_rev(as_factor(depth)))

main_figure2 = ggplot(all_data, aes(x=spei, y=avg_soil_moisture_change)) + 
  geom_point() +
  geom_hline(yintercept = 0, color='red') + 
  facet_grid(depth~profile, label=label_both) +
  theme_bw(15) +
  labs(x='Monthly SPEI', y='Monthly change in soil moisture %')

ggsave('./scan_soil_moisture_and_spei_scatterplot.png', main_figure2, dpi=300, width=20, height=20, units='cm')


