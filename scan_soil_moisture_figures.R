library(tidyverse)
library(patchwork)

#----------------------------------------------

scan_spei_and_weather_data = read_csv('./scan/scan_spei_and_weather.csv')
soil_moisture_monthly_average = read_csv('./scan/scan_monthly_soil_moisture.csv')

#----------------------------------------------
spei_fig = ggplot(scan_spei_and_weather_data, aes(x=month_date, y=spei)) +
  geom_col(aes(fill=spei<0)) +
  scale_fill_manual(values=c('blue','red')) + 
  scale_x_date(date_breaks = '6 month', limits = as.Date(c('2009-01-01','2021-12-31'))) +
  theme_bw() + 
  theme(axis.text = element_text(color='black'),
        axis.text.x = element_text(angle = -45, hjust=0),
        legend.position = 'none') +
  labs(y='Monthly SPEI', x='')

precip_fig = ggplot(scan_spei, aes(x=month_date, y=spei)) +
  geom_col(aes(fill=spei<0)) +
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
  ggplot(aes(x=month_date, y=as.factor(depth), fill=avg_soil_moisture_change)) + 
    geom_tile(height=0.8, width=25) + 
    #scale_fill_gradient2(low='red', mid='white', high='blue',midpoint=15) + 
    scale_fill_viridis_c(option='D', na.value='white') + 
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
spei_fig + soil_moisture_fig1 + soil_moisture_fig2 + soil_moisture_fig3 + plot_layout(ncol=1)


#----------------------------------------------
# scatter splot of spei x soil moisture

all_data = soil_moisture_monthly_average %>%
  left_join(scan_spei_and_weather_data, by=c('year','month','month_date')) %>%
  mutate(depth = fct_rev(as_factor(depth)))

ggplot(all_data, aes(x=spei, y=avg_soil_moisture_change)) + 
  geom_point() +
  facet_grid(depth~profile, label=label_both)



