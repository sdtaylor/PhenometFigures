library(tidyverse)
library(janitor)

#------------------------
# A three panel figure which compares using box plots the duration, onset,
# and end for GR_04- Flower heads for perennial grasses and for sites
# GI, SC, and P9
# 
# uses the perennial grass data downloaded in get_gcc_and_phenomet_data.R
#------------------------


grasses = read_csv('data/PHENOMET_PG_DATA.csv') %>%
  janitor::clean_names() %>%
  filter(phenophase %in% c('GR_04')) %>%
  filter(site_code %in% c('GI','SC','P9')) %>%
  filter(status %in% c(0,1))

# The onset, end, and duration for each individual plant
phenophase_doys = grasses %>%
  filter(status==1) %>%
  group_by(plant_id,spp_code,site_code, year) %>%
  summarise(fl_onset = min(doy),
            fl_end   = max(doy)) %>%
  ungroup() %>%
  mutate(fl_duration = fl_end - fl_onset)

# Plot using box plots.
phenophase_doys %>%
  pivot_longer(starts_with('fl_'), names_to='metric', values_to='metric_value') %>%
ggplot(aes(x=metric_value, y=spp_code)) +
  geom_boxplot(fill='transparent', outlier.colour = 'transparent') + 
  geom_jitter(aes(color=site_code),width=0, height = 0.2, alpha=0.5, size=2) + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_x_continuous(breaks = seq(0,300,25)) + 
  facet_wrap(~metric, ncol=1) +
  theme_bw(15) +
  labs(y='Species', x='DOY or Duration', title='Phenology metrics from GR_04')
