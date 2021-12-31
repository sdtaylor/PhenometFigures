library(tidyverse)
library(patchwork)
library(janitor)

#------------------------
# A three panel figure which compares, for every individual and year, the onset day of:
#  Grass Flower heads vs Open Flowers
#  Grass Open Flowers vs Unripe Fruit
#  Grass Unripe Fruit vs Ripe Fruit
#
# uses the perennial grass data downloaded in get_gcc_and_phenomet_data.R
#------------------------

phenophase_info = tribble(
  ~phenophase, ~phenophase_description,
  'GR_04',     'Flower heads',
  'GR_05',     'Open flowers',
  'GR_06',     'Unripe fruits',
  'GR_07',     'Ripe fruits'
)

# all grasses at all sites
grasses = read_csv('data/PHENOMET_PG_DATA.csv') %>%
  janitor::clean_names() %>%
  filter(phenophase %in% phenophase_info$phenophase) %>%
  filter(site_code %in% c('GI','SC','P9')) %>%
  filter(status %in% c(0,1))

first_event = grasses %>%
  filter(status==1) %>%
  filter(phenophase=='GR_04') %>%
  group_by(plant_id, year) %>%
  summarize(first_event = min(doy)) %>%
  ungroup()

# Make it so the onset day fo reach plant/year has a column for each phenophase
grasses_wide = grasses %>%
  filter(status==1) %>%
  left_join(first_event, by=c('plant_id','year')) %>%
  filter(doy>=first_event) %>%
  group_by(plant_id, site_code, spp_code, year, phenophase) %>%
  summarise(doy=min(doy)) %>%
  ungroup() %>%
  pivot_wider(names_from='phenophase', values_from='doy') 

# A plot template to use for the 3 sub plots
base_plot = ggplot(grasses_wide, aes(color=spp_code)) + 
  geom_abline(slope=1, intercept = 1) + 
  scale_color_brewer(palette = 'Dark2') + 
  theme_bw(12) +
  theme(legend.position = 'none')

# flower head vs open flowers
one = base_plot + 
  geom_point(aes(x=GR_04, y=GR_05), size=3) +
  labs(x='Flower Heads First DOY', y='Open Flowers First DOY')

# open flowers vs unripe fruit
two = base_plot + 
  geom_point(aes(x=GR_05, y=GR_06), size=3) +
  labs(x='Open Flowers First DOY', y='Unripe Fruits First DOY') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# unripe fruit vs ripe fruit
three = base_plot + 
  geom_point(aes(x=GR_06, y=GR_07), size=3) +
  labs(x='Unripe Fruits First DOY', y='Ripe Fruits First DOY') 

# put them all together. this uses the patchwork package
one + two + three + 
  plot_annotation(title = 'Grass Phenology Correlations. ',
                  subtitle = 'Every point is an individual plant in 1 year. Black line is 1:1 line.')

