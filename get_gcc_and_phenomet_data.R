library(tidyverse)
library(phenometR)
library(phenocamr)

phenomet_data = phenometR::get_fg_phenophase(functional_group = 'PG')

phenomet_data = phenomet_data %>%
  filter(SITE_CODE=='GI', SPP_CODE=='BOER', PHENOPHASE=='GR_202')

write_csv(phenomet_data, './data/GI_BOER_GR202_data.csv')

phenocamr::download_phenocam(site='ibp$', 
                             veg_type='GR',
                             frequency='3',
                             roi_id = 1000,
                             out_dir = './data/phenocam/')
