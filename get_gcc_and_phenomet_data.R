library(tidyverse)
library(phenometR)
library(phenocamr)

all_pg_data = phenometR::get_fg_phenophase(functional_group = 'PG')

write_csv(all_pg_data, './data/PHENOMET_PG_DATA.csv')

phenocamr::download_phenocam(site='ibp$', 
                             veg_type='GR',
                             frequency='3',
                             roi_id = 1000,
                             out_dir = './data/phenocam/')
