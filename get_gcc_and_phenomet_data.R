library(tidyverse)
library(phenometR)
library(phenocamr)

# data for all grasses
all_pg_data = phenometR::get_fg_phenophase(functional_group = 'PG')

write_csv(all_pg_data, './data/PHENOMET_PG_DATA.csv')

# data for everything at scan
all_scan_data = phenometR::get_site_phenophase(site_code = 'SC')

write_csv(all_scan_data, './data/PHENOMET_SCAN_DATA.csv')

phenocamr::download_phenocam(site='ibp$', 
                             veg_type='GR',
                             frequency='3',
                             roi_id = 1000,
                             out_dir = './data/phenocam/')
