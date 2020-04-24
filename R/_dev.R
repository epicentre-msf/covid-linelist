

################################################################################
## Set global directories/paths
################################################################################

library(here)
library(glue)

path_project <- here::here()
setwd(path_project)

path_rds <- file.path(path_project, "rds")
path_data <- file.path(path_project, "data")

path_onedrive <- path.expand("~/MSF/GRP-EPI-COVID-19 - NCoVEpi")

path_data_raw <- file.path(path_onedrive, "data-raw/linelist")
path_data_write <- file.path(path_onedrive, "data/linelist")

path_cleaning <- file.path(path_data_raw, "cleaning")
path_dictionaries <- file.path(path_cleaning, "dictionaries")

dict_factors <- readxl::read_xlsx(file.path(path_dictionaries, "dict_factors.xlsx"))
dict_facilities <- readxl::read_xlsx(file.path(path_dictionaries, "dict_facilities.xlsx"))

path_export <- file.path(path_onedrive, "data/linelist/HIS-export")

ll_template <- names(readxl::read_xlsx(file.path(path_dictionaries, "ll_template_v1.1.xlsx")))

path_shapefiles <- file.path(path_onedrive, "data/shapefiles")




################################################################################
#### Update compiled linelist
## Import and combine linelists from each site, clean and check variables,
## geo-match, and write resulting cleaned linelist files
################################################################################

source("R/update.R")

d <- update_linelist(path_data_raw = path_data_raw,
                     path_shapefiles = path_shapefiles,
                     dict_facilities = dict_facilities,
                     dict_factors = dict_factors,
                     ll_template = ll_template,
                     country = "AFG")


## write cleaned linelist files to Sharepoint data directory
# path_out_covid_ll <- file.path(path_export, glue("msf_covid_linelist_afg_{lubridate::today()}.xlsx"))
# path_out_covid_ll_feather <- file.path(path_export, glue("msf_covid_linelist_afg_{lubridate::today()}.feather"))
# 
# writexl::write_xlsx(d, path_out_covid_ll)
# feather::write_feather(d, path_out_covid_ll_feather)

OC <- "oca"

path_out_oc <- file.path(path_export, OC, glue("msf_covid19_linelist_{OC}_{lubridate::today()}"))

write.csv(d, paste0(path_out_oc, ".csv"), row.names = FALSE)
write_pretty_xlsx(d, paste0(path_out_oc, ".xlsx"))

