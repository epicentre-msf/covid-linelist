
### Required libraries
library(readxl)
library(dplyr)
library(here)
library(glue)
source("R/utilities.R")


### Set global paths
path_project <- here::here()
path_onedrive <- path.expand("~/MSF/GRP-EPI-COVID-19 - NCoVEpi")
path_data_raw <- file.path(path_onedrive, "data-raw/linelist")
path_cleaning <- file.path(path_data_raw, "cleaning")
path_dictionaries <- file.path(path_cleaning, "dictionaries")
path_shapefiles <- file.path(path_onedrive, "data/shapefiles")
path_export <- file.path(path_onedrive, "data/linelist/HIS-export")
path_export_fp <- file.path(path_onedrive, "coordination/Surveillance focal points coordination/Data compilation")
path_export_global <- file.path(path_onedrive, "data/linelist/world")
path_dict_linelist <- file.path(path_onedrive, "template/linelist/dev/dico")
path_dict_countries <- file.path(path_onedrive, "template/linelist/dev/base_geo/WORLD_1_20200403.xlsx")
path_queries <- file.path(path_onedrive, "data-cleaning/queries")
path_date_figs <- file.path(path_onedrive, "data-cleaning/date-figures")
path_geobase <- file.path(path_onedrive, "template/linelist/dev/base_geo")
path_implementation <- file.path(path_onedrive, "coordination/Surveillance focal points coordination/Logbook LL implementation")

### Read dictionaries
dict_linelist <- read_xlsx(file.path(path_dictionaries, "dict_linelist_v1.1.xlsx")) # corrected MSF_readmission -> MSF_visit_type
dict_facilities <- read_xlsx(file.path(path_dictionaries, "dict_facilities.xlsx"))
dict_factors <- read_xlsx(file.path(path_dictionaries, "dict_factors.xlsx"))
dict_countries <- read_xlsx(path_dict_countries)
dict_extra_vars <- read_xlsx(file.path(path_dictionaries, "dict_extra_vars.xlsx"))
dict_date_categories <- read_xlsx(file.path(path_dictionaries, "dict_date_categories.xlsx"))
date_vars <- c("upload_date", dict_linelist$code_name[dict_linelist$data_type == "Date"])


