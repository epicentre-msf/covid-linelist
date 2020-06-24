
### Required libraries
library(tidyverse)
library(here)
library(readxl)
library(stringr)
library(janitor)
library(glue)
library(matchmaker) # remotes::install.github("patrickbarks/matchmaker")
library(hmatch)     # remotes::install.github("epicentre-msf/hmatch")
source("R/utilities.R")



### Set global paths
path_project <- here::here()
path_onedrive <- path.expand("~/MSF/GRP-EPI-COVID-19 - NCoVEpi")
path_data_raw <- file.path(path_onedrive, "data-raw/linelist")
path_cleaning <- file.path(path_onedrive, "data-cleaning")
path_dictionaries <- file.path(path_cleaning, "dictionaries")
path_corrections <- file.path(path_cleaning, "manual-corrections")
path_corrections_dates <- file.path(path_corrections, "dates")
path_corrections_geocodes <- file.path(path_corrections, "geocodes")
path_shapefiles <- file.path(path_onedrive, "data/shapefiles")
path_export <- file.path(path_onedrive, "data/linelist/HIS-export")
path_export_fp <- file.path(path_onedrive, "coordination/Surveillance focal points coordination/Data compilation")
path_export_global <- file.path(path_onedrive, "data/linelist/world")
path_dict_linelist <- file.path(path_onedrive, "template/linelist/dev/dico")
path_dict_countries <- file.path(path_onedrive, "template/linelist/dev/base_geo/WORLD_1_20200403.xlsx")
path_queries <- file.path(path_cleaning, "queries")
path_date_figs <- file.path(path_cleaning, "date-figures")
path_geobase <- file.path(path_onedrive, "template/linelist/dev/base_geo")
path_implementation <- file.path(path_onedrive, "coordination/Surveillance focal points coordination/Logbook LL implementation")

### Other linelists (AFG triage, YEM, etc.)
path_linelist_other <- file.path(path_onedrive, "data-raw/linelist-other")
path_afg_triage <- file.path("~/msf/GRP-EPI-PROJ-COVID-19-AFG - triage_HRH_IDP")



### Compare v1 and v2 dictionaries
# setdiff(dict_linelist$code_name, dict_linelist_v1$code_name)
# setdiff(dict_linelist_v1$code_name, dict_linelist$code_name)
# 
# dict_linelist_v1 %>%
#   select(code_name, data_type1 = data_type) %>%
#   full_join(dict_linelist) %>%
#   select(code_name, data_type1, data_type2 = data_type) %>%
#   filter(data_type1 != data_type2)

# ll1 <- "linelist_Covid_anonymous__MLI__OCBA__106__Structure de santé__CSRef Douentza__5799__2020-06-23_15-37.xlsb"
# d1 <- readxlsb::read_xlsb(file.path(path_data_raw, "MLI", ll1), sheet = "linelist", col_types = "string") %>% 
#   select(-starts_with("Ajouter"))
# 
# ll2 <- "linelist_Covid_anonymous__VEN__OCBA__103__Hospital__HOSPITAL VARGAS DE CARACAS__5799__2020-06-23_20-21.xlsb"
# d2 <- readxlsb::read_xlsb(file.path(path_data_raw, "VEN", ll2), sheet = "linelist", col_types = "string") %>% 
#   select(-starts_with("Añadir"), -(Atencion.de.SM:X0.1))
# 
# setdiff(names(d1), dict_linelist$code_name)
# setdiff(dict_linelist$code_name, names(d1))
# 
# setdiff(names(d2), dict_linelist$code_name)
# setdiff(dict_linelist$code_name, names(d2))
# 
# dict_vars_exclude <- data.frame(var = setdiff(names(d1), dict_linelist$code_name))
# llct::write_simple_xlsx(dict_vars_exclude, file.path(path_dictionaries, "dict_vars_exclude.xlsx"))



### Read dictionaries
# dict_linelist_v1 <- read_xlsx(file.path(path_dictionaries, "dict_linelist_v1.1.xlsx")) # corrected MSF_readmission -> MSF_visit_type
dict_linelist <- read_xlsx(file.path(path_dictionaries, "dict_linelist_v2.0.xlsx"))
dict_facilities <- read_xlsx(file.path(path_dictionaries, "dict_facilities.xlsx"))
dict_factors <- read_xlsx(file.path(path_dictionaries, "dict_factors.xlsx"))
dict_countries <- read_xlsx(path_dict_countries)
dict_extra_vars <- read_xlsx(file.path(path_dictionaries, "dict_extra_vars.xlsx"))
dict_date_categories <- read_xlsx(file.path(path_dictionaries, "dict_date_categories.xlsx"))
dict_numeric_correct <- read_xlsx(file.path(path_dictionaries, "dict_numeric_correct.xlsx"))
dict_factors_correct <- read_xlsx(file.path(path_dictionaries, "dict_factors_correct.xlsx"))
dict_countries_correct <- read_xlsx(file.path(path_dictionaries, "dict_countries_correct.xlsx"))
dict_vars_exclude <- read_xlsx(file.path(path_dictionaries, "dict_vars_exclude.xlsx"))

date_vars <- c("upload_date", dict_linelist$code_name[dict_linelist$data_type == "Date"])


### Vector of ISO3 country codes for which we have linelists in data-raw
country_dirs <- list_dirs(path_data_raw, pattern = "\\/[[:alpha:]]{3}$")
countries <- stringr::str_sub(country_dirs, start = -3)

