
### Required libraries
library(tidyverse, warn.conflicts = FALSE)
library(rlang, warn.conflicts = FALSE)
library(here, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)
library(matchmaker, warn.conflicts = FALSE)    # remotes::install_github("patrickbarks/matchmaker")
library(hmatch, warn.conflicts = FALSE)        # remotes::install_github("epicentre-msf/hmatch")
library(llutils, warn.conflicts = FALSE)       # remotes::install_github("epicentre-msf/llutils")
source("R/utilities.R")



### Set global paths
path_onedrive <- switch(
  Sys.info()["user"],
  "patrickbarks" = path.expand("~/MSF/GRP-EPI-COVID-19 - NCoVEpi"),
  "M-MOUSSET"     = path.expand("D:/MSF/GRP-EPI-COVID-19 - NCoVEpi"),                        # Mathilde to edit
  path.expand("~/MSF/GRP-EPI-COVID-19 - NCoVEpi")          # generic Mac path 
  )

path_afg_triage <- switch(
  Sys.info()["user"],
  "patrickbarks" = path.expand("~/MSF/GRP-EPI-PROJ-COVID-19-AFG - triage_HRH_IDP"),
  "M-MOUSSET"     = path.expand(""),                                   # Mathilde to edit
  path.expand("~/MSF/GRP-EPI-PROJ-COVID-19-AFG - triage_HRH_IDP")     # generic Mac path 
)

path_project <- here::here()
path_data_raw <- file.path(path_onedrive, "data-raw", "linelist")
path_cleaning <- file.path(path_onedrive, "data-cleaning")
path_dictionaries <- file.path(path_cleaning, "dictionaries")
path_resources <- file.path(path_cleaning, "ll-resources")
path_corrections <- file.path(path_cleaning, "manual-corrections")
path_corrections_dates <- file.path(path_corrections, "dates")
path_corrections_geocodes <- file.path(path_corrections, "geocodes")
path_shapefiles <- file.path(path_onedrive, "data", "shapefiles")
path_export <- file.path(path_onedrive, "data", "linelist", "HIS-export")
path_export_fp <- file.path(path_onedrive, "coordination", "Surveillance focal points coordination", "Data compilation")
path_export_global <- file.path(path_onedrive, "data", "linelist", "world")
path_export_country <- file.path(path_onedrive, "data", "linelist", "country")
path_dict_linelist <- file.path(path_onedrive, "template", "linelist", "dev", "dico")
path_dict_countries <- file.path(path_onedrive, "template", "linelist", "dev", "base_geo", "WORLD_1_20200403.xlsx")
path_queries <- file.path(path_cleaning, "queries")
path_date_figs <- file.path(path_cleaning, "date-figures")
path_geobase <- file.path(path_onedrive, "template", "linelist", "dev", "base_geo")
path_implementation <- file.path(path_onedrive, "coordination", "Surveillance focal points coordination", "Logbook LL implementation")

### Other linelists (AFG triage, YEM, etc.)
path_linelist_other <- file.path(path_onedrive, "data-raw", "linelist-other")

### Read dictionaries
dict_linelist_v1 <- read_xlsx(file.path(path_dictionaries, "archive", "dict_linelist_v1.1.xlsx")) # corrected MSF_readmission -> MSF_visit_type
dict_linelist <- read_xlsx(file.path(path_dictionaries, "dict_linelist_v2.0.xlsx"))
dict_facilities <- read_xlsx(file.path(path_dictionaries, "dict_facilities.xlsx"))
dict_factors <- read_xlsx(file.path(path_dictionaries, "dict_factors.xlsx"))
dict_countries <- read_xlsx(path_dict_countries)
dict_extra_vars <- read_xlsx(file.path(path_dictionaries, "dict_extra_vars.xlsx"))
dict_date_categories <- read_xlsx(file.path(path_dictionaries, "dict_date_categories.xlsx"))
dict_numeric_correct <- read_xlsx(file.path(path_dictionaries, "dict_numeric_correct.xlsx"))
dict_factors_correct <- read_xlsx(file.path(path_dictionaries, "dict_factors_correct.xlsx"))
dict_countries_correct <- read_xlsx(file.path(path_dictionaries, "dict_countries_correct.xlsx"))
date_vars <- c("upload_date", dict_linelist$code_name[dict_linelist$data_type == "Date"])


### Vector of ISO3 country codes for which we have linelists in data-raw
country_dirs <- list_dirs(path_data_raw, pattern = "\\/[[:alpha:]]{3}$")
countries <- stringr::str_sub(country_dirs, start = -3)

