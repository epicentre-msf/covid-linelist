
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
path_export_global <- file.path(path_onedrive, "data/linelist/world")


### Read dictionaries
dict_facilities <- read_xlsx(file.path(path_dictionaries, "dict_facilities.xlsx"))
dict_numeric_correct <- read_xlsx(file.path(path_dictionaries, "dict_numeric_correct.xlsx"))
dict_factors <- read_xlsx(file.path(path_dictionaries, "dict_factors.xlsx"))
dict_factors_correct <- read_xlsx(file.path(path_dictionaries, "dict_factors_correct.xlsx"))
ll_template <- names(readxl::read_xlsx(file.path(path_dictionaries, "ll_template_v1.1.xlsx")))


### Clean/compile country-specific linelists
# Import and combine linelists from each country, clean and check variables,
# geo-match, and write resulting cleaned linelist files

source("R/update.R")

# focal country ISO code (AFG, KEN, IRQ, NGA)
country <- "AFG"

# run update routines
d_country <- update_linelist(path_data_raw = path_data_raw,
                             path_shapefiles = path_shapefiles,
                             path_cleaning = path_cleaning,
                             country = country,
                             dict_facilities = dict_facilities,
                             dict_numeric_correct = dict_numeric_correct,
                             dict_factors = dict_factors,
                             dict_factors_correct = dict_factors_correct,
                             ll_template = ll_template,
                             run_cleaning = TRUE,
                             write_checks = FALSE)

# write country-specific compilation to local folder
saveRDS(d_country, glue("local/msf_covid19_linelist_{country}.rds"))



### Compile global linelist
d_global <- list.files("local", pattern = "msf_covid19_linelist", full.names = TRUE) %>% 
  lapply(readRDS) %>% 
  dplyr::bind_rows() %>% 
  mutate(db_row = 1:n())

path_out_global <- file.path(path_export_global, glue("msf_covid19_linelist_global_{lubridate::today()}"))
write_pretty_xlsx(d_global, paste0(path_out_global, ".xlsx"))
saveRDS(d_global, paste0(path_out_global, ".rds"))



### Write OC-specific files
oc_list <- unique(d_global$OC)

for (oc_focal in oc_list) {
  d_oc <- filter(d_global, OC == oc_focal)
  file_out_oc <- glue("msf_covid19_linelist_{tolower(oc_focal)}_{lubridate::today()}.xlsx")
  path_out_oc <- file.path(path_export, oc_focal, file_out_oc)
  write_pretty_xlsx(d_oc, path_out_oc)
}

