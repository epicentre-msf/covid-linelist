

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
dict_factors_correct <- readxl::read_xlsx(file.path(path_dictionaries, "dict_factors_correct.xlsx"))

path_export <- file.path(path_onedrive, "data/linelist/HIS-export")
path_export_global <- file.path(path_onedrive, "data/linelist/world")

ll_template <- names(readxl::read_xlsx(file.path(path_dictionaries, "ll_template_v1.1.xlsx")))

path_shapefiles <- file.path(path_onedrive, "data/shapefiles")


################################################################################
#### Update compiled linelist
## Import and combine linelists from each site, clean and check variables,
## geo-match, and write resulting cleaned linelist files
################################################################################

source("R/update.R")

country <- "NGA" # AFG KEN IRQ NGA

d <- update_linelist(path_data_raw = path_data_raw,
                     path_shapefiles = path_shapefiles,
                     dict_facilities = dict_facilities,
                     dict_factors = dict_factors,
                     dict_factors_correct = dict_factors_correct,
                     ll_template = ll_template,
                     country = country)


# write country-specific compilation to local folder
saveRDS(d, glue::glue("local/msf_covid19_linelist_{country}.rds"))



### Compile global linelist
d_global <- list_files("local", pattern = "msf_covid19_linelist", full.names = TRUE) %>% 
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
  path_out_oc <- file.path(path_export, oc_focal, glue("msf_covid19_linelist_{tolower(oc_focal)}_{lubridate::today()}"))
  write_pretty_xlsx(d_oc, paste0(path_out_oc, ".xlsx"))
}

