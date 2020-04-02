

################################################################################
## Set global directories/paths
################################################################################

library(here)

path_project <- here::here()
setwd(path_project)

path_rds <- file.path(path_project, "rds")
path_data <- file.path(path_project, "data")

path_onedrive <- path.expand("~/MSF/GRP-EPI-COVID-19 - Documents")

path_data_raw <- file.path(path_onedrive, "data-raw/linelist")
path_data_write <- file.path(path_onedrive, "data/linelist")

path_cleaning_checks <- file.path(path_data_raw, "manual_cleaning")
path_dictionaries <- file.path(path_cleaning_checks, "dictionaries")

path_export <- file.path(path_onedrive, "data/linelist/export")

path_geocode_correct <- file.path(path_onedrive, "geo_cleaning")
path_geocode_edit <- "local/data_cleaning"

ll_template_dir <- file.path(path_onedrive, "templates/linelist")
ll_export_all_path <- file.path(ll_template_dir, "___.xlsx")

ll_cols <- names(readxl::read_xlsx(file.path(ll_template_dir, "___.xlsx")))
ll_recode <- repi::dict_to_vec(readxl::read_xlsx(file.path(path_dictionaries, "column_name_recoding.xlsx")))



################################################################################
## Set dictionary of site values/valid/display names
################################################################################

library(readxl)
library(repi)

site_dict_full <- read_xlsx(file.path(path_dictionaries, "dict_site_full.xlsx"))
sites_display <- repi::dict_to_vec(etc_dict_full[,c("value", "display")])
sites_valid <- repi::dict_to_vec(etc_dict_full[,c("value", "valid")])
sites_valid_to_display <- repi::dict_to_vec(etc_dict_full[,c("valid", "display")])




################################################################################
#### Update data
## Import and combine linelists from each site, clean and check variables,
## geo-match, and write resulting cleaned linelist files
## - msf_covid_linelist.feather
################################################################################

source("R/update_data.R")

out_update <- update_data(path_data_raw,
                          path_cleaning_checks,
                          path_geocode_edit,
                          path_geocode_correct,
                          path_vhf,
                          ETC_valid,
                          ETC_display,
                          etc_recode,
                          run_cleaning = TRUE,
                          run_duplicates = TRUE,
                          write_checks = TRUE,
                          verbose_import = FALSE)


## write cleaned linelist files to Sharepoint data directory
path_out_covid_ll <- file.path(path_data, "msf_covid_linelist.feather")
feather::write_feather(out_update, path_out_covid_ll)



################################################################################
#### Upload data
## Upload various linelist files to Sharepoint folders and git repo
################################################################################

source("R/upload_data.R")

upload_data(path_upload,
            path_git = path_git_data,
            push_git = TRUE)


