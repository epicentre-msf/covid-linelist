
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
path_dict_master <- file.path(path_onedrive, "template/linelist/dev/dico")


### Read dictionaries
dict_facilities <- read_xlsx(file.path(path_dictionaries, "dict_facilities.xlsx"))
dict_factors <- read_xlsx(file.path(path_dictionaries, "dict_factors.xlsx"))
dict_extra_vars <- read_xlsx(file.path(path_dictionaries, "dict_extra_vars.xlsx"))
ll_template <- names(readxl::read_xlsx(file.path(path_dictionaries, "ll_template_v1.1.xlsx")))


### Clean/compile country-specific linelists
# Import and combine linelists from each country, clean and check variables,
# geo-match, and write resulting cleaned linelist files

source("R/update.R")

# focal country ISO code
countries <- c("AFG", "KEN", "IRQ",
               "NGA", "CMR", "MEX",
               "SOM", "COD", "BGD",
               "GIN", "MLI", "SDN",
               "SSD", "NER", "YEM",
               "HTI", "VEN", "GRC",
               "CAF")

for (country in countries) {
  
  # country <- "BGD"
  
  # run update routines
  d_country <- update_linelist(path_data_raw = path_data_raw,
                               path_shapefiles = path_shapefiles,
                               path_cleaning = path_cleaning,
                               path_dictionaries = path_dictionaries,
                               country = country,
                               dict_facilities = dict_facilities,
                               dict_factors = dict_factors,
                               dict_extra_vars = dict_extra_vars,
                               ll_template = ll_template,
                               run_cleaning = TRUE,
                               write_checks = TRUE)
  
  # d_country %>%
  #   select(site, MSF_admin_location_past_week, matches("name__res$")) %>%
  #   unique() %>%
  #   print(n = "all")
  # 
  # ref <- fetch_georef(country)
  # 
  # ref %>% 
  #   filter(grepl("beyede", adm4, ignore.case = TRUE)) %>% 
  #   print(n = "all")
  
  
  # write country-specific compilation to local folder
  saveRDS(d_country, glue("local/msf_covid19_linelist_{country}.rds"))
}


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

d_global_prep <- d_global %>% 
  mutate(across(c(upload_date,
                  report_date,
                  Lab_date1,
                  patcourse_dateonset,
                  matches("MSF_symptom_.*_date_onset"),
                  MSF_date_consultation,
                  patcourse_presHCF,
                  patcourse_dateiso,
                  starts_with("expo_travel_date"),
                  starts_with("expo_case_date"),
                  outcome_submitted_date,
                  outcome_patcourse_presHCF,
                  outcome_date_of_outcome,
                  outcome_lab_date,
                  MSF_date_treament1,
                  MSF_date_treament2,
                  MSF_date_treament3),
                .fns = date_format))


for (oc_focal in oc_list) {
  
  file_out_oc <- glue::glue("msf_covid19_linelist_{tolower(oc_focal)}_{lubridate::today()}.xlsx")
  
  # HIS-export
  d_oc_his <- filter(d_global_prep, OC == oc_focal)
  path_out1_oc <- file.path(path_export, oc_focal, file_out_oc)
  write_pretty_xlsx(d_oc_his, path_out1_oc)
  
  # focal point
  d_oc_foc <- filter(d_global, OC == oc_focal)
  path_out2_oc <- file.path(path_export_fp, oc_focal, file_out_oc)
  write_pretty_xlsx(d_oc_foc, path_out2_oc)
}

