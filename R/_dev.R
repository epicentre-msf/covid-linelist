
### Required libraries, paths, and global dictionaries
source("R/zzz.R")



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
               "CAF", "SYR", "TZA",
               "BFA", "IND", "ETH",
               "TJK")

for (country in countries) {
  
  # country <- "CAF"
  
  # run update routines
  d_country <- update_linelist(path_data_raw = path_data_raw,
                               path_shapefiles = path_shapefiles,
                               path_cleaning = path_cleaning,
                               path_dictionaries = path_dictionaries,
                               country = country,
                               date_vars = date_vars,
                               dict_facilities = dict_facilities,
                               dict_linelist = dict_linelist,
                               dict_countries = dict_countries,
                               dict_factors = dict_factors,
                               dict_extra_vars = dict_extra_vars,
                               run_cleaning = TRUE,
                               write_checks = FALSE)
  
  
  # d_country %>%
  #   select(site, MSF_admin_location_past_week, matches("name__res$")) %>%
  #   unique() %>%
  #   print(n = "all")
  # 
  # ref <- fetch_georef(country)
  # 
  # ref %>%
  #   # filter(adm1 == "Cox's Bazar Paurashava") %>%
  #   filter(grepl("lando", adm4, ignore.case = TRUE))
  
  # write country-specific compilation to local folder
  saveRDS(d_country, glue("local/msf_covid19_linelist_{country}.rds"))
}


### Compile global linelist
d_global <- list.files("local", pattern = "msf_covid19_linelist", full.names = TRUE) %>% 
  lapply(readRDS) %>% 
  dplyr::bind_rows() %>% 
  rename(ll_language = linelist_lang, ll_version = linelist_vers) %>% 
  mutate(db_row = 1:n())


if (FALSE) {
  path_out_global <- file.path(path_export_global, glue("msf_covid19_linelist_global_{lubridate::today()}"))
  write_pretty_xlsx(d_global, paste0(path_out_global, ".xlsx"))
  saveRDS(d_global, paste0(path_out_global, ".rds"))
}




### Write OC-specific files
# note: was previously missing outcome_onset_symptom from date var explicit "NA"
d_global_prep <- mutate(d_global, across(all_of(date_vars), .fns = date_format))
OC_list <- unique(d_global$OC)


if (FALSE) {
  for (OC_focal in OC_list) {
    file_out_oc <- glue("msf_covid19_linelist_{tolower(OC_focal)}_{lubridate::today()}.xlsx")
    
    # HIS-export
    d_oc_his <- filter(d_global_prep, OC == OC_focal)
    path_out1_oc <- file.path(path_export, OC_focal, file_out_oc)
    write_pretty_xlsx(d_oc_his, path_out1_oc)
    
    # focal point
    d_oc_foc <- filter(d_global, OC == OC_focal)
    path_out2_oc <- file.path(path_export_fp, OC_focal, file_out_oc)
    write_pretty_xlsx(d_oc_foc, path_out2_oc)
  }
}
