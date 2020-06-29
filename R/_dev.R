
### Required libraries, paths, and global dictionaries
source("R/zzz.R")
source("R/import.R")
source("R/clean.R")
source("R/geocode.R")


### Clean/compile country-specific linelists
# Import and combine linelists from each country, clean and check variables,
# geo-match, and write resulting cleaned linelist files

# focal country ISO code
countries_update <- countries


### Import non-Epicentre linelists
source("R/import_other_afg.R")
source("R/import_other_yem.R")

ll_other_afg <- import_other_afg(path_linelist_other)
ll_other_yem <- import_other_yem(path_linelist_other)


### Import Epicentre-version linelists
ll_import <- purrr::map_dfr(countries_update,
                            import_linelists,
                            path_data_raw = path_data_raw,
                            dict_facilities = dict_facilities,
                            dict_linelist = dict_linelist,
                            dict_extra_vars = dict_extra_vars,
                            dict_vars_exclude = dict_vars_exclude) %>% 
  dplyr::bind_rows(ll_other_afg, ll_other_yem)


# check for missing site or MSF_N_Patient
llct::query(ll_import, is.na(site), id_cols = c(country, OC), count = TRUE)
llct::query(ll_import, is.na(MSF_N_Patient), id_cols = c(country, OC), count = TRUE)


# save raw country-specific RDS files
purrr::walk(
  countries_update,
  write_by_country,
  dat = ll_import
)

### Implement cleaning routines
ll_cleaned <- ll_import %>% 
  filter(site != "ETH_E_GRH") %>% 
  filter(!is.na(MSF_N_Patient)) %>% 
  clean_linelist(
    path_dictionaries,
    path_corrections_dates,
    date_vars = date_vars,
    dict_factors = dict_factors,
    dict_countries = dict_countries,
    dict_numeric_correct,
    dict_factors_correct,
    dict_countries_correct,
    write_checks = TRUE
  )

purrr::walk(
  countries_update,
  write_by_country,
  dat = ll_cleaned,
  path_prefix = "local/ll_covid_cleaned_"
)


### Geocoding routines
ll_geocode <- purrr::map_dfr(
  countries_update,
  clean_geo,
  path_corrections_geocodes = path_corrections_geocodes,
  path_shapefiles = path_shapefiles,
  write_checks = FALSE
)

llct::query(ll_geocode, is.na(site), id_cols = c(country, OC), count = TRUE)
llct::query(ll_geocode, is.na(MSF_N_Patient), id_cols = c(country, OC), count = TRUE)
llct::query(ll_geocode, duplicated(patient_id), id_cols = c(country, OC), count = TRUE)

purrr::walk(
  countries_update,
  write_by_country,
  dat = ll_geocode,
  path_prefix = "local/msf_covid19_linelist_"
)


# ref <- fetch_georef("CAF")
# 
# ref %>% 
#   filter(grepl("kabo", adm1, ignore.case = TRUE)) %>%
#   filter(grepl("Koutiala", adm3, ignore.case = TRUE))



### Compile global linelist
d_global <- list.files("local", pattern = "msf_covid19_linelist", full.names = TRUE) %>% 
  purrr::map_dfr(readRDS) %>% 
  select(-starts_with("MSF_variable_additional")) %>%
  select(-starts_with("extra"), everything(), starts_with("extra")) %>% 
  arrange(site) %>% 
  rename(ll_language = linelist_lang, ll_version = linelist_vers) %>% 
  mutate(db_row = 1:n())


# double-check for allowed values
matchmaker::check_df(
  d_global,
  dict_factors,
  col_vals = "values_en",
  col_vars = "variable",
  always_allow_na = TRUE
)




if (FALSE) {
  path_out_global <- file.path(path_export_global, glue("msf_covid19_linelist_global_{lubridate::today()}"))
  llct::write_simple_xlsx(d_global, paste0(path_out_global, ".xlsx"))
  saveRDS(d_global, paste0(path_out_global, ".rds"))
}




### Write OC-specific files
d_global_his <- d_global %>% 
  mutate(across(all_of(date_vars), .fns = date_format)) %>% 
  mutate(
    MSF_main_diagnosis = recode(
      MSF_main_diagnosis,
      "Chronic lung disease (asthma, COPD…)" = "Chronic lung disease (asthma, COPD, etc)"
    )
  )

OC_list <- unique(d_global$OC)



if (FALSE) {
  for (OC_focal in OC_list) {
    file_out_oc <- glue("msf_covid19_linelist_{tolower(OC_focal)}_{lubridate::today()}.xlsx")
    
    # HIS-export
    d_oc_his <- filter(d_global_his, OC == OC_focal)
    # if (OC_focal == "OCBA") { d_oc_his$age_in_years <- as.integer(round(d_oc_his$age_in_years, 0)) }
    path_out1_oc <- file.path(path_export, OC_focal, file_out_oc)
    llct::write_simple_xlsx(d_oc_his, path_out1_oc)
    
    # focal point
    d_oc_foc <- filter(d_global, OC == OC_focal)
    path_out2_oc <- file.path(path_export_fp, OC_focal, file_out_oc)
    llct::write_simple_xlsx(d_oc_foc, path_out2_oc)
  }
}


