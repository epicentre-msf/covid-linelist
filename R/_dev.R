
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
source("R/import_other_afg_tri.R")
source("R/import_other_bel_ocb.R")
source("R/import_other_cod_oca.R")
source("R/import_other_hti_ocb.R")
# source("R/import_other_ssd_ocg.R") # temp solution
source("R/import_other_yem_ocb.R")
source("R/import_other_yem_ocp.R")
source("R/import_other_yem_pra.R")




ll_other_afg_tri <- import_other_afg_tri(path_linelist_other, dict_linelist)
ll_other_bel_ocb <- import_other_bel_ocb(path_linelist_other, dict_linelist)
ll_other_cod_oca <- import_other_cod_oca(path_linelist_other, dict_linelist)
ll_other_hti_ocb <- import_other_hti_ocb(path_linelist_other, dict_linelist)
# ll_other_ssd_ocg <- import_other_ssd_ocg(path_linelist_other, dict_linelist) # these rows now in main export
ll_other_yem_ocb <- import_other_yem_ocb(path_linelist_other, dict_linelist)
ll_other_yem_ocp <- import_other_yem_ocp(path_linelist_other, dict_linelist)
ll_other_yem_pra <- import_other_yem_pra(path_linelist_other, dict_linelist)


### Import Epicentre-version linelists
ll_import_epicentre <- purrr::map_dfr(
  countries_update,
  import_linelists,
  path_data_raw = path_data_raw,
  dict_facilities = dict_facilities,
  dict_linelist = dict_linelist,
  dict_extra_vars = dict_extra_vars,
  dict_vars_exclude = dict_vars_exclude
)


### Bind Epicentre and Other imports
ll_import <- dplyr::bind_rows(
  ll_import_epicentre,
  ll_other_afg_tri,
  ll_other_bel_ocb,
  ll_other_cod_oca,
  ll_other_hti_ocb,
  # ll_other_ssd_ocg,
  ll_other_yem_ocb,
  ll_other_yem_ocp,
  ll_other_yem_pra
)


# check for missing values among important columns
queryr::query(
  ll_import,
  is.na(.x),
  cols_dotx = c(
    "MSF_N_Patient",
    "site",
    "patient_id",
    "linelist_lang",
    "upload_date",
    "country",
    "shape"
  ),
  cols_base = c(country, OC)
)



# save raw country-specific RDS files
purrr::walk(
  countries_update,
  write_by_country,
  dat = ll_import
)



### Implement cleaning routines
ll_cleaned <- ll_import %>% 
  # temp solution for ETH_E_GRH to remove names (will be replace with TEMP_001, ...)
  mutate(MSF_N_Patient = ifelse(site == "ETH_E_GRH", NA_character_, MSF_N_Patient)) %>%
  # temp solution for AFG_P_GZG to fix expo_contact_case
  mutate(expo_contact_case = 
    case_when(
      site == "AFG_P_GZG" & is.na(expo_contact_case) ~ extra__expo_contact_case,
      TRUE ~ expo_contact_case
    )
  ) %>% 
  # CAF_E_BAT has wrong language in options sheet
  mutate(linelist_lang = ifelse(site == "CAF_E_BAT", "English", linelist_lang)) %>% 
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
  path_prefix = "local/clean/ll_covid_cleaned_"
)



### Geocoding routines
ll_geocode <- purrr::map_dfr(
  countries_update,
  clean_geo,
  path_corrections_geocodes = path_corrections_geocodes,
  path_shapefiles = path_shapefiles,
  write_checks = TRUE
)


# ref <- fetch_georef("YEM")
# 
# ref %>%
#   # filter(adm1 == "Manipur") %>%
#   filter(grepl("yahya", pcode, ignore.case = TRUE))



# check again for missing values among important columns
queryr::query(ll_geocode, is.na(site), cols_base = c(country, OC), count = TRUE)
queryr::query(ll_geocode, is.na(MSF_N_Patient), cols_base = c(country, OC), count = TRUE)
queryr::query(ll_geocode, is.na(patient_id), cols_base = c(country, OC), count = TRUE)
queryr::query(ll_geocode, duplicated(patient_id), cols_base = c(country, OC), count = TRUE)


purrr::walk(
  countries_update,
  write_by_country,
  dat = ll_geocode,
  path_prefix = "local/final/msf_covid19_linelist_"
)



### Compile global linelist
d_global <- list.files("local/final", pattern = "msf_covid19_linelist", full.names = TRUE) %>% 
  purrr::map_dfr(readRDS) %>% 
  select(-starts_with("MSF_variable_additional")) %>%
  select(-starts_with("extra"), everything(), starts_with("extra")) %>% 
  arrange(site) %>% 
  rename(ll_language = linelist_lang, ll_version = linelist_vers) %>% 
  mutate(db_row = 1:n()) %>% 
  mutate(triage_site = ifelse(site %in% c("AFG_P_HRH", "AFG_P_IDP"), "Yes", "No"), .after = "site_type")



# double-check for allowed values
matchmaker::check_df(
  d_global,
  dict_factors,
  col_vals = "values_en",
  col_vars = "variable",
  always_allow_na = TRUE
)



### Write global export
if (FALSE) {
  path_out_global <- file.path(path_export_global, glue("msf_covid19_linelist_global_{lubridate::today()}"))
  llct::write_simple_xlsx(d_global, paste0(path_out_global, ".xlsx"))
  saveRDS(d_global, paste0(path_out_global, ".rds"))
}




### Write OC-specific files
queryr::query(d_global, is.na(MSF_N_Patient), cols_base = c(country, OC), count = TRUE)
dup_id <- queryr::query(d_global, duplicated(patient_id), cols_base = c(country, OC), count = TRUE)



d_global_his <- d_global %>% 
  mutate(across(all_of(date_vars), .fns = date_format)) %>% 
  mutate(
    MSF_main_diagnosis = recode(
      MSF_main_diagnosis,
      "Chronic lung disease (asthma, COPD…)" = "Chronic lung disease (asthma, COPD, etc)"
    )
  ) %>% 
  filter(!patient_id %in% dup_id$value1)



OC_list <- unique(d_global_his$OC)


if (FALSE) {
  for (OC_focal in OC_list) {
    file_out_oc <- glue("msf_covid19_linelist_{tolower(OC_focal)}_{lubridate::today()}.xlsx")
    
    # HIS-export
    d_oc_his <- filter(d_global_his, OC == OC_focal)
    path_out1_oc <- file.path(path_export, OC_focal, file_out_oc)
    llct::write_simple_xlsx(d_oc_his, path_out1_oc)
    
    # focal point
    d_oc_foc <- filter(d_global, OC == OC_focal)
    path_out2_oc <- file.path(path_export_fp, OC_focal, file_out_oc)
    llct::write_simple_xlsx(d_oc_foc, path_out2_oc)
  }
}

