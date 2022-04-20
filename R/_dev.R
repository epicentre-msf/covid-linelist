
### Required libraries, paths, and global dictionaries
source("R/zzz.R")
source("R/import.R")
source("R/clean.R")
source("R/geocode.R")
source("R/compare.R")
source("R/indicators.R")


### Clean/compile country-specific linelists
# Import and combine linelists from each country, clean and check variables,
# geo-match, and write resulting cleaned linelist files

# focal country ISO code
# (update to include countries in linelist-other)
countries_update <- sort(unique(c(countries, "BEL", "MMR", "TUN")))


### Import MSF Intersectional linelists
ll_import_epicentre <- purrr::map_dfr(
  countries_update,
  import_linelists,
  path_data_raw = path_data_raw,
  dict_facilities = dict_facilities,
  dict_linelist = dict_linelist,
  dict_extra_vars = dict_extra_vars,
  site_exclude = "BGD_P_IPD" # switched to godata
) %>% 
  # temp measure to exclude 6 duplicate rows
  filter(!(site %in% "MWI_P_QCH" & MSF_name_facility %in% "South Lunzu Health Centre | South Lunzu Ward"))


### MSF_N_Patient from OCA/BGD intersectional linelists, before they switched to GoData
# used to filter out a few duplicates entries between intersectional ll and GoData
id_oca_bgd_intersect <- ll_import_epicentre %>%
  filter(site == "BGD_A_KUT") %>%
  select(site, MSF_N_Patient)


### Import non-intersectional linelists
source("R/import_other_afg_redcap.R")
source("R/import_other_afg_tri.R")
source("R/import_other_bel_ocb.R")
source("R/import_other_bel_ocb_gal.R")
source("R/import_other_bel_ocb_snk.R")
source("R/import_other_bra_ocb.R")
source("R/import_other_bra_ocp.R")
source("R/import_other_cod_ocp.R")
source("R/import_other_hti_ocb.R")
source("R/import_other_ind_ocb.R") # newest export requires new mapping template
source("R/import_other_irq_ocb.R")
source("R/import_other_jor_ocp.R")
source("R/import_other_mmr_oca.R")
source("R/import_other_pak_ocb.R")
source("R/import_other_tun_ocb.R")
source("R/import_other_yem_ocb_moh.R") # still using MoH version for now
source("R/import_other_yem_ocp.R")
source("R/import_other_yem_pra.R")
source("R/import_other_bgd_godata.R")
source("R/import_other_bgd_godata_ocp_crf1.R")

ll_other_afg_redcap <- import_other_afg_redcap(path_linelist_other, dict_linelist)
ll_other_afg_tri <- import_other_afg_tri(path_linelist_other, dict_linelist)
ll_other_bel_ocb <- import_other_bel_ocb(path_linelist_other, dict_linelist)
ll_other_bel_ocb_gal <- import_other_bel_ocb_gal(path_linelist_other, dict_linelist)
ll_other_bel_ocb_snk <- import_other_bel_ocb_snk(path_linelist_other, dict_linelist)
ll_other_bra_ocb <- import_other_bra_ocb(path_linelist_other, dict_linelist)
ll_other_bra_ocp <- import_other_bra_ocp(path_linelist_other, dict_linelist)
ll_other_cod_ocp <- import_other_cod_ocp(path_linelist_other, dict_linelist)
ll_other_hti_ocb <- import_other_hti_ocb(path_linelist_other, dict_linelist)
ll_other_ind_ocb <- import_other_ind_ocb(path_linelist_other, dict_linelist)
ll_other_irq_ocb <- import_other_irq_ocb(path_linelist_other, dict_linelist)
ll_other_jor_ocp_1 <- import_other_jor_ocp(path_linelist_other, dict_linelist, date_cutoff = "2021-01-01")
ll_other_jor_ocp_2 <- import_other_jor_ocp(path_linelist_other, dict_linelist)
ll_other_mmr_oca <- import_other_mmr_oca(path_linelist_other, dict_linelist)
ll_other_pak_ocb <- import_other_pak_ocb(path_linelist_other, dict_linelist)
ll_other_tun_ocb <- import_other_tun_ocb(path_linelist_other, dict_linelist)
ll_other_yem_ocb <- import_other_yem_ocb(path_linelist_other, dict_linelist) # still using MoH version for now
ll_other_yem_ocp <- import_other_yem_ocp(path_linelist_other, dict_linelist, vars_date) # one date problem
ll_other_yem_pra <- import_other_yem_pra(path_linelist_other, dict_linelist)
ll_other_bgd_godata <- import_other_bgd_godata(path_linelist_other, dict_linelist, vars_date = vars_date, exclude = id_oca_bgd_intersect)
ll_other_bgd_godata_ocp1 <- import_other_bgd_godata_ocp_crf1(path_linelist_other, dict_linelist, vars_date = vars_date) # Always problems. If too bad, omit the file.


### Bind Intersectional and Other imports
ll_import <- dplyr::bind_rows(
  ll_import_epicentre,
  ll_other_afg_redcap,
  ll_other_afg_tri,
  ll_other_bel_ocb,
  ll_other_bel_ocb_gal,
  ll_other_bel_ocb_snk,
  ll_other_bra_ocb,
  ll_other_bra_ocp,
  ll_other_cod_ocp,
  ll_other_hti_ocb,
  ll_other_ind_ocb,
  ll_other_irq_ocb,
  ll_other_jor_ocp_1,
  ll_other_jor_ocp_2,
  ll_other_mmr_oca,
  ll_other_pak_ocb,
  ll_other_tun_ocb,
  ll_other_yem_ocb,
  ll_other_yem_ocp,
  ll_other_yem_pra,
  ll_other_bgd_godata,
  ll_other_bgd_godata_ocp1
)


# check for missing values among important columns
queryr::query(ll_import, is.na(MSF_N_Patient), cols_base = c(country, OC), count = TRUE)
queryr::query(ll_import, is.na(site), cols_base = c(country, OC), count = TRUE) #!!!!
queryr::query(ll_import, is.na(linelist_lang), cols_base = c(country, OC), count = TRUE) #!!
queryr::query(ll_import, is.na(upload_date), cols_base = c(country, OC), count = TRUE)
queryr::query(ll_import, is.na(country), cols_base = c(country, OC), count = TRUE) #!!!
queryr::query(ll_import, is.na(shape), cols_base = c(country, OC), count = TRUE) #!!!


# check for patient_id dropped since previous compilation
# known issues:
# - ETH_E_GRH (use patient names as IDs)
ll_prev <- llutils::list_files(path_export_global, "\\.rds$", select = "latest") %>%
  readRDS()

# dropped IDs
ll_prev %>%
  filter(!grepl("TEMPID_", MSF_N_Patient)) %>% 
  anti_join(ll_import, by = c("site", "MSF_N_Patient")) %>%
  count(site)

# difference in # of entries
left_join(
  count(ll_prev, site, site_name, name = "n_old"),
  count(ll_import, site, site_name, name = "n_new"),
  by = c("site", "site_name")
) %>% 
  mutate(diff = n_new - n_old) %>% 
  filter(diff != 0)


# save raw country-specific RDS files
if (!dir.exists("local")) dir.create("local")
if (!dir.exists("local/clean")) dir.create("local/clean")
if (!dir.exists("local/final")) dir.create("local/final")
if (!dir.exists("local/raw")) dir.create("local/raw")

purrr::walk(
  sort(unique(ll_import$country)),
  write_by_country,
  dat = ll_import
)


### Implement cleaning routines
source("R/zzz.R") # in case of update to dictionaries

ll_cleaned <- ll_import %>% 
  # temp solution for ETH_E_GRH to remove names (will be replace with TEMP_001, ...)
  mutate(MSF_N_Patient = ifelse(site == "ETH_E_GRH", NA_character_, MSF_N_Patient)) %>%
  # temp solution for AFG_P_GZG to fix expo_contact_case
  mutate(
    expo_contact_case = 
      case_when(
        site == "AFG_P_GZG" & is.na(expo_contact_case) ~ extra__expo_contact_case,
        TRUE ~ expo_contact_case
      )
  ) %>% 
  clean_linelist(
    path_dictionaries,
    path_corrections_dates,
    vars_date = vars_date,
    vars_numeric = vars_numeric,
    dict_factors = dict_factors,
    dict_countries = dict_countries,
    corr_numeric = corr_numeric,
    dict_factors_correct,
    dict_countries_correct,
    write_checks = TRUE # Can set to FALSE to debug
  )


matchmaker::check_df(
  ll_cleaned,
  dict_factors,
  col_vals = "values_en",
  col_vars = "variable",
  always_allow_na = TRUE
)

purrr::walk(
  sort(unique(ll_cleaned$country)),
  write_by_country,
  dat = ll_cleaned,
  path_prefix = file.path("local", "clean", "ll_covid_cleaned_")
)


### Geocoding routines
ll_geocode <- purrr::map_dfr(
  countries_update,
  clean_geo,
  path_corrections_geocodes = path_corrections_geocodes,
  path_shapefiles = path_shapefiles,
  write_checks = TRUE
)

# fetch_georef("COD") %>%
#   filter(adm1 == "Miranda") %>%
#   filter(grepl("altos", pcode, ignore.case = TRUE))
# 
# View(fetch_georef("AFG"))


# check again for missing values among important columns
queryr::query(ll_geocode, is.na(site), cols_base = c(country, OC), count = TRUE)
queryr::query(ll_geocode, is.na(MSF_N_Patient), cols_base = c(country, OC), count = TRUE)
queryr::query(ll_geocode, is.na(patient_id), cols_base = c(country, OC), count = TRUE)
queryr::query(ll_geocode, duplicated(patient_id), cols_base = c(country, OC, site), count = TRUE) %>% count(site) %>% filter(n > 5)

# write local
purrr::walk(
  countries_update,
  write_by_country,
  dat = ll_geocode,
  path_prefix = file.path("local", "final", "msf_covid19_linelist_")
)


### Compile global linelist
triage_sites <- dict_facilities %>% 
  distinct(site, triage_site)

d_global <- list.files(file.path("local", "final"), pattern = "msf_covid19_linelist", full.names = TRUE) %>% 
  purrr::map_dfr(readRDS) %>% 
  derive_event_date() %>% 
  select(-starts_with("MSF_variable_additional")) %>%
  select(-starts_with("extra"), everything(), starts_with("extra")) %>% 
  rename(ll_language = linelist_lang, ll_version = linelist_vers) %>% 
  arrange(site) %>% 
  mutate(db_row = 1:n()) %>% 
  left_join(triage_sites, by = "site") %>%
  mutate(triage_site = if_else(is.na(triage_site), "No", triage_site)) %>% 
  relocate(triage_site, .after = "site_type") %>% 
  mutate(OC = ifelse(OC == "OCB_&_OCP", "OCB/OCP", OC)) %>% 
  # filter(!(site == "CAF_A_BBY" & is.na(patcourse_asymp) & is.na(patinfo_ageonset))) # remove autopopulated ghost-rows
  mutate(
    OC_OCA = ifelse(grepl("OCA", OC), TRUE, NA),
    OC_OCB = ifelse(grepl("OCB(?!A)", OC, perl = TRUE), TRUE, NA),
    OC_OCBA = ifelse(grepl("OCBA", OC), TRUE, NA),
    OC_OCG = ifelse(grepl("OCG", OC), TRUE, NA),
    OC_OCP = ifelse(grepl("OCP", OC), TRUE, NA),
    .after = "OC"
  )

# check for patient_id dropped since previous compilation (worry if big numbers)
llutils::list_files(path_export_global, "\\.rds$", select = "latest") %>%
  readRDS() %>% 
  anti_join(d_global, by = "patient_id") %>%
  count(site)


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
  d_global_write <- d_global %>% 
    dplyr::select(-starts_with("extra_")) %>% 
    prepare_msf_dta()
  
  path_out_global <- file.path(path_export_global, glue::glue("msf_covid19_linelist_global_{lubridate::today()}"))
  writexl::write_xlsx(d_global_write, paste0(path_out_global, ".xlsx"))
  saveRDS(d_global_write, paste0(path_out_global, ".rds"))
}


### Country-specific exports
purrr::walk(
  unique(d_global$country),
  write_ll_by_country,
  d_global = d_global,
  path_export_country = path_export_country
)


### Write OC-specific files
queryr::query(d_global, is.na(MSF_N_Patient), cols_base = c(country, OC), count = TRUE)
dup_id <- queryr::query(d_global, duplicated(patient_id), cols_base = c(site), count = TRUE)

d_global_his <- d_global %>% 
  mutate(across(all_of(vars_date), .fns = date_format)) %>% 
  mutate(
    MSF_main_diagnosis = recode(
      MSF_main_diagnosis,
      # recode … because causes problems for HIS import
      "Chronic lung disease (asthma, COPD…)" = "Chronic lung disease (asthma, COPD, etc)"
    )
  ) %>% 
  filter(!patient_id %in% dup_id$value1)


OC_list <- unique(d_global_his$OC)
OC_list <- OC_list[!grepl("/", OC_list)]

# list(d_global_write, ll_cleaned, ll_geocode, 
#     ll_import_epicentre, ll_other_afg_tri) %>% 
#   map(rm)

# memory.limit()
# gc(full = TRUE)



# Export file summarizing changes (new, updated, dropped) for OCBA HIS
d_ocba <- filter(d_global_his, OC %in% "OCBA")

d_ocba_long <- d_ocba %>% 
  mutate(across(everything(), as.character)) %>% 
  select(-db_row, -linelist_row, -upload_date) %>% 
  pivot_longer(cols = !patient_id)

d_ocba_prev <- list.files(file.path(path_export, "OCBA"), pattern = "^msf_covid19_linelist_ocba", full.names = TRUE) %>% 
  max() %>% 
  readxl::read_xlsx(guess_max = 1e5)

d_ocba_prev_long <- d_ocba_prev %>% 
  mutate(across(everything(), as.character)) %>% 
  select(-db_row, -linelist_row, -upload_date) %>% 
  pivot_longer(cols = !patient_id, values_to = "value_prev")

d_ocba_join <- d_ocba_long %>% 
  inner_join(d_ocba_prev_long, by = c("patient_id", "name")) %>% 
  mutate(across(c(value, value_prev), ~ if_else(is.na(.x), "<missing>", .x))) %>% 
  mutate(across(c(value, value_prev), ~ if_else(name %in% "MSF_admin_location_past_week", stringr::str_squish(.x), .x)))

d_ocba_updated <- d_ocba_join %>% 
  filter(value != value_prev) %>% 
  distinct(patient_id) %>% 
  left_join(d_ocba, by = "patient_id")

d_ocba_new <- d_ocba %>% 
  filter(!patient_id %in% d_ocba_prev$patient_id)

d_ocba_dropped <- d_ocba_prev %>% 
  anti_join(d_ocba, by = "patient_id") %>% 
  distinct(patient_id)

qxl::qxl(
  list(
    new = d_ocba_new,
    updated = d_ocba_updated,
    dropped = d_ocba_dropped
  ),
  file.path(path_export, "OCBA", glue::glue("msf_covid19_linelist_updates_ocba_{lubridate::today()}.xlsx"))
)


# Standard exports for all OCs
if (FALSE) {
  for (OC_focal in OC_list) {
    file_out_oc <- glue::glue("msf_covid19_linelist_{tolower(OC_focal)}_{lubridate::today()}.xlsx")
    
    # account for sites run by 2+ OCs
    OC_focal_sub <- OC_focal
    if (OC_focal_sub %in% c("OCB", "OCP")) OC_focal_sub <- c(OC_focal_sub, "OCB/OCP")
    
    # HIS-export
    d_oc_his <- filter(d_global_his, OC %in% OC_focal_sub)
    path_out1_oc <- file.path(path_export, OC_focal, file_out_oc)
    writexl::write_xlsx(d_oc_his, path_out1_oc)

    # focal point
    d_oc_foc <- filter(d_global, OC %in% OC_focal_sub)
    path_out2_oc <- file.path(path_export_fp, OC_focal, file_out_oc)
    writexl::write_xlsx(d_oc_foc, path_out2_oc)
    
    # patient_id losses
    ### update to compare across HIS exports, not HIS export to global (no rm of duplicates)
    df_oc_compare <- compare_ids(OC = OC_focal, path_export = path_export)
    path_out3_oc <- file.path(path_export, OC_focal, glue::glue("patient_id_losses_{tolower(OC_focal)}_{Sys.Date()}.xlsx"))
    llutils::write_simple_xlsx(df_oc_compare, file = path_out3_oc, group = compilation_date)
  }
}

