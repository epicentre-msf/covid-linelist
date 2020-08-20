#' Import, standardize, and combine linelists from each facility
#'
#' @param country Country ISO code
#' @param path_data_raw Path to directory containing linelists
#' @param dict_facilities Dictionary mapping site-ID columns (country, OC,
#'   project) to site codes
#' @param dict_linelist Master linelist variable dictionary
#'
#' @return
#' Combined linelist <tibble> created by binding together the most recent
#' linelist version for each facility, with minor cleaning (e.g. removing
#' almost-empty lines) and standardizing (e.g. variable names)
#' 
import_other_yem_ocp <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  source("R/utilities.R")
  source("R/import_other_yem_ocp.R")
  
  file_sana <- llutils::list_files(
    path = file.path(path_linelist_other, "OCP", "YEM"),
    pattern = "Sana.*\\.xlsm",
    ignore.case = TRUE,
    full.names = TRUE,
    select = "latest"
  )
  
  file_aden <- llutils::list_files(
    path = file.path(path_linelist_other, "OCP", "YEM"),
    pattern = "Aden.*\\.xlsm",
    ignore.case = TRUE,
    full.names = TRUE,
    select = "latest"
  )
  
  date_vars_convert <- date_vars[!date_vars %in% "upload_date"]
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)
  
  d_orig <- dplyr::bind_rows(
    import_yem_ocp_helper(file_sana, "YEM_P_SAN"),
    import_yem_ocp_helper(file_aden, "YEM_P_ADE")
  ) %>% 
    dplyr::left_join(dict_facilities_join, by = "site")
    
  
  # d_orig %>% 
  #   mutate_at(c("admission_date_dd_mm_yyyy", "date_admitted_to_icu_dd_mm_yyyy"), parse_dates) %>% 
  #   select(admission_date_dd_mm_yyyy, date_admitted_to_icu_dd_mm_yyyy, received_mechanical_ventilation) %>% 
  #   filter(!is.na(date_admitted_to_icu_dd_mm_yyyy)) %>% 
  #   mutate(same_date = admission_date_dd_mm_yyyy == date_admitted_to_icu_dd_mm_yyyy) %>% 
  #   count(same_date)
  
  # d_orig %>% 
  #   mutate_at(c("admission_date_dd_mm_yyyy", "date_admitted_to_icu_dd_mm_yyyy"), parse_dates) %>% 
  #   select(admission_date_dd_mm_yyyy, date_admitted_to_icu_dd_mm_yyyy, received_mechanical_ventilation) %>% 
  #   filter(!is.na(received_mechanical_ventilation)) %>% 
  #   mutate(same_date = admission_date_dd_mm_yyyy == date_admitted_to_icu_dd_mm_yyyy) %>% 
  #   mutate(received_mechanical_ventilation = tolower(received_mechanical_ventilation)) %>% 
  #   count(received_mechanical_ventilation, same_date)
  # 
  # d_orig %>% 
  #   mutate_at(c("admission_date_dd_mm_yyyy", "date_admitted_to_icu_dd_mm_yyyy"), parse_dates) %>% 
  #   select(admission_date_dd_mm_yyyy, date_admitted_to_icu_dd_mm_yyyy, received_oxygen) %>% 
  #   filter(!is.na(received_oxygen)) %>% 
  #   mutate(same_date = admission_date_dd_mm_yyyy == date_admitted_to_icu_dd_mm_yyyy) %>% 
  #   mutate(received_oxygen = tolower(received_oxygen)) %>% 
  #   count(received_oxygen, same_date)
  
  
  ### Recode names to match Epicentre Covid ll
  d_recode <- d_orig %>% 
    rename(MSF_N_Patient = number_patient,
           # full_name,
           patinfo_ageonset = age,
           patinfo_ageonsetunit = unit_of_age,
           # x5,
           patinfo_sex = sex,
           # governorate,   # MSF_admin_location_past_week
           # district,      # MSF_admin_location_past_week
           # subdistrict,   # MSF_admin_location_past_week
           # village_camp,  # MSF_admin_location_past_week
           MSF_date_consultation = admission_date_dd_mm_yyyy, # =patcourse_presHCF
           # week_12,
           MSF_refer_from = referred_from,
           patcourse_dateonset = date_of_onset_symptoms_dd_mm_yyyy,
           MSF_severity = severity,
           Comcond_preg = pregnancy_status_for_woman,
           Comcond_cardi = cardiovascular_disease,
           Comcond_lung = chronic_lung_disease_including_tb,
           Comcond_diabetes = diabetes,
           Comcond_renal = renal_disease,
           Comcond_liver = liver_disease,
           Comcond_malig = malignancy,
           Comcond_other = other_condition_specify,
           patinfo_occuhcw = is_the_patient_a_healthcare_worker_or_someone_who_works_in_a_health_facility,
           patinfo_occuhcw_name = if_yes_facility_name,
           Lab_date1 = date_of_first_laboratory_test,
           MSF_test_results = first_lab_test_results,
           outcome_lab_date = date_of_last_laboratory_test,
           outcome_lab_result = last_lab_test_results,
           # date_admitted_to_icu_dd_mm_yyyy,
           # date_discharged_from_icu_dd_mm_yyyy,
           # x32,
           MSF_outcome_received_oxygen = received_oxygen, ### switched to outcome only 
           # received_non_invasive_ventilation_cpap,
           outcome_patcourse_vent = received_mechanical_ventilation, ### if date_icu != date_admit, fill outcome only
           # x36,
           outcome_patcourse_status = patient_outcome,
           outcome_date_of_outcome = date_of_outcome_dd_mm_yy)
  
  ### Further cleaning and standardization
  d_clean <- d_recode %>% 
    mutate(across(any_of(date_vars_convert), parse_excel_dates)) %>% 
    mutate(across(governorate:village_camp, ~ ifelse(is.na(.x), "", .x))) %>% 
    unite("MSF_admin_location_past_week", governorate:village_camp, sep = " | ") %>% 
    mutate(outcome_patcourse_status = case_when(
      outcome_patcourse_status == "death in the hospital" ~ "Died",
      outcome_patcourse_status == "DAMA" ~ "Left against medical advice",
      outcome_patcourse_status == "discharged home" ~ "Cured",
      outcome_patcourse_status == "death on arrival" ~ "Died",
      TRUE ~ outcome_patcourse_status
    )) %>% 
    mutate(
      MSF_date_consultation = as.character(parse_dates(MSF_date_consultation)),
      date_admitted_to_icu_dd_mm_yyyy = as.character(parse_dates(date_admitted_to_icu_dd_mm_yyyy)),
      report_country = "YEM",
      patinfo_idadmin0 = "YEM",
      patinfo_resadmin0 = "YEM",
      MSF_covid_status = "Probable",
      patcourse_asymp = ifelse(!is.na(patcourse_dateonset), "No", "Unknown"),
      MSF_visit_type = "First hospitalisation",
      patcourse_admit = "Yes",
      outcome_patcourse_admit = patcourse_admit,
      patcourse_presHCF = MSF_date_consultation,
      outcome_patcourse_presHCF = patcourse_presHCF,
      outcome_patcourse_icu = ifelse(!is.na(date_admitted_to_icu_dd_mm_yyyy) | !is.na(date_discharged_from_icu_dd_mm_yyyy), "Yes", "No"), ### if date_icu != date_admit, fill outcome only
      patcourse_icu = ifelse(date_admitted_to_icu_dd_mm_yyyy == MSF_date_consultation | tolower(outcome_patcourse_icu) %in% "no", outcome_patcourse_icu, NA_character_),
      patcourse_vent = ifelse(date_admitted_to_icu_dd_mm_yyyy == MSF_date_consultation | tolower(outcome_patcourse_vent) %in% "no", outcome_patcourse_vent, NA_character_),
      outcome_onset_symptom = patcourse_dateonset
    )
  
  ## derived columns
  cols_derive <- c("db_row",
                   "linelist_row",
                   "upload_date",
                   "linelist_lang",
                   "linelist_vers",
                   "country",
                   "shape",
                   "OC",
                   "project",
                   "site_type",
                   "site_name",
                   "site",
                   "uid",
                   "MSF_N_Patient",
                   "patient_id")

  ## import and prepare
  df_data <- d_clean %>% 
    mutate(patient_id = paste(site, format_text(MSF_N_Patient), sep = "_")) %>% 
    mutate(db_row = 1:n())
  
  
  ## columns to add (from original ll template)
  ll_template <- dict_linelist$code_name
  cols_to_add <- setdiff(ll_template, names(df_data))
  df_data[cols_to_add] <- NA_character_
  
  # check for new columns to be manually renamed
  extra_cols <- grep("^extra__", names(df_data), value = TRUE)
  new_cols <- setdiff(names(df_data), c(cols_derive, ll_template, extra_cols))
  
  ## return
  dplyr::select(df_data, all_of(cols_derive), all_of(ll_template), starts_with("extra_"))
}
 



import_yem_ocp_helper <- function(path, site) {
  
  upload_date <- as.Date(
    stringr::str_extract(path, "[0-9]{8}"),
    format = "%d%m%Y"
  )
  
  readxl::read_xlsx(
    path,
    sheet = "LL-COVID19",
    skip = 6,
    col_types = "text",
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>% 
    janitor::clean_names() %>% 
    janitor::remove_empty("rows") %>% 
    mutate(linelist_row = 1:n(),
           upload_date = as.character(upload_date),
           linelist_lang = "English",
           linelist_vers = "Other",
           # country = "YEM",
           # shape = "YEM",
           # OC = "OCP",
           # project = project,
           # site_type = "Hospital",
           # site_name = site_name,
           site = site,
           # uid = NA_character_
    )
}

