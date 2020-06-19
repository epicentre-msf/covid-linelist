

queries_ident <- function(dat_raw, dat_clean) {
  
  queries <- list()
  
  # IDENT_01 Missing value for MSF_N_patient
  queries[["IDENT_01"]] <- query_missing_id(dat_raw)
  
  # IDENT_02 Duplicated value of MSF_N_patient
  queries[["IDENT_02"]] <- query_duplicate_id(dat_raw)
  
  return(bind_query_list(queries))
}



queries_dates <- function(dat_raw, date_vars, dict_date_categories) {
  
  ## prep data
  date_vars <- date_vars[!date_vars == "upload_date"]
  id_cols <- c("OC", "country", "site", "MSF_N_Patient", "linelist_row", "upload_date")
  
  dict_date_categories_prep <- dict_date_categories %>% 
    select(variable1 = code_name, date_category)
  
  dates_raw <- dat_raw %>%
    select(all_of(id_cols), all_of(date_vars))
  
  dates_gather_raw <- dates_raw %>% 
    tidyr::pivot_longer(cols = all_of(date_vars), names_to = "variable1", values_to = "value1") %>% 
    mutate(date1 = parse_excel_dates(value1),
           date1 = parse_other_dates(date1),
           date1 = as.Date(date1)) %>% 
    left_join(dict_date_categories_prep, by = "variable1")
  
  dates_raw_date <- dates_gather_raw %>% 
    select(-value1, -date_category) %>% 
    pivot_wider(id_cols = all_of(id_cols), names_from = "variable1", values_from = "date1")
  
  
  ## queries
  queries <- list()
  
  # DATES_01 Date value not unambiguously coercible to YYYY-MM-DD
  queries[["DATES_01"]] <- dates_gather_raw %>% 
    filter(!is.na(value1) & is.na(date1)) %>% 
    select(-date1, -date_category) %>%
    format_queries()
  
  # DATES_02 Date in future (relative to upload date)
  queries[["DATES_02"]] <- dates_gather_raw %>% 
    filter(date1 > upload_date) %>% 
    select(-date1, -date_category) %>% 
    format_queries()
  
  # DATES_03 Date too early (before 2020-02-01)
  queries[["DATES_03"]] <- dates_gather_raw %>% 
    filter(date1 < as.Date("2020-02-01")) %>% 
    select(-date1, -date_category) %>% 
    format_queries()
  
  # DATES_04 Date of last contact with confirmed case earlier than date of first contact
  queries[["DATES_04"]] <- bind_queries(
    query_cond(dates_raw_date, expo_case_date_last1 < expo_case_date_first1),
    query_cond(dates_raw_date, expo_case_date_last2 < expo_case_date_first2),
    query_cond(dates_raw_date, expo_case_date_last3 < expo_case_date_first3),
    query_cond(dates_raw_date, expo_case_date_last4 < expo_case_date_first4),
    query_cond(dates_raw_date, expo_case_date_last5 < expo_case_date_first5)
  )
  
  # DATES_05 Report date earlier than EXPOSURE or ONSET date(s)
  queries[["DATES_05"]] <- bind_queries(
    query_cond(dates_raw_date, report_date < expo_travel_date1),
    query_cond(dates_raw_date, report_date < expo_travel_date2),
    query_cond(dates_raw_date, report_date < expo_travel_date3),
    query_cond(dates_raw_date, report_date < expo_case_date_first1),
    query_cond(dates_raw_date, report_date < expo_case_date_first2),
    query_cond(dates_raw_date, report_date < expo_case_date_first3),
    query_cond(dates_raw_date, report_date < expo_case_date_first4),
    query_cond(dates_raw_date, report_date < expo_case_date_first5),
    query_cond(dates_raw_date, report_date < patcourse_dateonset)
  )
  
  # DATES_06 Specific symptom onset date(s) earlier than Date symptom onset
  queries[["DATES_06"]] <- dates_raw_date %>% 
    select(all_of(id_cols), patcourse_dateonset, starts_with("MSF_symp")) %>% 
    tidyr::pivot_longer(cols = starts_with("MSF_symp"), names_to = "variable1", values_to = "value1") %>% 
    tidyr::pivot_longer(cols = patcourse_dateonset, names_to = "variable2", values_to = "value2") %>% 
    filter(value1 < value2) %>%
    format_queries()
  
  # DATES_07 ONSET date(s) earlier than EXPOSURE date(s)
  queries[["DATES_07"]] <- query_dates_chrono(dates_gather_raw, "ONSET", "EXPOSURE")
  
  # DATES_08 ADMISSION date(s) earlier than ONSET or EXPOSURE date(s)
  queries[["DATES_08"]] <- query_dates_chrono(dates_gather_raw, "ADMISSION", "ONSET")
  
  # DATES_09 OUTCOME date(s) earlier than ADMISSION or ONSET or EXPOSURE date(s)
  queries[["DATES_09"]] <- query_dates_chrono(dates_gather_raw, "OUTCOME", "ADMISSION")
  
  
  return(bind_query_list(queries))
}





queries_categorical <- function(dat_raw, dict_factors, dict_countries) {
  
  ## requires
  library(dplyr)
  library(tidyr)
  library(hmatch)
  
  ## prep data
  id_cols <- c("linelist_lang", "OC", "country", "site", "MSF_N_Patient", "linelist_row", "upload_date")
  iso_cols <- c("report_country", "patinfo_idadmin0", "patinfo_resadmin0", "patinfo_occuhcw_country", "expo_case_location")
  iso_valid_std <- hmatch::string_std(c(unique(dict_countries$iso), "Unknown", NA))
  
  dat_prep <- dat_raw %>% 
    select(all_of(id_cols), all_of(unique(dict_factors$variable))) %>% 
    tidyr::pivot_longer(cols = all_of(unique(dict_factors$variable)), names_to = "variable") %>% 
    mutate(value_std = hmatch::string_std(value))
  
  dat_iso_prep <- dat_raw %>% 
    select(all_of(id_cols), all_of(iso_cols)) %>% 
    tidyr::pivot_longer(cols = all_of(iso_cols), names_to = "variable1", values_to = "value1")
  
  ## queries
  queries <- list()
  
  # CATEG_01 Value of coded-list variable doesn't match dictionary (ignoring variation in case and punctuation)
  queries[["CATEG_01"]] <- bind_queries(
    query_match_dict(dat_prep, dict_factors, lang = "en"),
    query_match_dict(dat_prep, dict_factors, lang = "es"),
    query_match_dict(dat_prep, dict_factors, lang = "fr")
  )
  
  # CATEG_02 Country value does not match valid ISO3 country code
  queries[["CATEG_02"]] <- dat_iso_prep %>% 
    filter(!is.na(value1)) %>% 
    filter(!hmatch::string_std(value1) %in% iso_valid_std) %>% 
    select(-linelist_lang) %>% 
    format_queries()
  
  
  return(bind_query_list(queries))
}



queries_multi <- function(dat_raw, dat_clean) {
  
  queries <- list()
  
  # MUTI_01 Comorbidity count doesn't match count indicated by subsequent Comcond_* variables
  queries[["MULTI_01"]] <- query_cond(dat_raw, !valid_numeric(Comcond_present))
  
  # MUTI_02 Covid19 Status is "Confirmed" but there is also an entry for Main diagnosis (if not a case)
  queries[["MULTI_02"]] <- query_cond(dat_clean, MSF_covid_status == "Confirmed" & !is.na(MSF_main_diagnosis))
  
  # MUTI_03 Asymptomatic is "Yes" but date of symptom onset still given
  queries[["MULTI_03"]] <- query_cond(dat_clean, patcourse_asymp == "Yes" & !is.na(patcourse_dateonset))
  
  # MULTI_04 Date of symptom onset is given but symptom status is not "Yes"
  queries[["MULTI_04"]] <- bind_queries(
    query_cond(dat_clean, !is.na(MSF_symptom_fever_date_onset) & !MSF_symptom_fever %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_cough_date_onset) & !MSF_symptom_cough %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_breathlessness_date_onset) & !MSF_symptom_breathlessness %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_sorethoat_date_onset) & !MSF_symptom_sorethoat %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_chills_date_onset) & !MSF_symptom_chills %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_headache_date_onset) & !MSF_symptom_headache %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_abdominal_pain_date_onset) & !MSF_symptom_abdominal_pain %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_tiredness_date_onset) & !MSF_symptom_tiredness %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_aches_date_onset) & !MSF_symptom_aches %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_nose_congestion_date_onset) & !MSF_symptom_nose_congestion %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_runny_nose_date_onset) & !MSF_symptom_runny_nose %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_diarrhoea_date_onset) & !MSF_symptom_diarrhoea %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_vomiting_date_onset) & !MSF_symptom_vomiting %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_loss_taste_date_onset) & !MSF_symptom_loss_taste %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_anosmia_date_onset) & !MSF_symptom_anosmia %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_symptom_chest_pain_date_onset) & !MSF_symptom_chest_pain %in% "Yes")
  )
  
  # MULTI_05 Trimester of pregnancy is given (excluding "Unknown") but Pregnancy is not "Yes"
  queries[["MULTI_05"]] <- query_cond(dat_clean, !Comcond_pregt %in% c("Unknown", NA) & !Comcond_preg %in% "Yes")
  
  # MULTI_06 CD4 count or HIV viral load given but HIV status not a form of "Positive..."
  queries[["MULTI_06"]] <- bind_queries(
    query_cond(dat_clean, !is.na(MSF_hiv_cd4) & !grepl("posit", MSF_hiv_status, ignore.case = TRUE)),
    query_cond(dat_clean, !is.na(MSF_hiv_viral_load) & !grepl("posit", MSF_hiv_status, ignore.case = TRUE))
  )
  
  # MULTI_07 Hypertension is "Yes" but Cardiovascular disease (incl. hypertension) is not "Yes"
  queries[["MULTI_07"]] <- query_cond(dat_clean, MSF_hypertension == "Yes" & !Comcond_cardi %in% "Yes")

  # MULTI_08 Type of chronic lung disease is specified but Chronic lung disease is not "Yes"
  queries[["MULTI_08"]] <- query_cond(dat_clean, !is.na(MSF_type_lung_disease) & !Comcond_lung %in% "Yes")
  
  # MULTI_09 Resistant TB is 'Yes' but Active TB is not a form of "Yes…"
  queries[["MULTI_09"]] <- query_cond(dat_clean, MSF_tb_resistant == "Yes" & !grepl("yes", MSF_tb_active, ignore.case = TRUE))
  
  # MULTI_10 ID of origin is specified but Health structure referred from is missing
  queries[["MULTI_10"]] <- query_cond(dat_clean, !is.na(MSF_ID_refer_from) & is.na(MSF_refer_from))
  
  # MULTI_11 Hospital-related variable (oxygen, ICU, ventilated, and/or ECMO) is 'Yes' but Type of visit does not correspond to "Hospitalization"
  # don't flag if MSF_visit_type = <NA> because separate query for that (OTHER_01)
  queries[["MULTI_11"]] <- bind_queries(
    query_cond(dat_clean, MSF_received_oxygen == "Yes" & MSF_visit_type %in% c("First consultation")),
    query_cond(dat_clean, patcourse_icu == "Yes" & MSF_visit_type %in% c("First consultation")),
    query_cond(dat_clean, patcourse_vent == "Yes" & MSF_visit_type %in% c("First consultation")),
    query_cond(dat_clean, patcourse_ecmo == "Yes" & MSF_visit_type %in% c("First consultation"))
  )
  
  # MULTI_12 Date of isolation is given but Case isolated is not "Yes"
  queries[["MULTI_12"]] <- query_cond(dat_clean, !is.na(patcourse_dateiso) & !patcourse_iso %in% "Yes")
  
  # MULTI_13 Other main complication is specified but Main complication is missing
  queries[["MULTI_13"]] <- query_cond(dat_clean, !is.na(MSF_other_complications) & is.na(MSF_complications))
  
  # MULTI_14 Healthcare worker country is specified but Healthcare worker is not "Yes" (*country currently autopopulated in v1.2 and earlier)
  
  # MULTI_15 Country, city, and/or date of travel is specified but Travels is not "Yes"
  queries[["MULTI_15"]] <- bind_queries(
    query_cond(dat_clean, !is.na(expo_travel_country1) & !expo_travel %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_travel_country2) & !expo_travel %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_travel_country3) & !expo_travel %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_travel_city1) & !expo_travel %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_travel_city2) & !expo_travel %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_travel_city3) & !expo_travel %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_travel_date1) & !expo_travel %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_travel_date2) & !expo_travel %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_travel_date3) & !expo_travel %in% "Yes")
  )
  
  # MULTI_16 Case contact details are specified (excluding "Unknown") but Case contact is not "Yes"
  queries[["MULTI_16"]] <- bind_queries(
    query_cond(dat_clean, !expo_case_setting_detail %in% c("Unknown", NA) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_ID1) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_ID2) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_ID3) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_ID4) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_ID5) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_first1) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_first2) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_first3) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_first4) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_first5) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_last1) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_last2) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_last3) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_last4) & !expo_contact_case %in% "Yes"),
    query_cond(dat_clean, !is.na(expo_case_date_last5) & !expo_contact_case %in% "Yes")
  )
  
  # MULTI_17 Case contact other setting is specified but Case contact setting is not specified 
  queries[["MULTI_17"]] <- query_cond(dat_clean, !is.na(MSF_expo_other_setting) & is.na(expo_case_setting_detail))
  
  # MULTI_18 Outcome date of symptom onset is specified but Outcome asymptomatic is not 'No'
  
  # MULTI_19 Visit type corresponds to 'Hospitalization' but Outcome admission to hospital is not 'Yes'
  queries[["MULTI_19"]] <- query_cond(dat_clean, patcourse_admit == "Yes" & !outcome_patcourse_admit %in% "Yes")
  
  # MULTI_20 Outcome date of admission specified but Outcome admission to hospital is not 'Yes'
  # queries[["MULTI_20"]] <- query_cond(dat_clean, !is.na(outcome_patcourse_presHCF) & !outcome_patcourse_admit %in% "Yes")
  # M19 <- queries[["MULTI_19"]] %>% select(site, MSF_N_Patient)
  # M20 <- queries[["MULTI_20"]] %>% select(site, MSF_N_Patient)
  # anti_join(M19, M20)
  # anti_join(M20, M19) # all MULTI_20 already picked up by MULTI_19
  
  # MULTI_21 Received oxygen at admission is 'Yes' but Outcome received oxyen is not 'Yes'
  queries[["MULTI_21"]] <- query_cond(dat_clean, MSF_received_oxygen == "Yes" & !MSF_outcome_received_oxygen %in% "Yes")
  
  # MULTI_22 Admitted to ICU at admission is 'Yes' but Outcome ICU is not 'Yes'
  queries[["MULTI_22"]] <- query_cond(dat_clean, patcourse_icu == "Yes" & !outcome_patcourse_icu %in% "Yes")
  
  # MULTI_23 Ventilated at admission is 'Yes' but Outcome ventilated is not 'Yes'
  queries[["MULTI_23"]] <- query_cond(dat_clean, patcourse_vent == "Yes" & !outcome_patcourse_vent %in% "Yes")
  
  # MULTI_24 Received ECMO at admission is 'Yes' but Outcome ECMO is not 'Yes'
  queries[["MULTI_24"]] <- query_cond(dat_clean, patcourse_ecmo == "Yes" & !outcome_patcourse_ecmo %in% "Yes")
  
  # MULTI_25 Other patient outcome status is specified but Patient outcome status is not 'Other'
  queries[["MULTI_25"]] <- query_cond(dat_clean, !is.na(outcome_patcourse_status_other) & is.na(outcome_patcourse_status))
  
  # MULTI_26 Outcome date of release or death is given but Outcome patient status is missing
  queries[["MULTI_26"]] <- query_cond(dat_clean, !is.na(outcome_date_of_outcome) & is.na(outcome_patcourse_status))
  
  # MULTI_27 Facility refer to is specified but Outcome patient status is not 'Transferred'
  queries[["MULTI_27"]] <- query_cond(dat_clean, !is.na(MSF_refer_to) & !outcome_patcourse_status %in% "Transferred")
  
  # MULTI_28 Lab date/result specified but Tested over the course of the disease is 'No' or 'Unknown'
  queries[["MULTI_28"]] <- bind_queries(
    query_cond(dat_clean, !is.na(Lab_date1) & MSF_outcome_tested %in% c("No", "Unknown")),
    query_cond(dat_clean, !is.na(outcome_lab_date) & MSF_outcome_tested %in% c("No", "Unknown")),
    query_cond(dat_clean, !is.na(outcome_lab_result) & MSF_outcome_tested %in% c("No", "Unknown"))
  )
    
  # MULTI_29 Number of contact followed is specified but Number contacts unknown is 'Yes'
  queries[["MULTI_29"]] <- query_cond(dat_clean, !is.na(outcome_contacts_followed) & outcome_contacts_followed_unknown == "Yes")
  
  # MULTI_30 Treatment name(s) are specified but Specific treament received is not 'Yes'
  queries[["MULTI_30"]] <- bind_queries(
    query_cond(dat_clean, !is.na(MSF_treament1) & !MSF_treatment %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_treament2) & !MSF_treatment %in% "Yes"),
    query_cond(dat_clean, !is.na(MSF_treament3) & !MSF_treatment %in% "Yes")
  )
  
  # MULTI_31 Date initiated treatment is specified but Name of treatment is missing
  queries[["MULTI_31"]] <- bind_queries(
    query_cond(dat_clean, !is.na(MSF_date_treament1) & is.na(MSF_treament1)),
    query_cond(dat_clean, !is.na(MSF_date_treament2) & is.na(MSF_treament2)),
    query_cond(dat_clean, !is.na(MSF_date_treament3) & is.na(MSF_treament3))
  )
  
  # MULTI_32 Former ID is specified but Type of visit doesn't correspond to a form of re-admission/consultation
  queries[["MULTI_32"]] <- query_cond(dat_clean, !is.na(MSF_former_ID_readmission) & MSF_visit_type %in% c("First consultation", "First hospitalisation"))
  
  # MULTI_33 Type of visit correspond to a form of re-admission/consultation but former ID is missing
  # queries[["MULTI_33"]] <- query_cond(dat_clean, MSF_visit_type %in% c("First hospitalisation after a consultation", "Rehospitalisation") & is.na(MSF_former_ID_readmission))
  
  
  
  return(bind_query_list(queries))
}



queries_other <- function(dat_raw, dat_clean) {
  
  queries <- list()
  
  # OTHER_01 Essential mandatory variable is missing
  queries[["OTHER_01"]] <- bind_queries(
    query_cond(dat_clean, is.na(patinfo_ageonset)),
    query_cond(dat_clean, is.na(patinfo_ageonsetunit)),
    query_cond(dat_clean, is.na(patinfo_sex)),
    query_cond(dat_clean, is.na(MSF_covid_status)),
    # query_cond(dat_clean, is.na(Lab_date1)),
    query_cond(dat_clean, is.na(MSF_visit_type)),
    # query_cond(dat_clean, is.na(MSF_date_consultation)),
    # query_cond(dat_clean, is.na(outcome_patcourse_status)),
    # query_cond(dat_clean, is.na(outcome_date_of_outcome))
  )
  
  
  # OTHER_02 ECMO variable is "Yes" (to be checked by field)
  queries[["OTHER_02"]] <- query_cond(dat_clean, patcourse_ecmo %in% "Yes" | outcome_patcourse_ecmo %in% "Yes")
  
  return(bind_query_list(queries))
}



bind_query_list <- function(x) {
  lnames <- names(x)
  lnrows <- vapply(x, nrow, 0L)
  query_id <- rep(lnames, times = lnrows)
  dplyr::bind_cols(query_id = query_id, dplyr::bind_rows(x))
}


query_cond <- function(dat, cond) {
  
  library(dplyr)
  library(rlang)
  
  vars <- all.vars(substitute(cond))
  
  col_var <- paste0("variable", seq_along(vars))
  col_val <- paste0("value", seq_along(vars))
  vars_out <- c(rbind(col_var, col_val))
  col_var_recode <- setNames(vars, col_val)
  
  out <- dat %>% 
    filter(!!enquo(cond)) %>% 
    rename(!!!col_var_recode)
  
  # 
  for (i in seq_along(col_var)) {
    out[[col_var[i]]] <- vars[i]
  }
  
  out %>% 
    select(OC, country, site, MSF_N_Patient, linelist_row, upload_date, all_of(vars_out)) %>% 
    mutate_all(as.character)
}


query_missing_id <- function(dat) {
  
  library(dplyr)
  
  dat %>% 
    filter(is.na(MSF_N_Patient)) %>% 
    select(OC, country, site, MSF_N_Patient, linelist_row, upload_date) %>% 
    mutate_all(as.character)
}


query_duplicate_id <- function(dat) {
  
  library(dplyr)
  
  dat %>% 
    filter(!is.na(MSF_N_Patient)) %>% 
    group_by(OC, country, site, MSF_N_Patient) %>% 
    summarize(n = n(),
              linelist_row = paste(linelist_row, collapse = "; "),
              upload_date = paste(upload_date, collapse = "; "),
              .groups = "drop") %>% 
    filter(n > 1) %>% 
    select(OC, country, site, MSF_N_Patient, linelist_row, upload_date) %>% 
    mutate_all(as.character)
}


format_queries <- function(d, col_arrange = c("country", "site", "MSF_N_Patient")) {
  d %>%
    mutate_all(as.character) %>% 
    arrange(dplyr::across(all_of(col_arrange)))
}


query_match_dict <- function(dat_prep, dict_factors, lang) {
  
  lang <- match.arg(lang, c("en", "es", "fr"))
  dict_var <- paste0("values_", lang)
  
  lang_val <- switch(lang,
                     "en" = "English",
                     "es" = "Espagnol",
                     "fr" = "Français")
  
  dict_prep <- dict_factors %>% 
    mutate(value_std = hmatch::string_std(!!ensym(dict_var))) %>% 
    select(variable, value_std)
  
  dat_prep %>%
    filter(linelist_lang == lang_val) %>%
    filter(!is.na(value)) %>%
    anti_join(dict_prep, by = c("variable", "value_std")) %>% 
    select(-linelist_lang, -value_std) %>% 
    mutate_all(as.character) %>% 
    rename(variable1 = variable, value1 = value)
}



query_dates_chrono <- function(d, cat1, cat2) {
  
  # TODO: change to single pivot_longer 
  d1 <- d %>% 
    filter(date_category == cat1) %>% 
    select(-date_category)
  
  d2 <- d %>% 
    filter(date_category == cat2) %>% 
    select(-date_category) %>% 
    setNames(gsub("1", "2", names(.)))
  
  inner_join(d1, d2, by = c("OC", "country", "site", "MSF_N_Patient",
                            "linelist_row", "upload_date")) %>% 
    filter(date1 < date2) %>% 
    select(-starts_with("date")) %>% 
    format_queries()
}

