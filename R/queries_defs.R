

# set id columns
library(queryr)
id_cols <- c("OC", "country", "site", "MSF_N_Patient", "linelist_row", "upload_date")
options(queryr_cols_base = id_cols)



queries_ident <- function(dat_raw, dat_clean) {
  
  queries <- list()
  
  # IDENT_01 Missing value for MSF_N_patient
  queries[["IDENT_01"]] <- query(dat_raw, is.na(MSF_N_Patient))
  
  # IDENT_02 Duplicated value of MSF_N_patient
  queries[["IDENT_02"]] <- query(dat_clean, duplicated(patient_id))
  
  return(bind_query_list(queries))
}



queries_dates <- function(dat_raw, date_vars, dict_date_categories) {
  
  ## prep data
  vars_check <- setdiff(date_vars, "upload_date")
  
  dates_raw <- dat_raw %>%
    mutate_at(all_of(date_vars), as.character)
  
  dates_parse <- dates_raw %>% 
    mutate_at(all_of(vars_check), parse_dates)

  ## queries
  queries <- list()
  
  # DATES_01 Date value not unambiguously coercible to YYYY-MM-DD
  queries[["DATES_01"]] <- query(dates_raw, non_valid_date(.x), cols_dotx = all_of(vars_check))
  
  # DATES_02 Date in future (relative to upload date)
  queries[["DATES_02"]] <- query(dates_parse, .x > as.Date(upload_date), cols_dotx = all_of(vars_check))
  
  # DATES_03 Date too early (before 2020-02-01)
  queries[["DATES_03"]] <- query(dates_parse, .x < as.Date("2020-02-01"), cols_dotx = all_of(vars_check))
  
  # DATES_04 Date of last contact with confirmed case earlier than date of first contact
  queries[["DATES_04"]] <- dplyr::bind_rows(
    query(dates_parse, expo_case_date_last1 < expo_case_date_first1),
    query(dates_parse, expo_case_date_last2 < expo_case_date_first2),
    query(dates_parse, expo_case_date_last3 < expo_case_date_first3),
    query(dates_parse, expo_case_date_last4 < expo_case_date_first4),
    query(dates_parse, expo_case_date_last5 < expo_case_date_first5)
  )
  
  # DATES_05 Report date earlier than EXPOSURE or ONSET date(s)
  queries[["DATES_05"]] <- dplyr::bind_rows(
    query(dates_parse, report_date < .x, cols_dotx = starts_with("expo_travel_date")),
    query(dates_parse, report_date < .x, cols_dotx = starts_with("expo_case_date_first")),
    query(dates_parse, report_date < patcourse_dateonset)
  )
  
  # DATES_06 Specific symptom onset date(s) earlier than Date symptom onset
  queries[["DATES_06"]] <- query(dates_parse, .x < patcourse_dateonset, cols_dotx = matches("MSF_symptom.*date_onset"))
  
  # DATES_07 ONSET date(s) earlier than EXPOSURE date(s)
  vars_expos <- dict_date_categories$code_name[dict_date_categories$date_category %in% "EXPOSURE"]
  vars_onset <- dict_date_categories$code_name[dict_date_categories$date_category %in% "ONSET"]
  vars_admit <- dict_date_categories$code_name[dict_date_categories$date_category %in% "ADMISSION"]
  vars_outco <- dict_date_categories$code_name[dict_date_categories$date_category %in% "OUTCOME"]
  
  queries[["DATES_07"]] <- query(dates_parse, patcourse_dateonset < .x, cols_dotx = all_of(vars_expos))
  
  # DATES_08 ADMISSION date(s) earlier than ONSET or EXPOSURE date(s)
  queries[["DATES_08"]] <- dplyr::bind_rows(
    query(dates_parse, Lab_date1 < .x, cols_dotx = all_of(vars_onset)),
    query(dates_parse, MSF_date_consultation < .x, cols_dotx = all_of(vars_onset))
  )
  
  # DATES_09 OUTCOME date(s) earlier than ADMISSION or ONSET or EXPOSURE date(s)
  queries[["DATES_09"]] <- dplyr::bind_rows(
    query(dates_parse, outcome_submitted_date < .x, cols_dotx = all_of(vars_admit)),
    query(dates_parse, outcome_date_of_outcome < .x, cols_dotx = all_of(vars_admit)),
    query(dates_parse, outcome_lab_date < .x, cols_dotx = all_of(vars_admit))
  )
  
  return(bind_query_list(queries))
}



queries_categorical <- function(dat_raw,
                                dict_factors,
                                dict_countries,
                                categ_query_exclude,
                                dict_countries_correct) {
  
  ## requires
  library(dplyr)
  library(tidyr)
  library(hmatch)
  
  ## prep data
  cols_base <- c(
    "linelist_lang",
    "OC",
    "country",
    "site",
    "MSF_N_Patient",
    "linelist_row",
    "upload_date"
  )
  
  cols_iso <- c(
    "report_country",
    "patinfo_idadmin0",
    "patinfo_resadmin0",         # String free text
    "patinfo_occuhcw_country",   # String free text
    "expo_case_location",        # String free text
    "expo_travel_country1",      # String free text
    "expo_travel_country2",      # String free text
    "expo_travel_country3"       # String free text
  )
  
  dict_countries_prep <- dict_countries_correct %>% 
    filter(query == "Yes")
  
  iso_valid_std <- hmatch::string_std(
    c(unique(dict_countries$iso), "Unknown", NA_character_)
  )
  
  dat_prep <- dat_raw %>% 
    select(all_of(cols_base), all_of(unique(dict_factors$variable))) %>% 
    tidyr::pivot_longer(cols = all_of(unique(dict_factors$variable)), names_to = "variable") %>% 
    mutate(value_std = hmatch::string_std(value))
  
  ## queries
  queries <- list()
  
  # CATEG_01 Value of coded-list variable doesn't match dictionary (ignoring variation in case and punctuation)
  queries[["CATEG_01"]] <- dplyr::bind_rows(
    query_match_dict(dat_prep, dict_factors, lang = "en"),
    query_match_dict(dat_prep, dict_factors, lang = "es"),
    query_match_dict(dat_prep, dict_factors, lang = "fr")
  ) %>% 
    # use hmatch rather than dplyr::anti_join to allow variation in case
    hmatch::hmatch(
      categ_query_exclude,
      by = c("variable1", "value1"),
      allow_gaps = FALSE,
      type = "anti",
      std_fn = tolower
    )
  
  # CATEG_02 Country value does not match valid ISO3 country code
  # strict for vars report_country and patinfo_idadmin0; relaxed for others of data type Sting free text
  queries[["CATEG_02"]] <- query(dat_raw, !hmatch::string_std(.x) %in% iso_valid_std, cols_dotx = all_of(cols_iso)) %>% 
    dplyr::semi_join(dict_countries_prep, by = c("variable1", "value1"))
  
  
  return(bind_query_list(queries))
}



queries_multi <- function(dat_raw, dat_clean) {
  
  queries <- list()
  
  # MUTI_01 Comorbidity count doesn't match count indicated by subsequent Comcond_* variables
  queries[["MULTI_01"]] <- query(dat_raw, !valid_numeric(Comcond_present))
  
  # MUTI_02 Covid19 Status is "Confirmed" but there is also an entry for Main diagnosis (if not a case)
  queries[["MULTI_02"]] <- query(dat_clean, MSF_covid_status == "Confirmed" & !is.na(MSF_main_diagnosis))
  
  # MUTI_03 Asymptomatic is "Yes" but date of symptom onset still given
  queries[["MULTI_03"]] <- query(dat_clean, patcourse_asymp == "Yes" & !is.na(patcourse_dateonset))
  
  # MULTI_04 Date of symptom onset is given but symptom status is not "Yes"
  queries[["MULTI_04"]] <- dplyr::bind_rows(
    query(dat_clean, !is.na(MSF_symptom_fever_date_onset) & !MSF_symptom_fever %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_cough_date_onset) & !MSF_symptom_cough %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_breathlessness_date_onset) & !MSF_symptom_breathlessness %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_sorethoat_date_onset) & !MSF_symptom_sorethoat %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_chills_date_onset) & !MSF_symptom_chills %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_headache_date_onset) & !MSF_symptom_headache %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_abdominal_pain_date_onset) & !MSF_symptom_abdominal_pain %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_tiredness_date_onset) & !MSF_symptom_tiredness %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_aches_date_onset) & !MSF_symptom_aches %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_nose_congestion_date_onset) & !MSF_symptom_nose_congestion %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_runny_nose_date_onset) & !MSF_symptom_runny_nose %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_diarrhoea_date_onset) & !MSF_symptom_diarrhoea %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_vomiting_date_onset) & !MSF_symptom_vomiting %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_loss_taste_date_onset) & !MSF_symptom_loss_taste %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_anosmia_date_onset) & !MSF_symptom_anosmia %in% "Yes"),
    query(dat_clean, !is.na(MSF_symptom_chest_pain_date_onset) & !MSF_symptom_chest_pain %in% "Yes")
  )
  
  # MULTI_05 Trimester of pregnancy is given (excluding "Unknown") but Pregnancy is not "Yes"
  queries[["MULTI_05"]] <- query(dat_clean, !Comcond_pregt %in% c("Unknown", NA) & !Comcond_preg %in% "Yes")
  
  # MULTI_06 CD4 count or HIV viral load given (excluding 'Unknown') but HIV status not a form of "Positive..."
  queries[["MULTI_06"]] <- query(
    dat_clean,
    !is.na(.x) & !tolower(.x) %in% c("inconnu", "unknown") & !grepl("posit", MSF_hiv_status, ignore.case = TRUE),
    cols_dotx = all_of(c("MSF_hiv_cd4", "MSF_hiv_viral_load"))
  )
  
  # MULTI_07 Hypertension is "Yes" but Cardiovascular disease (incl. hypertension) is not "Yes"
  queries[["MULTI_07"]] <- query(dat_clean, MSF_hypertension == "Yes" & !Comcond_cardi %in% "Yes")

  # MULTI_08 Type of chronic lung disease is specified but Chronic lung disease is not "Yes"
  queries[["MULTI_08"]] <- query(dat_clean, !is.na(MSF_type_lung_disease) & !Comcond_lung %in% "Yes")
  
  # MULTI_09 Resistant TB is 'Yes' but Active TB is not a form of "Yes…"
  queries[["MULTI_09"]] <- query(dat_clean, MSF_tb_resistant == "Yes" & !grepl("yes", MSF_tb_active, ignore.case = TRUE))
  
  # MULTI_10 ID of origin is specified but Health structure referred from is missing
  queries[["MULTI_10"]] <- query(dat_clean, !is.na(MSF_ID_refer_from) & is.na(MSF_refer_from))
  
  # MULTI_11 Hospital-related variable (oxygen, ICU, ventilated, and/or ECMO) is 'Yes' but Type of visit does not correspond to "Hospitalization"
  # don't flag if MSF_visit_type = <NA> because separate query for that (OTHER_01)
  vars_hosp <- c("MSF_received_oxygen", "patcourse_icu", "patcourse_vent", "patcourse_ecmo")
  queries[["MULTI_11"]] <- query(dat_clean, .x == "Yes" & MSF_visit_type %in% "First consultation", cols_dotx = all_of(vars_hosp))
  
  # MULTI_12 Date of isolation is given but Case isolated is not "Yes"
  queries[["MULTI_12"]] <- query(dat_clean, !is.na(patcourse_dateiso) & !patcourse_iso %in% "Yes")
  
  # MULTI_13 Other main complication is specified but Main complication is missing
  queries[["MULTI_13"]] <- query(dat_clean, !is.na(MSF_other_complications) & is.na(MSF_complications))
  
  # MULTI_14 Healthcare worker country is specified but Healthcare worker is not "Yes" (*country currently autopopulated in v1.2 and earlier)
  
  # MULTI_15 Country, city, and/or date of travel is specified but Travels is not "Yes"
  queries[["MULTI_15"]] <- query(dat_clean, !is.na(.x) & !expo_travel %in% "Yes", cols_dotx = matches("expo_travel_.*[0-9]"))
  
  # MULTI_16 Case contact details are specified (excluding "Unknown") but Case contact is not "Yes"
  queries[["MULTI_16"]] <- dplyr::bind_rows(
    query(dat_clean, !expo_case_setting_detail %in% c("Unknown", NA) & !expo_contact_case %in% "Yes"),
    query(dat_clean, !is.na(.x) & !expo_contact_case %in% "Yes", cols_dotx = matches("expo_ID[0-9]")),
    query(dat_clean, !is.na(.x) & !expo_contact_case %in% "Yes", cols_dotx = matches("expo_case_date_first[0-9]")),
    query(dat_clean, !is.na(.x) & !expo_contact_case %in% "Yes", cols_dotx = matches("expo_case_date_last[0-9]"))
  )
  
  # MULTI_17 Case contact other setting is specified but Case contact setting is not specified 
  queries[["MULTI_17"]] <- query(dat_clean, !is.na(MSF_expo_other_setting) & is.na(expo_case_setting_detail))
  
  # MULTI_18 Outcome date of symptom onset is specified but Outcome asymptomatic is not 'No'
  
  # MULTI_19 Visit type corresponds to 'Hospitalization' but Outcome admission to hospital is not 'Yes'
  queries[["MULTI_19"]] <- query(dat_clean, patcourse_admit == "Yes" & !outcome_patcourse_admit %in% "Yes")
  
  # MULTI_20 Outcome date of admission specified but Outcome admission to hospital is not 'Yes'
  # queries[["MULTI_20"]] <- query(dat_clean, !is.na(outcome_patcourse_presHCF) & !outcome_patcourse_admit %in% "Yes")
  # M19 <- queries[["MULTI_19"]] %>% select(site, MSF_N_Patient)
  # M20 <- queries[["MULTI_20"]] %>% select(site, MSF_N_Patient)
  # anti_join(M19, M20)
  # anti_join(M20, M19) # all MULTI_20 already picked up by MULTI_19
  
  # MULTI_21 Received oxygen at admission is 'Yes' but Outcome received oxyen is not 'Yes'
  queries[["MULTI_21"]] <- query(dat_clean, MSF_received_oxygen == "Yes" & !MSF_outcome_received_oxygen %in% "Yes")
  
  # MULTI_22 Admitted to ICU at admission is 'Yes' but Outcome ICU is not 'Yes'
  queries[["MULTI_22"]] <- query(dat_clean, patcourse_icu == "Yes" & !outcome_patcourse_icu %in% "Yes")
  
  # MULTI_23 Ventilated at admission is 'Yes' but Outcome ventilated is not 'Yes'
  queries[["MULTI_23"]] <- query(dat_clean, patcourse_vent == "Yes" & !outcome_patcourse_vent %in% "Yes")
  
  # MULTI_24 Received ECMO at admission is 'Yes' but Outcome ECMO is not 'Yes'
  queries[["MULTI_24"]] <- query(dat_clean, patcourse_ecmo == "Yes" & !outcome_patcourse_ecmo %in% "Yes")
  
  # MULTI_25 Other patient outcome status is specified but Patient outcome status is not 'Other'
  queries[["MULTI_25"]] <- query(dat_clean, !is.na(outcome_patcourse_status_other) & is.na(outcome_patcourse_status))
  
  # MULTI_26 Outcome date of release or death is given but Outcome patient status is missing
  queries[["MULTI_26"]] <- query(dat_clean, !is.na(outcome_date_of_outcome) & is.na(outcome_patcourse_status))
  
  # MULTI_27 Facility refer to is specified but Outcome patient status is not 'Transferred'
  queries[["MULTI_27"]] <- query(dat_clean, !is.na(MSF_refer_to) & !outcome_patcourse_status %in% "Transferred")
  
  # MULTI_28 Lab date/result specified but Tested over the course of the disease is 'No' or 'Unknown'
  vars_lab <- c("Lab_date1", "outcome_lab_date", "outcome_lab_result")
  queries[["MULTI_28"]] <- query(dat_clean, !is.na(.x) & MSF_outcome_tested %in% c("No", "Unknown"), cols_dotx = all_of(vars_lab))
  
  # MULTI_29 Number of contact followed is specified but Number contacts unknown is 'Yes'
  queries[["MULTI_29"]] <- query(dat_clean, !is.na(outcome_contacts_followed) & outcome_contacts_followed_unknown == "Yes")
  
  # MULTI_30 Treatment name(s) are specified but Specific treament received is not 'Yes'
  queries[["MULTI_30"]] <- query(dat_clean, !is.na(.x) & !MSF_treatment %in% "Yes", cols_dotx = matches("MSF_treament[0-9]"))
  
  # MULTI_31 Date initiated treatment is specified but Name of treatment is missing
  queries[["MULTI_31"]] <- dplyr::bind_rows(
    query(dat_clean, !is.na(MSF_date_treament1) & is.na(MSF_treament1)),
    query(dat_clean, !is.na(MSF_date_treament2) & is.na(MSF_treament2)),
    query(dat_clean, !is.na(MSF_date_treament3) & is.na(MSF_treament3))
  )
  
  # MULTI_32 Former ID is specified but Type of visit doesn't correspond to a form of re-admission/consultation
  queries[["MULTI_32"]] <- query(dat_clean, !is.na(MSF_former_ID_readmission) & MSF_visit_type %in% c("First consultation", "First hospitalisation"))
  
  # MULTI_33 Type of visit correspond to a form of re-admission/consultation but former ID is missing
  # queries[["MULTI_33"]] <- query(dat_clean, MSF_visit_type %in% c("First hospitalisation after a consultation", "Rehospitalisation") & is.na(MSF_former_ID_readmission))
  
  
  # MULTI_36 Asymptomatic is 'Yes' but some symptoms are recorded as 'Yes'
  cols_symptom <- grep("MSF_symptom(?!.*date_onset$)", value = TRUE, names(dat_clean), perl = TRUE)
  queries[["MULTI_36"]] <- query(dat_clean, patcourse_asymp == "Yes" & .x == "Yes", cols_dotx = all_of(cols_symptom))
  
  
  return(bind_query_list(queries))
}



queries_other <- function(dat_raw, dat_clean) {
  
  queries <- list()
  
  # OTHER_01 Essential mandatory variable is missing
  vars_essential <- c(
    "patinfo_ageonset",
    "patinfo_ageonsetunit",
    "patinfo_sex",
    "MSF_covid_status",
    # "Lab_date1",
    "MSF_visit_type",
    "MSF_date_consultation"
    # "outcome_patcourse_status",
    # "outcome_date_of_outcome"
  )
  
  queries[["OTHER_01"]] <- query(dat_clean, is.na(.x), cols_dotx = all_of(vars_essential))
  
  # OTHER_02 ECMO variable is "Yes" (to be checked by field)
  queries[["OTHER_02"]] <- query(dat_clean, patcourse_ecmo %in% "Yes" | outcome_patcourse_ecmo %in% "Yes")
  
  return(bind_query_list(queries))
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
    rename(variable1 = variable, value1 = value)
}

