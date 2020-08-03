#' Import triage linelist from AFG
#' 
#' 
import_other_afg_tri <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  source("R/zzz.R")
  source("R/utilities.R")
  source("R/import_other_afg_tri.R")
  
  
  ### Read linelist
  file_ll <- llutils::list_files(
    file.path(path_linelist_other, "OCP", "AFG"),
    pattern = "\\.xlsx",
    full.names = TRUE,
    select = "latest"
  )
  
  d_orig <- import_afg_tri_helper(file_ll)
  
  site_meta <- data.frame(
    msf_facitity = c("hrh", "idp"),
    site = c("AFG_P_HRH", "AFG_P_IDP"),
    project = c("HET", "HEI"),
    site_type = c("Other facility", "Health Centre"),
    site_name = c("Herat Regional Hospital MSF COVID-19 Triage", "MSF IDP Clinic")
  )
  
  ### Check for unseen values in derivation variables
  test_set_equal(d_orig$msf_facitity, c("hrh", "idp"))
  test_set_equal(d_orig$does_the_patient_have_symptoms, c("yes", "no", NA))
  test_set_equal(d_orig$hiv_status, c("positive", "negative", "no", "unknown", NA))
  test_set_equal(d_orig$if_positive_on_arv, c("yes", "no", "unknown", NA))
  test_set_equal(d_orig$previous_test_result, c("pasitive", "positive", "negative", "unknown", NA))
  test_set_equal(d_orig$lab_result, c("indeterminate", NA))
  test_set_equal(d_orig$category_according_to_clinical_examination, c("critical", "severe", "moderate", "mild", "not a suspect", NA))
  test_set_equal(d_orig$sent_for_testing, c("yes", "no", NA))
  test_set_equal(d_orig$other_immunodeficiency, c("yes", "no","unknown",  NA))
  test_set_equal(d_orig$malignancy_cancer, c("yes", "no", "unknown", NA))
  
  ### Constants and derived variables
  d_derived <- d_orig %>% 
    # site metadata
    mutate(msf_facitity = tolower(msf_facitity)) %>% 
    left_join(site_meta, by = "msf_facitity") %>% 
    # add various constants
    mutate(
      report_country = "AFG",
      patinfo_idadmin0 = "AFG",
      patinfo_resadmin0 = "AFG",
      patinfo_ageonsetunit = "Year",
      patinfo_idadmin1 = "Herat",
      MSF_visit_type = "First consultation"
    ) %>% 
    # derive MSF_admin_location_past_week
    mutate(across(province:city_village_idp_camp, ~ ifelse(is.na(.x), "", .x))) %>% 
    unite("MSF_admin_location_past_week", province:city_village_idp_camp, sep = " | ") %>% 
    # derive patcourse_asymp
    mutate(patcourse_asymp = case_when(
      tolower(does_the_patient_have_symptoms) == "yes" ~ "No",
      tolower(does_the_patient_have_symptoms) == "no" ~ "Yes"
    )) %>% 
    # derive Comcond_immuno
    mutate(across(c(hiv_status, other_immunodeficiency, malignancy_cancer), tolower)) %>% 
    mutate(Comcond_immuno = case_when(
      hiv_status == "positive" | other_immunodeficiency == "yes" | malignancy_cancer == "yes" ~ "Yes",
      TRUE ~ NA_character_
    )) %>% 
    # derive MSF_hiv_status
    mutate(across(c(hiv_status, if_positive_on_arv), tolower)) %>% 
    mutate(MSF_hiv_status = case_when(
      hiv_status == "positive" & if_positive_on_arv == "yes" ~ "Positive (on ARV)",
      hiv_status == "positive" & if_positive_on_arv == "no" ~ "Positive (no ARV)",
      hiv_status == "positive" & !if_positive_on_arv %in% c("yes", "no") ~ "Positive (unknown ARV)",
      hiv_status %in% c("no", "negative") ~ "Negative",
      hiv_status == "unknown" ~ "Unknown"
    )) %>% 
    # derive MSF_covid_status
    mutate(across(c(previous_test_result, lab_result, category_according_to_clinical_examination, sent_for_testing), tolower)) %>% 
    mutate(previous_test_result = recode(previous_test_result, "pasitive" = "positive")) %>% 
    mutate(MSF_covid_status = case_when(
      previous_test_result == "positive" | lab_result == "positive"	~ "Confirmed",
      category_according_to_clinical_examination %in% c("mild", "moderate", "severe", "critical") & sent_for_testing == "yes" ~ "Suspected",
      category_according_to_clinical_examination %in% c("mild", "moderate", "severe", "critical") & !sent_for_testing %in% "yes" ~ "Probable",
      category_according_to_clinical_examination == "not a suspect"	~ "Not a suspect"
      # if lab_result negative should they be "Not a case"?
    )) %>% 
    # derive MSF_refer_to
    mutate(
      MSF_refer_to = ifelse(referral == "Other:", if_other_58, referral),
      MSF_refer_to = ifelse(tolower(MSF_refer_to) == "not suspected", NA_character_, MSF_refer_to)
    )
  
  
  # # examine variable combos relating to derivation of MSF_covid_status
  # d_derived %>%
  #   select(previous_test_result, lab_result, clinical_category = category_according_to_clinical_examination, sent_for_testing, MSF_covid_status) %>%
  #   mutate_all(tolower) %>%
  #   mutate(clinical_category = ifelse(clinical_category %in% c("critical", "severe", "moderate", "mild"), "mild|moderate|severe|critical", clinical_category)) %>%
  #   count(previous_test_result, lab_result, clinical_category, sent_for_testing, MSF_covid_status)
  
  # # examine variable combos relevant to derivation of MSF_refer_to
  # d_derived %>% 
  #   count(referral, if_other_58, MSF_refer_to)
  
  
  ### Recode columns with straightforward mapping to Epicentre variables
  d_recode <- d_derived %>% 
    rename(
      # no,
      # solar_date,
      MSF_date_consultation = gregorian_date, # also report_date?
      # day_of_the_year,
      # patient_no,
      # msf_facitity,
      MSF_N_Patient = msf_id,
      # 8,
      # x9,
      patinfo_ageonset = age, # constant patinfo_ageonsetunit = "Years"
      patinfo_sex = gender,
      Comcond_preg = if_female_pregnant,
      # if_female_postpartum,
      # first_time_at_covid_19_traige,  # MSF_visit_type?
      # if_not_when_was_the_last_time,
      # does_the_patient_have_symptoms,
      patcourse_dateonset = if_no_onset_of_symptoms,
      # province,               # component of MSF_admin_location_past_week
      # district,               # component of MSF_admin_location_past_week
      # city_village_idp_camp,  # component of MSF_admin_location_past_week
      # location_detail,
      # idp,
      # x23,
      MSF_refer_from = referred_from,
      MSF_symptom_fever = fever,
      MSF_symptom_cough = cough,
      MSF_symptom_breathlessness = difficulty_breathing,
      MSF_symptom_anosmia = loss_or_change_of_smell,
      MSF_symptom_loss_taste = loss_or_change_of_taste,
      MSF_symptom_sorethoat = sore_throat,
      MSF_symptom_tiredness = fatigue,
      MSF_symptom_headache = headache,
      MSF_symptom_vomiting = vomiting,
      # confusion,
      MSF_symptom_chest_pain = chest_pain,
      MSF_symptom_diarrhoea = diarrhoea,
      # able_to_walk_alone,
      Comcond_cardi = cardiovascular_disease,
      MSF_hypertension = hypertension,
      Comcond_diabetes = diabetes,
      Comcond_liver = liver_disease,
      Comcond_renal = renal_disease,
      Comcond_lung = chronic_lung_disease,
      MSF_tb_active = active_tuberculosis,
      MSF_malnutrition = acute_malnutrition,
      # obesity,
      Comcond_malig = malignancy_cancer,
      # hiv_status,
      # if_positive_on_arv,
      # other_immunodeficiency,
      patinfo_occuhcw = health_care_worker,
      # family_member_with_the_same_symptoms,
      expo_contact_case = contact_to_confirmed_case_last_14_days_before_onset,
      expo_travel = history_of_travel_last_14_days_before_onset,
      expo_travel_country1 = if_yes_destination,
      MSF_severity = category_according_to_clinical_examination,
      # referral,
      # if_other_58,
      # patient_refusing_referral,
      # temperature_celsius,
      # sp_o2_percent,
      # heart_rate_per_minute,
      # respiratory_rate_per_minute,
      # blood_pressure_high,
      # blood_pressure_low,
      # tested_previously,
      # if_yes_when,
      # previous_test_result,
      # sent_for_testing,
      Lab_date1 = if_yes_when_2,
      # if_no_why,
      # if_other_72,
      MSF_test_results = lab_result,
      # date_lab_result,
      # doctor_name
    )


  ## global derived columns
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

  
  ## add patient_id and db_row
  df_data <- d_recode %>% 
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
 


import_afg_tri_helper <- function(path, site) {
  
  upload_date <- as.Date(
    stringr::str_extract(path, "[0-9]{4}_[0-9]{2}_[0-9]{2}"),
    format = "%Y_%m_%d"
  )
  
  readxl::read_xlsx(
    path, 
    sheet = "data",
    skip = 1, col_types = "text",
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>% 
    janitor::clean_names() %>% 
    mutate(linelist_row = 1:n(),
           upload_date = as.character(upload_date),
           linelist_lang = "English",
           linelist_vers = "Other",
           country = "AFG",
           shape = "AFG",
           OC = "OCP",
           # project = NA_character_,
           # site_type = NA_character_,
           # site_name = NA_character_,
           # site = NA_character_,
           uid = NA_character_)
}

