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
import_other_bel_ocb_snk <- function(path_linelist_other, dict_linelist) {
  
  ## requires
  library(dplyr)
  library(hmatch)
  
  path_to_files <- file.path(path_linelist_other, "OCB", "BEL")
  
  df_map <- file.path(path_to_files, "LL_v3.0_mapping_Belgium_SNK.xlsx") %>% 
    readxl::read_xlsx() %>% 
    janitor::clean_names() %>% 
    select(1, 6, 7, 8, 9, 10) %>% 
    setNames(c("var_epi", "category", "map_type", "map_direct", "map_constant", "map_derive"))
  
  df_map_direct <- df_map %>% 
    filter(map_type == "1:1 correspondence") %>% 
    select(var_epi, map_direct) %>% 
    mutate(map_direct_std = janitor::make_clean_names(map_direct))
  
  vec_map_direct <- setNames(
    df_map_direct$map_direct_std,
    df_map_direct$var_epi
  )
  
  df_map_constant <- df_map %>% 
    filter(map_type == "Constant value", !category %in% "Site metadata") %>% 
    select(var_epi, map_constant) %>% 
    tidyr::pivot_wider(names_from = "var_epi", values_from = "map_constant")
  
  df_map_derive <- df_map %>% 
    filter(map_type == "Requires derivation") %>% 
    select(var_epi, map_derive)
  
  file_ll <- llutils::list_files(
    path_to_files,
    pattern = "^SNK_cleaned linelist.*\\.xlsx",
    ignore.case = TRUE,
    full.names = TRUE,
    select = "latest"
  )
  
  dict_facilities_join <- dict_facilities %>% 
    mutate_all(as.character) %>% 
    select(site, country, shape, OC, project, site_name, site_type, uid)
  
  d_orig <- readxl::read_xlsx(
    file_ll,
    col_types = "text",
    # skip = 1,
    .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
  ) %>% 
    janitor::remove_empty("rows") %>% 
    janitor::clean_names() %>% 
    mutate(site = "BEL_B_SNK") %>% 
    dplyr::left_join(dict_facilities_join, by = "site") %>% 
    mutate(upload_date = as.character(llutils::extract_date(file_ll)))

  ### Check for unseen values in derivation variables
  # test_set_equal(d_orig$substance_abuse, c("yes", "no", NA))
  # test_set_equal(d_orig$gastro_intestinal, c("yes", "no", NA))
  # test_set_equal(d_orig$psychiatric_condition, c("yes", "no", NA))
  
  df_comcond_other <- d_orig %>% 
    select(
      hep_c, epilepsy, depression, psychosis, bipolar_disorder, schizophrenia,
      scabies, alcool, addiction_medication, cocain, opioid_addication,
      drug_addition_incl_cannabis_haschich_cocain_heroin_medication
    ) %>% 
    map2_df(., names(.), ~ dplyr::recode(.x, "1" = .y)) %>% 
    mutate(Comcond_other = pmap_chr(., collapse_unique, to_chr =  TRUE))
  
  ### Derived variables
  d_derive <- d_orig %>% 
    mutate(
      MSF_N_Patient = formatC(1:n(), width = 3, flag = "0"),
      MSF_covid_status = case_when(
        test_result %in% "Pos" ~ "Confirmed",
        test_result %in% "Neg" ~ "Not a case",
        reason_of_admission %in% "Confirmed" ~ "Confirmed",
        reason_of_admission %in% "SAS" ~ "Not a suspect",
        reason_of_admission %in% "Contact" & symptoms_on_admission %in% "Yes" ~ "Suspect",
        reason_of_admission %in% "Contact" & !symptoms_on_admission %in% "Yes" ~ "Not a suspect",
        reason_of_admission %in% "Suspect" ~ "Suspect",
        exit_type %in% "Not a case" ~ "Not a case"
      ),
      MSF_hiv_status = case_when(
        hiv %in% "1" ~ "Positive (unknown)"
      ),
      MSF_type_lung_disease = case_when(
        copd %in% "1" ~ "COPD",
        asthma %in% "1" ~ "Asthma"
      ),
      MSF_smoking = case_when(
        smoking %in% "1" ~ "Yes (current)",
        smoking %in% "0" ~ "No"
      ),
      outcome_patcourse_status = case_when(
        exit_type %in% "Discharged: end of confinement" ~ "Cured",
        exit_type %in% "Discharged: end of quarantine" ~ "Cured",
        exit_type %in% "Not a case" ~ "Other",
        exit_type %in% "Left against medical advice" ~ "Left against medical advice",
        exit_type %in% "Referred out" ~ "Transferred"
      ),
      Comcond_other = df_comcond_other$Comcond_other,
      across(
        c(
          pregnant, post_partum,
          cvd_including_hbp, hbp, diabetis, chronic_liver_disease, chronic_kidney_disease,
          malignancy, chronic_lung_disease_incl_asthma_copd, obesity
        ),
        ~ case_when(
          .x %in% "0" ~ "No",
          .x %in% "1" ~ "Yes",
          TRUE ~ .x
        )
      )
    )
  
  
  ### Constants and 1:1 mappings
  d_out <- d_derive %>% 
    rename(all_of(vec_map_direct)) %>% 
    select(-any_of(names(df_map_constant))) %>% # reassess
    dplyr::bind_cols(df_map_constant, .)
  
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
  df_data <- d_out %>% 
    group_by(site) %>% 
    mutate(linelist_row = 1:n()) %>% 
    ungroup() %>% 
    mutate(patient_id = paste(site, format_text(MSF_N_Patient), sep = "_")) %>% 
    mutate(db_row = 1:n()) %>% 
    mutate(linelist_lang = "English",
           linelist_vers = "Other")
  
  ## columns to add (from original ll template)
  ll_template <- dict_linelist$code_name
  cols_to_add <- setdiff(c(cols_derive, ll_template), names(df_data))
  df_data[cols_to_add] <- NA_character_
  
  # check for new columns to be manually renamed
  extra_cols <- grep("^extra__", names(df_data), value = TRUE)
  new_cols <- setdiff(names(df_data), c(cols_derive, ll_template, extra_cols))
  
  ## return
  dplyr::select(df_data, all_of(cols_derive), all_of(ll_template), starts_with("extra_"))
}
 
