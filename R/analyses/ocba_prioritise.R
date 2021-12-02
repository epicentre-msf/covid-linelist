
### Analysis for Sakib, OCBA (2021-11-26)
# The nominator would be looking at patients presenting to MSF facilities with
# the following criteria, while the denominator would be the total number of
# patients presenting to MSF facilities:

# 1. Age >/= 18
# 2. Systemic symptoms : Breathing difficulty, or fever AND chest pain OR abdominal pain OR loose stool OR severe myalgia
# 3. Does not require Oxygen at presentation (i.e. RR < 30 and SpO2 >/= 94%)


## required libraries and globals ----------------------------------------------
source("R/zzz.R")


## read latest compilation -----------------------------------------------------
d <- llutils::list_files(
  path_export_global,
  pattern = "^msf_covid19_linelist_global_.*\\.rds",
  full.names = TRUE,
  select = "latest"
) %>%
  readRDS()


## clean MSF_oxygen_saturation -------------------------------------------------
dat_clean <- d %>% 
  mutate(
    MSF_oxygen_saturation_clean = case_when(
      grepl("\\d\\d%$", MSF_oxygen_saturation) ~ suppressWarnings(as.numeric(gsub("%$", "", MSF_oxygen_saturation)) / 100),
      TRUE ~ suppressWarnings(as.numeric(MSF_oxygen_saturation))
    ),
    MSF_oxygen_saturation_clean = case_when(
      MSF_oxygen_saturation_clean > 1 & MSF_oxygen_saturation_clean <= 100 ~ MSF_oxygen_saturation_clean / 100,
      MSF_oxygen_saturation_clean == 199 ~ 0.99,
      MSF_oxygen_saturation_clean > 500 & MSF_oxygen_saturation_clean < 1000 ~ MSF_oxygen_saturation_clean / 1000,
      TRUE ~ MSF_oxygen_saturation_clean
    )
  )


## subset to vars of interest --------------------------------------------------
dat_use <- dat_clean %>% 
  select(
    site, OC, country, date_event,
    age_in_years,
    MSF_symptom_breathlessness, MSF_symptom_fever, MSF_symptom_chest_pain,
    MSF_symptom_abdominal_pain, MSF_symptom_diarrhoea, MSF_symptom_aches,
    MSF_received_oxygen, MSF_oxygen_saturation_clean
  )


## generate indicator vars -----------------------------------------------------
df_indicators <- dat_use %>% 
  mutate(
    crit_age = age_in_years >= 18 & !is.na(age_in_years),
    has_age = !is.na(age_in_years),
    crit_symptoms = MSF_symptom_breathlessness %in% "Yes" |
                          (MSF_symptom_fever %in% "Yes" & MSF_symptom_chest_pain %in% "Yes") | 
                          MSF_symptom_abdominal_pain %in% "Yes" |
                          MSF_symptom_diarrhoea %in% "Yes" |
                          MSF_symptom_aches %in% "Yes",
    has_symptoms = MSF_symptom_breathlessness %in% c("Yes", "No") |
                       (MSF_symptom_fever %in% c("Yes", "No") & MSF_symptom_chest_pain %in% c("Yes", "No")) | 
                       MSF_symptom_abdominal_pain %in% c("Yes", "No") |
                       MSF_symptom_diarrhoea %in% c("Yes", "No") |
                       MSF_symptom_aches %in% c("Yes", "No"),
    crit_oxygen = MSF_oxygen_saturation_clean >= 0.94 & !is.na(MSF_oxygen_saturation_clean),
    has_oxygen = !is.na(MSF_oxygen_saturation_clean),
    crit_all = crit_age & crit_symptoms & crit_oxygen,
    has_all = has_age & has_symptoms & has_oxygen
  ) %>% 
  select(site, country, date_event, starts_with("crit"), starts_with("has_"))


## summarize indicator vars ----------------------------------------------------
df_indicator_summary <- df_indicators %>% 
  summarize(
    crit_age = sum(crit_age),
    crit_symptoms = sum(crit_symptoms),
    crit_oxygen = sum(crit_oxygen),
    crit_all = sum(crit_all),
    has_age = sum(has_age),
    has_symptoms = sum(has_symptoms),
    has_oxygen = sum(has_oxygen),
    has_all = sum(has_all)
  )


## output for analysis based on three criteria (age, symptoms, oxygen) ---------
df_criteria <- tibble(
  criteria = c("Age >= 18", "Systemic symptoms", "SpO2 >/= 94%", "ALL"),
  n_meet_criteria = as.numeric(df_indicator_summary[1,])[1:4],
  n_criteria_known = as.numeric(df_indicator_summary[1,])[5:8]
) %>% 
  mutate(pct_meet_criteria = n_meet_criteria / n_criteria_known * 100, .after = n_meet_criteria) %>% 
  mutate(pct_criteria_known = n_criteria_known / nrow(df_indicators) * 100)

df_criteria_meta <- df_indicators %>% 
  summarize(
    n_countries_known_criteria = length(unique(country[has_all])),
    n_countries_total = length(unique(country)),
    date_range_known_criteria = paste(range(date_event[has_all], na.rm = TRUE), collapse = " - ")
  )


## output for analysis based only on MSF_severity ------------------------------
df_severity <- dat_clean %>% 
  count(MSF_severity, name = "n_with_severity_type") %>% 
  slice(2, 3, 4, 1) %>% 
  filter(!is.na(MSF_severity)) %>% 
  mutate(
    n_severity_known = sum(!is.na(dat_clean$MSF_severity)),
    pct_with_severity_type = round(n_with_severity_type / n_severity_known * 100, 2)
  )

df_severity_meta <- dat_clean %>% 
  transmute(
    country,
    date_event,
    has_severity = !is.na(MSF_severity)
  ) %>% 
  summarize(
    n_countries_known_severity = length(unique(country[has_severity])),
    n_countries_total = length(unique(country)),
    date_range_known_severity = paste(range(date_event[has_severity], na.rm = TRUE), collapse = " - ")
  )


## write to file ---------------------------------------------------------------
qxl::qxl(
  list(
    severity = df_severity, severity_meta = df_severity_meta,
    criteria = df_criteria, criteria_meta = df_criteria_meta
  ),
  file.path(path_onedrive, "analyses", glue::glue("Severity Analysis OCBA PRIORITISE {Sys.Date()}.xlsx"))
)


