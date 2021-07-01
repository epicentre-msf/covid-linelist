

### Required libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(qxl)
library(openxlsx)
library(glue)
library(llutils)
source("R/zzz.R")
source("R/utilities.R")


## Global analysis -------------------------------------------------------------
dat_clean <- llutils::list_files(
  path_export_global,
  pattern = "^msf_covid19_linelist_global_.*\\.rds",
  full.names = TRUE,
  select = "latest"
) %>%
  readRDS() %>% 
  mutate(upload_date = as.character(upload_date)) %>% 
  mutate(site_name = if_else(site == "LBN_P_ELH", "Elias Hrawi Governmental Hospital", site_name)) %>% 
  select(-c(MSF_name_surname, MSF_phone_number, MSF_phone_owner, MSF_comment))

dat_long <- dat_clean %>% 
  select(OC, country, site_name, any_of(dict_linelist$code_name)) %>% 
  mutate(across(everything(), as.character)) %>% 
  tidyr::pivot_longer(-c(OC, country, site_name))

d_missing_site <- dat_long %>% 
  group_by(OC, country, site_name, name) %>% 
  summarize(n_rows = n(),
            n_known = sum(!value %in% c(NA, "Unknown")),
            p_known = round(n_known / n_rows * 100, 0),
            .groups = "drop") %>% 
  select(-n_known)

d_missing_oc <- dat_long %>%
  group_by(OC, name) %>%
  summarize(n_rows = n(),
            n_known = sum(!value %in% c(NA, "Unknown")),
            p_known = round(n_known / n_rows * 100, 0),
            .groups = "drop") %>%
  select(-n_known) %>%
  mutate(country = "ALL", site_name = "ALL", .after = 1)

d_missing_total <- dat_long %>%
  group_by(name) %>%
  summarize(n_rows = n(),
            n_known = sum(!value %in% c(NA, "Unknown")),
            p_known = round(n_known / n_rows * 100, 0),
            .groups = "drop") %>%
  select(-n_known) %>%
  mutate(OC = "ALL", country = "ALL", site_name = "ALL", .before = 1)


d_summary_site <- d_missing_site %>% 
  tidyr::pivot_wider(id_cols = c(OC, country, site_name),
                     names_from = name,
                     values_from = p_known) %>% 
  arrange(OC, country, site_name) %>% 
  select(OC, country, site_name, any_of(dict_linelist$code_name))

d_summary_OC <- d_missing_oc %>% 
  tidyr::pivot_wider(id_cols = c(OC, country, site_name),
                     names_from = name,
                     values_from = p_known) %>% 
  arrange(OC, country, site_name) %>% 
  select(OC, country, site_name, any_of(dict_linelist$code_name))

d_summary_total <- d_missing_total %>% 
  tidyr::pivot_wider(id_cols = c(OC, country, site_name),
                     names_from = name,
                     values_from = p_known) %>% 
  arrange(OC, country, site_name) %>% 
  select(OC, country, site_name, any_of(dict_linelist$code_name))

d_summary_full <- bind_rows(
  d_summary_total,
  # tibble(OC = NA),
  d_summary_OC,
  d_summary_site,
  # tibble(OC = NA)
)

df_site <- d_summary_full %>% 
  select(OC:site_name)

var_labs <- file.path(path_dict_linelist, "LLcovid_var_dictionary_V3.0_Eng.xlsx") %>% 
  readxl::read_xlsx() %>% 
  janitor::clean_names() %>% 
  select(name = code_name, label = variable_short_label)

d_summary_t <- d_summary_full %>% 
  tidyr::unite("site", OC:site_name, sep = "__") %>% 
  pivot_longer(cols = -site) %>% 
  pivot_wider(names_from = site, values_from = value) %>% 
  left_join(var_labs, by = "name") %>% 
  relocate(label, .after = "name")

# write
wb <- qxl::qxl(
  d_summary_t,
  freeze_col = 2,
  style_head = qstyle(rows = 1, cols = -c(name, label), textRotation = 90),
  col_widths = c(40, 60, rep(4.2, ncol(d_summary_t)-2))
)

conditionalFormatting(
  wb, 1,
  cols = 3:ncol(d_summary_t),
  rows = 2:(nrow(d_summary_t)+1),
  type = "databar",
  rule = c(0, 100),
  style = c("#6baed6", "#c6dbef")
)

suppressMessages(
  openxlsx::saveWorkbook(
    wb,
    file = file.path(path_onedrive, "analyses", glue::glue("covid19_linelist_completeness_{Sys.Date()}.xlsx")),
    overwrite = TRUE
  )
)



## Specific analysis for OCP ---------------------------------------------------
d_ocp <- dat_clean %>% 
  filter(OC == "OCP", country %in% c("AFG", "BGD", "YEM"))

d_ocp_long <- d_ocp %>% 
  select(country, site_name, Comcond_preg:MSF_smoking, MSF_complications:MSF_other_complications) %>% 
  tidyr::pivot_longer(-c(country, site_name))

var_order <- d_ocp %>% 
  select(Comcond_preg:MSF_smoking, MSF_complications:MSF_other_complications) %>% 
  names()

d_ocp_missing_long <- d_ocp_long %>% 
  group_by(country, site_name, name) %>% 
  summarize(n_rows = n(),
            n_unknown = sum(value %in% "Unknown"),
            n_missing = sum(is.na(value)),
            p_unknown = round(n_unknown / n_rows * 100, 0),
            p_missing = round(n_missing / n_rows * 100, 0),
            .groups = "drop") %>% 
  select(-n_unknown, -n_missing)

d_ocp_summary <- d_ocp_missing_long %>% 
  mutate(
    site_name = recode(
      site_name,
      "Gazhar Gah CTC" = "Gazhar Gah",
      "Herat Regional Hospital MSF COVID-19 Triage" = "Herat RH Triage",
      "AL SALAM HOSPITAL" = "Al Salam Hospital",
      "HAYDAN HOSPITAL" = "Haydan Hospital"
    )
  ) %>% 
  mutate(site = paste0(country, ": ", site_name)) %>% 
  mutate(name = factor(name, levels = var_order)) %>% 
  select(-country, -site_name)

d_ocp_summary_missing <- d_ocp_summary %>% 
  tidyr::pivot_wider(id_cols = name,
                     names_from = site,
                     values_from = p_missing) %>% 
  arrange(name)

d_ocp_summary_unknown <- d_ocp_summary %>% 
  tidyr::pivot_wider(id_cols = name,
                     names_from = site,
                     values_from = p_unknown) %>% 
  arrange(name)

# write to file
# qxl::qxl(d_ocp_summary_missing, file.path(path_onedrive, "analyses", "covid19_ocp_missing.xlsx"))
# qxl::qxl(d_ocp_summary_unknown, file.path(path_onedrive, "analyses", "covid19_ocp_unknown.xlsx"))

