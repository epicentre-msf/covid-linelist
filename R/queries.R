
### Required libraries
library(tidyverse)
library(lubridate)
library(glue)
source("R/zzz.R")
source("R/utilities.R")
source("R/queries_defs.R")


### Query definition sheet
query_defs <- readxl::read_xlsx(file.path(path_queries, "query_definitions.xlsx")) %>% 
  janitor::clean_names() %>% 
  select(query_id, category, description) %>% 
  mutate(description = gsub("\\\"", "'", description)) %>% 
  filter(!is.na(query_id))

# variable/value combinations to exclude from Categorical queries, because
# they're simple and common, or relate to a dictionary error
categ_query_exclude <- readxl::read_xlsx(file.path(path_queries, "exclude_query_categ.xlsx"))
sites_query_exclude <- readxl::read_xlsx(file.path(path_queries, "exclude_query_sites.xlsx"))


### Compile global linelist of raw imports
dat_raw <- list.files("local/raw", pattern = "^ll_covid_raw", full.names = TRUE) %>%
  purrr::map_dfr(readRDS)

dat_clean <- llutils::list_files(
  path_export_global,
  pattern = "^msf_covid19_linelist_global_.*\\.rds",
  full.names = TRUE,
  select = "latest"
) %>%
  readRDS() %>% 
  mutate(upload_date = as.character(upload_date))


  
# dat_raw %>%
#   filter(OC == "OCBA") %>%
#   group_by(site) %>%
#   summarize(n_missing_report_date = sum(is.na(report_date)),
#             n_total = n(),
#             .groups = "drop") %>%
#   filter(n_missing_report_date > 0) %>%
#   arrange(desc(n_missing_report_date))


### Run all queries
df_queries <- dplyr::bind_rows(
  queries_ident(dat_raw, dat_clean),
  queries_dates(dat_raw, date_vars, dict_date_categories),
  queries_categorical(dat_raw, dict_factors, dict_countries, categ_query_exclude, dict_countries_correct),
  queries_multi(dat_raw, dat_clean),
  queries_other(dat_raw, dat_clean)
) %>% 
  anti_join(sites_query_exclude, by = c("query_id", "site"))


df_queries_join <- df_queries %>%
  select(c("site", "MSF_N_Patient", "query_id", "linelist_row", "variable1", "variable2")) %>%
  unique() %>%
  mutate(resolved_join = FALSE)


### Queries old
OC_list <- c("OCG", "OCBA", "OCB", "OCP", "OCA")

query_dirs <- purrr::map(
  OC_list,
  ~ list.dirs(file.path(path_export_fp, .x, "queries"), recursive = FALSE),
) %>% unlist()

query_tracker_files <- purrr::map(
  query_dirs,
  llutils::list_files,
  full.names = TRUE,
  pattern = "query_tracker",
  select = "latest"
) %>% unlist()

queries_written_site <- purrr::map_dfr(
  query_tracker_files,
  read_query_trackers
) %>% 
  rename(!!query_recode_inv) %>% 
  select(-i) %>% 
  mutate(across(c(linelist_row), as.integer)) %>% 
  select(-any_of(c("exclude")))

# tracker_files <- llutils::list_files(
#   path_queries, pattern = "query_tracker",
#   full.names = TRUE,
#   select = "latest"
# )
# 
# queries_written <- map_dfr(tracker_files, read_query_trackers) %>%
#   rename(!!query_recode_inv) %>%
#   select(-i) %>%
#   mutate(across(c(linelist_row), as.integer))

queries_written_groups <- queries_written_site %>% 
  select(query_group, query_id, site, MSF_N_Patient, linelist_row) %>% 
  unique()

query_group_next <- queries_written_groups %>% 
  mutate(q = as.integer(gsub("Q", "", query_group))) %>% 
  group_by(site) %>% 
  summarize(query_group_next = max(q) + 1L, .groups = "drop")

query_number_next <- queries_written_site %>% 
  mutate(group = as.integer(gsub("Q", "", query_group)),
         number = as.integer(substr(query_number, 8, 10))) %>% 
  group_by(site, query_group) %>% 
  summarize(query_number_next = max(number) + 1L, .groups = "drop")

queries_new <- df_queries %>% 
  anti_join(queries_written_site, by = c("site", "MSF_N_Patient", "query_id", "linelist_row", "variable1", "variable2")) %>% 
  left_join(queries_written_groups, by = c("query_id", "site", "MSF_N_Patient", "linelist_row")) %>%
  left_join(query_defs, by = "query_id") %>% 
  arrange(site, query_id, MSF_N_Patient) %>% 
  mutate(
    date_generated = as.character(lubridate::today()),
    resolved_auto = "No"
  )

queries_new_no_group <- queries_new %>% 
  filter(is.na(query_group)) %>% 
  left_join(query_group_next, by = "site") %>% 
  mutate(query_group_next = ifelse(is.na(query_group_next), 1L, query_group_next)) %>% 
  prep_query_group_int(., "query_group_next") %>% 
  mutate(query_group = format_query_group(query_group_int)) %>% 
  prep_query_number_int() %>% 
  mutate(query_number = format_query_number(query_number_int),
         query_number = paste(query_group, query_number, sep = "_")) %>% 
  select(-query_group_next, -query_group_int, -query_number_int)

queries_new_with_group <- queries_new %>% 
  filter(!is.na(query_group)) %>% 
  left_join(query_number_next, by = c("site", "query_group")) %>% 
  mutate(query_number_next = ifelse(is.na(query_number_next), 1L, query_number_next)) %>% 
  prep_query_number_int("query_number_next") %>% 
  mutate(query_number = format_query_number(query_number_int),
         query_number = paste(query_group, query_number, sep = "_")) %>% 
  select(-query_number_next, -query_number_int)

queries_full <- bind_rows(
  queries_written_site,
  queries_new_no_group,
  queries_new_with_group
) %>% 
  arrange(site, query_id, MSF_N_Patient)



queries_out <- queries_full %>% 
  left_join(df_queries_join, by = c("site", "MSF_N_Patient", "linelist_row", "query_id", "variable1", "variable2")) %>% 
  left_join(sites_query_exclude, by = c("site", "query_id")) %>% 
  left_join(categ_query_exclude, by = c("query_id", "variable1", "value1")) %>% 
  mutate(resolved_join = ifelse(is.na(resolved_join), TRUE, resolved_join),
         excluded_join = ifelse(exclude %in% "Yes" | exclude_categ %in% "Yes", TRUE, FALSE)) %>% 
  mutate(
    resolved_auto = case_when(
      resolved_join & excluded_join ~ "Removed",
      resolved_join ~ "Resolved",
      TRUE ~ "Unresolved"
    )
  ) %>% 
  mutate(date_resolved_auto = ifelse(is.na(date_resolved_auto) & resolved_join, as.character(Sys.Date()), date_resolved_auto)) %>% 
  select(-resolved_join, -excluded_join, -exclude_categ, -exclude) %>% 
  arrange(site, query_number)



# write global query tracker
if (FALSE) {
  write_query_tracker(queries_out, path = file.path(path_queries, glue::glue("query_tracker_{today()}.xlsx")))
}

# write site-specific query trackers
if (FALSE) {
  write_query_tracker_site(queries_out)
}

