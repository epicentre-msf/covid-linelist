
#### Requires
source("R/zzz.R")



#### Compare resources among linelist versions
prep_resources <- function(path) {
  readxl::read_xlsx(path) %>% 
    janitor::clean_names() %>% 
    mutate(code = map(code, ~ strsplit(.x, "\\|")[[1]])) %>% 
    unnest("code") %>% 
    arrange(code) %>% 
    filter(!code %in% c("MSF_SECTION", "TypeStructure", "graph", "deprecated")) %>% 
    filter(!grepl("_[0-9]+$", code)) %>% 
    filter(!is.na(levels)) %>%
    mutate(code = recode(code, "MSF_test_first_results" = "MSF_test_results"))
}

res_v2.0 <- prep_resources(file.path(path_resources, "ll-resources_v2.0.xlsx"))
res_v2.1 <- prep_resources(file.path(path_resources, "ll-resources_v2.1.xlsx"))

unique(res_v2.0$code)

setdiff(res_v2.0$code, res_v2.1$code)
setdiff(res_v2.1$code, res_v2.0$code)

anti_join(res_v2.0, res_v2.1)
anti_join(res_v2.1, res_v2.0)



#### Compare v2.0 resources to v2.0 export
res_v2.0 <- prep_resources(
  file.path(path_resources, "ll-resources_v2.0.xlsx")
)

export_v2.0 <- list_files(
  file.path(path_data_raw, "MLI"),
  pattern = "Douentza__5799",
  last.sorted = TRUE,
  full.names = TRUE
) %>%
  readxlsb::read_xlsb(sheet = "linelist", col_types = "string") %>% 
  as_tibble() %>% 
  select(-starts_with("Ajouter"))

# note already swapped "MSF_test_first_results" -> "MSF_test_results"
setdiff(res_v2.0$code, names(export_v2.0))



#### Compare resources to dico (v2.1)

# prepare dictionary and resources
dict_full <- path_dict_linelist %>% 
  file.path("LLcovid_var_dictionary_V2.0_Eng.xlsx") %>% 
  readxl::read_xlsx() %>%
  janitor::clean_names()

dict_coded <- dict_full %>%
  filter(data_type == "Coded list") %>%
  select(variable = code_name, options = options) %>%
  filter(grepl("\\/|\\,", options) & !variable %in% "patinfo_idadmin0") %>%
  mutate(options_sort = map(options, ~ strsplit(.x, "[[:space:]]*\\/[[:space:]]*")[[1]])) %>%
  unnest("options_sort") %>%
  group_by(variable, options) %>%
  summarize(
    options_dict_sort = paste(sort(hmatch::string_std(options_sort)), collapse = " | "),
    .groups = "drop"
  )

resources_full <- res_v2.1

# discrepencies in variable names
setdiff(resources_full$code, dict_coded$variable)
setdiff(dict_coded$variable, resources_full$code)

resources_full %>%
  filter(!code %in% dict_coded$variable) %>%
  print(n = "all")

dict_coded %>%
  filter(!variable %in% resources_full$code) %>%
  filter(!grepl("Yes\\/No", options)) %>% 
  print(n = "all")

# discrepencies in coded-list values
dict_match <- dict_coded %>%
  mutate(
    options = recode(
      options,
      "Household, work place, community, health facility, unknown, other" = "Household/work place/community/health facility/unknown/other"
    ),
    options = gsub(". Leave blanks for name of RDT", "", options)
  ) %>%
  mutate(value = map(options, ~ strsplit(.x, "[[:space:]]*\\/[[:space:]]*")[[1]])) %>%
  unnest("value") %>%
  select(-options) %>%
  group_by(variable) %>%
  mutate(i = 1:n()) %>%
  ungroup()

resources_summary <- resources_full %>%
  select(-variables) %>%
  filter(code %in% dict_coded$variable) %>%
  mutate(order = case_when(
    grepl("Yes|Posit", levels) ~ 1,
    grepl("Inconclus", levels) ~ 3,
    grepl("Not a", levels) ~ 4,
    grepl("Other", levels) ~ 5,
    grepl("Unknown", levels) ~ 6,
    grepl("Not done", levels) ~ 7,
    TRUE ~ 2
  )) %>%
  arrange(code, order) %>%
  group_by(code) %>%
  summarize(options_res = paste(levels, collapse = " | "),
            options_res_sort = paste(sort(hmatch::string_std(levels)), collapse = " | "),
            .groups = "drop")

dict_coded_gen <- dict_coded %>%
  mutate(options_ed = gsub("\\/", " | ", options)) %>%
  left_join(resources_summary, by = c("variable" = "code")) %>%
  mutate(discrepancy = ifelse(options_dict_sort == options_res_sort, NA_character_, "Yes")) %>%
  mutate(options_res = ifelse(is.na(options_res), options_ed, options_res)) %>%
  select(code_name = variable, options_res, discrepancy)

dict_coded_gen_out <- dict_full %>%
  select(code_name, variable_short_label, data_type, options) %>%
  left_join(dict_coded_gen, by = "code_name")

# write to file
wb <- llct::write_simple_xlsx(dict_coded_gen_out)

openxlsx::addStyle(
  wb,
  sheet = 1,
  style = openxlsx::createStyle(wrapText = TRUE),
  rows = 2:(nrow(dict_coded_gen_out) + 1),
  cols = 4:5,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  wb,
  1,
  cols = 1:ncol(dict_coded_gen_out),
  widths = c(30, 30, 20, 50, 50, 15)
)

openxlsx::saveWorkbook(
  wb,
  file = file.path(path_resources, "llv2.1_dico_discrepancies.xlsx"),
  overwrite = TRUE
)



#### Variables written but not fillable in v2.0 
setdiff(dict_linelist$code_name, dict_linelist_v1$code_name)
setdiff(dict_linelist_v1$code_name, dict_linelist$code_name)

dict_full <- path_dict_linelist %>% 
  file.path("LLcovid_var_dictionary_V2.0_Eng.xlsx") %>% 
  readxl::read_xlsx() %>%
  janitor::clean_names()


dict_linelist_v1 %>%
  select(code_name, data_type1 = data_type) %>%
  full_join(dict_linelist) %>%
  select(code_name, data_type1, data_type2 = data_type) %>%
  filter(data_type1 != data_type2)


(vars_exclude <- setdiff(names(export_v2.0), dict_full$code_name))
setdiff(dict_full$code_name, names(export_v2.0))

dict_vars_exclude <- data.frame(var = vars_exclude)
llct::write_simple_xlsx(dict_vars_exclude, file.path(path_dictionaries, "dict_vars_exclude.xlsx"))


