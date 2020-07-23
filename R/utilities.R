

map_columns <- function(data, vec_map) {
  
  for (i in seq_along(vec_map)) {
    col_old <- as.character(vec_map[i])
    col_new <- names(vec_map[i])
    data[[col_new]] <- data[[col_old]]
  }
  
  data[,!names(data) %in% as.character(vec_map)]
}


read_query_trackers <- function(path) {
  dplyr::bind_rows(
    readxl::read_xlsx(path, sheet = "Current", col_types = "text"),
    readxl::read_xlsx(path, sheet = "Resolved", col_types = "text")
  )
}

test_set_equal <- function(x, allowed, to.lower = TRUE) {
  
  var <- deparse(substitute(x))
  var <- gsub("^.*\\$", "", var)
  
  if (to.lower) {
    x <- tolower(x)
    allowed <- tolower(allowed)
  }
  
  nonvalid <- rev(setdiff(x, allowed))
  nonvalid[!is.na(nonvalid)] <- dQuote(nonvalid[!is.na(nonvalid)], q = FALSE)
  
  if (length(nonvalid) > 0) {
    warning(
      var, " includes unexpected values: ",
      paste0("c(", paste(nonvalid, collapse = ", "), ")"),
      call. = FALSE
    )
  }
}


bind_query_list <- function(...,
                            .id = "query_id",
                            cols_arrange = c("query_id",
                                             "site",
                                             "MSF_N_Patient",
                                             "linelist_row")) {
  out <- dplyr::bind_rows(..., .id = .id)
  dplyr::arrange(out, dplyr::across(all_of(cols_arrange)))
}


write_by_country <- function(x, dat, path_prefix = "local/raw/ll_covid_raw_") {
  dat_write <- dat[dat$country == x,]
  path_write <- paste0(path_prefix, x, ".rds")
  saveRDS(dat_write, path_write)
}


is_excel_numeric <- function(x) {
  # check whether character string is probable excel date
  # i.e. string consisting solely of 4-6 digits
  grepl("^[[:digit:]]{4,6}$", x)
}


parse_excel_dates <- function(x) {
  # parse character string consisting of R date (YYYY-MM-DD) OR excel date
  #  (NNNNN); output will be character in "YYYY-MM-DD" format
  x <- as.character(x)
  i <- is_excel_numeric(x)
  x[i] <- as.character(janitor::excel_numeric_to_date(as_integer_quiet(x[i])))
  x
}


parse_other_dates <- function(x, order = c("%d/%m/%Y", "%d/%m/%y", "%Y-%m-%d")) {
  x <- as.character(x)
  i <- grepl("\\/", x)
  x[i] <- as.character(lubridate::parse_date_time(x[i], order = order))
  x
}


parse_dates <- function(x) {
  x <- as.character(x)
  x <- parse_excel_dates(x)
  x <- parse_other_dates(x)
  suppressWarnings(lubridate::as_date(x))
}

non_valid_date <- function(x) {
  x_date <- parse_dates(x)
  !is.na(x) & is.na(x_date)
}


min_safe <- function(x) {
  if (all(is.na(x))) {
    vctrs::vec_cast(NA, x)
  } else {
    min(x, na.rm = TRUE)
  }
}


max_safe <- function(x) {
  if (all(is.na(x))) {
    vctrs::vec_cast(NA, x)
  } else {
    max(x, na.rm = TRUE)
  }
}


get_site_meta <- function(file_path) {
  
  df <- try(
    readxl::read_xlsx(
      file_path,
      sheet = "Options",
      col_types = "text"
    ),
    silent = TRUE
  )
  
  if ("try-error" %in% class(df)) {
    
    ll <- readxl::read_xlsx(
      file_path,
      sheet = "linelist",
      col_types = "text"
    )
    
    # if no Options tab, find linelist language the hardcore way
    language_counts <- c(
      English = sum(ll == "Yes", na.rm = TRUE),
      Français = sum(ll == "Oui", na.rm = TRUE),
      Espagnol = sum(ll == "Si", na.rm = TRUE),
      Portuguese = sum(ll == "Sim", na.rm = TRUE)
    )
    
    language <- names(which.max(language_counts))
    version <- "Other"
    
  } else {
    language <- df$value[df$variable == "language"]
    version <- df$value[df$variable == "version"]
  }
  
  return(tibble::tibble(linelist_lang = language, linelist_vers = version))
}


bind_queries <- function(..., col_arrange = c("country", "site", "MSF_N_Patient")) {
  # requires dplyr >= 1.0
  # library(dplyr)
  out <- dplyr::bind_rows(...)
  arrange(out, dplyr::across(all_of(col_arrange)))
}


valid_numeric <- function(x) {
  xnum <- suppressWarnings(as.numeric(x))
  is.na(x) | (!is.na(x) & !is.na(xnum))
}



test_duplicated <- function(x, name) {
  dups <- duplicated(x)
  if (any(dups)) {
    warning(sprintf("The following values of %s are duplicated: ", name),
            concat_out(x[dups]), call. = FALSE)
  }
}


concat_out <- function(x) {
  paste0("c(", paste(dQuote(x, q = FALSE), collapse = ", "), ")")
}


check_files_to_dict <- function(path) {
  
  library(readxl)
  library(dplyr)
  
  out <- read_xlsx(path, col_types = "text") %>% 
    filter(!is.na(value), !is.na(date_correct)) %>%
    mutate(ignore = ifelse(date_correct == ".ignore", TRUE, FALSE),
           date_correct = ifelse(date_correct == ".ignore", NA_character_, date_correct),
           date = ifelse(ignore, as.character(value), as.character(date_correct)),   # overwrite date with date_corrected
           date = ifelse(date == ".na", NA_character_, date),
           date = vapply(date, parse_excel_dates, ""),
           date = as.Date(date)) %>%
    dplyr::select(patient_id, variable, value, date, flag, ignore)
  
  return(out)
}


create_empty_dict_dates <- function() {
  tibble::tibble(
    patient_id = character(0),
    variable = character(0),
    value = character(0),
    date = as.Date(character(0)),
    flag = character(0),
    ignore = logical(0)
  )
}


fetch_georef <- function(iso, path = path_shapefiles) {
  readRDS(file.path(path_shapefiles, iso, glue::glue("adm_reference_{iso}.rds")))
}


is_date <- function(x) {
  class(x) == "Date"
}


date_format <- function(x) {
  xx <- as.character(x)
  xx[is.na(xx)] <- "NA"
  return(xx)
}


format_text <- function(x) {
  library(stringr)
  xx <- toupper(x)
  xx <- str_trim(xx)
  # xx <- str_replace_all(xx, "[^[:alnum:]]+", "_")
  # xx <- str_replace(xx, "[_]+\\b", "")
  xx <- base::iconv(xx, to = "ASCII//TRANSLIT")
  xx <- str_replace_all(xx, "[^[[:alnum:]]]+", "")
  xx[xx == ""] <- NA_character_
  return(xx)
}


best_geolevel <- function(dat, pattern) {
  cols <- grep(pattern, names(dat), value = TRUE)
  apply(dat[, sort(cols), drop = FALSE], 1, max_not_na)
}


max_not_na <- function(x, no_na = 0L, value = FALSE) {
  ifelse(any(!is.na(x)), max(which(!is.na(x))), no_na)
}


dist_match <- function(x, y, max_dist = 1) {
  abs(as.numeric(x - y)) <= max_dist | is.na(x) | is.na(y)
}


equal_or_na <- function(x, y) {
  x == y | is.na(x) | is.na(y)
}


time_stamp <- function(t = Sys.time(), format = "%Y-%m-%d_%H%M") {
  # convenience function for creating time stamps
  format(t, format = format)
}


is_posix <- function(x) {
  # test whether object 'x' is of class POSIXt/POSIXct/POSIXlt
  any(class(x) %in% c("POSIXt", "POSIXct", "POSIXlt"))
}


list_dirs <- function(path = ".", pattern = NULL) {
  x <- list.dirs(path)
  if (!is.null(pattern)) x <- x[grepl(pattern, x)]
  x
}


left_join_replace <- function(x, y, cols_match) {
  # add columns in y to x, by = cols_match, overwriting any common columns
  library(dplyr)
  
  cols_orig_x <- names(x)
  cols_replace <- setdiff(names(y), cols_match)
  cols_keep <- setdiff(names(x), cols_replace)
  
  x %>%
    dplyr::select(!!!cols_keep) %>% 
    dplyr::left_join(y, by = cols_match) %>% 
    select(all_of(cols_orig_x))
}


integer_id <- function(x) {
  # convert character or factor vector 'x' to integer ids
  as.integer(factor(x, levels = unique(x)))
}


as_numeric_quiet <- function(x) {
  # as.numeric without coerce warnings
  suppressWarnings(as.numeric(x))
}


as_integer_quiet <- function(x) {
  # as.integer without coerce warnings
  suppressWarnings(as.integer(x))
}


get_date_differences <- function(x, col) {
  
  dat_diff <- mutate_all(x, as.integer) - as.integer(x[[col]])
  
  dat_diff %>% 
    as_tibble() %>% 
    gather("variable", "value") %>% 
    mutate(variable_focal = col, .before = 2,
           variable_label = paste0(variable, " - ", variable_focal)) %>% 
  filter(variable != variable_focal)
}



query_recode <- c(
  "i" = "i",
  "Query group" = "query_group",
  "Query #" = "query_number",
  "OC" = "OC",
  "Country" = "country",
  "Site" = "site",
  "Upload date" = "upload_date",
  "# Patient" = "MSF_N_Patient",
  "Row (export)" = "linelist_row",
  "Query category" = "category",
  "Query ID" = "query_id",
  "Description" = "description",
  "Variable 1" = "variable1",
  "Value 1" = "value1",
  "Variable 2" = "variable2",
  "Value 2" = "value2",
  "Date query generated" = "date_generated",
  "Resolved (auto)" = "resolved_auto",
  "Date resolved (auto)" = "date_resolved_auto",
  "Resolved (field)" = "resolved_field",
  "Date resolved (field)" = "date_resolved_field",
  "Comment (field)" = "comment"
)

query_recode_inv <- setNames(names(query_recode), query_recode)



write_query_tracker <- function(queries_out, site_focal = NULL, path) {
  
  if (!is.null(site_focal)) {
    queries_out <- queries_out %>% 
      filter(site %in% site_focal)
  }
  
  queries_resolved <- queries_out %>% 
    filter(resolved_auto == "Yes") %>% 
    mutate(i = integer_id(paste(site, query_group)) %% 2, .before = 1)
  
  queries_outstanding <- queries_out %>% 
    filter(resolved_auto == "No") %>% 
    mutate(i = integer_id(paste(site, query_group)) %% 2, .before = 1)
  
  queries_summary <- queries_out %>% 
    group_by(category, query_id, resolved_auto, description) %>% 
    summarize(n_total = n(), .groups = "drop") %>% 
    tidyr::pivot_wider(names_from = "resolved_auto", values_from = "n_total", values_fill = 0)
  
  if (!"Yes" %in% names(queries_summary)) {
    queries_summary$Yes <- 0L
  }
  
  if (!"No" %in% names(queries_summary)) {
    queries_summary$No <- 0L
  }
  
  queries_summary <- queries_summary %>% 
    mutate(total = Yes + No) %>% 
    arrange(desc(total)) %>% 
    select(Category = category,
           `Query ID` = query_id,
           `N Total` = total,
           `N Resolved` = Yes,
           `N Outstanding` = No,
           Description = description)
  
  header_recode <- c(
    "i" = "i",
    "Query group" = "query_group",
    "Query #" = "query_number",
    "OC" = "OC",
    "Country" = "country",
    "Site" = "site",
    "Upload date" = "upload_date",
    "# Patient" = "MSF_N_Patient",
    "Row (export)" = "linelist_row",
    "Query category" = "category",
    "Query ID" = "query_id",
    "Description" = "description",
    "Variable 1" = "variable1",
    "Value 1" = "value1",
    "Variable 2" = "variable2",
    "Value 2" = "value2",
    "Date query generated" = "date_generated",
    "Resolved (auto)" = "resolved_auto",
    "Date resolved (auto)" = "date_resolved_auto",
    "Resolved (field)" = "resolved_field",
    "Date resolved (field)" = "date_resolved_field",
    "Comment (field)" = "comment"
  )
  
  
  queries_outstanding <- queries_outstanding %>% 
    rename(!!!header_recode)
  
  queries_resolved <- queries_resolved %>% 
    rename(!!!header_recode)
  
  
  ## Write updated query tracker sheets to file
  library(openxlsx)
  options("openxlsx.dateFormat" = "yyyy-mm-dd")
  wb <- openxlsx::createWorkbook()
  hs <- openxlsx::createStyle(halign = "center", textDecoration = "Bold")
  la <- openxlsx::createStyle(halign = "left")
  
  # sheet 1 (Main query tracker)
  openxlsx::addWorksheet(wb, "Current", zoom = 130)
  openxlsx::writeData(wb, 1, queries_outstanding, withFilter = TRUE)
  openxlsx::setColWidths(wb, 1, cols = 1:ncol(queries_outstanding),
                         widths = c(6, 15, 14, 10, 12, 13, 15, 20, 15, 20, 15, 100, 40, 30, 40, 30, 22, 17, 22, 17, 22, 70))
  openxlsx::freezePane(wb, 1, firstActiveRow = 2)
  openxlsx::addStyle(wb, 1, style = hs, rows = 1, cols = 1:ncol(queries_outstanding), gridExpand = TRUE)
  openxlsx::addStyle(wb, 1, style = la, rows = 2:(nrow(queries_outstanding) + 1L), cols = 1:ncol(queries_outstanding), gridExpand = TRUE)
  openxlsx::conditionalFormatting(wb, 1,
                                  cols = 1:ncol(queries_outstanding),
                                  rows = 2:(nrow(queries_outstanding) + 1L),
                                  rule = paste0("$A2>0"),
                                  style = openxlsx::createStyle(bgFill = "#fddbc7"))
  
  # sheet 2 (Resolved queries)
  openxlsx::addWorksheet(wb, "Resolved", zoom = 130)
  openxlsx::writeData(wb, 2, queries_resolved, withFilter = TRUE)
  openxlsx::setColWidths(wb, 2, cols = 1:ncol(queries_resolved),
                         widths = c(6, 15, 14, 10, 12, 13, 15, 20, 15, 20, 15, 100, 40, 30, 40, 30, 22, 17, 22, 17, 22, 70))
  openxlsx::freezePane(wb, 2, firstActiveRow = 2)
  openxlsx::addStyle(wb, 2, style = hs, rows = 1, cols = 1:ncol(queries_resolved), gridExpand = TRUE)
  openxlsx::addStyle(wb, 2, style = la, rows = 2:(nrow(queries_resolved) + 1L), cols = 1:ncol(queries_resolved), gridExpand = TRUE)
  openxlsx::conditionalFormatting(wb, 2,
                                  cols = 1:ncol(queries_resolved),
                                  rows = 2:(nrow(queries_resolved) + 1L),
                                  rule = paste0("$A2>0"),
                                  style = openxlsx::createStyle(bgFill = "#fddbc7"))
  
  # sheet 3 (summary)
  openxlsx::addWorksheet(wb, "Summary", zoom = 130)
  openxlsx::writeData(wb, 3, queries_summary)
  openxlsx::freezePane(wb, 3, firstActiveRow = 2)
  openxlsx::addStyle(wb, 3, style = hs, rows = 1, cols = 1:ncol(queries_summary), gridExpand = TRUE)
  openxlsx::addStyle(wb, 3, style = la, rows = 2:(nrow(queries_summary) + 1L), cols = 1:ncol(queries_summary), gridExpand = TRUE)
  openxlsx::setColWidths(wb, 3, cols = 1:ncol(queries_summary),
                         widths = c(21, 14, 12, 12, 12, 110))
  
  suppressMessages(
    openxlsx::saveWorkbook(
      wb,
      file = path,
      overwrite = TRUE
    )
  )
}




write_query_tracker_site <- function(queries_out, OC_focal) {
  
  if (!missing(OC_focal)) {
    queries_out <- filter(queries_out, OC %in% OC_focal)
  }
  
  paths_oc_site <- distinct(queries_out, OC, site) %>% 
    arrange(OC, site) %>% 
    mutate(path_site = file.path(path_export_fp, OC, "queries", site),
           path_file = file.path(path_site, glue("query_tracker_{site}_{today()}.xlsx")))
  
  for (i in seq_len(nrow(paths_oc_site))) {
    if (!dir.exists(paths_oc_site$path_site[i])) {
      dir.create(paths_oc_site$path_site[i])
    }
    
    write_query_tracker(queries_out,
                        site_focal = paths_oc_site$site[i],
                        path = paths_oc_site$path_file[i])
  }
}





guess_language <- function(ll) {
  
  ll <- dplyr::mutate_all(ll, ~tolower(as.character(.x)))
  
  language_counts <- c(
    en = sum(ll == "yes", na.rm = TRUE),
    fr = sum(ll == "oui", na.rm = TRUE),
    es = sum(ll == "si",  na.rm = TRUE),
    pt = sum(ll == "sim", na.rm = TRUE) 
  )
  
  names(which.max(language_counts))
}







prep_query_group_int <- function(d, group_start) {
  
  out <- d %>% 
    arrange(site, query_id, MSF_N_Patient) %>% 
    group_by(site) %>% 
    mutate(query_group_int = paste(query_id, MSF_N_Patient, linelist_row),
           query_group_int = integer_id(query_group_int)) %>% 
    ungroup()
  
  if (!missing(group_start)) {
    out$query_group_int <- out$query_group_int + out[[group_start]] - 1L
  }
  
  return(out)
}

format_query_group <- function(x) {
  paste0("Q", formatC(x, width = 5, flag = "0"))
}

prep_query_number_int <- function(d, number_start) {
  
  out <- d %>% 
    group_by(site, query_group) %>% 
    mutate(query_number_int = 1:n()) %>% 
    ungroup()
  
  if (!missing(number_start)) {
    out$query_number_int <- out$query_number_int + out[[number_start]] - 1L
  }
  
  return(out)
}

format_query_number <- function(x) {
  formatC(x, width = 3, flag = "0")
}


