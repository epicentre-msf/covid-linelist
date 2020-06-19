
parse_excel_dates <- function(x) {
  dplyr::if_else(grepl("^[[:digit:]]{4,6}$", x), # if likely excel-numeric
                 suppressWarnings(as.character(janitor::excel_numeric_to_date(as.integer(x)))),
                 x)
}


parse_other_dates <- function(x, order = c("%d/%m/%Y", "%Y-%m-%d")) {
  dplyr::if_else(grepl("\\/", x), # if other date format
                 suppressWarnings(as.character(lubridate::parse_date_time(x, order = order))),
                 x)
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
  
  df <- readxlsb::read_xlsb(
    file_path,
    sheet = "options",
    col_types = "string"
  )
  
  language <- df$value[df$variable == "language"]
  version <- df$value[df$variable == "version"]
  
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


add_na_dict <- function(d) {
  dplyr::bind_rows(
    d,
    tibble::tibble(values_en = NA, variable_en = unique(d$variable_en))
  )
}


string_to_numeric_prep <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace("[,;/]+", ".") %>%
    # recode(!!!dict_numeric) %>%
    stringr::str_replace("^\\.$", NA_character_)
}


paste_collapse <- function(x, y, collapse = "; ") {
  z <- c(x, y)
  paste(z[!is.na(z)], collapse = collapse)
}

paste_collapse <- Vectorize(paste_collapse, vectorize.args = c("x", "y"),
                            USE.NAMES = FALSE)


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


# test whether each row of data.frame is almost-empty, defined in relation
#  to a critical number of non-<NA> values
almost_empty_rows <- function(df, n_crit = 2, cols_exclude = NULL) {
  df <- df[,!names(df) %in% cols_exclude]
  ncol(df) - rowSums(is.na(df)) < n_crit
}


time_stamp <- function(t = Sys.time(), format = "%Y-%m-%d_%H%M") {
  # convenience function for creating time stamps
  format(t, format = format)
}


date_to_excel_numeric <- function(x) {
  # opposite of janitor::excel_numeric_to_date() (1899-12-30 is date 0)
  as.integer(as.Date(x) - as.Date("1899-12-30"))
}


is_posix <- function(x) {
  # test whether object 'x' is of class POSIXt/POSIXct/POSIXlt
  any(class(x) %in% c("POSIXt", "POSIXct", "POSIXlt"))
}


read_xlsx_and_label <- function(path, name = "file_index", pattern = path) {
  # read xlsx file and add new column based on pattern extracted from path
  library(readxl)
  library(dplyr)
  library(stringr)
  library(rlang)
  out <- readxl::read_xlsx(path)
  out <- dplyr::mutate(out, !!name := stringr::str_extract(path, pattern))
  out
}


list_files <- function(path = ".", pattern = NULL, full.names = FALSE,
                       ignore.case = FALSE, ignore.temp = TRUE,
                       last.sorted = FALSE) {
  # convenience version of list.files to find files within a directory while
  #  ignoring temporary files (which on some systems begin with "~$")
  # if pattern matches multiple files differing only in their time stamps, arg
  #  last.sorted allows user to retain only the most recent file
  files <- list.files(path = path, pattern = pattern)
  if (ignore.temp) {
    # remove temporary files (negative look-behind for "~$" before 'pattern')
    files <- grep(paste0("(?<!\\~\\$)", pattern), files,
                  ignore.case = ignore.case, value = TRUE, perl = TRUE)
  }
  if (last.sorted) {
    # select only the last-sorted file
    files <- tail(sort(files), 1)
  }
  if (full.names) {
    files <- file.path(path, files)
  }
  files
}


left_join_verbose <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  # dplyr::left_join() with warning if any rows of 'x' are duplicated
  library(dplyr)
  x$TEMPORARY_INDEX_COL <- 1:nrow(x)
  out <- dplyr::left_join(x, y, by = by, copy = copy, suffix = suffix, ...)
  if (any(duplicated(out$TEMPORARY_INDEX_COL))) {
    rows_dup <- out$TEMPORARY_INDEX_COL[duplicated(out$TEMPORARY_INDEX_COL)]
    warning("The following rows of 'x' have been duplicated by `left_join()`: ",
            paste(rows_dup, collapse = ", "),
            call. = FALSE)
  }
  out[,names(out) != "TEMPORARY_INDEX_COL"]
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


length_unique <- function(x) {
  # find number of unique values in vector 'x'
  length(unique(x))
}


unique_no_na <- function(x) {
  # return logical indicating whether vector 'x' contains single unique value
  #  with no <NA>
  !any(is.na(x)) && length_unique(x) == 1
}


dates_within_error <- function(x, day_range_max) {
  # takes a vector of dates and determines whether the max difference in days
  #  is less than a given threshold (i.e. whether all the dates are close
  #  together)
  range_in_days <- as.integer(diff(range(as.Date(x))))
  ifelse(range_in_days <= day_range_max, TRUE, FALSE)
}


numeric_within_error <- function(x, range_max) {
  # take a vector of numeric values and determine whether the range (max - min)
  #  is less than a given threshold (i.e. whether all the numbers are close
  #  together)
  numeric_range <- diff(range(x))
  ifelse(numeric_range <= range_max, TRUE, FALSE)
}


is_excel_numeric <- function(x) {
  # check whether character string is probable excel date
  # i.e. string consisting solely of 4-6 digits
  grepl("^[[:digit:]]{4,6}$", x)
}


parse_excel_dates <- function(x) {
  # parse character string consisting of R date (YYYY-MM-DD) OR excel date
  #  (NNNNN); output will be character in "YYYY-MM-DD" format
  library(janitor)
  ifelse(is_excel_numeric(x),
         as.character(janitor::excel_numeric_to_date(as.integer(x))),
         x)
}


as_numeric_quiet <- function(x) {
  # as.numeric without coerce warnings
  suppressWarnings(as.numeric(x))
}



print_and_capture <- function(x) {
  # useful for adding a data.frame to a message() or warning()
  if ("tbl" %in% class(x)) x <- as.data.frame(x)
  paste(utils::capture.output(print(x)), collapse = "\n")
}


## convert age to years
age_to_years_scalar <- function(value, unit) {
  switch(tolower(unit),
         "year" = value,
         "month" = value / 12,
         "day" = value / 365.25,
         value)
}

age_to_years <- function(value, unit) {
  if (!all(tolower(unit) %in% c("year", "month", "day", NA_character_))) {
    warning("unit must be 'year', 'month', 'day', or NA (case-insensitive)")
  }
  mapply(age_to_years_scalar, value, unit)
}


# write nicely-formatted Excel file using openxlsx
write_pretty_xlsx <- function(x, file, fill = "#ffcccb", date_format = "yyyy-mm-dd",
                              firstActiveRow = 2, firstActiveCol = NULL, zoom = 130,
                              group_shade = NULL, overwrite = TRUE,
                              return_wb = FALSE) {
  library(openxlsx)
  options("openxlsx.dateFormat" = date_format)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1", zoom = zoom)
  
  if (!is.null(group_shade)) {
    x <- x[order(x[[group_shade]]),]
    x[["i"]] <- as.integer(integer_id(x[[group_shade]]) %% 2L)
    x <- x[,c("i", names(x)[names(x) != "i"])]
  }
  
  nrow_x <- nrow(x) + 1
  ncol_x <- ncol(x)
  openxlsx::writeData(wb, 1, x)
  openxlsx::setColWidths(wb, 1, cols = 1:ncol_x, widths = "auto")
  openxlsx::freezePane(wb, 1, firstActiveRow = 2)
  
  if (!is.null(group_shade)) {
    openxlsx::conditionalFormatting(wb, 1,
                                    cols = 1:ncol_x,
                                    rows = 2:nrow_x,
                                    rule = paste0("$A2>0"),
                                    style = openxlsx::createStyle(bgFill = fill))
  }
  
  if (return_wb) {
    return(wb)
  } else {
    suppressMessages(openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite))
  }
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



write_query_tracker <- function(queries_out, header_recode, site_focal = NULL, path) {
  
  if (!is.null(site_focal)) {
    queries_out <- queries_out %>% 
      filter(site == site_focal)
  }
  
  queries_summary <- queries_out %>% 
    group_by(category, query_id, description) %>% 
    summarize(n_total = n(), .groups = "drop") %>% 
    arrange(desc(n_total)) %>% 
    select(Category = category, `Query ID` = query_id, `N (Total)` = n_total, Description = description)
  
  queries_out <- queries_out %>% 
    rename(!!!header_recode)
  
  ## Write updated query tracker sheets to file
  library(openxlsx)
  options("openxlsx.dateFormat" = "yyyy-mm-dd")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Queries", zoom = 130)
  hs <- openxlsx::createStyle(halign = "center", textDecoration = "Bold")
  
  # sheet 1 (Main query tracker)
  openxlsx::writeData(wb, 1, queries_out, withFilter = TRUE)
  openxlsx::setColWidths(wb, 1, cols = 1:ncol(queries_out),
                         widths = c(6, 15, 14, 10, 12, 13, 15, 20, 15, 20, 15, 100, 40, 30, 40, 30, 22, 17, 22, 17, 22, 70))
  openxlsx::freezePane(wb, 1, firstActiveRow = 2)
  openxlsx::addStyle(wb, 1, style = hs, rows = 1, cols = 1:ncol(queries_out), gridExpand = TRUE)
  openxlsx::conditionalFormatting(wb, 1,
                                  cols = 1:ncol(queries_out),
                                  rows = 2:(nrow(queries_out) + 1L),
                                  rule = paste0("$A2>0"),
                                  style = openxlsx::createStyle(bgFill = "#fddbc7"))
  
  # sheet 2 (summary)
  openxlsx::addWorksheet(wb, "Summary", zoom = 130)
  openxlsx::writeData(wb, 2, queries_summary)
  openxlsx::freezePane(wb, 2, firstActiveRow = 2)
  openxlsx::addStyle(wb, 2, style = hs, rows = 1, cols = 1:ncol(queries_summary), gridExpand = TRUE)
  openxlsx::setColWidths(wb, 2, cols = 1:ncol(queries_summary),
                         widths = c(21, 14, 10, 110))
  
  suppressMessages(
    openxlsx::saveWorkbook(
      wb,
      file = path,
      overwrite = TRUE
    )
  )
}

