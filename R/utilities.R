
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



cap_first <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}



dist_match <- function(x, y, max_dist = 1) {
  abs(as.numeric(x - y)) <= max_dist | is.na(x) | is.na(y)
}


equal_or_na <- function(x, y) {
  x == y | is.na(x) | is.na(y)
}


expand_names <- function(names_x, names_y) {
  
  if (length(names_x) != length(names_y)) {
    stop("Arguments names_x and names_y must be of same length")
  }
  
  mapply(expand_grid_simple,
         group = seq_along(names_x), x1 = names_x, x2 = names_y,
         SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

expand_grid_simple <- function(group, x1, x2) {
  y1 <- rep(x1, each = length(x2))
  y2 <- rep(x2, times = length(x1))
  cbind(group = group, x1 = y1, x2 = y2)
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
  cols_replace <- setdiff(names(y), cols_match)
  cols_keep <- setdiff(names(x), cols_replace)
  
  x %>%
    dplyr::select(!!!cols_keep) %>% 
    dplyr::left_join(y, by = cols_match)
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




names_within_error <- function(x, dist_range_max) {
  # takes a vector of proper names and determines whether the stringdist between
  #  all pairs is less than a given threshold (i.e. whether all the names are all
  #  similar)
  library(repi)
  combinations <- combn(x, 2)
  similarity_metrics <- apply(
    combinations, 2, function(x) repi::string_similarity(x[1], x[2])
  )
  min_distances <- apply(similarity_metrics, 2, min, na.rm = TRUE)
  ifelse(all(min_distances <= dist_range_max), TRUE, FALSE)
}


string_sort <- function(x, sep = "[[:space:]]+") {
  # alphabetically sort the words within a string
  xx <- strsplit(x, sep)[[1]]
  paste(sort(xx), collapse = " ")
}


string_n_terms <- function(x) {
  # count number of terms within a string (where terms are separated by one or
  #  more space)
  xx <- strsplit(x, "[[:space:]]+")[[1]]
  length(xx)
}


string_combinations <- function(x, m) {
  # generate all combinations of m terms from a string (x)
  # if x contains fewer than m terms, returns NA
  xx <- strsplit(x, "[[:space:]]+")[[1]]
  if (length(xx) >= m) {
    yy <- t(utils::combn(xx, m))
    return(apply(yy, 1, function(x) paste(sort(x), collapse = " ")))
  } else {
    return(NA)
  }
}


is_excel_numeric <- function(x) {
  # check whether character string is probable excel date
  # i.e. string consisting solely of 4-6 digits
  grepl("^[[:digit:]]{4,6}$", x)
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


firstup <- function(x) {
  # fist character in string to capital
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


print_and_capture <- function(x) {
  # useful for adding a data.frame to a message() or warning()
  if ("tbl" %in% class(x)) x <- as.data.frame(x)
  paste(utils::capture.output(print(x)), collapse = "\n")
}


format_text <- function(x) {
  library(stringr)
  xx <- str_to_lower(x)
  xx <- str_trim(xx)
  xx <- str_replace_all(xx, "[^[:alnum:]]+", "_")
  xx <- str_replace(xx, "[_]+\\b", "")
  xx <- base::iconv(xx, to = "ASCII//TRANSLIT")
  xx <- str_replace_all(xx, "[^[[:alnum:]|_]]+", "")
  xx[xx == ""] <- NA_character_
  return(xx)
}


## convert age to years
age_to_years_scalar <- function(value, unit) {
  switch(tolower(unit),
         "months" = value / 12,
         "weeks" = value / 52,
         "days" = value / 365,
         value)
}

age_to_years <- function(value, unit) {
  if (!all(tolower(unit) %in% c("years", "months", "weeks", "days", NA_character_))) {
    warning("unit must be 'years', 'months', 'weeks', 'days', or NA (case-insensitive)")
  }
  mapply(age_to_years_scalar, value, unit)
}


# write nicely-formatted Excel file using openxlsx
write_pretty_xlsx <- function(x, file, fill = "#ffcccb", date_format = "yyyy-mm-dd",
                              firstActiveRow = 2, firstActiveCol = NULL, zoom = 130,
                              group_shade = NULL, overwrite = TRUE) {
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
  
  suppressMessages(openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite))
}




################################################################################
## Functions moved from app/utils.R
################################################################################

complete_epiweek <- function(df, var_epiweek_date, ...) {
  library(rlang)
  library(dplyr)
  library(tidyr)
  
  var_epiweek_date_enq <- enquo(var_epiweek_date)
  var_nested_enq <- quos(...)
  
  var_not_gathered_enq <- c(var_epiweek_date_enq, var_nested_enq)
  
  var_epiweek_date_name <- quo_name(var_epiweek_date_enq)
  var_nested_name <- sapply(var_nested_enq, rlang::quo_name)
  
  df %>%
    tidyr::gather(variable, value, -c(!!!var_not_gathered_enq)) %>%
    tidyr::complete(nesting_(!!var_nested_name),
                    !!var_epiweek_date_name := tidyr::full_seq(!!var_epiweek_date_enq, period = 7),
                    variable, fill = list(value = 0)) %>%
    tidyr::spread(variable, value)
}


make_epiweek_label <- function(epiweek_date) {
  library(lubridate)
  library(stringr)
  sprintf("%s-W%s",
          lubridate::isoyear(epiweek_date),
          stringr::str_sub(100 + lubridate::isoweek(epiweek_date), 2L, 3L))
}


make_epiweek_date <- function(date) {
  library(lubridate)
  lubridate::wday(date, week_start = 1) <- 7
  return(date)
}



anonymise_data <- function(df) {
  library(dplyr)
  dplyr::select(df,
                -contains("patient_name"),
                -contains("phone"),
                -ends_with("_details"),
                -contains("infector_name"),
                -contains("comment"),
                -contains("longitude"),
                -contains("latitude"))
}


is_yn_var <- function(x) {
  all(x %in% c("Yes", "No", "Unknown", NA_character_)) & !all(is.na(x))
}


is_PNI_var <- function(x) {
  all(x %in% c("Positive", "Negative", "Inconclusive", NA_character_)) & !all(is.na(x))
}


recode_yn_FR <- function(x) {
  library(dplyr)
  dplyr::recode(x, Yes = "Oui", No = "Non", Unknown = "Inc.")
}


recode_PNI_FR <- function(x) {
  library(dplyr)
  dplyr::recode(x, Positive = "Positif", Negative = "Négatif", Inconclusive = "Inconclusif")
}


translate_EN2FR <- function(df) {
  library(dplyr)
  df %>%
    dplyr::mutate(
      EVD_status = EVD_status %>% dplyr::recode(
        Suspect = "Suspect",
        Unknown = NA_character_,
        Confirmed = "Confirmé",
        `Not a case` = "Non cas"
      ),
      vaccination_rvsv_yn = vaccination_rvsv_yn %>% dplyr::recode(
        `Not vaccinated` = "Non",
        Unknown = "Inc.",
        Vaccinated = "Oui"
      ),
      treatment_yn = treatment_yn %>% dplyr::recode(
        Yes = "Oui",
        No = "Non"
      ),
      treatment = treatment %>% dplyr::recode(
        `Not treated` = NA_character_
      ),
      type_of_exit = type_of_exit %>% dplyr::recode(
        `Cured` = "Guéri",
        `Died` = "Décédé",
        `Lost to follow-up` = "Abandon",
        `Sent back home (not a case)` = "Retour a la maison (non cas)",
        `Transferred` = "Transféré",
        `Transferred (not a case)` = "Transféré (non cas)",
        `Transferred to ETC` = "Transféré au CTE",
        `Still treated/Unknown` = NA_character_
      ),
      age_unit = age_unit %>% dplyr::recode(
        Days = "Jour",
        Months = "Mois",
        Unknown = NA_character_,
        Years = "Ans"
      ),
      sex = sex %>% dplyr::recode(
        Female = "F",
        Male = "M",
        Unknown = NA_character_
      )
    ) %>%
    dplyr::mutate_if(is_yn_var, recode_yn_FR) %>%
    dplyr::mutate_if(is_PNI_var, recode_PNI_FR)
}



name_permutations <- function(x,
                              m = 2,
                              to.lower = TRUE,
                              to.ascii = TRUE,
                              terms.omit = NULL) {
  
  ## standardize
  if (to.lower) {
    x <- tolower(x)
    terms.omit <- tolower(terms.omit)
  } 
  if (to.ascii) {
    x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
    terms.omit <- stringi::stri_trans_general(terms.omit, id = "Latin-ASCII")
  }
  
  ## split
  xx <- strsplit(x, "[[:space:]]+")[[1]]
  
  ## combinations
  xx_sub <- xx[!is.na(xx) & nchar(xx) > 2 & !xx %in% terms.omit]
  x_combn <- if (length(xx_sub) >= m) {
    apply(t(combn(xx_sub, m)), 1, function(x) paste(sort(x), collapse = " "))
  } else {
    NULL
  }
  
  ## simple sort
  x_sorted <- paste(sort(xx), collapse = " ")
  
  ## return uniques
  return(unique(c(x, x_sorted, x_combn)))
}



filter_unique_id <- function(dat, col_group, col_id, invert = FALSE) {
  
  out <- dat %>% 
    dplyr::group_by(!!ensym(col_group)) %>% 
    dplyr::mutate(HAS_UNIQUE_ID = unique_no_na(!!ensym(col_id))) %>% 
    dplyr::ungroup()
  
  if (invert) {
    out <- filter(out, !HAS_UNIQUE_ID)
  } else {
    out <- filter(out, HAS_UNIQUE_ID)
  }
  
  return(dplyr::select(out, -HAS_UNIQUE_ID))
}


