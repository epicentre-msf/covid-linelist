
## requires
library(tidyverse)
library(forecast)


## function to model trends
model_trends <- function(x,
                         dates_extent,
                         ma_window = 3,
                         min_sum = 30) {
  
  # if running locally
  if (FALSE) {
    x <- lst_dta[[1]]
    dates_extent <- c(max(x$date) - 13, max(x$date))
    ma_window = 3
    min_sum = 3
  }
  
  # filter data to date range of interest
  xsub <- x %>% 
    filter(between(date, dates_extent[1], dates_extent[2])) %>% 
    tidyr::complete(
      date = seq.Date(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = 1), 
      fill = list(cases = NA_real_, deaths = NA_real_)
    )
  
  if (nrow(xsub) > ma_window & sum(xsub$cases, na.rm = TRUE) > min_sum) {
    
    # moving average
    xsub$ma <- as.numeric(forecast::ma(xsub$cases, order = ma_window))
    xsub$ma <- dplyr::na_if(xsub$ma, 0) # PB: I don't think this is good idea, but kept from prev code
    
    # linear model and confidence intervals
    mdl <- lm(log(ma) ~ date, data = xsub)
    ci80 <- confint(mdl, level = 0.80)
    ci95 <- confint(mdl, level = 0.95)
    
    # prep output
    mdl_coeffs <- tibble::tibble(
      coeff = coefficients(mdl)[[2]], 
      lwr80  = ci80[2,1], 
      upr80  = ci80[2,2],
      lwr95  = ci95[2,1], 
      upr95  = ci95[2,2]
    )
  } else {
    mdl_coeffs <- tibble::tibble(
      coeff = NA_real_, 
      lwr80 = NA_real_, 
      upr80 = NA_real_,
      lwr95 = NA_real_, 
      upr95 = NA_real_
    ) 
  }
  
  # return
  mdl_coeffs
}


## load JHU data
# # originally created with script in sitrep repo
# dta_jhu <- get_owid_jhcsse()
# 
# # JHU data right censored until date_max
# dta_jhu_right_censored <- dta_jhu %>% 
#   tidyr::drop_na(iso_a3) %>% 
#   filter(between(date, left = date_min_report, right = date_max_report))
# 
# # JHU split data in a list of countries with iso_a3 as key
# lst_dta_jhu <- dta_jhu_right_censored %>% 
#   multisplit("iso_a3")
lst_dta <- readRDS("local/trends/lst_dta_jhu.rds")


## params
last_date <- max(bind_rows(lst_dta)$date)
time_unit_extent <- 14
dates_extent <- c(last_date - (time_unit_extent - 1), last_date)


## get trends
trends_all <- lst_dta %>% 
  purrr::map_dfr(
    model_trends,
    dates_extent = dates_extent,
    .id = "iso_a3"
  )


## add trend classification
trends_all_class <- trends_all %>% 
  mutate(
    trend = case_when(
      lwr95 > 0 ~ "Increasing",
      lwr95 <= 0 & lwr80 > 0 ~ "Likely increasing",
      upr95 < 0 ~ "Decreasing",
      upr95 >= 0 & upr80 < 0  ~ "Likely decreasing",
      lwr80 < 0 & upr80 > 0 ~ "Stable",
      TRUE ~ NA_character_
    )
  )

