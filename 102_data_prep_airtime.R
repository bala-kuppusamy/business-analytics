
library(tidyverse)
library(lubridate)

prep_airtime <- function(product_airtime, product_master) {
  airtime <- product_airtime %>%
    dplyr::left_join(product_master, by = 'PRODUCT_NBR', keep = TRUE)

  airtime$ONAIR_DATE <- lubridate::ymd(airtime$ONAIR_DATE)
  airtime$ONAIR_START_TMS <- lubridate::ymd_hms(airtime$ONAIR_START_TMS)
  airtime$ONAIR_END_TMS <- lubridate::ymd_hms(airtime$ONAIR_END_TMS)
  airtime$PRODUCT_DESCRIPTION <- as.character(airtime$PRODUCT_DESCRIPTION)

  return(airtime)
}
