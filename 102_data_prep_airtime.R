
library(tidyverse)
library(lubridate)
library(fuzzyjoin)

clean_airtime <- function(airtime, product_master) {
  airtime$PRODUCT_NBR <- as.character(airtime$PRODUCT_NBR)
  product_master$PRODUCT_NBR <- as.character(product_master$PRODUCT_NBR)

  airtime <- airtime %>%
    dplyr::left_join(product_master, by = 'PRODUCT_NBR', keep = TRUE)

  airtime$ONAIR_DATE <- lubridate::ymd(airtime$ONAIR_DATE)
  airtime$ONAIR_START_TMS <- lubridate::ymd_hms(airtime$ONAIR_START_TMS)
  airtime$ONAIR_END_TMS <- lubridate::ymd_hms(airtime$ONAIR_END_TMS)
  airtime$PRODUCT_DESCRIPTION <- as.character(airtime$PRODUCT_DESCRIPTION)

  return(airtime)
}

merge_onair_into_orders <- function() {

}

prep_airtime <- function(airtime, product_master, orders) {
  airtime <- clean_airtime(airtime, product_master)

  return(airtime)
}
