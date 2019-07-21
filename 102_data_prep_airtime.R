
library(tidyverse)
library(chron)

convert_to_time <- function(time_orig) {
  date_time_1 <- stringr::str_split(string = time_orig, pattern = ' ', simplify = TRUE)
  time_1 <- chron::chron(dates. = date_time_1[, 1],
                         times. = date_time_1[, 2],
                         format = c('y-m-d', 'h:m:s'))
  return(time_1)
}

prep_airtime <- function(product_airtime, product_master) {
  airtime_merged <- product_airtime %>%
    dplyr::left_join(product_master, by = 'PRODUCT_NBR', keep = TRUE)

  airtime_merged$ONAIR_DATE <- as.Date.factor(airtime_merged$ONAIR_DATE, '%Y-%m-%d')
  airtime_merged$ONAIR_START_TMS <- convert_to_time(airtime_merged$ONAIR_START_TMS)
  airtime_merged$ONAIR_END_TMS <- convert_to_time(airtime_merged$ONAIR_END_TMS)
  airtime_merged$PRODUCT_DESCRIPTION <- as.character(airtime_merged$PRODUCT_DESCRIPTION)
  airtime_merged$ONAIR_DATE <- as.Date.factor(airtime_merged$ONAIR_DATE, '%Y-%m-%d')

  return(airtime_merged)
}
