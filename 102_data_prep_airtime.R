
library(tidyverse)
library(lubridate)
library(purrr)

# global variable
processed_count <- 0

clean_airtime <- function(airtime, product_master) {
  airtime$PRODUCT_NBR <- as.character(airtime$PRODUCT_NBR)
  product_master$PRODUCT_NBR <- as.character(product_master$PRODUCT_NBR)

  airtime <- airtime %>%
    dplyr::left_join(product_master, by = 'PRODUCT_NBR', keep = TRUE)

  airtime$ONAIR_DATE <- lubridate::ymd(airtime$ONAIR_DATE)
  airtime$ONAIR_START_TMS <- lubridate::ymd_hms(airtime$ONAIR_START_TMS)
  airtime$ONAIR_END_TMS <- lubridate::ymd_hms(airtime$ONAIR_END_TMS)
  airtime$PRODUCT_DESCRIPTION <- as.character(airtime$PRODUCT_DESCRIPTION)

  # add an ID column to every record
  airtime <- tibble::rowid_to_column(airtime, "ID")

  return(airtime)
}

calc_tot_sale_amt <- function(airtime, orders) {
  processed_count <<- processed_count + 1
  if (processed_count %% 10 == 0) {
    print(paste('Completed days', processed_count, sep = ' : '))
  }

  airtime_sales <- airtime %>%
    dplyr::left_join(orders, by = c('PRODUCT_NBR' = 'PRODUCT_NBR', 'ONAIR_DATE' = 'ORDER_DATE'), keep = TRUE) %>%
    dplyr::filter(lubridate::`%within%`(ORDER_TIME, lubridate::interval(ONAIR_START_TMS, ONAIR_END_TMS)) == TRUE) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(SALE_AMT = sum(TOTAL_LINE_AMT))

  return(airtime_sales)
}

prep_airtime <- function(airtime, product_master, orders) {
  airtime <- clean_airtime(airtime, product_master)

  print('Calculating Airtime total sales; Total days to be processed = 90')
  print('NOTE: This is a resource intensive process & could take ~ 5-10 mins')

  airtime_sales <- airtime %>%
    split(.$ONAIR_DATE) %>%
    purrr::map(.f = calc_tot_sale_amt, orders) %>%
    dplyr::bind_rows()

  airtime <- airtime %>%
    dplyr::left_join(airtime_sales, by = 'ID') %>%
    dplyr::mutate(SALE_AMT = dplyr::if_else(is.na(SALE_AMT) == TRUE, 0, SALE_AMT))

  return(airtime)
}
