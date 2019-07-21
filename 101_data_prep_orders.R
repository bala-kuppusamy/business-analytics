
library(tidyverse)
library(lubridate)

prep_orders <- function(order_master, product_master, customer_master) {
  orders <- order_master %>%
    dplyr::left_join(product_master, by = 'PRODUCT_NBR', keep = TRUE) %>%
    dplyr::left_join(customer_master, by = 'CUSTOMER_NBR', keep = TRUE)

  orders$ORDER_NBR <- as.character(orders$ORDER_NBR)
  orders$PRODUCT_DESCRIPTION <- as.character(orders$PRODUCT_DESCRIPTION)
  orders$ORDER_TIME <- lubridate::ymd_hms(paste(orders$ORDER_DATE, orders$ORDER_TIME, sep = ' '))
  orders$ORDER_DATE <- lubridate::ymd(orders$ORDER_DATE)
  orders$SHOPPER_SEGMENT_CODE = factor(ifelse(orders$SHOPPER_SEGMENT_CODE == 'NULL', '6', orders$SHOPPER_SEGMENT_CODE))

  orders$ORDER_PLATFORM = as.character(orders$ORDER_PLATFORM)
  orders$ORDER_PLATFORM = ifelse(orders$ORDER_PLATFORM == 'On Air - 2nd Channel', 'On Air', orders$ORDER_PLATFORM)
  orders$ORDER_PLATFORM = factor(orders$ORDER_PLATFORM)

  orders <- na.omit(orders)

  # skip 2014 year order data.
  yr2015 <- lubridate::ymd('2015-01-01')
  orders <- orders %>%
    dplyr::filter(ORDER_DATE >= yr2015)

  return(orders)
}
