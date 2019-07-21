
library(tidyverse)
library(chron)

prep_orders <- function(order_master, product_master, customer_master) {
  orders_merged <- order_master %>%
    dplyr::left_join(product_master, by = 'PRODUCT_NBR', keep = TRUE) %>%
    dplyr::left_join(customer_master, by = 'CUSTOMER_NBR', keep = TRUE)

  orders_merged$ORDER_NBR <- as.character(orders_merged$ORDER_NBR)
  orders_merged$PRODUCT_DESCRIPTION <- as.character(orders_merged$PRODUCT_DESCRIPTION)

  orders_merged$ORDER_DATE <- as.character(orders_merged$ORDER_DATE)
  orders_merged$ORDER_TIME <- as.character(orders_merged$ORDER_TIME)
  orders_merged$ORDER_TIME <- chron::chron(dates. = orders_merged$ORDER_DATE,
                                           times. = as.character(orders_merged$ORDER_TIME),
                                           format = c('y-m-d', 'h:m:s'))
  orders_merged$ORDER_DATE <- as.Date.factor(orders_merged$ORDER_DATE, '%Y-%m-%d')

  orders_merged$SHOPPER_SEGMENT_CODE = ifelse(orders_merged$SHOPPER_SEGMENT_CODE == 'NULL', '6', orders_merged$SHOPPER_SEGMENT_CODE)
  orders_merged$SHOPPER_SEGMENT_CODE = factor(orders_merged$SHOPPER_SEGMENT_CODE)

  orders_merged$ORDER_PLATFORM = as.character(orders_merged$ORDER_PLATFORM)
  orders_merged$ORDER_PLATFORM = ifelse(orders_merged$ORDER_PLATFORM == 'On Air - 2nd Channel', 'On Air', orders_merged$ORDER_PLATFORM)
  orders_merged$ORDER_PLATFORM = factor(orders_merged$ORDER_PLATFORM)

  orders_merged <- na.omit(orders_merged)

  # skip 2014 year order data.
  orders_merged <- orders_merged %>%
    dplyr::filter(ORDER_DATE >= as.Date('2015-01-01', '%Y-%m-%d'))

  return(orders_merged)
}
