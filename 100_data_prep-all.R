
library(tidyverse)
library(lubridate)
library(fuzzyjoin)

source(file = '101_data_prep_orders.R')
source(file = '102_data_prep_airtime.R')
source(file = '103_data_prep_email.R')

## flags to control whether to load data from csv or from saved rds files.
use_rds_order_master <- TRUE
use_rds_orders <- TRUE

## read all data sets
if(use_rds_order_master) {
  print('Loading order-master - from saved rds file...')
  order_master <- readRDS(file = 'rdata/order_master.Rda')
} else {
  print('Loading order-master - from csv file...')
  order_master <- utils::read.csv(file = 'data/order_master.csv', na.strings = c("NA", ""))
  saveRDS(order_master, file = 'rdata/order_master.Rda')
}

product_master <- utils::read.csv(file = 'data/product_master.csv', na.strings = c("NA", ""))
customer_master <- utils::read.csv(file = 'data/customer_master.csv', na.strings = c("NA", ""))
email_campaign <- utils::read.csv(file = 'data/email_campaign.csv', na.strings = c("NA", ""))
product_airtime <- utils::read.csv(file = 'data/product_airtime.csv', na.strings = c("NA", ""))

customer_master <- customer_master %>%
  dplyr::mutate(COUNTRY = if_else(STATE %in% state.abb, 'US', 'Non-US'))
customer_master$COUNTRY <- factor(customer_master$COUNTRY)

## prepare orders
if(use_rds_orders) {
  print('Loading orders - from saved rds file...')
  orders <- readRDS(file = 'rdata/orders_merged.Rda')
} else {
  print('Loading orders - by merging datasets...')
  orders <- prep_orders(order_master = order_master, product_master = product_master, customer_master = customer_master)
  saveRDS(orders, file = 'rdata/orders_merged.Rda')
}
summary(orders)
dplyr::glimpse(orders)


## prepare email
orders_web <- orders %>%
  dplyr::filter(ORDER_PLATFORM == 'QVC.COM')

emails <- prep_emails(email_campaign = email_campaign, orders = orders_web)
saveRDS(emails, file = 'rdata/emails_merged.Rda')
summary(emails)
dplyr::glimpse(emails)


# TODO - add order data for airtime
## prepare airtime
orders_onair <- orders %>%
  dplyr::filter(ORDER_PLATFORM == 'On Air')

airtime <- prep_airtime(airtime = product_airtime, product_master = product_master, orders = orders_onair)
saveRDS(orders, file = 'rdata/airtime_merged.Rda')
summary(airtime)
dplyr::glimpse(airtime)

####------------------------------------------------------------

# orders_onair_min <- head(orders_onair, 10)
#
# orders_onair_matched <- fuzzyjoin::fuzzy_left_join(x = orders_onair_min, y = airtime,
#                            by = c('PRODUCT_NBR' = 'PRODUCT_NBR', 'ORDER_TIME' = 'ONAIR_START_TMS', 'ORDER_TIME' = 'ONAIR_END_TMS'),
#                            match_fun = c(`==`, `>=`, `<=`))
# dplyr::glimpse(orders_onair_matched)
#
# has_airtime <- function(order, airtime) {
#   prod_nbr <- order[4]
#   order_time <- order[7]
#   matched_airtime <- airtime %>%
#     dplyr::filter(PRODUCT_NBR == prod_nbr) %>%
#     dplyr::filter(ONAIR_START_TMS <= order_time) %>%
#     dplyr::filter(ONAIR_END_TMS >= order_time)
#
#   # dplyr::glimpse(matched_airtime)
#   return(matched_airtime)
# }
#
# matched_airtimes <- apply(X = orders_onair_min, FUN = has_airtime, MARGIN = 1, airtime = airtime)
# matched_airtimes <- dplyr::as_tibble(matched_airtimes)
# dplyr::glimpse(matched_airtimes)
