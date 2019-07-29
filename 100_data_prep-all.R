
library(tidyverse)
library(lubridate)
library(purrr)
# library(fuzzyjoin)

source(file = '101_data_prep_orders.R')
source(file = '102_data_prep_airtime.R')
source(file = '103_data_prep_email.R')

## flags to control whether to build dataframes from csv or load from saved rds files.
use_rds_order_master <- FALSE
use_rds_orders <- FALSE
use_rds_emails <- FALSE
use_rds_airtime <- FALSE

## read all data sets
if (use_rds_order_master) {
  print('Loading order-master - from saved rds file...')
  order_master <- readRDS(file = 'rdata/order_master.Rda')
} else {
  print('Building order-master - from csv file...')
  order_master <- utils::read.csv(file = 'data/order_master.csv', na.strings = c("NA", ""))
  saveRDS(order_master, file = 'rdata/order_master.Rda')
}

product_master <- utils::read.csv(file = 'data/product_master.csv', na.strings = c("NA", ""))
customer_master <- utils::read.csv(file = 'data/customer_master.csv', na.strings = c("NA", ""))
email_campaign <- utils::read.csv(file = 'data/email_campaign.csv', na.strings = c("NA", ""))
product_airtime <- utils::read.csv(file = 'data/product_airtime.csv', na.strings = c("NA", ""))

customer_master <- customer_master %>%
  dplyr::mutate(COUNTRY = dplyr::if_else(STATE %in% state.abb, 'US', 'Non-US'))
customer_master$COUNTRY <- factor(customer_master$COUNTRY)

## prepare orders
if (use_rds_orders) {
  print('Loading orders - from saved rds file...')
  orders <- readRDS(file = 'rdata/orders_merged.Rda')
} else {
  print('Building orders - by merging datasets...')
  orders <- prep_orders(order_master = order_master, product_master = product_master, customer_master = customer_master)
  saveRDS(orders, file = 'rdata/orders_merged.Rda')
}
summary(orders)
dplyr::glimpse(orders)


## prepare airtime
orders_onair <- orders %>%
  dplyr::filter(ORDER_PLATFORM == 'On Air')

if (use_rds_airtime) {
  print('Loading airtime - from saved rds file...')
  orders <- readRDS(file = 'rdata/airtime_merged.Rda')
} else {
  print('Building airtime - by merging datasets...')
  airtime <- prep_airtime(airtime = product_airtime, product_master = product_master, orders = orders_onair)
  saveRDS(orders, file = 'rdata/airtime_merged.Rda')
}
summary(airtime)
dplyr::glimpse(airtime)


## prepare emails
orders_web <- orders %>%
  dplyr::filter(ORDER_PLATFORM == 'QVC.COM')

if (use_rds_emails) {
  print('Loading emails - from saved rds file...')
  emails <- readRDS(file = 'rdata/emails_merged.Rda')
} else {
  print('Building emails - by merging datasets...')
  emails <- prep_emails(email_campaign = email_campaign, orders = orders_web)
  saveRDS(emails, file = 'rdata/emails_merged.Rda')
}
summary(emails)
dplyr::glimpse(emails)


