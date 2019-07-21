
library(tidyverse)
library(chron)

source(file = '101_data_prep_orders.R')
source(file = '102_data_prep_airtime.R')

# flags to control whether to load data from csv or from saved rds files.
use_rds_order_master <- TRUE
use_rds_orders <- TRUE

## read all data sets
if(use_rds_order_master) {
  print('loading order-master - from saved rds file...')
  order_master <- readRDS(file = 'rdata/order_master.Rda')
} else {
  print('loading order-master - from csv file...')
  order_master <- utils::read.csv(file = 'data/order_master.csv', na.strings = c("NA", ""))
  saveRDS(order_master, file = 'rdata/order_master.Rda')
}

product_master <- utils::read.csv(file = 'data/product_master.csv', na.strings = c("NA", ""))
customer_master <- utils::read.csv(file = 'data/customer_master.csv', na.strings = c("NA", ""))
product_airtime <- utils::read.csv(file = 'data/product_airtime.csv', na.strings = c("NA", ""))

customer_master <- customer_master %>%
  dplyr::mutate(COUNTRY = if_else(STATE %in% state.abb, 'US', 'Non-US'))
customer_master$COUNTRY <- factor(customer_master$COUNTRY)

# prepare orders
if(use_rds_orders) {
  print('loading orders - from saved rds file...')
  orders <- readRDS(file = 'rdata/orders_merged.Rda')
} else {
  print('loading orders - by merging datasets...')
  orders <- prep_orders(order_master = order_master, product_master = product_master, customer_master = customer_master)
  saveRDS(orders, file = 'rdata/orders_merged.Rda')
}
summary(orders)
str(orders)

# prepare product airtime
airtime <- prep_airtime(product_airtime = product_airtime, product_master = product_master)
saveRDS(orders, file = 'rdata/airtime_merged.Rda')
summary(airtime)
str(airtime)
