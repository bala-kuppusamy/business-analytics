
library(tidyverse)
library(lubridate)
library(purrr)
library(ggplot2)
library(forecast)

orders_daily_summary <- readRDS(file = 'rdata/orders_daily_summary.Rda')

# filter the data for 1 product category
orders_daily_summary_accessories <- orders_daily_summary %>%
  dplyr::filter(PRODUCT_CATEGORY == 'Accessories')


# build time-series data
ts <- stats::ts(data = orders_daily_summary_accessories$DAILY_SALE_AMT, start = c(0, 1), frequency = 7)
plot(ts)


# do data partition
nvalid <- 10
ntrain <- length(ts) - nvalid
train_ts <- window(ts, start = c(0, 1), end = c(0, ntrain))
valid_ts <- window(ts, start = c(0, ntrain + 1), end = c(0, ntrain + nvalid))


# build forecast model & predict
model_ts <- forecast::tslm(train_ts~trend+season)
summary(model_ts)
model_ts_pred <- forecast::forecast(model_ts, h = nvalid, level = 0)


# visualize the linear trend model
par(mfrow = c(1, 1))
plot(model_ts_pred, ylim = c(0, 1500000),  ylab = "Daily Sales ($)", xlab = "Week",
     bty = "l", xaxt = "n", xlim = c(1, 13), main = "", flty = 2)
axis(1, at = seq(1, 13, 1), labels = format(seq(1, 13, 1)))
lines(model_ts_pred$fitted, lwd = 2, col = "blue")
lines(valid_ts)


# measure accuracy of the prediction
forecast::accuracy(model_ts_pred, valid_ts)

