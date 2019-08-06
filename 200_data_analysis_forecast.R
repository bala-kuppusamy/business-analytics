
library(tidyverse)
library(lubridate)
library(purrr)
library(ggplot2)
library(forecast)

orders_daily_summary <- readRDS(file = 'rdata/orders_daily_summary.Rda')

# category <- 'Accessories'
category <- 'Home Decor'

# filter the data for 1 product category
orders_daily_summary_accessories <- orders_daily_summary %>%
  dplyr::filter(PRODUCT_CATEGORY == category)

had_campaign <- orders_daily_summary_accessories$HAD_CAMPAIGN
had_campaign <- ifelse(had_campaign == 'Y', 1, 0)

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
plot(model_ts_pred, ylim = c(0, 1500000),  ylab = 'Daily Sales ($)', xlab = 'Week',
     bty = 'l', xaxt = 'n', xlim = c(1, 13), main = '', flty = 2)
axis(1, at = seq(1, 13, 1), labels = format(seq(1, 13, 1)))
lines(model_ts_pred$fitted, lwd = 2, col = 'blue')
lines(valid_ts)


# measure accuracy of the prediction
forecast::accuracy(model_ts_pred, valid_ts)


# ARIMA based forecast model
ts_log <- log(ts)

arima.fit <- forecast::auto.arima(ts_log, xreg = had_campaign, approximation = FALSE, trace = FALSE)
arima.fit
summary(arima.fit)

had_campaign_valid <- had_campaign[(ntrain + 1):length(had_campaign)]
pred <- stats::predict(arima.fit, newxreg = had_campaign_valid, n.ahead = 10)
pred

par(mfrow = c(1, 1))
plot(ts, type = 'l', xlim = c(1, 13), ylim = c(0, 1500000), xlab = 'Week', ylab = 'Daily Sales ($)')
lines(10^(pred$pred), col = 'blue')
lines(10^(pred$pred + 2 * pred$se), col = 'orange')
lines(10^(pred$pred - 2 * pred$se), col = 'orange')


