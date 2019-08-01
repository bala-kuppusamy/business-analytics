
library(tidyverse)
library(lubridate)

clean_email_categories <- function(email_campaign) {
  email_campaign$CAMPAIGN_DATE <- lubridate::mdy(email_campaign$CAMPAIGN_DATE)

  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel & Accessories') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Apparel', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 2) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel & Accessories') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Accessories', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 2) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign <- email_campaign %>%
    dplyr::filter(PRODUCT_CATEGORY != 'Apparel & Accessories')

  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel, Accessories & Jewelry') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Apparel', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 3) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel, Accessories & Jewelry') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Accessories', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 3) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel, Accessories & Jewelry') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Jewelry', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 3) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign <- email_campaign %>%
    dplyr::filter(PRODUCT_CATEGORY != 'Apparel, Accessories & Jewelry')

  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel, Accessories, Jewelry & Beauty') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Apparel', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 4) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel, Accessories, Jewelry & Beauty') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Accessories', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 4) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel, Accessories, Jewelry & Beauty') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Jewelry', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 4) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Apparel, Accessories, Jewelry & Beauty') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Beauty', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 4) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign <- email_campaign %>%
    dplyr::filter(PRODUCT_CATEGORY != 'Apparel, Accessories, Jewelry & Beauty')

  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Kitchen & Food') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Kitchen', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 2) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign %<>%
    dplyr::filter(PRODUCT_CATEGORY == 'Kitchen & Food') %>%
    dplyr::mutate(PRODUCT_CATEGORY = 'Food', CAMPAIGN_SPEND = CAMPAIGN_SPEND / 2) %>%
    dplyr::bind_rows(email_campaign)
  email_campaign <- email_campaign %>%
    dplyr::filter(PRODUCT_CATEGORY != 'Kitchen & Food')

  email_campaign <- email_campaign %>%
    dplyr::mutate(PRODUCT_CATEGORY = dplyr::if_else(PRODUCT_CATEGORY == 'Beauty', 'Health/Beauty', PRODUCT_CATEGORY)) %>%
    dplyr::mutate(PRODUCT_CATEGORY = dplyr::if_else(PRODUCT_CATEGORY == 'Home & Garden', 'Home Decor', PRODUCT_CATEGORY)) %>%
    dplyr::mutate(PRODUCT_CATEGORY = dplyr::if_else(PRODUCT_CATEGORY == 'Kitchen', 'Housewares', PRODUCT_CATEGORY)) %>%
    dplyr::mutate(PRODUCT_CATEGORY = dplyr::if_else(PRODUCT_CATEGORY == 'Food', 'Health', PRODUCT_CATEGORY))

  email_campaign <- email_campaign %>%
    dplyr::group_by(CAMPAIGN_DATE, PRODUCT_CATEGORY) %>%
    dplyr::summarize(CAMPAIGN_SPEND = sum(CAMPAIGN_SPEND))

  return(email_campaign)
}

mark_campaign_orders <- function(emails, orders) {
  orders <- orders %>%
    dplyr::left_join(emails, by = c('ORDER_DATE' = 'CAMPAIGN_DATE', 'PRODUCT_CATEGORY'))

  orders <- orders %>%
    dplyr::mutate(IS_JANUARY_ORDER = dplyr::if_else(lubridate::month(ORDER_DATE) == 1, TRUE, FALSE))

  orders <- orders %>%
    dplyr::mutate(IS_CAMPAIGN = FALSE) %>%
    dplyr::mutate(IS_CAMPAIGN = dplyr::if_else(!is.na(CAMPAIGN_SPEND) & CAMPAIGN_SPEND > 0, TRUE, IS_CAMPAIGN)) %>%
    dplyr::mutate(IS_CAMPAIGN = dplyr::if_else(EMAIL_HOLD_IND == 'Y', FALSE, IS_CAMPAIGN)) %>%
    dplyr::mutate(IS_CAMPAIGN = dplyr::if_else(EMAIL_JANUARAY_ONLY_HOLD_IND == 'Y' & IS_JANUARY_ORDER == TRUE, FALSE, IS_CAMPAIGN))

  return(orders)
}

calc_avg_daily_sale <- function(orders) {
  order_summary <- orders %>%
    dplyr::group_by(ORDER_DATE, PRODUCT_CATEGORY) %>%
    dplyr::summarise(ORDER_AMT = sum(TOTAL_LINE_AMT))

  avg_daily_sale <- order_summary %>%
    dplyr::group_by(PRODUCT_CATEGORY) %>%
    dplyr::summarise(TOT_ORDER_AMT = sum(ORDER_AMT), ORDER_DAYS = n(), AVG_DAILY_SALE = TOT_ORDER_AMT / ORDER_DAYS)

  avg_daily_sale %<>%
    dplyr::summarise(TOT_ORDER_AMT = sum(TOT_ORDER_AMT), ORDER_DAYS = sum(ORDER_DAYS),
                     AVG_DAILY_SALE = TOT_ORDER_AMT / ORDER_DAYS, PRODUCT_CATEGORY = 'All') %>%
    dplyr::bind_rows(avg_daily_sale)

  return(avg_daily_sale)
}

merge_email_sale_amt <- function(emails, orders, avg_daily_sale) {
  order_summary <- orders %>%
    dplyr::group_by(ORDER_DATE, PRODUCT_CATEGORY) %>%
    dplyr::summarize(CAMPAIGN_ORDER_AMT = sum(TOTAL_LINE_AMT)) %>%
    dplyr::mutate(NEXT_DATE = ORDER_DATE + lubridate::days(1)) %>%
    dplyr::mutate(CAMPAIGN_ORDER_AMT_DAY_1 = dplyr::if_else(is.na(CAMPAIGN_ORDER_AMT), 0, CAMPAIGN_ORDER_AMT))

  order_summary_2day <- order_summary %>%
    dplyr::rename(CAMPAIGN_ORDER_AMT_DAY_2 = CAMPAIGN_ORDER_AMT_DAY_1)

  order_summary %<>%
    dplyr::left_join(order_summary_2day, by = c('NEXT_DATE' = 'ORDER_DATE', 'PRODUCT_CATEGORY')) %>%
    dplyr::select(ORDER_DATE, PRODUCT_CATEGORY, CAMPAIGN_ORDER_AMT_DAY_1, CAMPAIGN_ORDER_AMT_DAY_2) %>%
    dplyr::mutate(CAMPAIGN_ORDER_AMT_TOTAL = CAMPAIGN_ORDER_AMT_DAY_1 + CAMPAIGN_ORDER_AMT_DAY_2) %>%
    dplyr::mutate(CAMPAIGN_ORDER_AMT_DAY_2 = dplyr::if_else(is.na(CAMPAIGN_ORDER_AMT_DAY_2), 0, CAMPAIGN_ORDER_AMT_DAY_2)) %>%
    dplyr::mutate(CAMPAIGN_ORDER_AMT_TOTAL = dplyr::if_else(is.na(CAMPAIGN_ORDER_AMT_TOTAL), 0, CAMPAIGN_ORDER_AMT_TOTAL))

  # add 'All' category record
  order_summary %<>%
    dplyr::group_by(ORDER_DATE) %>%
    dplyr::summarize(CAMPAIGN_ORDER_AMT_DAY_1 = sum(CAMPAIGN_ORDER_AMT_DAY_1),
                     CAMPAIGN_ORDER_AMT_DAY_2 = sum(CAMPAIGN_ORDER_AMT_DAY_2),
                     CAMPAIGN_ORDER_AMT_TOTAL = sum(CAMPAIGN_ORDER_AMT_TOTAL),
                     PRODUCT_CATEGORY = 'All') %>%
    dplyr::bind_rows(order_summary)

  emails <- emails %>%
    dplyr::left_join(order_summary, by = c('CAMPAIGN_DATE' = 'ORDER_DATE', 'PRODUCT_CATEGORY'))

  avg_daily_sale <- avg_daily_sale %>%
    dplyr::select(PRODUCT_CATEGORY, AVG_DAILY_SALE)

  emails <- emails %>%
    dplyr::left_join(avg_daily_sale, by = c('PRODUCT_CATEGORY'))

  return(emails)
}

prepare_daily_summary <- function(orders, emails) {
  orders_web_daily_summary <- orders %>%
    dplyr::group_by(ORDER_DATE, PRODUCT_CATEGORY) %>%
    dplyr::summarise(DAILY_SALE_AMT = sum(TOTAL_LINE_AMT)) %>%
    dplyr::left_join(emails, by = c('ORDER_DATE' = 'CAMPAIGN_DATE', 'PRODUCT_CATEGORY')) %>%
    dplyr::mutate(HAD_CAMPAIGN = dplyr::if_else(is.na(CAMPAIGN_SPEND), 'N', 'Y')) %>%
    dplyr::mutate(WEEKDAY = lubridate::wday(ORDER_DATE, label = TRUE))

  orders_web_daily_summary$PRODUCT_CATEGORY = factor(orders_web_daily_summary$PRODUCT_CATEGORY)
  orders_web_daily_summary$HAD_CAMPAIGN = factor(orders_web_daily_summary$HAD_CAMPAIGN)

  return(orders_web_daily_summary)
}

prep_emails <- function(email_campaign, orders) {
  emails <- clean_email_categories(email_campaign = email_campaign)

  orders_with_email <- mark_campaign_orders(emails = emails, orders = orders)
  dplyr::glimpse(orders_with_email)
  saveRDS(orders_with_email, file = 'rdata/orders_web_with_email.Rda')

  orders_daily_summary <- prepare_daily_summary(orders = orders_with_email, emails = emails)
  dplyr::glimpse(orders_daily_summary)
  saveRDS(orders_daily_summary, file = 'rdata/orders_daily_summary.Rda')

  non_campaign_orders <- orders_with_email %>%
    dplyr::filter(IS_CAMPAIGN == FALSE)

  avg_daily_sale <- calc_avg_daily_sale(orders = non_campaign_orders)

  emails <- merge_email_sale_amt(emails = emails, orders = orders, avg_daily_sale = avg_daily_sale)

  emails$PRODUCT_CATEGORY = factor(emails$PRODUCT_CATEGORY)
  return(emails)
}
