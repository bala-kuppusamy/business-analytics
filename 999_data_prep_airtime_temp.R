
####------------------------------------------------------------

# product_list <- c('P150028635', 'P150025286', 'P150048858', 'P150000250')
product_list <- c('P150028635', 'P150025286', 'P150048858', 'P150000250', 'P150018581', 'P150039481', 'P150042231', 'P150066849', 'P150067048', 'P150065581', 'P150032693', 'P150064176', 'P150002209', 'P150048029', 'P150014482', 'P150028482', 'P150018378', 'P150014234', 'P150047468', 'P150063986', 'P150015381', 'P150047011', 'P150013511', 'P150015937', 'P150007559', 'P150012749', 'P150015454', 'P150032179', 'P150054068', 'P150005738', 'P150001552', 'P150014639', 'P150011250', 'P150025439', 'P150047250', 'P150033546', 'P150049474', 'P150065594', 'P150062192', 'P150008707', 'P150023592', 'P150016223', 'P150013568', 'P150033522', 'P150056473', 'P150009484', 'P150021308', 'P150056277', 'P150043193', 'P150066974')

orders_onair_min <- orders_onair %>%
  dplyr::filter(PRODUCT_NBR %in% product_list)

# orders_onair_matched <- orders_onair_min %>%
#   dplyr::left_join(airtime, by = 'PRODUCT_NBR', keep = TRUE) %>%
#   dplyr::filter(lubridate::`%within%`(ORDER_TIME, lubridate::interval(ONAIR_START_TMS, ONAIR_END_TMS)) == TRUE)
# # dplyr::filter(ORDER_TIME >= ONAIR_START_TMS && ORDER_TIME <= ONAIR_END_TMS)
# dplyr::glimpse(orders_onair_matched)

myf2 <- function(my_orders, my_airtime) {
  # glimpse(my_orders)
  cnt <<- cnt + 1
  if(cnt %% 100 == 0) {
    print(paste0(cnt , ' :: Processing Product #', my_orders[1, ]$PRODUCT_NBR))
  }
  orders_matched <- my_orders %>%
    dplyr::left_join(my_airtime, by = 'PRODUCT_NBR', keep = TRUE) %>%
    dplyr::filter(lubridate::`%within%`(ORDER_TIME, lubridate::interval(ONAIR_START_TMS, ONAIR_END_TMS)) == TRUE)
  return(orders_matched)
}

cnt <- 0
o2 <- orders_onair %>%
  split(.$PRODUCT_NBR) %>%
  purrr::map(.f = myf2, airtime) %>%
  dplyr::bind_rows()

dplyr::glimpse(o2)



myf3 <- function(my_airtime, my_orders) {
  # dplyr::glimpse(my_airtime)
  cnt3 <<- cnt3 + 1
  if(cnt3 %% 10 == 0) {
    print(paste(cnt3 , '=> Processing Airtime', sep = ' : '))
  }

  # order_interval <- lubridate::interval(my_airtime$ONAIR_START_TMS, my_airtime$ONAIR_END_TMS)

  matching_airtime <- my_airtime %>%
    dplyr::left_join(my_orders, by = c('PRODUCT_NBR' = 'PRODUCT_NBR', 'ONAIR_DATE' = 'ORDER_DATE'), keep = TRUE) %>%
    # dplyr::filter(PRODUCT_NBR == my_airtime$PRODUCT_NBR) %>%
    dplyr::filter(lubridate::`%within%`(ORDER_TIME, lubridate::interval(ONAIR_START_TMS, ONAIR_END_TMS)) == TRUE) %>%
    dplyr::group_by(ID) %>%
    # dplyr::group_by(ID, PRODUCT_NBR, ONAIR_DATE, ONAIR_START_TMS, ONAIR_END_TMS, ONAIR_MINS, HOST1, HOST2) %>%
    dplyr::summarise(SALE_AMT = sum(TOTAL_LINE_AMT))
  # dplyr::summarise(cnt = n(), total = sum(TOTAL_LINE_AMT))

  # my_airtime$SALE_AMT <- matching_orders$total
  # glimpse(matching_orders)
  return(matching_airtime)
}


# a2 <- airtime_min %>%
#   rowwise() %>%
#   purrr::map(.f = myf3, orders_onair) %>%
#   dplyr::bind_rows()

ap <- airtime %>%
  select(ONAIR_DATE) %>%
  distinct(ONAIR_DATE)
nrow(ap)

airtime_min <- airtime
# airtime_min <- head(airtime, 2000)
airtime_min <- tibble::rowid_to_column(airtime_min, "ID")
# airtime_min$HOST1 <- forcats::fct_explicit_na(airtime_min$HOST1)
# airtime_min$HOST2 <- forcats::fct_explicit_na(airtime_min$HOST2)

cnt3 <- 0
a2 <- airtime_min %>%
  split(.$ONAIR_DATE) %>%
  # dplyr::group_by(1:nrow(airtime_min)) %>%
  purrr::map(.f = myf3, orders_onair) %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(a2, by = 'ID') %>%
  dplyr::mutate(SALE_AMT = if_else(is.na(SALE_AMT) == TRUE, 0, SALE_AMT))
# dplyr::mutate(SALE_AMT = myf3(., orders_onair)) %>%
# dplyr::rbind_all()

dplyr::glimpse(a3)


#==============================================================================

airtime_min <- head(airtime, 2000)
airtime_min <- tibble::rowid_to_column(airtime_min, "ID")

gc()

airtime_tot <- airtime %>%
  dplyr::left_join(orders_onair, by = c('PRODUCT_NBR' = 'PRODUCT_NBR', 'ONAIR_DATE' = 'ORDER_DATE'), keep = TRUE) %>%
  dplyr::filter(lubridate::`%within%`(ORDER_TIME, lubridate::interval(ONAIR_START_TMS, ONAIR_END_TMS)) == TRUE) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(SALE_AMT = sum(TOTAL_LINE_AMT))

a4 <- airtime_min %>%
  dplyr::left_join(airtime_tot, by = 'ID')

# orders_onair_matched_2 <- orders_onair_matched %>%
#   dplyr::filter(lubridate::`%within%`(ORDER_TIME, lubridate::interval(ONAIR_START_TMS, ONAIR_END_TMS)) == TRUE)
#
# dplyr::glimpse(orders_onair_matched_2)


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
#   print(length(matched_airtime))
#
#   order$IS_ONAIR <- if_else(length(matched_airtime) > 0, TRUE, FALSE)
#
#   # dplyr::glimpse(matched_airtime)
#   return(order)
# }
#
# matched_airtimes <- apply(X = orders_onair_min, FUN = has_airtime, MARGIN = 1, airtime = airtime)
# matched_airtimes <- dplyr::as_tibble(matched_airtimes)
# dplyr::glimpse(matched_airtimes)
