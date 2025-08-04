
origin <- lubridate::as_date("1900-01-01")

#' Create grouping column based on day/wee/month/quarter/year
#'
#' The function returns unique numbers of day/week/month/quarter taking into account the year.
#' The output can directly be used for creating aggregation groups.
#'
#' @param date_vec Dates vector
#' @rdname day_num
#' @importFrom assertthat assert_that is.date
#'
day_num <- function(date_vec){
  assert_that(is.date(date_vec),
              msg = "date_vec should be type date")
  as.integer(date_vec - origin)
}


#' @title Aggregate data on weekly basis
#' @rdname week_num
#' @param date_vec Dates vector
#' @importFrom assertthat assert_that is.date
week_num <- function(date_vec){
  assert_that(is.date(date_vec),
              msg = "date_vec should be type date")
  as.integer((day_num(date_vec)) %/% 7)
}



#' @title Aggregate data on monthly basis
#' @rdname month_num
#' @param date_vec Dates vector
#' @importFrom assertthat assert_that is.date
month_num <- function(date_vec){
  assert_that(is.date(date_vec),
              msg = "date_vec should be type date")
  lt <- as.POSIXlt(as.Date(date_vec, origin = origin))
  lt$year * 12 + lt$mon
}



#' @title Aggregate data on quarterly basis
#' @rdname quarter_num
#' @param date_vec Dates vector
#' @importFrom assertthat assert_that is.date
quarter_num <- function(date_vec){
  assert_that(is.date(date_vec),
              msg = "date_vec should be type date")
  as.integer(month_num(date_vec) %/% 3)
}

#' @title Aggregate data on yearly basis
#' @rdname day_num
#' @param date_vec Dates vector
#' @importFrom assertthat assert_that is.date
#' @importFrom lubridate year
#' @export
year_num <- function(date_vec) {
  assert_that(is.date(date_vec),
              msg = "date_vec should be type date")
  lubridate::year(date_vec)
}


#' @title Aggregate df with the series to different period lengths
#'
#' @param df data frame to aggregate
#' @param period period day/month
#'
#' @importFrom dplyr group_by summarise ungroup select first last
#' @importFrom tibble column_to_rownames
#' @importFrom xts as.xts
#' @importFrom stats quantile
#' @importFrom rlang .data
#'
#' @return df
#' @export
#'
aggregate_data <- function(df, period) {

  agg_fn <- switch (
    period,
    "week"    = week_num,
    "month"   = month_num,
    "quarter" = quarter_num,
    "year"    = year_num
  )

  df %>%
    dplyr::group_by(period = agg_fn(.data$Date)) %>%
    dplyr::summarise(Date     = first(.data$Date),
                     # End_Date = last(Date),
                     Open     = first(.data$Open, na_rm = TRUE),
                     High     = max(.data$High, na.rm = TRUE),
                     Low      = min(.data$Low, na.rm = TRUE),
                     Close    = last(.data$Close, na_rm = TRUE),
                     Volume   = round(quantile(.data$Volume, 0.75, na.rm = TRUE), 0),
                     Adjusted = last(.data$Adjusted, na_rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-period) %>%
    tibble::column_to_rownames(var = "Date") %>%
    xts::as.xts()
}
