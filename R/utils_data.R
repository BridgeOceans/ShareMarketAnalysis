simple_returns <- function(price) paste(round((price / dplyr::lag(price) - 1.0) * 100, 2), "%")

#' Get the data stock from yahoo!
#'
#' @param symbol symbol of the stock
#' @param type NSE or BSE
#' @param start_date Start Date
#' @param end_date End Date
#'
#' @importFrom quantmod getSymbols
#' @importFrom stringr str_replace_all
#' @importFrom stats na.omit
#'
#' @return Returns a data of selected stock
#' @export
#'
get_data <- function(symbol,
                     type,
                     start_date,
                     end_date) {

  tryCatch({
    symbol <- toupper(symbol)
    stock_name <- switch(type,
                         "NSE" = paste(symbol, "NS", sep = "."),
                         "NYSE" = symbol)

    if (symbol %in% c("NIFTY", "BANKNIFTY")) {
      symbol <- switch(symbol,
                       "NIFTY" = "NSEI",
                       "BANKNIFTY" = "NSEBANK")

      df <- getSymbols(
        paste0("^", symbol),
        src = "yahoo",
        from = start_date,
        to = end_date + 1,
        auto.assign = FALSE
      )

      names(df) <- stringr::str_replace_all(names(df),
                                            pattern = paste0(symbol, "."),
                                            replacement = "")
    } else {
      df <- getSymbols(
        stock_name,
        src = "yahoo",
        from = start_date,
        to = end_date + 1,
        auto.assign = FALSE
      )
      names(df) <- stringr::str_replace_all(names(df),
                                            pattern = paste0(stock_name, "."),
                                            replacement = "")
    }

    df <- df[df$Volume != 0, ]

    na.omit(df)
  },
  error = function(e) {
    return(e)
  })
}


#' Add Patterns in the table
#'
#' @param Trend What is the trend currently
#' @param Type Type of pattern name
#' @param Open Open Price
#' @param High Close Price
#' @param Low Low Price
#' @param Close Close Price
#'
#' @return a table with pattern
#' @export
#'
add_patterns <- function(Trend, Type, Open, High, Low, Close) {
  # Calculate body size and shadow sizes
  body_size <- abs(Close - Open)
  upper_shadow <- High - pmax(Open, Close)
  lower_shadow <- pmin(Open, Close) - Low

  if (Type == "Inv_Hammer") {
    cond <- Trend &&
      body_size < (0.3 * (High - Low)) &&   # Small real body
      upper_shadow <= (3.55 * body_size) &&   # Long upper shadow
      upper_shadow >= (1.98 * body_size) &&   # Long upper shadow
      lower_shadow <= (0.2 * (High - Low)) # Small lower shadow
  } else if (Type == "Hammer") {
    cond <- Trend &&
      body_size < (0.3 * (High - Low)) &&
      lower_shadow <= (3.5 * body_size) &&
      lower_shadow >= (2 * body_size) &&
      upper_shadow <= (0.15 * (High - Low))
  } else if (Type == "Hanging_Man") {
    cond <- Trend &&
      body_size < (0.31 * (High - Low)) &&
      # lower_shadow <= (3.5 * body_size) &&
      lower_shadow > (2 * body_size) &&
      upper_shadow <= (0.15 * (High - Low))
  } else if (Type == "Shooting_Star") {
    cond <- Trend &&
      body_size < (0.3 * (High - Low)) &&
      # upper_shadow <= (4 * body_size) &&
      upper_shadow >= (2 * body_size) &&
      lower_shadow <= (0.135 * (High - Low))
  }

  return(ifelse(cond, 1, 0))
}


#' Function to detect specific patterns
#'
#' @param Trend Market trend
#' @param Type Type of candlestick pattern
#' @param Open1 Open price of first candle
#' @param Close1 Close price of first candle
#' @param High1 High price of first candle
#' @param Low1 Low price of first candle
#' @param Open2 Open price of second candle
#' @param Close2 Close price of second candle
#' @param High2 High price of second candle
#' @param Low2 Low price of second candle
#'
#' @return A data frame with patterns
#' @export
#'
detect_patterns <- function(Trend,
                            Type,
                            Open1,
                            Close1,
                            High1,
                            Low1,
                            Open2,
                            Close2,
                            High2,
                            Low2) {

  if (Type == "bearish_engulfing") {
    cond <- Trend &&
      Open1 < Close1 && Open2 > Close2 &&
      Open2 > Close1 && Close2 < Open1 &&
      Low2 <= Low1 && High2 >= High1

  } else if (Type == "bullish_engulfing") {
    cond <- Trend &&
      Open1 > Close1 && Open2 < Close2 &&
      Open2 < Close1 && Close2 > Open1 &&
      Low2 <= Low1 && High2 >= High1

  } else if (Type == "bearish_harami") {
    cond <- Trend &&
      Open1 < Close1 && Open2 > Close2 &&
      Open1 < Close2 &&
      (Close1 > Open2|| assertthat::are_equal(Close1, Open2, tol = 0.004)) &&
      Low1 <= Low2 && High1 >= High2

  } else if (Type == "bullish_harami") {
    cond <- Trend &&
      Open1 > Close1 && Open2 < Close2 &&
      Open1 > Close2 && Close1 < Open2 &&
      Low1 <= Low2 && High1 >= High2

  } else if (Type == "dark_cloud_cover") {

    closes_below_midpoint <- (Close2 <= ((Open1 + Close1) / 1.99)) &&
      # Low2 > Open1 &&
      (Close2 - Open1 > 1)

    cond <- Trend &&
      Close1 < Open2 && Close1 > Close2 &&
      (High1 <= max(Open2, High2) ||
         assertthat::are_equal(High1, pmax(Open2, High2), tol = 0.009)) &&
      closes_below_midpoint
  }

  return(ifelse(cond, 1, 0))
}


#' Function to compute LuxAlgo Nadaraya-Watson Envelope and generate signals
#'
#' @param price close price of the stock
#' @param bandwidth set bandwidth
#' @param alpha set the alpha level
#'
#' @importFrom stats ksmooth
#'
#' @return returns a signals whether to buy or sell
#' @export
#'
nadaraya_watson_envelope <- function(price,
                                     bandwidth = 8,
                                     alpha = 0.02) {
  # Kernel Regression using Nadaraya-Watson
  smoothed <- ksmooth(1:length(price), price, kernel = "normal", bandwidth = bandwidth)$y

  # Calculate the envelope bands
  upper_band <- smoothed * (1 + alpha)
  lower_band <- smoothed * (1 - alpha)

  # Generate buy and sell signals
  signals <- c(NA)  # Placeholder for signals
  for (i in 2:length(price)) {
    if (price[i] > upper_band[i] && price[i - 1] <= upper_band[i - 1]) {
      signals[i] <- "Sell"
    } else if (price[i] < lower_band[i] && price[i - 1] >= lower_band[i - 1]) {
      signals[i] <- "Buy"
    } else {
      signals[i] <- ""
    }
  }

  return(list(signals = signals,
              upper_band = upper_band,
              lower_band = lower_band))
}

#' Add candle_stick_patterns in dataframe
#'
#' @param df data to be passed
#'
#' @import CandleStickPattern
#' @importFrom purrr map2_lgl
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate mutate_all filter
#'
#' @description A utils function
#'
#' @return Return a dataframe with adding different candle stick patterns as a variables
#' @noRd
candle_stick_patterns <- function(df) {
  df$up_trend             <- up.trend(df, L = 15)
  df$down_trend           <- down.trend(df, L = 15)
  df$gap_up               <- gap.up(df)
  df$gap_down             <- gap.down(df)
  # df$doji                 <- doji(df)
  # df$dragon_fly_doji      <- dragonfly.doji(df)
  # df$gravestone_doji      <- gravestone.doji(df)
  df$bullish_engulfing    <- ifelse(df$down_trend == 1,
                                    bullish.engulf(df),
                                    0)
  df$bearish_engulfing    <- ifelse(df$up_trend == 1,
                                    bearish.engulf(df),
                                    0)
  df$bullish_harami       <- ifelse(df$down_trend == 1,
                                    bullish.harami(df),
                                    0)
  df$bearish_harami       <- ifelse(df$up_trend == 1,
                                    bearish.harami(df),
                                    0)
  # # df$piercing_line        <- piercing.line(df)
  # df$dark_cloud_cover     <- dark.cloud.cover(df)
  df$kick_up              <- kick.up(df)
  df$kick_down            <- kick.down(df)
  df$three_white_soldiers <- three.white.soldiers(df)
  df$three_black_crows    <- three.black.crows(df)
  df$morning_star         <- morning.star(df)
  df$evening_star         <- evening.star(df)
  df$rising_three         <- rising.three(df)
  df$falling_three        <- falling.three(df)
  df$hammer               <- ifelse(df$down_trend == 1,
                                    hammer(df),
                                    0)
  df$inverted_hammer      <- ifelse(df$down_trend == 1,
                                    inverted.hammer(df),
                                    0)
  df$hanging_man          <- ifelse(df$up_trend == 1,
                                    hammer(df),
                                    0)
  df$shooting_star        <- ifelse(df$up_trend == 1,
                                    inverted.hammer(df),
                                    0)

  df <- df %>%
    as.data.frame() %>%
    # na.omit() %>%
    mutate_all(.funs = ~as.numeric(round(., 4))) %>%
    mutate(`Returns %` = simple_returns(.data$Close)) %>%
    tibble::rownames_to_column(var = "Date") %>%
    filter(.data$Volume != 0)

  # Dark Cloud Cover
  df$dark_cloud_cover <- c(FALSE, map2_lgl(
    seq_len(nrow(df) - 1), seq_len(nrow(df))[-1],
    function(i, j) {
      detect_patterns(
        Trend  = df$up_trend[j],
        Type = "dark_cloud_cover",
        Open1  = as.numeric(df$Open[i]),
        Close1 = as.numeric(df$Close[i]),
        High1  = as.numeric(df$High[i]),
        Low1   = as.numeric(df$Low[i]),
        Open2  = as.numeric(df$Open[j]),
        Close2 = as.numeric(df$Close[j]),
        High2  = as.numeric(df$High[j]),
        Low2   = as.numeric(df$Low[j])
      )
    }
  ))

  # df$signals <- nadaraya_watson_envelope(df$Close)[["signals"]]

  return(df)
}


#' Function to convert data in TS format
#'
#' @param df data frame
#' @param months_back months to look back
#'
#' @importFrom lubridate year month floor_date
#' @importFrom dplyr select mutate filter row_number group_by last
#' @importFrom tibble rownames_to_column
#' @importFrom stats ts
#' @importFrom rlang .data
#'
#'
#' @return return a time series data
#' @export
#'
ts_format_data <- function(df, months_back = 0) {
  df <- df %>%
    as.data.frame() %>%
    rownames_to_column(var = "Date") %>%
    mutate(Date = as.Date(.data$Date)) %>%
    mutate(Date = lubridate::floor_date(.data$Date, unit = "month")) %>%
    select(.data$Date, .data$Close) %>%
    group_by(.data$Date) %>%
    filter(row_number() == last(row_number()))

  min_date <- c(year(min(df$Date)), lubridate::month(min(df$Date)))
  max_date <- c(year(max(df$Date)), lubridate::month(max(df$Date))-months_back)

  df_ts <- stats::ts(as.vector(df$Close),
                     start = min_date,
                     end = max_date,
                     frequency = 12)

  return(df_ts)
}

#' Chaikin Volatility
#'
#' @param df stock data
#'
#' @importFrom quantmod Hi Lo Lag
#' @importFrom TTR EMA
#'
#' @return TRUE/FALSE
#' @export
#'
chaikin_volatility <- function(df) {
  # Calculate High-Low Range
  hl_range <- Hi(df) - Lo(df)

  # Calculate 10-period EMA of High-Low Range
  ema_hl <- EMA(hl_range, n = 10)

  # Shift the EMA by 3 periods to compare
  ema_hl_shifted <- Lag(ema_hl, 3)

  # Chaikin Volatility Calculation
  ch_voltlty <- (ema_hl / ema_hl_shifted - 1) * 100
  df$ch_vol <- round(as.numeric(ch_voltlty), 2)

  # Signal for Entry and Exit Points
  df$signal <- as.character(ifelse(ch_voltlty > 10, "BUY",
                                   ifelse(ch_voltlty < -10, "SELL", "")))

  return(df)
}

#' Calculate Simple Moving Average
#'
#' @param df data frame
#' @param ma_days number of days
#'
#' @importFrom TTR SMA
#'
#' @return Simple Moving Average
#' @export
#'
calculate_SMA <- function(df, ma_days = 9) {
  SMA_Val <- TTR::SMA(df$Close, ma_days)
  SMA_signal <- ifelse(
    SMA_Val < df$Close,
    glue("Above"), ""
  )

  return(SMA_signal)
}

#' Calculate Exponential Moving Average
#'
#' @param df data frame
#' @param ma_days number of days
#'
#' @return Exponential Moving Average
#' @export
#'
calculate_EMA <- function(df, ma_days = 9) {
  EMA_Val <- TTR::EMA(df$Close, ma_days)
  EMA_signal <- ifelse(
    EMA_Val < df$Close,
    glue("Above"), ""
  )

  return(EMA_signal)
}

#' Calculate is 9 SMA greater than 20 SMA
#'
#' @param df data frame
#' @importFrom TTR SMA
#'
#' @return return TRUE if 9 SMA is greater than 20 SMA else FALSE
#' @export
#'
calculate_9_SMA_gt_20 <- function(df) {
  SMA_9 <- TTR::SMA(df$Close, 9)
  SMA_20 <- TTR::SMA(df$Close, 20)

  calc_SMA_9 <- calculate_SMA(df, 9)
  calc_SMA_20 <- calculate_SMA(df, 20)

  SMA_9_above_SMA_20 <- ifelse((SMA_9 > SMA_20),
                               "Above",
                               "")

  return(SMA_9_above_SMA_20)
}

#' Calculate Technical Indicators
#'
#' @param df data frame
#'
#' @importFrom TTR MACD CCI SMI CMO
#' @importFrom dplyr lag
#' @importFrom magrittr %>%
#'
#' @return return different technical indicator signals
#' @export
#'
calculate_technical_indicators <- function(df) {
  macd_values <- TTR::MACD(df$Close) %>% as.data.frame()

  macd_values$macd_signal <-
    ifelse(
      (
        macd_values$macd >= -40 &
          macd_values$signal >= -40 &
          macd_values$macd > macd_values$signal &
          macd_values$macd > lag(macd_values$macd)
      ),
      "Above",
      ifelse((
        (
          macd_values$macd < macd_values$signal &
            macd_values$macd < lag(macd_values$macd)
        )
      ),
      "Below",
      "")
    )

  df$macd_signal <- macd_values$macd_signal

  # Stochastic Momentum Index signal
  HLC_matrix <- as.matrix(df[, c("High", "Low", "Close")])
  smi_values <- TTR::SMI(HLC_matrix) %>% as.data.frame()
  smi_values$smi_signal <-
    ifelse(smi_values$SMI > smi_values$signal & smi_values$SMI > lag(smi_values$SMI),
           "Above",
           ifelse(smi_values$SMI < smi_values$signal & smi_values$SMI < lag(smi_values$SMI),
                  "Below", ""))

  df$smi_signal <- smi_values$smi_signal

  # Chande Momentum Oscillator signal
  cmo_values <- TTR::CMO(df$Close)
  df$cmo_signal <-
    ifelse((cmo_values > 0 & cmo_values > lag(cmo_values)),
           "Above",
           ifelse((cmo_values < 0 & cmo_values < lag(cmo_values)),
                  "Below", ""))

  # Calculate Commodity Channel Index (14-period default)
  CCI <- TTR::CCI(HLC_matrix, n = 18)

  # Generate CCI signal: Crossing the 0 line
  df$cci_signal <-
    ifelse(CCI > 0 & CCI > lag(CCI),
           "Above",
           ifelse(CCI < 0 & CCI < lag(CCI),
                  "Below", ""))

  return(df)
}

#' Calculate the Buy signals
#'
#' @param df data frame
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
#'
#' @return data frame with Buy Signals
#' @export
#'
get_buy_signals <- function(df) {

  common_cond <-
    df$macd_signal == "Above" &
    lag(df$macd_signal, 1L) == "Above" &
    df$down_trend == 0

  cond_on_radar <-
    (lag(df$down_trend, 1L) & lag(df$down_trend, 2L)) &
    df$macd_signal == "Above" &
    (df$smi_signal == "Above" |
       df$cmo_signal == "Above" |
       df$cci_signal == "Above") &
    (df$SMA_1 == "Above" | df$EMA_1 == "Above" |
       df$SMA_2 == "Above" | df$EMA_2 == "Above")

  cond_first_beep <-
    df$macd_signal == "Above" &
    (lag(df$macd_signal, 1L) == "Above" | df$SMA_1 == "Above" | df$EMA_1 == "Above") &
    df$down_trend == 0 &
    (lag(df$down_trend, 2L) == 1 | lag(df$down_trend, 3L) == 1)

  cond_second_beep <-
    common_cond &
    (df$smi_signal == "Above" | df$cmo_signal == "Above" | df$cci_signal == "Above") &
    (df$SMA_2 == "Above") &
    ((lag(df$down_trend, 3L) == 1 | lag(df$down_trend, 4L) == 1) &
       lag(df$down_trend, 5L) == 1 & lag(df$down_trend, 6L) == 1 &
       lag(df$down_trend, 7L) == 1 & lag(df$down_trend, 8L) == 1)

  cond_buy <-
    common_cond &
    df$smi_signal == "Above" &
    df$cmo_signal == "Above" &
    df$cci_signal == "Above" &
    df$SMA_1 == "Above" &
    df$SMA_2 == "Above" &
    df$EMA_1 == "Above" &
    df$EMA_2 == "Above" &
    ((lag(df$down_trend, 4L) == 1 | lag(df$down_trend, 5L) == 1) &
       lag(df$down_trend, 6L) == 1 & lag(df$down_trend, 7L) == 1 &
       lag(df$down_trend, 8L) == 1 & lag(df$down_trend, 9L) == 1)


  cond_strong_buy <-
    common_cond &
    df$smi_signal == "Above" &
    df$cmo_signal == "Above" &
    df$cci_signal == "Above" &
    df$SMA_1 == "Above" &
    df$SMA_2 == "Above" &
    df$EMA_1 == "Above" &
    df$EMA_2 == "Above" &
    ((lag(df$down_trend, 1L) == 0 & lag(df$up_trend, 1L) == 0 &
        (df$SMA_9_20 == "Above" | df$up_trend == 1)) |
       (df$up_trend & (lag(df$down_trend, 5L) == 1 | lag(df$down_trend, 6L) == 1) &
          lag(df$down_trend, 7L) == 1 & lag(df$down_trend, 8L) == 1 &
          lag(df$down_trend, 9L) == 1 & lag(df$down_trend, 10L) == 1))


  df <- df %>%
    mutate(
      buy_signals = case_when(
        # cond_extremely_strong_buy ~ "Extremely Strong Buy",
        cond_strong_buy ~ "Strong Buy",
        cond_buy ~ "Buy",
        cond_second_beep ~ "Second Beep",
        cond_first_beep ~ "First Beep",
        cond_on_radar ~ "On Radar",
        TRUE ~ ""
      )
    ) %>%
    arrange(desc(.data$Date))

  return(df)
}
