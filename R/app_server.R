#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactiveValues showNotification
#'
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  rv <- reactiveValues(
    symbol        = NULL,
    stock_type    = NULL,
    stock_data    = NULL,
    tech_ind_data = NULL,
    arima_model   = NULL,
    ind_end_date = Sys.Date()
  )

  output$active_user <- renderUI({
    div(style = "margin-top: 15px; color:white", p(
      icon("user-check"),
      HTML("&nbsp"),
      tags$b(rv$user_login_data$user),
      HTML("&nbsp;"),
      HTML("&nbsp;")
    ))
  })

  observeEvent(input$run_analysis, {
    req(input$symbol)

    df <- suppressWarnings(
      get_data(symbol = input$symbol,
               type = input$type,
               start_date = input$dates[1],
               end_date = input$dates[2])
    )

    if ("message" %in% names(df)) {
      shiny::showNotification(df$message,
                              duration = NULL,
                              type = "error")
    } else {
      rv$symbol <- toupper(input$symbol)
      rv$stock_type <- input$type
      rv$stock_data <- df
      # rv$CSP_data <- candle_stick_patterns(df)
      rv$ind_start_date <- input$dates[2] - 300
      rv$ind_end_date <- input$dates[2]
      rv$ts_format_df <- ts_format_data(df)
      rv$arima_model <- NULL
    }
  })

  mod_stock_data_server("stock_data_1", rv)
  mod_candle_stick_patterns_server("candle_stick_patterns_1", rv)
  mod_technical_indicators_server("technical_indicators_1", rv)
  mod_all_tech_ind_server("all_tech_ind_1", rv)
  mod_arima_model_server("arima_model_1", rv)
}
