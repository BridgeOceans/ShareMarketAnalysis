#' arima_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_arima_model_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    "ARIMA Model",
    uiOutput(ns("arima_model"))
  )
}

#' arima_model Server Functions
#'
#' @noRd
mod_arima_model_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$arima_model <- renderUI({
      fluidPage(
        fluidRow(
          div(
            style = "display: flex; align-items: center; column-gap: 10px",

            actionButton(inputId = ns("run_arima"),
                         label = "Run Model",
                         icon = icon("refresh")),

            selectInput(inputId = ns("months_back"),
                        label = "Select data up to Month",
                        choices = c(0, 1:12),
                        selected = 2,
                        multiple = FALSE,
                        width = "100px"),

            numericInput(inputId = ns("predicted_months"),
                         label = "Predict for Future Months",
                         value = 10,
                         min = 1,
                         max = 5*12,
                         width = "100px")
          )
        ),

        shiny::wellPanel(
          fluidRow(
            column(width = 8,
                   plotOutput(ns("arima_prediction"))),
            column(width = 4,
                   reactable::reactableOutput(ns("predicted_time_series")))
          )
        ),
        shiny::wellPanel(
          fluidRow(
            plotOutput(ns("residual_plot"))
          ),
          fluidRow(
            column(width = 6,
                   fluidRow(verbatimTextOutput(ns("model_summary")))),
            column(
              width = 6,
              fluidRow(verbatimTextOutput(ns("residual_test"))),
              fluidRow(verbatimTextOutput(ns("adf_test"))),
              fluidRow(verbatimTextOutput(ns("pp_test")))
            )
          )
        )
      )
    })

    observeEvent(input$run_arima, {
      req(rv$stock_data)
      months_back <- as.numeric(input$months_back)
      df_ts <- ts_format_data(rv$stock_data, months_back = months_back)
      rv$ts_format_df <- df_ts
      rv$arima_model <- forecast::auto.arima(df_ts)
    })

    output$arima_prediction <- renderPlot({
      req(rv$arima_model)
      predicted_series <- forecast::forecast(rv$arima_model,
                                             level = c(95),
                                             h = input$predicted_months)
      rv$predicted_series <- predicted_series
      forecast::autoplot(predicted_series)
    })

    output$predicted_time_series <- reactable::renderReactable({
      req(rv$arima_model)
      predicted_series <- rv$predicted_series %>%
        as.data.frame() %>%
        mutate_all(.funs = ~round(., 2)) %>%
        tibble::rownames_to_column(var = "Date")

      reactable::reactable(predicted_series,
                           pagination = TRUE,
                           showPagination = TRUE)
    })

    output$residual_plot <- renderPlot({
      req(rv$arima_model)
      forecast::checkresiduals(rv$arima_model)
    })

    output$model_summary <- renderPrint({
      req(rv$arima_model)
      summary(rv$arima_model)
    })

    output$residual_test <- renderPrint({
      req(rv$arima_model)
      suppressWarnings(
        stats::Box.test(rv$arima_model$residuals, type = "Ljung-Box")
      )
    })

    output$adf_test <- renderPrint({
      req(rv$arima_model)
      suppressWarnings(
        tseries::adf.test(rv$arima_model$residuals)
      )
    })

    output$pp_test <- renderPrint({
      req(rv$arima_model)
      suppressWarnings(
        tseries::pp.test(rv$arima_model$residuals)
      )
    })

  })
}

## To be copied in the UI
# mod_arima_model_ui("arima_model_1")

## To be copied in the server
# mod_arima_model_server("arima_model_1")
