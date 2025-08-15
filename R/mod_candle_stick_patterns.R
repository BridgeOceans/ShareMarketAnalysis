#' candle_stick_patterns UI Function
#'
#' @description A shiny Module to display candle stick patterns.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tabPanel wellPanel downloadButton icon
#' @importFrom reactable reactableOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets materialSwitch
#' @importFrom plotly plotlyOutput
#'
#' @noRd
#'
mod_candle_stick_patterns_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    "Candle Stick Patterns",
    downloadButton(
      outputId = ns("download_candle_data"),
      label = "Download Data",
      icon = icon("download")
    ),
    wellPanel(
      shinyWidgets::radioGroupButtons(
        inputId = ns("period"),
        label = "Aggregation Period",
        choices = agg_periods,
        direction = "horizontal",
        justified = TRUE,
        status = "primary"
      ),
      fluidRow(
        shinydashboard::box(
          title = "Returns Plot",
          width = "100px",
          collapsible = TRUE,
          solidHeader = TRUE,
          status = "primary",
          collapsed = TRUE,
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("returns_plot"))
          )
        )
      ),
      fluidRow(
        div(
          style = "display: flex; align-items: center; column-gap: 10px",
          shiny::numericInput(inputId = ns("sma_days_1"),
                              label = "1st Simple MA Days",
                              value = 9,
                              min = 8,
                              width = "10%"),
          shiny::numericInput(inputId = ns("sma_days_2"),
                              label = "2nd Simple MA Days",
                              value = 20,
                              min = 8,
                              width = "10%"),
          shiny::numericInput(inputId = ns("ema_days_1"),
                              label = "1st Exponential MA Days",
                              value = 9,
                              min = 8,
                              width = "12%"),
          shiny::numericInput(inputId = ns("ema_days_2"),
                              label = "2nd Exponential MA Days",
                              value = 20,
                              min = 8,
                              width = "12%"),
          shinyWidgets::materialSwitch(inputId = ns("show_signals"),
                                       label = "Show Signals",
                                       value = FALSE,
                                       status = "primary"),
          shinyWidgets::materialSwitch(inputId = ns("show_candlestick"),
                                       label = "Candlestick Patterns",
                                       value = FALSE,
                                       status = "primary")
        ),
        shinycssloaders::withSpinner(
          reactable::reactableOutput(ns("candle_stick_patterns_data"))
        )
      )
    )
  )
}

#' candle_stick_patterns Server Functions
#' @importFrom shiny renderUI downloadHandler
#' @importFrom reactable renderReactable
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr select mutate_at mutate arrange desc
#' @importFrom magrittr  %>%
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom ggplot2 ggplot geom_line geom_point geom_hline labs
#' @importFrom ggplot2 aes theme theme_minimal element_text
#' @importFrom quantmod Lag
#' @importFrom stats na.omit
#' @importFrom utils head write.csv
#' @importFrom rlang .data
#'
#' @param rv reactive values
#'
#' @noRd
mod_candle_stick_patterns_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_csp <- reactive({
      req(rv$stock_data)
      data <- rv$stock_data
      if (input$period != "day") {
        data <- data %>%
          as.data.frame() %>%
          na.omit() %>%
          mutate_all(.funs = ~as.numeric(round(., 4))) %>%
          tibble::rownames_to_column("Date") %>%
          mutate(Date = as.Date(.data$Date))

        data <- aggregate_data(data, input$period)
      }

      csp_data <- candle_stick_patterns(data)
      csp_data$SMA_9_20 <- calculate_9_SMA_gt_20(csp_data)
      csp_data <- calculate_technical_indicators(csp_data)
      req(input$sma_days_1 >= 8, input$sma_days_2 >= 8)
      req(input$ema_days_1 >= 8, input$ema_days_2 >= 8)
      csp_data$SMA_1 <- calculate_SMA(csp_data, ma_days = as.numeric(input$sma_days_1))
      csp_data$SMA_2 <- calculate_SMA(csp_data, ma_days = as.numeric(input$sma_days_2))
      csp_data$EMA_1 <- calculate_EMA(csp_data, ma_days = as.numeric(input$ema_days_1))
      csp_data$EMA_2 <- calculate_EMA(csp_data, ma_days = as.numeric(input$ema_days_2))

      csp_data <-  csp_data %>%
        # as.data.frame() %>%
        # tibble::rownames_to_column(var = "Date") %>%
        # dplyr::arrange(desc(.data$Date)) %>%
        dplyr::mutate_at(.vars = c("Open", "High", "Low", "Close", "Adjusted"),
                         .funs = ~round(., 3))

      csp_data <- get_buy_signals(csp_data)

      csp_data <- csp_data %>%
        dplyr::select(c(.data$Date:.data$Volume, .data$buy_signals,
                        .data$`Returns %`,
                        .data$up_trend:.data$down_trend,
                        .data$SMA_9_20:.data$EMA_2,
                        .data$hammer:.data$dark_cloud_cover,
                        .data$bullish_engulfing:.data$falling_three))

      for (col in names(csp_data)) {
        if (col %in% bullish_signals) {
          if (col %in% c("up_trend", "gap_up")) {
            csp_data[[col]] <- ifelse(csp_data[[col]] == 1, "Yes", "")
          } else {
            csp_data[[col]] <- ifelse(csp_data[[col]] == 1, "Call", "")
          }
        } else if (col %in% bearish_signals) {
          if (col %in% c("down_trend", "gap_down") ) {
            csp_data[[col]] <- ifelse(csp_data[[col]] == 1, "Yes", "")
          } else {
            csp_data[[col]] <- ifelse(csp_data[[col]] == 1, "Put", "")
          }
        } else if (col == "doji") {
          csp_data[[col]] <- ifelse(csp_data[[col]] == 1, "In-Decision", "")
        }
      }
      rv$tech_ind_data = csp_data
      csp_data
    })

    output$returns_plot <- renderPlotly({
      req(rv$stock_data)
      nw_df <- data_csp() %>%
        select(.data$Date:.data$Volume, .data$`Returns %`) %>%
        # rename(Returns = `Returns %`) %>%
        filter(.data$`Returns %` != "NA %") %>%
        arrange(.data$Date) %>%
        mutate(Date = as.Date(.data$Date),
               PrevClose = quantmod::Lag(.data$Close),
               Returns = as.numeric(gsub(" %", "", .data$`Returns %`)),
               point_color = ifelse(.data$Returns > 0, "#0B6623", "#D21F3C"),
               label = sprintf("Date: %s \nOpen: %s \nClose: %s \nPrevClose: %s \nReturns: %s",
                               .data$Date, .data$Open, .data$Close, .data$PrevClose, .data$`Returns %`)) %>%
        arrange(desc(.data$Date)) %>%
        head(100)

      agg_period <- switch(
        input$period,
        "day"     = "Daily",
        "week"    = "Weekly",
        "month"   = "Monthly",
        "quarter" = "Quarterly",
        "year"    = "Yearly"
      )

      ggplot_obj <- ggplot(data = nw_df, aes(x = .data$Date, y = .data$Returns, label = .data$label)) +
        geom_line(color = "#0492C2", size = 0.7) +  # Line for returns
        geom_point(aes(fill = NULL), size = 1.5, color = nw_df$point_color, show.legend = FALSE) +
        # scale_color_manual(values = c("FALSE" = "#D21F3C", "TRUE" = "#0B6623")) +
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.8) +  # Horizontal line
        labs(
          title = paste0(agg_period, " Returns Over Time"),
          x = "Date",
          y = "Returns"
        ) +
        theme_minimal() +  # Clean theme
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5))

      plotly_obj <- ggplotly(ggplot_obj, tooltip = "label")
      plotly_obj
    })

    output$candle_stick_patterns_data <- reactable::renderReactable({
      req(data_csp())
      csp_data <- data_csp()
      if (isTRUE(input$show_signals) & isTRUE(input$show_candlestick)) {
        csp_data <- csp_data
      } else if (isTRUE(input$show_signals)) {
        csp_data <- data_csp() %>%
          select(!(.data$hammer:.data$falling_three))
      } else if (isTRUE(input$show_candlestick)) {
        csp_data <- data_csp() %>%
          select(!(.data$up_trend:.data$EMA_2))
      } else {
        csp_data <- data_csp() %>%
          select(!(.data$up_trend:.data$falling_three))
      }
      reactable_formatting(head(csp_data, 500),
                           sticky_cols = "Date",
                           stock_type = rv$stock_type,
                           minWidth = 120)
    })

    # Download the data
    output$download_candle_data <- downloadHandler(
      filename = function() {
        paste("Candle Stick Pattern Data", "csv", sep = ".")
      },
      content = function(file) {
        req(data_csp)
        write.csv(data_csp(), file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_candle_stick_patterns_ui("candle_stick_patterns_1")

## To be copied in the server
# mod_candle_stick_patterns_server("candle_stick_patterns_1")
