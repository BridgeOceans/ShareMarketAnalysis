#' single_plot UI Function to plot the indicator chart
#'
#' @param indicator indicator type
#' @param start_date start date
#' @param end_date end date
#' @param n1_period First Period window
#' @param n2_period Second Period window
#' @param col_1 First indicator color
#' @param col_2 Second indicator color
#' @param wilder wilder
#' @param maType Moving Avg Type
#'
#' @importFrom shiny NS tagList fluidRow column dateInput numericInput selectInput plotOutput
#' @importFrom shinyWidgets dropdownButton tooltipOptions radioGroupButtons
#' @importFrom colourpicker colourInput
#' @importFrom shinycssloaders withSpinner
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_single_plot_ui <- function(id,
                               indicator = "RSI",
                               start_date = Sys.Date() - 200,
                               end_date = Sys.Date(),
                               n1_period = 14,
                               n2_period = 0,
                               col_1 = "#0000FF",
                               col_2 = "#FF0505",
                               wilder = FALSE,
                               maType = "EMA") {
  ns <- NS(id)
  tagList(

    div(
      style = "display: flex; align-items: center; column-gap: 10px",
      shinyWidgets::dropdownButton(
        circle = TRUE,
        status = "danger",
        icon = icon("cog"),
        width = "300px",
        size = "sm",
        tooltip = tooltipOptions(title = "Set Indicator Inputs"),

        tags$h3("Set Inputs"),

        {
          fluidPage(
            shiny::fluidRow(
              column(6, shiny::dateInput(
                inputId = ns("start_date"),
                label = "Start Date",
                value = start_date,
                daysofweekdisabled = c(0, 6)
              )),
              column(6, shiny::dateInput(
                inputId = ns("end_date"),
                label = "End Date",
                value = end_date,
                daysofweekdisabled = c(0, 6)
              ))
            )
          )
        },

        if (indicator == "RSI") {

          fluidPage(
            fluidRow(
              column(6, shiny::numericInput(
                inputId = ns("n1_period"),
                label = "First Period Window",
                value = n1_period,
                min = 7
              )),
              column(6, shiny::numericInput(
                inputId = ns("n2_period"),
                label = "Second Period Window",
                value = n2_period,
                min = 7
              ))
            ),
            fluidRow(
              column(6, shiny::selectInput(
                inputId = ns("maType"),
                label = "Mvg Avg Type",
                choices = c("SMA", "EMA"),
                selected = "EMA"
              )),
              column(6, checkboxInput(
                inputId = ns("wilder"),
                label   = "Wilder",
                value   = wilder
              ))
            )
          )
        } else if (indicator == "EMA") {

          fluidPage(
            fluidRow(
              column(4, shiny::numericInput(
                inputId = ns("n1_period"),
                label = "First Period Window",
                value = n1_period,
                min = 5
              )),
              column(4, shiny::numericInput(
                inputId = ns("n2_period"),
                label = "Second Period Window",
                value = n2_period,
                min = 5
              )),
              column(4, selectInput(
                inputId = ns("ma_type"),
                label   = "MA Type",
                choices = c("SMA", "EMA"),
                selected = "SMA"
              ))
            ),
            fluidRow(
              column(4, colourpicker::colourInput(
                inputId = ns("col_1"),
                label = "First Period",
                value = col_1
              )),
              column(4, colourpicker::colourInput(
                inputId = ns("col_2"),
                label = "Second Period",
                value = col_2
              )),
              column(4, checkboxInput(
                inputId = ns("wilder"),
                label   = "Wilder",
                value   = wilder
              ))
            )
          )
        } else if (indicator == "DEMA") {

          fluidRow(
            column(4, shiny::numericInput(
              inputId = ns("n1_period"),
              label = "Period Window",
              value = n1_period,
              min = 5
            )),
            column(4, colourpicker::colourInput(
              inputId = ns("col_1"),
              label = "EMA",
              value = col_1
            )),
            column(4, colourpicker::colourInput(
              inputId = ns("col_2"),
              label = "DEMA",
              value = col_2
            ))
          )

        } else if (indicator == "MACD") {
          fluidRow(
            column(4, shiny::numericInput(
              inputId = ns("fast"),
              label = "Fast",
              value = 12,
              min = 1
            )),
            column(4, shiny::numericInput(
              inputId = ns("slow"),
              label = "Slow",
              value = 26,
              min = 10
            )),
            column(4, shiny::numericInput(
              inputId = ns("signal"),
              label = "Signal",
              value = 9,
              min = 1
            ))
          )
        } else if (indicator %in% c("BBands", "ATR", "KST", "ADX", "CCI")) {
          fluidRow(
            column(4, shiny::numericInput(
              inputId = ns("n1_period"),
              label = "Period Window",
              value = n1_period,
              min = 1
            )),
            column(4, shiny::selectInput(
              inputId = ns("maType"),
              label = "Mvg Avg Type",
              choices = c("SMA", "EMA"),
              selected = maType
            )),
            {
              if (indicator == "BBands") {
                column(4, shiny::numericInput(
                  inputId = ns("sd"),
                  label = "SD",
                  value = 2,
                  min = 0
                ))
              }
            }
          )
        } else if (indicator %in% c("Aroon", "SMI_CMO")) {
          fluidRow(
            column(6, shiny::numericInput(
              inputId = ns("n1_period"),
              label = "Period Window",
              value = n1_period,
              min = 10
            ))
          )
        }
      ),
      actionButton(ns("show_info"), paste0(indicator, " Info")),
      shinyWidgets::radioGroupButtons(
        inputId = ns("agg_period"),
        label = "Aggregation Period",
        choices = agg_periods[1:3],
        selected = agg_periods[2],
        direction = "horizontal",
        justified = TRUE,
        status = "primary"
      )
    ),
    shinycssloaders::withSpinner(
      shiny::plotOutput(
        outputId = ns("indicator_chart"),
        height = "500px"
      )
    )
  )
}

#' single_plot Server Functions to plot indicator chart
#' @param rv reactive values
#' @param indicator indicator type
#' @param plot_title plot title
#'
#' @importFrom shiny reactive renderPlot
#' @importFrom quantmod chartSeries addRSI addSAR addVo addEMA addDEMA addChVol
#' @importFrom quantmod addMACD addBBands addATR addKST addADX addCCI addCMF
#' @importFrom glue glue
#' @importFrom zoo index
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @importFrom rlang .data
#'
#' @noRd
mod_single_plot_server <- function(id,
                                   rv,
                                   indicator,
                                   plot_title = "Stock Data Plot"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    chart_indicator <- reactive({
      req(!is.null(rv$stock_data))  # Ensure stock_data is available

      # Subset the stock_data based on selected date range
      stock_data <- rv$stock_data
      if (input$agg_period != "day") {
        stock_data <- stock_data %>%
          as.data.frame() %>%
          na.omit() %>%
          mutate_all(.funs = ~as.numeric(round(., 4))) %>%
          tibble::rownames_to_column("Date") %>%
          mutate(Date = as.Date(.data$Date))

        stock_data <- aggregate_data(stock_data, input$agg_period)

        stock_data_filtered <- tail(stock_data, 200)

      } else {
        stock_data_filtered <- stock_data[index(stock_data) >= as.Date(input$start_date) &
                                            index(stock_data) <= as.Date(input$end_date)]
      }

      # Apply the indicator selected
      switch(
        indicator,
        "RSI" = {

          RSI_1 <- glue::glue("addRSI(n = ", input$n1_period, ", maType = '", input$maType, "', wilder = ", input$wilder,")")
          TA_list <- paste(c(RSI_1), collapse = ";")

          if (input$n2_period != 0) {
            RSI_2 <- glue::glue("addRSI(n = ", input$n2_period, ", maType = '", input$maType, "', wilder = ", input$wilder,")")
            TA_list <- paste(c(RSI_1, RSI_2), collapse = ";")
          }
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "EMA" = {

          # Add the EMA indicator with specified colors for each period
          if (input$ma_type == "SMA") {
            MA_1 <- glue::glue("addSMA(n = ", input$n1_period, ", col = '", input$col_1, "')" )
          } else {
            MA_1 <- glue::glue("addEMA(n = ", input$n1_period, ", col = '", input$col_1, "', wilder = ", input$wilder,")" )
          }

          TA_list <- MA_1

          if (input$n2_period != 0) {
            if (input$ma_type == "SMA") {
              MA_2 <- glue::glue("addSMA(n = ", input$n2_period, ", col = '", input$col_2, "')" )
            } else {
              MA_2 <- glue::glue("addEMA(n = ", input$n2_period, ", col = '", input$col_2, "', wilder = ", input$wilder,")" )
            }
            TA_list <- paste(c(MA_1, MA_2), collapse = ";")
          }

          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "DEMA" = {

          # Add the EMA and DEMA indicator with specified colors for each period
          EMA <- glue::glue("addEMA(n = ", input$n1_period, ", col = '", input$col_1, "')" )
          DEMA <- glue::glue("addDEMA(n = ", input$n1_period, ", col = '", input$col_2, "')" )
          TA_list <- paste(c(EMA, DEMA), collapse = ";")

          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "MACD" = {

          # Add the MACD indicator
          MACD <- glue::glue("addMACD(fast = ", input$fast, ", slow = ", input$slow, ", signal = ", input$signal,")" )
          TA_list <- paste(c(MACD, "addChVol()"), collapse = ";")
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "BBands" = {

          # Add the Bollinger Bands (BBands) indicator
          BBands <- glue::glue("addBBands(n = ", input$n1_period, ", sd = ", input$sd, ", maType = '", input$maType, "')" )
          TA_list <- paste(c("addVo()", BBands), collapse = ";")
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "ATR" = {

          # Add the Average True Range (ATR) indicator
          ATR <- glue::glue("addATR(n = ", input$n1_period, ", maType = '", input$maType, "')" )
          TA_list <- paste(c(ATR), collapse = ";")
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "KST" = {

          # Add the Know Sure Thing (KST) indicator
          KST <- glue::glue("addKST(nSig = ", input$n1_period, ", maType = '", input$maType, "')" )
          TA_list <- paste(c(KST), collapse = ";")
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "SAR" = {

          #  Parabolic Stop and Reverse (SAR) indicator
          SAR <- glue::glue("addSAR()")
          TA_list <- paste(c("addVo()", SAR), collapse = ";")
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "ADX" = {

          #  Add Directional Movement Index (ADX) indicator
          # ADX <- glue::glue("addADX(n = ", input$n1_period, ", maType = '", input$maType, "')" )
          TA_list <- glue::glue("addADX(n = ", input$n1_period, ", maType = '", input$maType, "')" )
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "CCI" = {

          #  Add Commodity Channel Index (CCI) indicator
          CCI <- glue::glue("addCCI(n = ", input$n1_period, ", maType = '", input$maType, "')" )
          CMF <- glue::glue("addCMF(n = ", input$n1_period, ")" )
          # TA_list <- glue::glue("addCCI(n = ", input$n1_period, ", maType = '", input$maType, "')" )
          TA_list <- paste(c(CCI, CMF), collapse = ";")
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "Aroon" = {

          #  Add Commodity Channel Index (CCI) indicator
          Aroon <- glue::glue("addAroon(n = ", input$n1_period, ", col = c('green', 'red'))" )
          AroonOsc <- glue::glue("addAroonOsc(n = ", input$n1_period, ")" )
          TA_list <- paste(c(Aroon, AroonOsc), collapse = ";")
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        "SMI_CMO" = {

          #  Add Commodity Channel Index (CCI) indicator
          SMI_ind <- "addSMI()"
          CMO_ind <- glue::glue("addCMO(n = ", input$n1_period, ")" )
          TA_list <- paste(c(SMI_ind, CMO_ind), collapse = ";")
          quantmod::chartSeries(stock_data_filtered, theme = "white",
                                name = plot_title, TA = TA_list)
        },

        {
          warning("Unknown indicator: ", indicator)  # Fallback for unknown indicators
        }
      )
    })

    output$indicator_chart <- renderPlot({
      req(chart_indicator())
      chart_indicator()
    })

    observeEvent(input$show_info, {

      indicator_info <- switch (
        indicator,
        "RSI"    = RSI_info,
        "EMA"    = EMA_info,
        "DEMA"   = DEMA_info,
        "MACD"   = MACD_CV_info,
        "BBands" = BBands_info,
        "ATR"    = ATR_info,
        "KST"    = KST_info,
        "SAR"    = SAR_info,
        "ADX"    = ADX_info,
        "CCI"    = CCI_info,
        "Aroon"  = Aroon_info,
        "SMI_CMO" = SMI_CMO_info,
        warning("Unknown indicator: ", indicator)
      )

      showModal(
        show_info_modal(indicator_info, plot_title)
      )
    })

  })
}

## To be copied in the UI
# mod_single_plot_ui("single_plot_1")

## To be copied in the server
# mod_single_plot_server("single_plot_1")
