#' technical_indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tabPanel fluidPage wellPanel fluidRow fluidRow column
#' @noRd
#'
mod_technical_indicators_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    "Technical Indicators",
    uiOutput(ns("technical_indicators_page"))
  )
}

#' technical_indicators Server Functions
#' @param rv reactive values
#'
#' @noRd
mod_technical_indicators_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$technical_indicators_page <- renderUI({
      fluidPage(
        shiny::wellPanel(
          fluidRow(
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("rsi"),
                     indicator = "RSI",
                     n1_period = 14,
                     n2_period = 0,
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   )),
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("sar"),
                     indicator = "SAR",
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   ))
          )
        ),
        shiny::wellPanel(
          fluidRow(
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("macd"),
                     indicator = "MACD",
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   )),
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("cci"),
                     indicator = "CCI",
                     n1_period = 20,
                     maType = "SMA",
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   ))
          )
        ),
        shiny::wellPanel(
          fluidRow(
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("adx"),
                     indicator = "ADX",
                     n1_period = 9,
                     maType = "EMA",
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   )),
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("bbands"),
                     indicator = "BBands",
                     n1_period = 10,
                     maType = "SMA",
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   ))
          )
        ),
        shiny::wellPanel(
          fluidRow(
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("aroon"),
                     indicator = "Aroon",
                     n1_period = 10,
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   )),
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("smi_cmo"),
                     indicator = "SMI_CMO",
                     n1_period = 14,
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   ))
          )
        ),
        shiny::wellPanel(
          fluidRow(
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("ema"),
                     indicator = "EMA",
                     n1_period = 9,
                     n2_period = 20,
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   )),
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("dema"),
                     indicator = "DEMA",
                     n1_period = 10,
                     col_1 = "#FF0505",
                     col_2 = "#0000FF",
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   ))
          )
        ),
        shiny::wellPanel(
          fluidRow(
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("atr"),
                     indicator = "ATR",
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   )),
            column(width = 6,
                   mod_single_plot_ui(
                     id = ns("kst"),
                     indicator = "KST",
                     n1_period = 20,
                     maType = "EMA",
                     start_date = rv$ind_start_date,
                     end_date = rv$ind_end_date
                   ))
          )
        )
      )
    })

    mod_single_plot_server("rsi",
                           rv = rv,
                           indicator = "RSI",
                           plot_title = "Relative Strength Indicator")
    mod_single_plot_server("sar",
                           rv = rv,
                           indicator = "SAR",
                           plot_title = "Parabolic Stop and Reverse")
    mod_single_plot_server("ema",
                           rv = rv,
                           indicator = "EMA",
                           plot_title = "Moving Average")
    mod_single_plot_server("dema",
                           rv = rv,
                           indicator = "DEMA",
                           plot_title = "EMA & Double EMA")
    mod_single_plot_server("macd",
                           rv = rv,
                           indicator = "MACD",
                           plot_title = "Moving Average Convergence Divergence")
    mod_single_plot_server("bbands",
                           rv = rv,
                           indicator = "BBands",
                           plot_title = "Bollinger Bands")
    mod_single_plot_server("adx",
                           rv = rv,
                           indicator = "ADX",
                           plot_title = "Directional Movement Index")
    mod_single_plot_server("cci",
                           rv = rv,
                           indicator = "CCI",
                           plot_title = "Commodity Channel Index")
    mod_single_plot_server("atr",
                           rv = rv,
                           indicator = "ATR",
                           plot_title = "Average True Range")
    mod_single_plot_server("kst",
                           rv = rv,
                           indicator = "KST",
                           plot_title = "Know Sure Thing")
    mod_single_plot_server("aroon",
                           rv = rv,
                           indicator = "Aroon",
                           plot_title = "Aroon Indicator and Oscillator")
    mod_single_plot_server("smi_cmo",
                           rv = rv,
                           indicator = "SMI_CMO",
                           plot_title = "SMI and CMO")

  })
}

## To be copied in the UI
# mod_technical_indicators_ui("technical_indicators_1")

## To be copied in the server
# mod_technical_indicators_server("technical_indicators_1")
