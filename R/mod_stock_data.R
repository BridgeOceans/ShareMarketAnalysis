#' stock_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS wellPanel tabPanel
#' @importFrom shinycssloaders withSpinner
#' @importFrom reactable reactableOutput
#'
#' @noRd
#'
#' @importFrom shiny  tagList
mod_stock_data_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    "Data",
    wellPanel(
      fluidRow(
        column(width = 8,
               shinycssloaders::withSpinner(
                 reactable::reactableOutput(ns("stock_data"))
               )),
        column(width = 4,
               shinycssloaders::withSpinner(
                 reactable::reactableOutput(ns("stock_data_research"))
               ))
      )
    )
  )
}

#' stock_data Server Functions
#'
#' @param rv rv
#' @importFrom reactable renderReactable
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate_all arrange desc
#' @importFrom rlang .data
#'
#' @noRd
mod_stock_data_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$stock_data <- reactable::renderReactable({
      req(rv$stock_data)
      stock_data <- rv$stock_data %>%
        as.data.frame() %>%
        mutate_all(.funs = ~round(., 4)) %>%
        tibble::rownames_to_column(var = "Date") %>%
        dplyr::arrange(desc(.data$Date))

      reactable_formatting(stock_data, stock_type = rv$stock_type)
    })

    output$stock_data_research <- reactable::renderReactable({
      req(rv$stock_data)
      if (rv$symbol != "NIFTY") {
        data <- data.frame(
          Info = c(
            "BirdsEyeView",
            "TechnicalAnalysis",
            "MovingAverage",
            "Candlestick",
            "PivotPoint",
            "FundamentalAnalysis",
            "FinancialStatements",
            "BetaAndVolatility",
            "InteractiveCharts"
          ),
          Icon = c(
            "birds_view.png",
            "technicals.png",
            "mvg_avg.png",
            "candlestick.png",
            "pivot_point.png",
            "fundamentals.png",
            "fin_statmnts.png",
            "beta_vol.png",
            "tech_charts.png"
          ),
          stringsAsFactors = FALSE
        )
      } else {
        data <- data.frame(
          Info = c(
            "BirdsEyeView",
            "TechnicalAnalysis",
            "MovingAverage",
            "Candlestick",
            "PivotPoint",
            "BetaAndVolatility",
            "InteractiveCharts",
            "FuturesAndOptions"
          ),
          Icon = c(
            "birds_view.png",
            "technicals.png",
            "mvg_avg.png",
            "candlestick.png",
            "pivot_point.png",
            "beta_vol.png",
            "tech_charts.png",
            "futures_options.png"
          ),
          stringsAsFactors = FALSE
        )
      }

      reactable(
        data,
        defaultColDef = colDef(
          align = "center",
          # format = colFormat(digits = 1),
          headerStyle = list(fontWeight = "bold"),
          style = list(color = "blue", fontWeight = "bold")
        ),
        columns = list(
          Info = colDef(
            cell = function(value) {
              url <-
                glue::glue("https://www.topstockresearch.com/rt/Stock/",
                           rv$symbol,
                           "/",
                           value)
              htmltools::tags$a(href = url, target = "_blank", as.character(value))
            }
          ),
          Icon = colDef(
            cell = function(value) {
              tags$img(src = file.path("www", value),
                       height = 50,
                       width = 100)  # Adjust size as needed
            }
          )
        )
      )
    })
  })
}

## To be copied in the UI
# mod_stock_data_ui("stock_data_1")

## To be copied in the server
# mod_stock_data_server("stock_data_1")
