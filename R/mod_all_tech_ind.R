#' all_tech_ind UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_all_tech_ind_ui <- function(id,
                                start_date = Sys.Date() - 300,
                                end_date = Sys.Date()){
  ns <- NS(id)
  tabPanel(
    "All Tech Indicators",
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
          }
        ),
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
      fluidPage(
        fluidRow(
          shinycssloaders::withSpinner(
            shiny::plotOutput(
              outputId = ns("all_indicator_chart"),
              height = "900px"
            )
          )
        )
      )
    )
  )
}

#' all_tech_ind Server Functions
#'
#' @noRd
mod_all_tech_ind_server <- function(id,
                                    rv,
                                    plot_title = "All Technical Indicators"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$all_indicator_chart <- renderPlot({
      req(!is.null(rv$stock_data))

      stock_data <- rv$stock_data

      if (input$agg_period != "day") {
        stock_data <- stock_data %>%
          as.data.frame() %>%
          na.omit() %>%
          mutate_all(.funs = ~as.numeric(round(., 4))) %>%
          tibble::rownames_to_column("Date") %>%
          mutate(Date = as.Date(.data$Date))

        stock_data <- aggregate_data(stock_data, input$agg_period)

        stock_data_filtered <- tail(stock_data, 300)

      } else {
        stock_data_filtered <- stock_data[index(stock_data) >= as.Date(input$start_date) &
                                            index(stock_data) <= as.Date(input$end_date)]
      }

      TA_list <- paste(c("addRSI()", "addSMA(n = 9, col = 'blue')",
                         "addSMA(n = 20, col = 'red')",
                         "addMACD()", "addBBands()","addKST()", "addSAR()",
                         "addCCI()", "addCMF()", "addCMO()", "addSMI()"), collapse = ";")

      quantmod::chartSeries(stock_data_filtered, theme = "white",
                            name = plot_title, TA = TA_list)
    })

  })
}

## To be copied in the UI
# mod_all_tech_ind_ui("all_tech_ind_1")

## To be copied in the server
# mod_all_tech_ind_server("all_tech_ind_1")
