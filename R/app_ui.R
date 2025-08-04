#' @title The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @importFrom shiny fluidPage tabsetPanel tabPanel textInput selectInput dateRangeInput actionButton
#' @importFrom shiny icon tags
#' @importFrom shinybusy busy_start_up spin_epic
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardHeader dashboardSidebar dashboardBody
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    shinybusy::busy_start_up(
      loader = shinybusy::spin_epic("orbit", color = "#123450"),
      text = tags$span(
        tags$h3("Please wait..."),
      ),
      mode = "auto",
      color = "#123450",
      background = "#FFFFFF"
    ),

    # Your application UI logic
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(
        title = "Share Market Analysis"
      ),

      shinydashboard::dashboardSidebar(width = "250px",
        shiny::tags$head(tags$style(".down{color: black !important;}")),
        tags$head(
          tags$style(HTML("
        .main-sidebar {
          position: fixed; /* Fix the sidebar */
          height: 100%; /* Full height */
          overflow-y: auto; /* Enable vertical scroll if content overflows */
        }
        .content-wrapper {
          margin-left: 250px; /* Adjust based on sidebar width */
          margin-top: 50px; /* Adjust based on header height */
        }
        .main-header {
          position: fixed; /* Fix the header */
          width: 100%; /* Full width */
          z-index: 1000; /* Ensure it stays above other content */
        }
        .main-footer {
          margin-left: 250px; /* Ensure footer aligns with the content */
          margin-top: 50px; /* Adjust based on header height */
        }
      "))
        ),

        fluidPage(
          selectInput(inputId = "type",
                      label = "Share Type",
                      choices = c("NSE", "BSE", "NYSE")),

          # shinyWidgets::pickerInput(inputId = 'symbol',
          #                           label = "Select Share:",
          #                           choices = NULL,
          #                           multiple = FALSE
          # ),

          textInput(inputId = "symbol",
                    label = "Symbol",
                    placeholder = "Enter Share Symbol..."),

          br(),br(),

          dateRangeInput(inputId = "dates",
                         label = "Date Range",
                         format = "yyyy-mm-dd",
                         start = "2015-01-01",
                         end = Sys.Date())
        ),
        br(), br(), br(),

        fluidPage(
          actionButton(inputId = "run_analysis",
                       label = "Run Analysis",
                       icon = icon("refresh"))
        )
      ),

      shinydashboard::dashboardBody(
        shiny::tabsetPanel(
          id = "tabset_all",

          # Stock data
          mod_stock_data_ui("stock_data_1"),

          # Tab Candle stick Patterns
          mod_candle_stick_patterns_ui("candle_stick_patterns_1"),

          # Tab Technical Indicators
          mod_technical_indicators_ui("technical_indicators_1"),

          # Tab All Technical Indicators
          mod_all_tech_ind_ui("all_tech_ind_1"),

          # Tab ARIMA Model
          mod_arima_model_ui("arima_model_1"),

          # Tab with ability to add custom plots
          tabPanel(
            "Custom",
            shiny::fluidPage(
              shiny::actionButton(inputId = "add_plot",
                                  label = "",
                                  icon = icon("plus"))
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(path = app_sys("app/www"),
                            app_title = "ShareMarketAnalysis")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
