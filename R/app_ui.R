#' @title The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @importFrom shiny fluidPage tabsetPanel tabPanel textInput selectInput dateRangeInput actionButton
#' @importFrom shiny icon tags
#' @importFrom shinybusy busy_start_up spin_epic
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardHeader dashboardSidebar dashboardBody
#' @importFrom shinyjs hidden runjs hide show
#' @importFrom shinyWidgets actionBttn dropdownButton
#' @noRd
app_ui <- function(request) {
  my_login <- NULL

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    tags$head(
      shinyjs::hidden(
        textInput(
          inputId = "my_user_login",
          label = "my_user_login_label",
          value = my_login
        )
      ),
      tags$script("document.getElementById('my_user_login').click()"),
    ),

    # Show the spinner till the background process completes
    shinybusy::busy_start_up(
      loader = shinybusy::spin_epic("self-building-square", color = "#123450"),
      text = tags$span(
        tags$h3("Loading..."),
      ),
      mode = "auto",
      color = "#123450",
      background = "#ffffff"
    ),

    # Landing div
    div(
      id = "landing", class = "landing-div",
      mod_landing_page_ui("landing_page_1")
    ),

    # Dashboard div
    shinyjs::hidden(
      div(
        id = "dashboard",
        # Call to shinydashboard, dashboardPage,
        # with modules for the server body
        shinydashboard::dashboardPage(
          shinydashboard::dashboardHeader(
            title = "Share Market Analysis",
            tags$li(class = "dropdown",
                    mod_pack_version_ui("pack_version_1")),
            tags$li(class = "dropdown", uiOutput("active_user")),
            tags$li(
              class = "dropdown",
              a(href = "https://www.linkedin.com/company/bridgeoceans/",
                icon('linkedin'),
                target = "_blank")),
            tags$li(
              class = "dropdown",
              a(href = "mailto:support@bridgeoceans.com?subject=Support%20Request&body=Hello%20Team,",
                icon("envelope", class="fa-solid"),
                target = "_blank")),
            tags$li(class = "dropdown", div(
              id = "add_return_home",
              shinyWidgets::dropdownButton(
                inputId = "returnhome",
                label = "Log Out",
                tooltip = TRUE,
                circle = TRUE,
                size = "xs",
                width = "150px",
                status = "bo_custom",
                icon = icon("power-off")
              )
            ))
          ),

          shinydashboard::dashboardSidebar(
            width = "250px",
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

              textInput(inputId = "symbol",
                        label = "Symbol",
                        placeholder = "Enter Share Symbol..."),

              br(),

              dateRangeInput(inputId = "dates",
                             label = "Date Range",
                             format = "yyyy-mm-dd",
                             start = "2015-01-01",
                             end = Sys.Date()),
              br(),

              shiny::actionButton(inputId = "view_users",
                                  label = "View Users",
                                  icon = icon("table")) %>% shinyjs::hidden()
            ),
            br(), br(), br(),

            fluidPage(
              shinyWidgets::actionBttn(inputId = "run_analysis",
                                       label = "Run Analysis",
                                       icon = icon("refresh"))
            ),
            div(
              style = "position: absolute; bottom: 40px; width: 100%; text-align: center;",
              tags$img(
                src = "www/BO_Large.png",
                width = "230px"
              )
            )
          ),

          shinydashboard::dashboardBody(
            tags$head(
              tags$style(
                HTML(" .content-wrapper { background-color: #ffffff; } ")
              )
            ),
            shiny::tabsetPanel(
              id = "tabset_all",

              # Stock data
              # mod_stock_data_ui("stock_data_1"),

              # Tab Candle stick Patterns
              mod_candle_stick_patterns_ui("candle_stick_patterns_1"),

              # Tab Technical Indicators
              mod_technical_indicators_ui("technical_indicators_1"),

              # Tab All Technical Indicators
              mod_all_tech_ind_ui("all_tech_ind_1"),

              # Tab ARIMA Model
              # mod_arima_model_ui("arima_model_1"),

              # Tab with ability to add custom plots
              # tabPanel(
              #   "Custom",
              #   shiny::fluidPage(
              #     shiny::actionButton(inputId = "add_plot",
              #                         label = "",
              #                         icon = icon("plus"))
              #   )
              # )
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
    shinyjs::useShinyjs(),
    shiny::includeCSS(app_sys("app/www/css/style.css")),
    golem::bundle_resources(path = app_sys("app/www"),
                            app_title = "ShareMarketAnalysis")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
