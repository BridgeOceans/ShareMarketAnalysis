#' landing_page UI Function
#'
#' @description A shiny Module for landing page.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column div hr actionButton
#' @importFrom shinyjs hidden disabled
mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),br(),br(),br(),
    fluidRow(
      column(12,
             align = "left",
             img(src = "www/BO_Large.png", class = "logo"))
    ),
    # tags$script(HTML(onInitialize)),
    fluidRow(div(class = "landing-wrapper",
                 hr(),
                 div(class = "landing-text",
                     h1("Share Market Analysis"),
                     br(),
                     br(),
                     br(),
                     br(),
                     shinyjs::hidden(actionButton(ns("launch"),
                                                  class = "btn-bo_custom",
                                                  "Launch"))
                 )
    )
    )
  )
}

#' landing_page Server Functions
#'
#' @param rv reactive variable
#' @importFrom shinyjs show
#' @importFrom dplyr filter pull
#' @importFrom magrittr  %>%
#' @importFrom rlang .data
#'
#' @noRd
mod_landing_page_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(rv$user_login_data$user)
      shinyjs::show("launch")
    })

    observeEvent(input$launch, {
      rv$launch_app <- NULL

      rv$launch_app <- "AppLaunch"
      futile.logger::flog.info("User enters into the application")
    })
  })
}

## To be copied in the UI
# mod_landing_page_ui("landing_page_1")

## To be copied in the server
# mod_landing_page_server("landing_page_1")
