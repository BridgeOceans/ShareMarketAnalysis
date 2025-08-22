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
#'
mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),br(),
    fluidRow(
      column(12,
             align = "left",
             img(src = "www/BO_Large.png", class = "logo"))
    ),
    br(),
    fluidRow(
      div(
        class = "landing-wrapper",
        hr(),
        div(
          class = "landing-text",
          h1("AI-Powered Trading Analysis Software", class = "landing-title"),
          h4("Powered By..."),
          h3("BridgeOceans Technologies"),
          br(),

          shinyjs::hidden(
            actionButton(
              ns("launch"),
              class = "btn-bo_custom",
              "Launch"
            )
          )
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

      showModal(
        modalDialog(
          title = "",
          easyClose = FALSE,
          size = "l",
          wellPanel(
            # Scrollable Terms and Conditions box
            div(
              style = "border: 1px solid #ccc; padding: 15px; height: 300px; overflow-y: auto; text-align: justify;",
              includeHTML("inst/app/www/terms.html")
            ),

            # Checkbox after scrolling
            checkboxInput(
              ns("terms"),
              label = "I have read and agree to the Terms and Conditions",
              value = FALSE
            )
          ),
          footer = tagList(
            modalButton("Dismiss"),   # closes modal
            actionButton(ns("accept"), "Accept")  # custom action
          )
        )
      )
    })

    observeEvent(input$accept, {
      if (isTRUE(input$terms)) {
        rv$user_agree <- TRUE
        futile.logger::flog.info("User accepted terms and conditions")
        rv$launch_app <- NULL

        rv$launch_app <- "AppLaunch"
        futile.logger::flog.info("User enters into the application")
        removeModal()
      } else {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Please accept Terms & Condtions",
          text = "",
          type = "message"
        )
      }
    })
  })
}

## To be copied in the UI
# mod_landing_page_ui("landing_page_1")

## To be copied in the server
# mod_landing_page_server("landing_page_1")
