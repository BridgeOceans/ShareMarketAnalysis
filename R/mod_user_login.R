#' user_login UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shinyWidgets textInputIcon dropdownButton
#' @importFrom shiny NS tagList
#' @noRd
#'
mod_user_login_ui <- function(id){
  ns <- NS(id)
  tagList(
    showModal(
      modalDialog(
        title = "Welcome to Share Market Analysis",
        footer = NULL,
        easyClose = FALSE,
        size = "s",
        fluidRow(
          column(
            width = 10,
            shinyWidgets::textInputIcon(
              inputId = ns("user_name"),
              label   = "Username",
              icon    = icon("user-cog"),
              size    = "sm",
              placeholder = "Insert your username here."
            )
          ),
          column(
            width = 2,
            style = "margin-top: 27px;",
            shinyWidgets::dropdownButton(
              helpText("Please sign in with your username."),
              circle = TRUE,
              size   = "xs",
              inline = TRUE,
              status = "primary",
              width  = "300px",
              icon   = icon("info")
            )
          )
        ),
        br(),
        fluidRow(
          class = "text-center",
          actionButton(
            inputId = ns("btn_login"),
            status  = "primary",
            icon    = icon("check-circle"),
            width   = "200px",
            label   = "Log In"
          )
        )
      )
    )
  )
}

#' user_login Server Functions
#'
#' @param input,output,session Internal parameters for shiny
#' @param rv A reactive values object:
#' `user_login_data` list containing user log in data
#'
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom glue glue
#' @importFrom dplyr distinct
#' @importFrom magrittr %>%
#'
#' @noRd
mod_user_login_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (is.null(session$user)) {
      # Launch login pop up
      observeEvent(input$btn_login, {
        active_user <- trimws(input$user_name)

        if (is.null(active_user) || active_user == "") {
          shinyWidgets::sendSweetAlert(
            session = session,
            type = "info",
            title = "Please insert user Username.",
            text = paste0("Note: First time users will be registered ",
                          "with the given username and logged in.")
          )
        } else {
          removeModal()

          # store users credentials
          rv$user_login_data$user <- active_user
          rv$user_role <- trimws(input$user_type)
          # validate_and_process_username(active_user)
          futile.logger::flog.info(paste0("mod_user_login: {",
                                          active_user,
                                          "} dev user logged in"))
        }
      })
    } else {

      # Fetch user login from rsconnect (No pop up)
      # when app is called from deployed env
      observeEvent(session$user, {
        futile.logger::flog.info(paste0(" entering session$user event."))
        rv$user_login_data$user <- session$user
        if (!is.null(session$user) &
            !purrr::is_empty(get_golem_config('public_ip'))) {
          rv$parallel_run <- TRUE
        }
        active_user <- session$user
        rv$user_role <- trimws(input$user_type)
        # validate_and_process_username(active_user)
        futile.logger::flog.info(paste0(
          "mod_user_login: {",
          active_user,
          "} dev user logged in through SSO"
        ))
      })
    }

  })
}

## To be copied in the UI
# mod_user_login_ui("user_login_1")

## To be copied in the server
# mod_user_login_server("user_login_1")
