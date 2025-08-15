#' pack_version UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom stringr str_extract
#' @importFrom utils packageVersion packageDescription
#' @importFrom shinyWidgets dropdownButton
#' @importFrom shiny NS div tagList
#'
#' @noRd
mod_pack_version_ui <- function(id){
  ns <- NS(id)
  # Get packages information
  app_version <- utils::packageVersion("ShareMarketAnalysis")
  app_timestamp <- stringr::str_extract(utils::packageDescription("ShareMarketAnalysis")$LastCommit,
                                        "\\d+-\\d+-\\d+")

  tagList(
    div(
      style = "margin-top: 14px;",
      shinyWidgets::dropdownButton(
        tags$table(
          class = "tg",
          tags$thead(
            tags$th(
              class = "tg-amwm",
              "App Version"
            )
          ),
          tags$tbody(
            tags$td(
              class = "tg-baqh",
              as.character(app_version)
            )
          )
        ),
        circle = TRUE,
        size   = "xs",
        status = "bo_custom",
        margin = "20px",
        width  = "210px",
        icon   = icon("info")
      )
    )
  )
}

#' pack_version Server Functions
#'
#' @noRd
mod_pack_version_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_pack_version_ui("pack_version_1")

## To be copied in the server
# mod_pack_version_server("pack_version_1")
