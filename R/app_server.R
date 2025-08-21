#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactiveValues showNotification
#' @importFrom aws.s3 s3readRDS
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable
#' @importFrom rhandsontable hot_cols hot_col hot_to_r
#' @importFrom dplyr filter
#'
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  futile.logger::flog.appender(futile.logger::appender.tee("logger.log"))
  futile.logger::flog.info(" entering app_server function")

  mod_pack_version_server("pack_version_1")

  users_data <-
    aws.s3::s3readRDS(
      object = Sys.getenv("AWS_OBJECT"),
      bucket = Sys.getenv("AWS_BUCKET")
    )

  rv <- reactiveValues(
    launch_app = NULL,
    user_login_data = NULL,
    users_data    = users_data,
    symbol        = NULL,
    stock_type    = NULL,
    stock_data    = NULL,
    tech_ind_data = NULL,
    arima_model   = NULL,
    ind_end_date = Sys.Date(),
    sent_otp = 123456
  )


  observeEvent(input$my_user_login, {
    futile.logger::flog.info(" entering event input$my_user_login.")

    if (isTruthy(input$my_user_login)) {
      rv$user_login_data$user <- input$my_user_login
      active_user <- input$my_user_login
      rv$user_role <- trimws(input$user_type)
      # validate_and_process_username(active_user)
    } else {
      mod_user_login_ui("user_login_1")
    }
    futile.logger::flog.info(" leaving event input$my_user_login")
  })

  # launch login module in dev version
  observeEvent(session, {
    futile.logger::flog.info(" entering session event.")
    host_url <- glue::glue(session$clientData$url_protocol, "//", session$clientData$url_hostname)

    if (grepl("bridge", host_url)) {
      host_url <- get_golem_config("default_host_url")
    }

    # update the rv here with plumber or user info
    if (is.null(session$user)) {
      shinyjs::runjs("document.getElementById('my_user_login').click()")
    }

    futile.logger::flog.info(" leaving session event.")
  })

  observe({
    req(rv$user_role)
    if (rv$user_role == "admin") {
      shinyjs::show(id = "view_users")
      futile.logger::flog.info("Admin user has logged in")
    }
  })

  observeEvent(input$view_users, {
    shiny::showModal(
      modalDialog(
        title = "Users Data",
        easyClose = TRUE,
        size = "l",
        wellPanel(
          actionButton(inputId = "del_users",
                       label = "Delete",
                       icon = icon("trash")),
          rhandsontable::rHandsontableOutput("users_info")
        )
      )
    )
  })

  output$users_info <- rhandsontable::renderRHandsontable({
    req(rv$users_data)
    users_data <- rv$users_data %>%
      rhandsontable::rhandsontable() %>%
      rhandsontable::hot_cols(colWidths = 100,
                              manualColumnResize = TRUE) %>%
      rhandsontable::hot_col("User_Access", type = "checkbox") %>%
      rhandsontable::hot_col("User_Pass", "password")
  })

  observeEvent(input$del_users, {
    curr_users_data <- rhandsontable::hot_to_r(input$users_info)

    is_user_selected <- curr_users_data %>%
      dplyr::filter(User_Access == TRUE)

    if (nrow(is_user_selected) == 0) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Please select user(s) to delete!",
        type = "warning"
      )
    } else {
      curr_users_data <- curr_users_data %>%
        dplyr::filter(User_Access == FALSE)

      rv$users_data <- curr_users_data
      # Save the new user data in aws s3
      aws.s3::s3saveRDS(
        x = rv$users_data,
        object = Sys.getenv("AWS_OBJECT"),
        bucket = Sys.getenv("AWS_BUCKET")
      )
      futile.logger::flog.info("Admin has deleted some users")

      shiny::showNotification(
        session = session,
        "Selected users deleted from the DB"
      )
    }
  })

  mod_user_login_server("user_login_1", rv)

  output$active_user <- renderUI({
    div(style = "margin-top: 15px; color:white", p(
      icon("user-check"),
      HTML("&nbsp"),
      tags$b(rv$user_login_data$user),
      HTML("&nbsp;"),
      HTML("&nbsp;")
    ))
  })

  observeEvent(
    rv$user_login_data$user,
    ignoreInit = TRUE,
    ignoreNULL = TRUE, {

      # data_path <-
      #   paste0(get_golem_config("aws_s3_env_location"),
      #          "/users/",
      #          rv$user_login_data$user)
      futile.logger::flog.info(paste0("{aws s3 bucket}", " connected to ",
                                      rv$user_login_data$user, " user"))
  })

  mod_landing_page_server("landing_page_1", rv)

  # Return to home
  observeEvent(input$returnhome, {
    shinyWidgets::confirmSweetAlert(
      session = session,
      inputId = "pop_return_home",
      type = "warning",
      title = "Are you sure ?",
      text = "",
      btn_labels = c("Cancel", "Yes")
    )
  })

  observeEvent(input$pop_return_home, {
    rv$proceed_return_home <- input$pop_return_home

    # Move to landing page only if user confirms
    if (isTruthy(rv$proceed_return_home)) {
      futile.logger::flog.info(paste0("User returned to home page"))
      session$reload() #it is taking too much time to restart the session
      # shinyjs::hide(id = "dashboard", anim = TRUE)
      # shinyjs::show(id = "landing", anim = TRUE)
    }
  })

  observeEvent(rv$launch_app, {
    shinyjs::hide(id = "landing", anim = TRUE)
    shinyjs::show(id = "dashboard", anim = TRUE)

    futile.logger::flog.info("Dashboard Page is Ready.")
  })

  observeEvent(input$run_analysis, {
    req(input$symbol)

    df <- suppressWarnings(
      get_data(symbol = input$symbol,
               type = input$type,
               start_date = input$dates[1],
               end_date = input$dates[2])
    )

    if ("message" %in% names(df)) {
      shiny::showNotification(df$message,
                              duration = NULL,
                              type = "error")
    } else {
      rv$symbol <- toupper(input$symbol)
      rv$stock_type <- input$type
      rv$stock_data <- df
      # rv$CSP_data <- candle_stick_patterns(df)
      rv$ind_start_date <- input$dates[2] - 300
      rv$ind_end_date <- input$dates[2]
      rv$ts_format_df <- ts_format_data(df)
      rv$arima_model <- NULL
    }
  })

  # mod_stock_data_server("stock_data_1", rv)
  mod_candle_stick_patterns_server("candle_stick_patterns_1", rv)
  mod_technical_indicators_server("technical_indicators_1", rv)
  mod_all_tech_ind_server("all_tech_ind_1", rv)
  # mod_arima_model_server("arima_model_1", rv)

  # Delete the log file on session ended
  session$onSessionEnded(function() {
    unlink("logger.log", force = TRUE)
  })
}
