#' user_login UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shinyWidgets textInputIcon dropdownButton
#' @importFrom shiny NS tagList passwordInput
#' @noRd
#'
mod_user_login_ui <- function(id) {
  ns <- NS(id)
  tagList(
    showModal(
      modalDialog(
        title = "Welcome to Share Market Analysis",
        footer = NULL,
        easyClose = FALSE,
        size = "s",
        tabsetPanel(
          id = ns("sign_up_in"),
          tabPanel(
            title = "Sign In",
            value = "sign_in_tab",
            br(),
            fluidRow(
              column(
                width = 10,
                shiny::textInput(
                  inputId = ns("user_name"),
                  label   = tagList(icon("user-cog"), "Username:"),
                  placeholder = "Insert your username here."
                )
                # shinyWidgets::textInputIcon(
                #   inputId = ns("user_name"),
                #   label   = "Username:",
                #   icon    = icon("user-cog"),
                #   size    = "sm",
                #   placeholder = "Insert your username here."
                # )
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
            fluidRow(
              column(width = 10,
                     shiny::passwordInput(
                       inputId = ns("user_pass"),
                       label   = tagList(icon("lock"), "Password:"),
                       placeholder = "Insert your password here."
                     )
              )
            ),
            fluidRow(
              div(
                style = "display: flex; align-items: center; column-gap: 10px",
                column(
                  width = 6,
                  shiny::textInput(
                    inputId = ns("enter_otp"),
                    label = "Enter OTP",
                    placeholder = "Enter OTP here."
                  )
                ),
                column(
                  width = 6,
                  actionButton(
                    inputId = ns("send_otp"),
                    status  = "primary",
                    icon    = icon("check-circle"),
                    # width   = "200px",
                    label   = "Send OTP"
                  )
                )
              )
            ) %>% shinyjs::hidden(),
            fluidRow(
              class = "text-center",
              checkboxInput(
                inputId = ns("reset_pass_check"),
                label = "Reset Password",
                value = FALSE
              )
            ),
            fluidRow(
              class = "text-center",
              actionButton(
                inputId = ns("btn_login"),
                status  = "primary",
                icon    = icon("check-circle"),
                width   = "200px",
                label   = "Sign In"
              )
            )
          ),
          tabPanel(
            title = "Sign Up",
            value = "sign_up_tab",
            br(),
            fluidRow(
              column(
                width = 6,
                shinyWidgets::textInputIcon(
                  inputId = ns("user_first_name"),
                  label   = "First Name",
                  icon    = icon("user-cog"),
                  size    = "sm",
                  placeholder = "Insert your first name here."
                )
              ),
              column(
                width = 6,
                shinyWidgets::textInputIcon(
                  inputId = ns("user_last_name"),
                  label   = "Last Name",
                  icon    = icon("user-cog"),
                  size    = "sm",
                  placeholder = "Insert your last name here."
                )
              )
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("user_email"),
              label   = "Email ID:",
              icon    = icon("envelope", class="fa-solid"),
              size    = "sm",
              placeholder = "Insert your mail id here."
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("user_mob"),
              label   = "Mobile No:",
              icon    = icon("mobile"),
              size    = "sm",
              placeholder = "Insert your 10 digits mobile number"
            ),
            fluidRow(
              div(
                style = "display: flex; align-items: center; column-gap: 10px",
                column(
                  width = 6,
                  shiny::textInput(
                    inputId = ns("signup_otp"),
                    label = "Enter OTP",
                    placeholder = "Enter OTP here."
                  )
                ),
                column(
                  width = 6,
                  actionButton(
                    inputId = ns("send_signup_otp"),
                    status  = "primary",
                    icon    = icon("check-circle"),
                    # width   = "200px",
                    label   = "Send OTP"
                  )
                )
              )
            ) %>% shinyjs::hidden(),
            br(),
            fluidRow(
              class = "text-center",
              actionButton(
                inputId = ns("btn_signup"),
                status  = "primary",
                icon    = icon("check-circle"),
                width   = "200px",
                label   = "Sign Up"
              )
            )
          ),
          tabPanel(
            title = "Reset Pass",
            value = "reset_pass",
            br(),
            fluidRow(
              column(
                width = 10,
                shinyWidgets::textInputIcon(
                  inputId = ns("user_email_reset"),
                  label   = "Email ID:",
                  icon    = icon("envelope", class="fa-solid"),
                  size    = "sm",
                  placeholder = "Insert your mail id here."
                )
              )
            ),
            fluidRow(
              column(
                width = 10,
                shinyWidgets::textInputIcon(
                  inputId = ns("new_pass"),
                  label   = "New Password:",
                  icon    = icon("lock"),
                  size    = "sm",
                  placeholder = "Insert new password here."
                )
              )
            ),
            fluidRow(
              column(
                width = 10,
                shinyWidgets::textInputIcon(
                  inputId = ns("confirm_new_pass"),
                  label   = "Confirm Password:",
                  icon    = icon("lock"),
                  size    = "sm",
                  placeholder = "Confirm new password."
                )
              )
            ),
            fluidRow(
              class = "text-center",
              actionButton(
                inputId = ns("reset_password"),
                status  = "primary",
                icon    = icon("check-circle"),
                width   = "200px",
                label   = "Reset Password"
              )
            )
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

      # Send OTP to user mobile numbers
      observeEvent(input$send_otp, {
        req(input$user_name, input$user_pass)
        active_user <- trimws(input$user_name)
        user_pass <- trimws(input$user_pass)
        users_data <- rv$users_data

        # validate user info and send OTP
        validate_user(
          session = session,
          active_user = active_user,
          user_pass = user_pass,
          users_data = users_data,
          on_success = function() {
            # Check if user is exist
            user_exist <-
              users_data[(users_data$User_Name == active_user &
                            users_data$User_Pass ==  user_pass), ]
            if (nrow(user_exist) == 1) {
              Mobile_No <- user_exist$Mobile_No
            }
            otp <- generate_otp()
            success <- send_otp_sms(Mobile_No, otp)
            if (isFALSE(success)) {
              shinyWidgets::sendSweetAlert(
                session = session,
                type = "error",
                title = "User Unverified",
                text = "Mobile number of this user is unverified, please contact app owner."
              )
              futile.logger::flog.info("send otp failed, user unverified")
            } else {
              updateActionButton(
                session = session,
                inputId = "send_otp",
                label = "OTP Sent"
              )
              rv$sent_otp <- otp
              futile.logger::flog.info("OTP sent, User verified")
            }
          })
      })

      # Launch login pop up
      observeEvent(input$btn_login, {
        active_user <- trimws(input$user_name)
        user_pass <- trimws(input$user_pass)
        users_data <- rv$users_data
        entered_otp <- 123456 #trimws(input$enter_otp)

        validate_user(session = session,
                      active_user = active_user,
                      user_pass = user_pass,
                      users_data = users_data,
                      on_success = function() {
                        # Check if user is exist
                        user_exist <-
                          users_data[(users_data$User_Name == active_user &
                                        users_data$User_Pass ==  user_pass), ]

                        if (nrow(user_exist) == 1) {
                          if (is.null(rv$sent_otp)) {
                            shinyWidgets::sendSweetAlert(
                              session = session,
                              type = "warning",
                              title = "User not verified",
                              text = "Please verify user by sending OTP."
                            )
                          } else if (rv$sent_otp != entered_otp) {
                            shinyWidgets::sendSweetAlert(
                              session = session,
                              type = "error",
                              title = "OTP Mismatch",
                              text = "Entered incorrect OTP."
                            )
                          } else if (rv$sent_otp == entered_otp) {
                            removeModal()
                            # store users credentials
                            rv$user_login_data$user <- active_user
                            rv$user_role <- trimws(user_exist$User_Type)
                            rv$is_user_admin <- active_user %in% user_admin_list
                            # validate_and_process_username(active_user)
                            futile.logger::flog.info(paste0("mod_user_login: {",
                                                            active_user,
                                                            "} dev user logged in"))
                          }
                        } else {
                          shinyWidgets::sendSweetAlert(
                            session = session,
                            type = "error",
                            title = "404",
                            text = paste0("Sorry, you don't have access to this application.\n
                             Please sign up for username and password.")
                          )
                        }
                      })
      })

      observeEvent(input$btn_signup, {
        user_first_name <- trimws(input$user_first_name)
        user_last_name <- trimws(input$user_last_name)
        user_email <- trimws(input$user_email)
        user_mob <- paste("+91", trimws(input$user_mob), sep = "")
        users_data <- rv$users_data

        validate_signup_user(
          session = session,
          user_first_name = user_first_name,
          user_last_name = user_last_name,
          user_email = user_email,
          user_mob = user_mob,
          users_data = users_data,
          on_success = function() {
            user_exist <-
              users_data[(users_data$User_Mail == user_email &
                            users_data$User_Mob ==  user_mob), ]

            if (nrow(user_exist) == 0) {
              username <- tolower(paste(user_first_name, user_last_name, sep = "_"))
              password <- paste0(sample(c(LETTERS, letters, 0:9), 8, replace = TRUE), collapse = "")

              new_user <- data.frame(
                First_Name = c(user_first_name),
                Last_Name = c(user_last_name),
                User_Name = c(username),
                User_Pass = c(password),
                User_Mob = c(user_mob),
                User_Mail = c(user_email),
                User_Type = c("single"),
                User_Access = c(FALSE),
                User_Paid = c("UNPAID")
              )

              users_data <- rbind(users_data, new_user)
              rv$users_data <- users_data

              success <- send_email(user_email = user_email,
                                    first_name = user_first_name,
                                    username = username,
                                    password = password)

              if ("message" %in% names(success)) {
                shinyWidgets::sendSweetAlert(
                  session = session,
                  title = "Failed to send email",
                  text = "Failed to send email to provided mail id.",
                  type = "error"
                )
                futile.logger::flog.info("Failed to send email")
              } else {

                # Save the new user data in aws s3
                aws.s3::s3saveRDS(
                  x = rv$users_data,
                  object = Sys.getenv("AWS_OBJECT"),
                  bucket = Sys.getenv("AWS_BUCKET")
                )
                futile.logger::flog.info("New user added in the database")

                shinyWidgets::sendSweetAlert(
                  session = session,
                  title = "Account created successfully!",
                  text = "Please check your mail for username and password, Thanks!",
                  type = "message"
                )

                updateTabsetPanel(
                  session = session,
                  inputId = "sign_up_in",
                  selected = "sign_in_tab"
                )
              }

            } else {
              shinyWidgets::sendSweetAlert(
                session = session,
                title = "User already Exist!",
                text = "This user already exist!",
                type = "warning"
              )
              futile.logger::flog.info("User already exist")
            }
          }
        )
      })

      # Reset password tab
      observeEvent(input$reset_pass_check, {
        if (isTRUE(input$reset_pass_check)) {
          shiny::showTab(inputId = "sign_up_in",
                         target = "reset_pass")

          updateTabsetPanel(
            session = session,
            inputId = "sign_up_in",
            selected = "reset_pass"
          )
        } else {
          shiny::hideTab(inputId = "sign_up_in",
                         target = "reset_pass")
          updateTabsetPanel(
            session = session,
            inputId = "sign_up_in",
            selected = "sign_in_tab"
          )
        }
      })

      # Reset Password
      observeEvent(input$reset_password, {
        user_email <- trimws(input$user_email_reset)
        new_pass <- trimws(input$new_pass)
        confirm_new_pass <- trimws(input$confirm_new_pass)
        users_data <- rv$users_data

        validate_reset_pass(
          session = session,
          user_email = user_email,
          new_pass = new_pass,
          confirm_new_pass = confirm_new_pass,
          users_data = users_data,
          on_success = function() {
            users_data[users_data$User_Mail == user_email, ]$User_Pass <- new_pass

            rv$users_data <- users_data

            # Save the password in the data in aws s3
            aws.s3::s3saveRDS(
              x = rv$users_data,
              object = Sys.getenv("AWS_OBJECT"),
              bucket = Sys.getenv("AWS_BUCKET")
            )
            futile.logger::flog.info("User reset the password")

            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Password Reset Successfully!",
              text = "Password has been reset successfully, Thanks!",
              type = "message"
            )

            updateTabsetPanel(
              session = session,
              inputId = "sign_up_in",
              selected = "sign_in_tab"
            )

            shiny::updateCheckboxInput(
              session = session,
              inputId = "reset_pass_check",
              value = FALSE
            )
          }
        )
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
        futile.logger::flog.info(" leaving session$user event.")
      })
    }
  })
}

## To be copied in the UI
# mod_user_login_ui("user_login_1")

## To be copied in the server
# mod_user_login_server("user_login_1")
