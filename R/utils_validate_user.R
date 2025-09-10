#' Function to validate user information
#'
#' @param session shiny session
#' @param active_user active user name
#' @param user_pass user password
#' @param users_data users data
#' @param on_success else part on success
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
validate_user <- function(session,
                          active_user,
                          user_pass,
                          users_data,
                          on_success) {
  if (is.null(active_user) || active_user == "") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please enter Username.",
      text = ""
    )
  } else if (!is.null(active_user) && !(active_user %in% users_data$User_Mail)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "error",
      title = "Username is incorrect",
      text = "Please enter correct username."
    )
  } else if (is.null(user_pass) || user_pass == "") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please enter password.",
      text = ""
    )
  } else if (!is.null(user_pass) &&
             !(user_pass %in% users_data[users_data$User_Mail == active_user, "User_Pass"])) {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "error",
      title = "Password is incorrect",
      text = "Please enter correct password."
    )
  } else {
    # Success case – execute custom logic
    on_success()
  }
}

#' Function to validate user info while signing up
#' @param session session
#' @param user_first_name First name of user
#' @param user_last_name Last name of user
#' @param user_email Email ID of user
#' @param user_mob Mobile number of user
#' @param users_data Users data
#' @param on_success else part on success
#'
#' @return return notifications if something wrong
#' @export
#'
validate_signup_user <- function(session,
                                 user_first_name,
                                 user_last_name,
                                 user_email,
                                 user_mob,
                                 users_data,
                                 on_success) {

  if (is.null(user_first_name) || user_first_name == "") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please enter First Name",
      text = ""
    )
  } else if (is.null(user_last_name) || user_last_name == "") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please enter Last Name",
      text = ""
    )
  } else if (is.null(user_email) || user_email == "") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please enter Mail ID.",
      text = ""
    )
  } else if (!is.null(user_email) && (user_email %in% users_data$User_Mail)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "error",
      title = "Mail ID already exist",
      text = "This mail id is already exist, please use different mail id."
    )
  } else if (is.null(user_mob) || user_mob == "" || user_mob == "+91") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please enter Mobile Number.",
      text = ""
    )
  } else if (!is.null(user_mob) && (user_mob %in% users_data$User_Mob)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "error",
      title = "Mobile number already exist",
      text = "This mobile number is already exist, please use different mobile no."
    )
  } else {
    on_success()
  }
}

#' FUnction to reset the password
#'
#' @param session current session
#' @param user_email user mail address
#' @param new_pass New password
#' @param confirm_new_pass Confirm New Password
#' @param users_data Users Data
#' @param on_success else part on success
#'
#' @importFrom shinyWidgets sendSweetAlert
#'
#' @return return notification if something is wrong
#' @export
#'
validate_reset_pass <- function(session,
                                user_email,
                                new_pass,
                                confirm_new_pass,
                                users_data,
                                on_success) {
  if (is.null(user_email) || user_email == "") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please enter registered mail ID",
      text = ""
    )
  } else if (!is.null(user_email) && !(user_email %in% users_data$User_Mail)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Mail ID does not exist",
      text = "This mail id does not exist, please enter registered mail id."
    )
  } else if (is.null(new_pass) || new_pass == "") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please enter New Password",
      text = ""
    )
  } else if (is.null(confirm_new_pass) || confirm_new_pass == "") {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Please Confirm your New Password.",
      text = ""
    )
  } else if (new_pass != confirm_new_pass) {
    shinyWidgets::sendSweetAlert(
      session = session,
      type = "warning",
      title = "Password Mistmatch!",
      text = "Passwords are incorrect."
    )
  } else {
    on_success()
  }
}
