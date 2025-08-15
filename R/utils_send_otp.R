#' Function to generate user name
#'
#' @param first_name Users first name
#' @param last_name Users last name
#'
#' @return A list of user name and password
#' @export
#'
generate_user_name <- function(first_name, last_name) {
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  username <- paste0(tolower(first_name), "_", tolower(last_name), "-", substr(timestamp, 11, 14))
  return(username)
}

#' Function to generate unique password
#'
#' @return A unique password
#' @export
#'
generate_password <- function() {
  password <- paste0(sample(c(LETTERS, letters, 0:9), 8, replace = TRUE), collapse = "")
  return(password)
}

#' Function to generate OTP
#'
#' @param n number of digits in OTP
#'
#' @return A OTP with 6 digits
#' @export
#'
generate_otp <- function(n = 6) {
  paste0(sample(0:9, n, replace = TRUE), collapse = "")
}

#' Function to send OTP on register mobile number
#'
#' @param to_number mobile number
#' @param otp_code OTP
#'
#' @importFrom httr POST authenticate status_code
#'
#' @return send the OTP to user's register mobile number
#' @export
#'
send_otp_sms <- function(to_number, otp_code) {
  message_body <- paste("Your OTP code is:", otp_code)

  twilio_sid <- Sys.getenv("TWILIO_SID")
  twilio_token <- Sys.getenv("TWILIO_AUTH")
  twilio_phone <- Sys.getenv("TWILIO_PHONE")

  url <- paste0("https://api.twilio.com/2010-04-01/Accounts/", twilio_sid, "/Messages.json")

  response <- POST(
    url,
    authenticate(twilio_sid, twilio_token),
    body = list(
      From = twilio_phone,
      To = to_number,
      Body = message_body
    ),
    encode = "form"
  )

  return(status_code(response) == 201)
}


#' Function to send mail and notification
#'
#' @param user_email User mail id
#' @param username username
#' @param password password
#'
#' @importFrom mailR send.mail
#' @importFrom shinyWidgets sendSweetAlert
#'
#' @return send notification based on email success or failure
#' @export
#'
send_email <- function(user_email,
                       username,
                       password) {

  tryCatch({
    mailR::send.mail(
      from = Sys.getenv("SMTP_USER"),
      to = c(user_email),
      subject = paste0("Stocks to be Up-Trend as of - ", Sys.Date()),
      body = paste0(paste("This research is just for educational purposes, please consult with your financial advisor.\n",
                          "Please find your credentials below - \n", collapse = "\n"), "\n",
                    "username - ", username, "\n",
                    "password - ", password),
      smtp = list(
        host.name = Sys.getenv("SMTP_SERVER"),
        port = Sys.getenv("SMTP_PORT"),
        user.name = Sys.getenv("SMTP_USER"),
        passwd = Sys.getenv("SMTP_PASS"),
        ssl = TRUE
      ),
      authenticate = TRUE,
      send = TRUE
    )
  }, error = function(e) {
    return(e)
  })
}
