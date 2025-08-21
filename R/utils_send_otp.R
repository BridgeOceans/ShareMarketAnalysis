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
                       first_name,
                       username,
                       password) {

  tryCatch({

    # create HTML body
    email_body <- sprintf(
      '
      <html>
      <body style="font-family: Arial, sans-serif; color: #333; line-height: 1.6;">

      <h2>Welcome to BridgeOceans Trading Analysis Software - Your Registration is Confirmed!</h2>

      <p>Dear %s,</p>

      <p>
      Thank you for registering with <b>BridgeOceans Trading Analysis Software</b>!
      We are excited to confirm that your account has been successfully created,
      and you are now ready to use our trading analysis tools.
      </p>

      <h3>Next Steps to Get Started</h3>
      <ul>
        <li><b>Log In to Your Account:</b> Visit <a href="https://www.bridgeoceans.com">www.bridgeoceans.com</a>
            and sign in using your credentials below.</li>
        <li><b>Explore Key Features:</b> Discover real-time data analytics, customizable charts,
            and predictive algorithms to start analyzing markets confidently.</li>
        <li><b>Personalize Your Experience:</b> Visit the Profile section to tailor your dashboard and preferences.</li>
        <li><b>Check Out Our Resources:</b> Visit our <a href="https://www.bridgeoceans.com/support">Support Page</a>
            for tutorials, FAQs, and tips.</li>
      </ul>

      <h3>Account Details</h3>
      <p>
        <b>Username:</b> %s<br>
        <b>Password:</b> %s<br>
        <b>Account Type:</b> Free Trial (upgrade anytime at <a href="https://www.bridgeoceans.com/account">Account Management</a>)
      </p>

      <h3>Support</h3>
      <p>
        Need help? Contact us at
        <a href="mailto:support@bridgeoceans.com">support@bridgeoceans.com</a>
        or via our <a href="https://www.bridgeoceans.com/contact">Support Portal</a>.
      </p>

      <h3>Important Notes</h3>
      <ul>
        <li><b>Terms and Conditions:</b> By using our software, you agree to our
            <a href="https://www.bridgeoceans.com/terms">Terms and Conditions</a> and
            <a href="https://www.bridgeoceans.com/privacy">Privacy Policy</a>.</li>
        <li><b>Trading Risks:</b> Trading involves financial risk. Please research thoroughly
        and consult a financial advisor before making trading decisions.</li>
      </ul>

      <p>
        We are thrilled to have you on board and cannot wait to see how <b>BridgeOceans</b>
        helps you achieve your trading goals. For questions or feedback, do not hesitate to contact us.
      </p>

      <p><b>Best Regards,<br>
      The BridgeOceans Team</b></p>

      <p>
        Website: <a href="https://www.bridgeoceans.com">www.bridgeoceans.com</a> |
        <a href="https://www.bowealth.com">www.bowealth.com</a><br>
        Email: support@bridgeoceans.com
      </p>

      <p><i>P.S. Keep an eye on your inbox for tips, exclusive offers, and updates to enhance your experience with BridgeOceans Trading Software!</i></p>

    </body>
    </html>
    ',
      first_name, username, password
    )

    mailR::send.mail(
      from = Sys.getenv("SMTP_USER"),
      to = c(user_email),
      bcc = c("amol@bridgeoceans.com"),
      subject = paste0("Registration Confirmation for BO Trading Analysis Software"),
      body = email_body,
      html = TRUE,
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
