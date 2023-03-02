# Define the login UI
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{fluidPage}}, \code{\link[shiny]{textInput}}, \code{\link[shiny]{passwordInput}}, \code{\link[shiny]{actionButton}}
#' @rdname login_ui
#' @export
#' @importFrom shiny fluidPage textInput passwordInput actionButton
login_ui <- function(id) {
  shiny::fluidPage(
    shiny::textInput(shiny::NS(id, "username"), "Username"),
    shiny::passwordInput(shiny::NS(id,"password"), "Password"),
    shiny::actionButton(shiny::NS(id,"login"), "Log In"),
    htmltools::p(
      "The token  is: ", shiny::verbatimTextOutput(shiny::NS(id, "token"))
    )
  )
}


# Define the login server
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{observeEvent}}
#'  \code{\link[httr]{POST}}, \code{\link[httr]{content_type}}
#' @rdname login_server
#' @export
#' @importFrom shiny observeEvent
#' @importFrom httr POST accept_json
login_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # Initialize the reactiveValues object
    values <- shiny::reactiveValues(token = NULL)

    # Make a request to the REST API to authenticate the user
    shiny::observeEvent(input$login, {

      shiny::req(input$username, input$password)

      auth_response <- httr::POST(paste0(Sys.getenv('DB_HOST'),
                                         database_info$endpoints$auth_token),
                                  body = list(username = input$username,
                                              password = input$password),
                                  encode = "json",
                                  httr::accept_json())

      if(httr::status_code(auth_response) == 200){
        # Send the token to the client side JavaScript
        session$sendCustomMessage(
          "saveAuthToken",
          httr::content(auth_response)$token)
        print(input$authToken)
      } else{
        session$sendCustomMessage("saveAuthToken", 'unknown username or password')
      }
    })

    shiny::observeEvent(input$authToken, {
      values$token = input$authToken
      print(values$token)
    })

    # Render the token
    output$token <- shiny::renderText({
      if (!is.null(values$token)) {
        values$token
      } else {
        "No token found"
      }
    })

  })
}

