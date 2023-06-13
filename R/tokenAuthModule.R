#' @title Login Modal
#' @description A modal which has a field for username and password, login
#'   and cancel buttons
#' @param id Element ID
#'
#' @seealso
#'  \code{tokenAuthUI}, \code{tokenAuthServer}, \code{tokenAuthModule}
#' @rdname login_modal
#'
#' @importFrom shiny NS modalDialog textInput passwordInput tagList modalButton actionButton
#' @export
login_modal <- function(id) {
  ns <- shiny::NS(id)
  shiny::modalDialog(
    id = ns("login_module"), # Add namespace to modal id
    title = "Login",
    shiny::textInput(ns("username"), "Username"), # Add namespace to username input
    shiny::passwordInput(ns("password"), "Password"), # Add namespace to password input
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("login"), "Log In") # Add namespace to login button
    ),
    size = "m",
    easyClose = FALSE,
    fade = TRUE
  )
}

#' @title Token Authentication UI
#' @description Token Authentication UI. Adds a Login button which opens a
#'   modal. Successful login will remove the button and replace it with the
#'   username. Unsuccessful will open a different modal with an error.
#'
#' @param id Character.Element ID
#'
#' @seealso
#'  \code{tokenAuthServer}, \code{tokenAuthModule}
#' @rdname tokenAuthUI
#'
#' @importFrom shiny NS uiOutput
#'
#' @export
tokenAuthUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("custom_header"))
}

#' @title Token Authentication Server
#' @description Server function of the Token Authentication Module
#' @param id Element ID
#' @return ReactiveValues. keys `username` and `token`

#' @seealso
#'  \code{tokenAuthUI}, \code{tokenAuthModule}
#'  \code{\link[labretriever]{get_user_auth_token}}, \code{\link[labretriever]{create_url}}
#'  \code{\link[yeastCCDBShiny]{base_url}}
#'
#' @rdname tokenAuthServer
#'
#' @importFrom shiny moduleServer reactiveValues renderUI tags div actionButton observeEvent showModal req showNotification removeNotification removeModal
#' @importFrom labretriever get_user_auth_token create_url
#'
#' @export
tokenAuthServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    auth_output <- shiny::reactiveValues(token = NULL, username = NULL)

    output$custom_header <- shiny::renderUI({
      if (is.null(auth_output$token)) {
        # User is not logged in
        shiny::tags$header(
          class = "main-header",
          shiny::div(
            class = "navbar navbar-static-top",
            shiny::actionButton(session$ns("show_login_modal"), "Login", class = "navbar-btn")
          )
        )
      } else {
        # User is logged in
        shiny::tags$header(
          class = "main-header",
          shiny::div(
            class = "navbar navbar-static-top",
            shiny::tags$span("User: ", auth_output$username)
          )
        )
      }
    })

    shiny::observeEvent(input$show_login_modal, {
      shiny::showModal(login_modal(id))
    })

    shiny::observeEvent(input$login, {
      shiny::req(input$username, input$password)
      shiny::showNotification("Authenticating...", id = "authenticating")
      token <-
        labretriever::get_user_auth_token(
          labretriever::create_url("auth_token",
            base_url = yeastCCDBShiny::base_url
          ),
          input$username,
          input$password
        )
      shiny::removeNotification(id = "authenticating")
      shiny::removeModal()
      # Token authentication logic here...
      # Let's say the authentication is successful, and the username is set as the token.
      auth_output$token <- token
      auth_output$username <- input$username
    })
    return(auth_output)
  })
}

#' @title Token Authentication Module
#' @description A login button which activates a modal to perform user
#'   authentication
#'
#' @seealso
#'  \code{tokenAuthUI}, \code{tokenAuthModule}
#'
#' @rdname tokenAuthModule
#'
#' @importFrom shiny fluidPage fluidRow column h4 observeEvent shinyApp
#'
#' @export
tokenAuthModule <- function() {
  ui <- shiny::fluidPage(
    shiny::fluidRow(
      height = 1, tokenAuthUI("header"),
      style = "float: right;"
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3, style = "height: 100vh; background-color: #f5f5f5;",
        h4("Calling Cards Review")
      ),
      shiny::column(
        width = 9,
        style = "height: 100vh;",
        rankResponsePlotUI("rr_plot")
      )
    )
  )
  server <- function(input, output, session) {
    auth_output <- tokenAuthServer("header")

    shiny::observe({
      req(auth_output$token, auth_output$username)
      message(auth_output$token)
      message(auth_output$username)
    })
  }
  shiny::shinyApp(ui, server)
}
