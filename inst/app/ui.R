library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    title = 'Calling Cards Review',
    tags$li(class = "dropdown",
            shinyjs::hidden(tags$span(id = "user_info")),
            actionButton("show_login_modal", "Login",
                         class = "navbar-btn",
                         style = "margin-left: 10px;"),
            style = "float: right;")
  ),
  dashboardSidebar(
    conditionalPanel(
      condition = "input.loginState == 1", # Show only when logged in
      selectInput("tf_selector", "Select an option",
                  choices = NULL)
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$script(paste0("Shiny.addCustomMessageHandler('loginState', ",
                       "function(msg) { Shiny.setInputValue('loginState', msg); });")),
    shiny::fluidRow(
      plotly::plotlyOutput("rank_response_plot")
    ),
    shiny::fluidRow(
      conditionalPanel(
        condition = "input.loginState == 1", # Show only when logged in
        actionButton("update_button", "Update")
      ),
      DT::dataTableOutput("tf_manual_review_table")
    )
  )
)
