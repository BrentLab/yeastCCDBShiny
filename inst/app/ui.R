library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    title = "Calling Cards Review",
    tags$li(
      class = "dropdown",
      shinyjs::hidden(tags$span(id = "user_info")),
      actionButton("show_login_modal", "Login",
        class = "navbar-btn",
        style = "margin-left: 10px; margin-right: 10px"
      ),
      style = "float: right;"
    )
  ),
  dashboardSidebar(
    conditionalPanel(
      condition = "input.loginState == 1", # Show only when logged in
      selectInput("tf_selector", "Select an option",
        choices = NULL
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML(
          ".plot-container {
            position: relative;
            width: 100%;
            padding-top: 40%; /* Set the aspect ratio here (e.g., 50% for 2:1) */
          }
          .plot-container .plotly {
            position: absolute;
            top: 0;
            left: 0;
            bottom: 0;
            right: 0;
          }

          div#update-cancel-row {
          margin-left: 10px;
          margin-top: 20px;
          margin-bottom: 20px;
          }

          div#update-cancel-row .btn {
          margin-right: 10px;
          margin-left: 10px;
          }


          .update-button {
          background-color: green;
          border-color: green;
          color: white;
          }

          .cancel-button {
          background-color: black;
          border-color: black;
          color: white;
          }"
        )
      )
    ),
    useShinyjs(),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('loginState', function(msg) {
        Shiny.setInputValue('loginState', msg);
      });

      Shiny.addCustomMessageHandler('updateManualReviewChangesLength',
        function(message) {
          if (message > 0) {
            $('#update_button').show();
            $('#cancel_update_button').show();
          } else {
            $('#update_button').hide();
            $('#cancel_update_button').hide();
          }
        }
      );")),
    shiny::fluidRow(
      div(
        class = "plot-container",
        plotly::plotlyOutput("rank_response_plot", width = "100%", height = "100%")
      )
    ),
    shiny::fluidRow(
      div(
        class = "update-cancel-row",
        conditionalPanel(
          condition = "input.loginState == 1",
          shiny::fluidRow(
            column(
              6,
              actionButton("update_button",
                "Update",
                style = "width: 100%; display:none;",
                class = "update-button"
              )
            ),
            column(
              6,
              actionButton("cancel_update_button",
                "Cancel",
                style = "width: 100%; display:none;",
                class = "cancel-button"
              )
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      DT::dataTableOutput("tf_manual_review_table")
    )
  )
)
