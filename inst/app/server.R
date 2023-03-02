server <- function(input, output, session) {

  table_detail_server('db_table', shiny::reactive(input$db_tables))
  login_server('user_auth')

}
