ui <- shiny::fluidPage(
  shiny::fluidRow(
    height = 1, tokenAuthUI("header"),
    style = "float: right;"
  ),
  shiny::fluidRow(
    shiny::column(
      width = 2,
      style = "height: 100vh; background-color: #f5f5f5;",
      fluidRow(h4("Calling Cards Review")),
      fluidRow(tfSelectorUI("tf_selector"))
    ),
    shiny::column(
      width = 10,
      style = "height: 100vh;",
      fluidRow(rankResponsePlotUI("rr_plot")),
      fluidRow(qcTableUI('qc_table'))
    )
  )
)
