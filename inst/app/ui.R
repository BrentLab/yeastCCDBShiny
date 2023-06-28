library(yeastCCDBShiny)

ui <- shiny::fluidPage(
  tags$head(
    tags$style(
      HTML("
           .plots-container {
             display: flex;
             justify-content: space-between;
           }
           #rr_plot-kemmeren_tfko_container, #rr_plot-mcisaac_zev_container {
             width: 50%;
           }
           ")
    ),
    tags$script(
      HTML("
           Shiny.addCustomMessageHandler('openTab', function(message) {
              window.open(message);
            });
           ")
    )
  ),
  shiny::fluidRow(
    height = 1, tokenAuthUI("header"),
    style = "float: right;"
  ),
  shiny::fluidRow(
    shiny::column(
      width = 2,
      style = "height: 100vh; background-color: #f5f5f5;",
      fluidRow(h4("Calling Cards Review")),
      fluidRow(tfSelectorUI("tf_selector")),
      fluidRow(goUI("go_module"))
    ),
    shiny::column(
      width = 10,
      style = "height: 100vh;",
      fluidRow(rankResponsePlotUI("rr_plot")),
      fluidRow(qcTableUI('qc_table'))
    )
  )
)
