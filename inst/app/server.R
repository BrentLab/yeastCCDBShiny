library(yeastCCDBShiny)

server <- function(input, output, session) {
  # set up reactive values
  token <- shiny::reactiveVal(NULL)
  selected_tf <- shiny::reactiveValues(tf_id = NULL, tf_gene = NULL)
  selected_tf_tracker <- shiny::reactive({
    paste(selected_tf$tf_id, selected_tf$tf_gene)
  })
  selected_row <- shiny::reactiveVal(NULL)

  # execution control
  rr_plot_init <- shiny::reactiveVal(TRUE)

  # Handle user authentication -- set the token reactive
  auth_output <- tokenAuthServer("header")
  shiny::observe({
    req(auth_output$token)
    token(auth_output$token)
  })

  observeEvent(token(), {
    tfSelectorServer(
      "tf_selector",
      token,
      selected_tf
    )
  })

  shiny::observeEvent(selected_tf_tracker(),
    {
      rankResponsePlotServer('rr_plot',
                             shiny::reactive(selected_tf$tf_id),
                             shiny::reactive(selected_tf$tf_gene),
                             selected_row,
                             token)
      qcTableServer('qc_table',
                    shiny::reactive(selected_tf$tf_id),
                    token)
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )
}
