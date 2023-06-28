library(yeastCCDBShiny)

server <- function(input, output, session) {
  # reactive value for token auth token
  token <- shiny::reactiveVal(NULL)
  # reactive val for the table selected row
  selected_row <- shiny::reactiveVal(NULL)
  # reactive val for the TF dropdown selector
  selected_tf <- shiny::reactiveValues(tf_id = NULL, tf_gene = NULL)
  selected_tf_tracker <- shiny::reactive({
    paste(selected_tf$tf_id, selected_tf$tf_gene)
  })
  # reactives to control the rank response data and plot
  hops_source <- shiny::reactiveVal(NULL)
  background_source <- shiny::reactiveVal(NULL)
  promoter_source <- shiny::reactiveVal(NULL)
  bin_size <- shiny::reactiveVal(NULL)
  normalize_selector <- shiny::reactiveVal(NULL)
  confidence_intervals <- shiny::reactiveVal(NULL)
  rank_threshold <- shiny::reactiveVal(NULL)
  # rank response data
  rank_response_df <- shiny::reactiveVal(NULL)

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
      selected_tf,
      hops_source,
      background_source,
      promoter_source,
      bin_size,
      normalize_selector,
      confidence_intervals,
      rank_threshold
    )
  })

  shiny::observeEvent(rank_threshold(), {
    goServer('go_module', rank_response_df, rank_threshold)
  }, ignoreNULL = TRUE)

  shiny::observeEvent(selected_tf_tracker(),
    {
      rankResponsePlotServer(
        "rr_plot",
        shiny::reactive(selected_tf$tf_id),
        shiny::reactive(selected_tf$tf_gene),
        rank_response_df,
        selected_row,
        hops_source,
        background_source,
        promoter_source,
        bin_size,
        normalize_selector,
        confidence_intervals,
        token
      )
      qcTableServer(
        "qc_table",
        shiny::reactive(selected_tf$tf_id),
        token
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )
}
