#' @title TF Selector UI
#' @description UI component of the TF selector module
#' @param id Character. Element ID
#'
#' @seealso
#'  \code{\link{tfSelectorServer}}, \code{\link{testTfSelectorModule}}
#'
#' @rdname tfSelectorUI
#'
#' @importFrom shiny NS tagList selectInput
#'
#' @export
tfSelectorUI <- function(id) {
  # create a namespace object with the appropriate `id`
  ns <- shiny::NS(id)
  # create a input dropdown selector
  shiny::tagList(
    shiny::selectInput(ns("tf_selector"), "Select a TF", choices = NULL),
    shiny::radioButtons(ns("hops_source"),
      "Select a source for the qbed files",
      choices = c("mitra", "nf-core/callingcards:dev"),
      selected = "mitra",
      inline = TRUE
    ),
    shiny::radioButtons(ns("background_source"),
      "Select a Background Set",
      choices = c("adh1", "dsir4_new"),
      selected = "adh1",
      inline = TRUE
    ),
    shiny::radioButtons(ns("promoter_source"),
      "Select a Promoter Set",
      choices = c("yiming", "not_orf"),
      selected = "yiming",
      inline = TRUE
    ),
    shiny::sliderInput(
      ns("bin_size"),
      "Set a Rank Response Bin Size",
      min = 5,
      max = 50,
      value = 5,
      step = 5
    ),
    shiny::checkboxInput(ns("normalize_selector"),
      "Normalize",
      value = FALSE
    ),
    shiny::checkboxInput(ns("confidence_intervals"),
      "Confidence Intervals",
      value = FALSE
    ),
    shiny::sliderInput(
      ns("rank_threshold"),
      "Set the rank threshold for GO Enrichment",
      min = 5,
      max = 50,
      value = 30,
      step = 5
    )
  )
}

#' @title TF Selector Server
#' @description Tf Selector Module Server
#'
#' @param id Element ID
#' @param token ReactiveValues. keys `username` and `token`
#' @param selected_tf ReactiveValues. keys `tf_id` and `tf_gene`
#' @param hops_source ReactiveVal char. Source (pipeline) of the experimental hops
#' @param background_source ReactiveVal char. Source of the background hops
#' @param promoter_source ReactiveVal char. Source of the promoter source
#' @param bin_size ReactiveVal integer. size of the rank response bins
#' @param normalize_selector ReactiveVal Boolean. Whether to normalize the
#'   number of responsive genes between sets
#' @param confidence_intervals ReactiveVal, Boolean. Toggle on/off the
#'   confidence intervals on the plot
#'
#' @return None, but the `selected_tf` reactive from the outerscope will
#'   trigger observeEvent in the outer scope.
#'
#' @seealso
#'  \code{\link{tfSelectorUI}}, \code{\link{testTfSelectorModule}}
#'
#' @rdname tfSelectorServer
#'
#' @importFrom shiny moduleServer is.reactive reactiveVal reactiveValues observe updateSelectInput observeEvent
#' @importFrom futile.logger flog.error
#' @importFrom labretriever retrieve create_url
#'
#' @return None
#'
#' @export
tfSelectorServer <- function(id,
                             token,
                             selected_tf,
                             hops_source,
                             background_source,
                             promoter_source,
                             bin_size,
                             normalize_selector,
                             confidence_intervals,
                             rank_threshold) {
  shiny::moduleServer(id, function(input, output, session) {
    input_check_list <- list(
      token = token
    )

    for (i in names(input_check_list)) {
      if (!shiny::is.reactive(input_check_list[[i]])) {
        futile.logger::flog.error(sprintf("input %s must be reactive", i))
      }
    }

    tf_id_gene_map_reactive <- shiny::reactiveVal()

    # retrieve list of active TFs
    shiny::observe({
      tf_list_df <- labretriever::retrieve(
        labretriever::create_url("cctf_tf_list",
          base_url = yeastCCDBShiny::base_url
        ), token()
      )

      tf_id_gene_map <- setNames(
        tf_list_df$tf_id,
        tf_list_df$tf_gene
      )

      # do this the opposite way to recover the TF gene from the TF id to
      # return in the observeEvent below this one
      tf_id_gene_map_reactive(setNames(
        tf_list_df$tf_gene,
        tf_list_df$tf_id
      ))

      shiny::updateSelectInput(session, "tf_selector",
        choices = tf_id_gene_map,
        selected = ""
      )
    })

    # Observe the tf_selector input
    shiny::observeEvent(input$tf_selector, {
      if (input$tf_selector != "") {
        selected_tf$tf_id <- input$tf_selector
        selected_tf$tf_gene <- tf_id_gene_map_reactive()[[input$tf_selector]]
      }
    })

    shiny::observeEvent(input$hops_source, {
      hops_source(input$hops_source)
    })

    shiny::observeEvent(input$background_source, {
      background_source(input$background_source)
    })

    shiny::observeEvent(input$promoter_source, {
      promoter_source(input$promoter_source)
    })

    shiny::observeEvent(input$bin_size, {
      bin_size(input$bin_size)
    })

    shiny::observeEvent(input$normalize_selector, {
      normalize_selector(input$normalize_selector)
    })

    shiny::observeEvent(input$confidence_intervals, {
      confidence_intervals(input$confidence_intervals)
    })

    shiny::observeEvent(input$rank_threshold, {
      rank_threshold(input$rank_threshold)
    })

  })
}

#' @title Test TF Selector Module
#' @description A function which launches an app of only this module
#'
#' @seealso
#'  \code{\link{tfSelectorUI}}, \code{\link{tfSelectorServer}}
#'
#' @rdname testTfSelectorModule
#'
#' @importFrom shiny fluidPage reactive observe shinyApp
#'
#' @export
testTfSelectorModule <- function() {
  ui <- shiny::fluidPage(
    tfSelectorUI("tf_selector")
  )
  server <- function(input, output, session) {
    selected_tf <- reactiveValues(tf_id = NULL, tf_gene = NULL)
    # reactives to control the rank response data and plot
    hops_source <- shiny::reactiveVal(NULL)
    background_source <- shiny::reactiveVal(NULL)
    promoter_source <- shiny::reactiveVal(NULL)
    bin_size <- shiny::reactiveVal(NULL)
    normalize_selector <- shiny::reactiveVal(NULL)
    confidence_intervals <- shiny::reactiveVal(NULL)
    rank_threshold <- shiny::reactiveVal(NULL)
    tfSelectorServer("tf_selector",
      token = shiny::reactive(Sys.getenv("TOKEN")),
      selected_tf,
      hops_source,
      background_source,
      promoter_source,
      bin_size,
      normalize_selector,
      confidence_intervals,
      rank_threshold
    )
    shiny::observe({
      message(selected_tf$tf_id)
      message(selected_tf$tf_gene)
    })
  }
  shiny::shinyApp(ui, server)
}
