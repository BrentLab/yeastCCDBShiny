#' @title TF Selector UI
#' @description UI component of the TF selector module
#' @param id Character. Element ID
#'
#' @seealso
#'  \code{\link{tfSelectorServer}, \code{tfSelectorModule}
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
    shiny::selectInput(ns("tf_selector"), "Select a TF", choices = NULL)
  )
}

#' @title TF Selector Server
#' @description Tf Selector Module Server
#'
#' @param id Element ID
#' @param token ReactiveValues. keys `username` and `token`
#' @param selected_tf ReactiveValues. keys `tf_id` and `tf_gene`
#'
#' @return None, but the `selected_tf` reactive from the outerscope will
#'   trigger observeEvent in the outer scope.
#'
#' @seealso
#'  \code{\link{tfSelectorServer}, \code{tfSelectorModule}
#'
#' @rdname tfSelectorServer
#'
#' @importFrom shiny moduleServer is.reactive reactiveVal reactiveValues observe updateSelectInput observeEvent
#'
#' @importFrom futile.logger flog.error
#' @importFrom labretriever retrieve create_url
#'
#' @export
tfSelectorServer <- function(id, token, selected_tf) {
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
  })
}

#' @title Test TF Selector Module
#' @description A function which launches an app of only this module
#'
#' @seealso
#'  \code{tfSelectorServer}, \code{tfSelectorModule}
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
    selected_tf = reactiveValues(tf_id = NULL, tf_gene = NULL)
    tfSelectorServer("tf_selector",
      token = shiny::reactive(Sys.getenv("TOKEN")),
      selected_tf
    )
    shiny::observe({
      print(selected_tf$tf_id)
      print(selected_tf$tf_gene)
    })
  }
  shiny::shinyApp(ui, server)
}
