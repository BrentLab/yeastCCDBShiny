#' GO Module User Interface
#'
#' This function defines the user interface of the GO Module.
#' The GO Module presents the user with two clickable buttons leading to
#' enrichment results for "Kemmeren TFKO" and "McIsaac ZEV".
#'
#' @param id A string that gives the namespace for the module's input and
#'   output.
#'
#' @return A Shiny tagList containing the UI elements for the module.
#'
#' @seealso \code{\link{goServer}} for the corresponding server function.
#' @seealso \code{\link{testGoModule}} for a function that tests the GO Module.
#'
#' @examples
#' \dontrun{
#' ui <- shiny::fluidPage(
#'   goUI("go_test")
#' )
#' }
#'
#' @importFrom shiny NS tagList actionButton
#'
#' @export
goUI <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    shiny::actionButton(
      ns("kemmeren_tfko_btn"),
      "Kemmeren TFKO Enrichment Results",
      style = "width: 100%; white-space: normal;"
    ),
    shiny::actionButton(
      ns("mcisaac_zev_btn"),
      "McIsaac ZEV Enrichment Results",
      style = "width: 100%; white-space: normal;"
    )
  )
}


#' GO Module Server
#'
#' This function defines the server-side functionality of the GO Module.
#' The GO Module provides the user with two clickable links that lead to
#' enrichment results for "Kemmeren TFKO" and "McIsaac ZEV".
#'
#' @param id A string that gives the namespace for the module's input and
#'   output.
#' @param rank_response_df A reactive expression that returns a data frame
#'   containing the rank response.
#' @param rank_threshold A reactive expression that returns a numeric value
#'   representing the rank threshold.
#'
#' @seealso \code{\link{goUI}} for the corresponding UI function.
#' @seealso \code{\link{testGoModule}} for a function that tests the GO Module.
#'
#' @examples
#' \dontrun{
#' rank_response_df_reactive <- shiny::reactive({
#'   yeastCCDBShiny::rank_response_df
#' })
#'
#' rank_threshold <- shiny::reactive({
#'   25
#' })
#'
#' goServer("go_test",
#'   rank_response_df = rank_response_df_reactive,
#'   rank_threshold = rank_threshold
#' )
#' }
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent req
#' @importFrom futile.logger flog.error flog.info
#'
#' @export
goServer <- function(id, rank_response_df, rank_threshold) {
  moduleServer(id, function(input, output, session) {
    # check that inputs are reactives
    input_check_list <- list(
      rank_response_df = rank_response_df,
      rank_threshold = rank_threshold
    )

    for (i in names(input_check_list)) {
      if (!shiny::is.reactive(input_check_list[[i]])) {
        futile.logger::flog.error(sprintf("input %s must be reactive", i))
      }
    }

    kemmeren_tfko_link <- reactiveVal()
    mcisaac_zev_link <- reactiveVal()

    # when rank_response_df changes, update the GO results
    observeEvent(rank_response_df(),
      {
        shiny::req(rank_response_df())
        futile.logger::flog.info('Getting Enrichment Results')
        go_results <- multi_query_enrichment_links(
          rank_response_df(),
          rank_threshold()
        )

        kemmeren_tfko_link(go_results[["kemmeren_tfko"]])
        mcisaac_zev_link(go_results[["mcisaac_zev"]])
      },
      ignoreNULL = TRUE
    )

    # when the rank_threshold changes, update the go results
    observeEvent(rank_threshold(),
      {
        shiny::req(rank_response_df(), rank_threshold())
        futile.logger::flog.info(
          paste0('Updating Enrichment Results with rank: ', rank_threshold()))
        go_results <- multi_query_enrichment_links(
          rank_response_df(),
          rank_threshold()
        )

        kemmeren_tfko_link(go_results[["kemmeren_tfko"]])
        mcisaac_zev_link(go_results[["mcisaac_zev"]])
      },
      ignoreInit = TRUE
    )

    observeEvent(input$kemmeren_tfko_btn, {
      req(kemmeren_tfko_link())
      message(paste0("clicking kemmeren tfko btn. link: ", kemmeren_tfko_link()))
      session$sendCustomMessage(type = "openTab", message = kemmeren_tfko_link())
    })

    observeEvent(input$mcisaac_zev_btn, {
      req(mcisaac_zev_link())
      message(paste0("clicking zev tfko btn. link: ", mcisaac_zev_link()))
      session$sendCustomMessage(type = "openTab", message = mcisaac_zev_link())
    })
  })
}



#' @title GO Module Test App
#' @description A function to launch an app which displays only the GO module UI
#'   and demonstrates the server functionality
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   testGoModule()
#' }
#' }
#' @rdname testGoModule
#'
#' @importFrom shiny fluidPage shinyApp reactive
#' @importFrom dplyr tibble
#' @importFrom tibble deframe
#'
#' @export
testGoModule <- function() {
  ui <- shiny::fluidPage(
    tags$head(
      tags$script(
        HTML("
          Shiny.addCustomMessageHandler('openTab', function(message) {
            window.open(message);
          });
          ")
      )
    ),
    goUI("go_test")
  )

  server <- function(input, output, session) {
    rank_response_df_reactive <- shiny::reactiveVal(yeastCCDBShiny::rank_response_df)

    rank_threshold <- shiny::reactiveVal(25)

    goServer("go_test",
             rank_response_df = rank_response_df_reactive,
             rank_threshold = rank_threshold
    )
  }

  shiny::shinyApp(ui, server)
}
