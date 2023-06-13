#' @title QC table UI component
#' @description The UI components of the QC table module
#'
#' @param id element ID
#'
#' @seealso
#'  \code{qcTableServer}, \code{qcTableModule}
#'
#' @rdname qcTableUI
#'
#' @importFrom shiny tagList div checkboxInput conditionalPanel fluidRow column actionButton tags
#' @importFrom DT dataTableOutput
#'
#' @export
qcTableUI <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    shiny::div(
      style = "display: none",
      shiny::checkboxInput(ns("showButtons"), label = "")
    ),
    shiny::conditionalPanel(
      condition = "input.showButtons",
      ns = ns,
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::actionButton(ns("update_button"),
            "Update",
            style = "width: 100%;",
            class = "btn-primary"
          )
        ),
        shiny::column(
          6,
          shiny::actionButton(ns("cancel_update_button"),
            "Cancel",
            style = "width: 100%;",
            class = "btn-danger"
          )
        )
      ),
      shiny::tags$style(type = "text/css", ".datatables { margin-top: 20px; }")
    ),
    DT::dataTableOutput(ns("tf_manual_review_table"))
  )
}

#' @title QC Table Server
#' @description Server component of the QC table module
#' @param id element ID
#'
#' @seealso
#'  \code{qcTableUI}, \code{qcTableModule}
#'
#' @rdname qcTableServer
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent updateCheckboxInput
#' @importFrom DT dataTableProxy renderDataTable datatable coerceValue replaceData
#' @importFrom labretriever create_url retrieve
#'
#' @export
qcTableServer <- function(id, tf_id, token) {
  shiny::moduleServer(id, function(input, output, session) {
    input_check_list <- list(
      token = token,
      tf_id = tf_id
    )

    for (i in names(input_check_list)) {
      if (!shiny::is.reactive(input_check_list[[i]])) {
        futile.logger::flog.error(sprintf("input %s must be reactive", i))
      }
    }

    proxy <- DT::dataTableProxy("tf_manual_review_table")

    # Initialize the reactiveValues object with your data frame
    rv <- shiny::reactiveValues(data = labretriever::retrieve(
      labretriever::create_url("qcreview",
        base_url = yeastCCDBShiny::base_url
      ),
      token(),
      filter_list = list(tf_id = tf_id())
    ) %>%
      dplyr::arrange(experiment_id))



    output$tf_manual_review_table <- DT::renderDataTable({
      DT::datatable(
        rv$data,
        editable = TRUE,
        selection = "single",
        options = list(
          scrollX = TRUE,
          width = "100%"
        )
      )
    }) # Set server to FALSE to make proxy work

    shiny::observeEvent(input$update_button, {
      # Code to handle the update button click event
      # For testing purposes, let's print a message to the console
      cat("Update button clicked\n")
      # Update the showButtons reactive value
      shiny::updateCheckboxInput(session, "showButtons", value = FALSE)
    })

    shiny::observeEvent(input$cancel_update_button, {
      # Code to handle the cancel button click event
      # You can add your desired logic here
      # For testing purposes, let's print a message to the console
      cat("Cancel button clicked\n")
      # Update the showButtons reactive value
      shiny::updateCheckboxInput(session, "showButtons", value = FALSE)
    })

    shiny::observeEvent(input$tf_manual_review_table_cell_edit, {
      # Code to handle the table cell edit event
      # Update the showButtons reactive value
      shiny::updateCheckboxInput(session, "showButtons", value = TRUE)

      info <- input$tf_manual_review_table_cell_edit
      str(info)
      i <- info$row
      j <- info$col
      v <- info$value
      # update the data base
      message("updating the database")
      # if successful, update the data
      # else, show the DB response
      # Update the data in the reactiveValues object
      rv$data[i, j] <<- DT::coerceValue(v, rv$data[i, j])
      DT::replaceData(proxy,
        rv$data,
        resetPaging = FALSE,
        rownames = FALSE
      ) # replaces data in table without resetting paging
    })
  })
}

#' @title QC Table Module Tester
#' @description A minimal test of the QC Example of the QC Table Module
#'
#' @seealso
#'  \code{qcTableServer}, \code{qcTableModule}
#'
#' @rdname testQcTableModule
#'
#' @importFrom shiny fluidPage shinyApp
#' @export
testQcTableModule <- function() {
  ui <- shiny::fluidPage(
    qcTableUI("qc_table")
  )
  server <- function(input, output, session) {
    qcTableServer("qc_table")
  }
  shiny::shinyApp(ui, server)
}
