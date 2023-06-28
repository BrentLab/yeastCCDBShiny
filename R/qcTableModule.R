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

    tf_manual_review_changes = reactiveValues(data = NULL)

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

    observeEvent(input$update_button,
       {
         shiny::req(tf_id())
         # Check if there are any changes
         if (length(tf_manual_review_changes$data) > 0) {
           # Process the changes and send updates to the database
           tryCatch({
             # Iterate through the changes and send updates for each changed field
             for (change in tf_manual_review_changes$data) {
               labretriever::send(
                 df = change,
                 url = labretriever::create_url(
                   "qcreview",
                   base_url = yeastCCDBShiny::base_url
                 ),
                 token = token(),
                 update = TRUE
               )
             }
             # Update the showButtons reactive value
             shiny::updateCheckboxInput(session, "showButtons", value = FALSE)
           }, error = function(err) {
             # If an error occurs, show a modal with the error message
             showModal(modalDialog(
               title = "Error",
               paste0(
                 "An error occurred while updating the data: ",
                 err$message, "\n\n",
                 "ALL CHANGES CLEARED"
               ),
               easyClose = TRUE
             ))
           }, finally = {
             # retrieve the updated data and update the reactive
             updated_data <- labretriever::retrieve(
               labretriever::create_url("qcreview",
                                        base_url = yeastCCDBShiny::base_url
               ),
               token(),
               filter_list = list(tf_id = tf_id())
             )
             # update the review reactive
             rv$data <- updated_data

             # Clear the changes list
             tf_manual_review_changes$data <- list()
           })
         }
       },
       ignoreInit = TRUE
    )

    shiny::observeEvent(input$tf_manual_review_table_cell_edit, {
      # Code to handle the table cell edit event
      # Update the showButtons reactive value
      shiny::updateCheckboxInput(session, "showButtons", value = TRUE)

      info <- input$tf_manual_review_table_cell_edit

      i <- info$row
      j <- info$col
      value <- info$value
      experiment_id <- rv$data[i, "experiment_id"]
      update_col <- colnames(rv$data)[j]
      df <- data.frame(id = experiment_id, value = value)
      df <- setNames(df, c("id", update_col))
      # Append the changes to the list with the corresponding id
      new_index <- length(tf_manual_review_changes$data) + 1
      tf_manual_review_changes$data[[new_index]] <- df

      # rv$data[i, j] <<- DT::coerceValue(v, rv$data[i, j])
      # DT::replaceData(proxy,
      #   rv$data,
      #   resetPaging = FALSE,
      #   rownames = FALSE
      # ) # replaces data in table without resetting paging
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
