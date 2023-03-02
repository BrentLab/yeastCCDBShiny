#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{fluidPage}}, \code{\link[shiny]{fileInput}}, \code{\link[shiny]{tableOutput}}
#' @rdname table_detail_ui
#' @importFrom shiny fluidPage fileInput tableOutput
#' @export
table_detail_ui <- function(id) {
  shiny::fluidPage(
    shiny::fileInput(shiny::NS(id, "upload"), NULL, accept = c(".csv", ".tsv")),
    shiny::tableOutput(shiny::NS(id, "head"))
  )
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param id PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{moduleServer}}, \code{\link[shiny]{reactive}}, \code{\link[shiny]{req}}, \code{\link[shiny]{validate}}, \code{\link[shiny]{tableOutput}}
#'  \code{\link[tools]{fileutils}}
#'  \code{\link[vroom]{vroom}}
#' @rdname table_detail_server
#' @importFrom shiny moduleServer reactive req validate renderTable
#' @importFrom tools file_ext
#' @importFrom vroom vroom
#' @export
table_detail_server <- function(id, db_tablename) {

  # stopifnot(shiny::is.reactive(uploaded_file))

  shiny::moduleServer(id, function(input, output, session) {

    data <- shiny::reactive({
      shiny::req(input$upload)

      ext <- tools::file_ext(input$upload$name)
      switch(ext,
             csv = vroom::vroom(input$upload$datapath, delim = ","),
             tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
             shiny::validate("Invalid file; Please upload a .csv or .tsv file")
      )
    })

    output$head <- shiny::renderTable({
      head(data(), 3)
    })

  })

}
