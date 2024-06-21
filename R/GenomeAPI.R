#' GenomeAPI R6 Class
#'
#' An R6 class to interact with the GenomeAPI endpoint.
#'
#' @family API
#'
#' @importFrom R6 R6Class
#' @importFrom futile.logger flog.info
#' @importFrom future future
#' @importFrom httr GET content stop_for_status
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom dplyr as_tibble
#' @importFrom readr read_csv
#'
#' @return A GenomeAPI object
GenomeAPI = R6Class(
  classname = "GenomeAPI",
  inherit = AbstractRecordsAndFilesAPI,
  public = list(
    initialize = function(url = Sys.getenv("GENOME_URL"), ...) {
      valid_param_keys = c()
      super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
    }
  )
)
