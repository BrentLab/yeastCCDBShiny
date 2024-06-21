#' PromoterSetAPI R6 Class
#'
#' An R6 class to interact with the PromoterSetAPI endpoint.
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
#' @return A PromoterSetAPI object
PromoterSetAPI = R6Class(
  classname = "PromoterSetAPI",
  inherit = AbstractRecordsAndFilesAPI,
  public = list(
    initialize = function(url = Sys.getenv("PROMOTERSET_URL"), ...) {
      valid_param_keys = c()
      super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
    }
  )
)
