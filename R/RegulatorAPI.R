#' RegulatorAPI R6 Class
#'
#' An R6 class to interact with the RegulatorAPI endpoint.
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
#' @return A RegulatorAPI object
RegulatorAPI = R6Class(
  classname = "RegulatorAPI",
  inherit = AbstractRecordsOnlyAPI,
  public = list(
    initialize = function(url = Sys.getenv("REGULATOR_URL"), ...) {
      valid_param_keys = c('id',
                           'regulator_locus_tag',
                           'regulator_symbol',
                           'under_development')
      super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
    }
  )
)
