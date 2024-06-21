#' ExpressionManualQCAPI R6 Class
#'
#' An R6 class to interact with the ExpressionManualQCAPI endpoint.
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
#' @return A ExpressionManualQCAPI object
ExpressionManualQCAPI = R6Class(
  classname = "ExpressionManualQCAPI",
  inherit = AbstractRecordsOnlyAPI,
  public = list(
    initialize = function(url = Sys.getenv("EXPRESSIONMANUALQC_URL"), ...) {
      valid_param_keys = c('id',
                           'expression',
                           'strain_verified',
                           'regulator_locus_tag',
                           'regulator_symbol',
                           'batch',
                           'replicate',
                           'control',
                           'mechanism',
                           'restriction',
                           'time',
                           'source',
                           'lab',
                           'assay',
                           'workflow')
      super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
    }
  )
)
