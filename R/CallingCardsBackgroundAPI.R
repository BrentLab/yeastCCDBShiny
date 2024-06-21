#' CallingCardsBackgroundAPI R6 Class
#'
#' An R6 class to interact with the CallingCardsBackgroundAPI endpoint.
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
#' @return A CallingCardsBackgroundAPI object
CallingCardsBackgroundAPI = R6Class(
  classname = "CallingCardsBackgroundAPI",
  inherit = AbstractRecordsAndFilesAPI,
  public = list(
    initialize = function(url = Sys.getenv("CALLINGCARDSBACKGROUND_URL"), ...) {
      valid_param_keys = c('id',
                           'regulator',
                           'regulator_locus_tag',
                           'regulator_symbol',
                           'batch',
                           'control',
                           'mechanism',
                           'restriction',
                           'time',
                           'source',
                           'source_time',
                           'lab',
                           'assay',
                           'workflow')
      super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
    }
  )
)
