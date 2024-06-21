#' GenomicFeatureAPI R6 Class
#'
#' An R6 class to interact with the GenomicFeatureAPI endpoint.
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
#' @return A GenomicFeatureAPI object
GenomicFeatureAPI = R6Class(
  classname = "GenomicFeatureAPI",
  inherit = AbstractRecordsOnlyAPI,
  public = list(
    initialize = function(url = Sys.getenv("GENOMICFEATURE_URL"), ...) {
      valid_param_keys = c('id',
                           'chr',
                           'start',
                           'end',
                           'strand',
                           'type',
                           'locus_tag',
                           'symbol',
                           'source',
                           'alias',
                           'note')
      super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
    }
  )
)
