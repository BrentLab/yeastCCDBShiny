#' FileFormatAPI R6 Class
#'
#' An R6 class to interact with the FileFormatAPI endpoint.
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
#' @return A FileFormatAPI object
FileFormatAPI = R6Class(
  classname = "FileFormatAPI",
  inherit = AbstractRecordsOnlyAPI,
  public = list(
    initialize = function(url = Sys.getenv("FILEFORMAT_URL"), ...) {
      valid_param_keys = c('fileformat',
                           'fields',
                           'separator',
                           'feature_identifier_col',
                           'effect_col',
                           'default_effect_threshold',
                           'pval_col',
                           'default_pvalue_threshold')
      super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
    }
  ),
  # private variables here. Recommend putting private variations with '.'
  # and then configuring getter/setter in the `active` section
  private = list(),
  active = list(),
  lock_objects = TRUE,
  class = TRUE,
  portable = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  parent_env = parent.frame()
)
