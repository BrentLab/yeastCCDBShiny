#' @title DataSourceAPI R6 Class
#'
#' @description An R6 class to interact with the DataSourceAPI endpoint.
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
#' @return A DataSourceAPI object
DataSourceAPI = R6Class(
  classname = "DataSourceAPI",
  inherit = AbstractRecordsOnlyAPI,
  public = list(
    initialize = function(url = Sys.getenv("DATASOURCE_URL"), ...) {
      valid_param_keys = c('id',
                           'fileformat_id',
                           'fileformat',
                           'lab',
                           'assay',
                           'workflow')
      super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
    },
    read = function(...){
      super$read(...)
    }
  )
)


#' DataSourceAPI = R6Class(
#'   classname = "DataSourceAPI",
#'   inherit = AbstractRecordsOnlyAPI,
#'   public = list(
#'     initialize = function(url = Sys.getenv("DATASOURCE_URL"), ...) {
#'       valid_param_keys = c('id',
#'                            'fileformat_id',
#'                            'fileformat',
#'                            'lab',
#'                            'assay',
#'                            'workflow')
#'       super$initialize(url = url, valid_param_keys = valid_param_keys, ...)
#'     },
#'     #' @description Retrieve data from the DataSourceAPI. The data will be
#'     #'   returned as a dataframe. The callback function must take metadata,
#'     #'   data, and storr as parameters. The metadata will be a dataframe,
#'     #'   the data slot will be NULL, and the storr will be the storr object.
#'     #'
#'     #' @param callback the function to call with the data
#'     #'
#'     #' @return a future object which will resolve with whatever the output
#'     #'   of the callback function is
#'     read = function(callback = function(metadata, data, storr, ...) {
#'       list(metadata, data)
#'     }, retrieve_files = FALSE, ...) {
#'       if (!is.function(callback) ||
#'           length(setdiff(
#'             c("metadata", "data", "storr"),
#'             names(formals(callback))
#'           )) > 0) {
#'         stop(
#'           "The callback must be a function with ",
#'           "`metadata`, `data`, and `storr` as parameters"
#'         )
#'       }
#'       additional_args <- list(...)
#'
#'       future::future(
#'         {
#'           # get the queryset as a dataframe first
#'           res = tryCatch({
#'             httr::GET(
#'               url,
#'               header,
#'               query = params)
#'           }, error = function(e){
#'             stop("Error in GET request: ", e$message)
#'           })
#'
#'           # raise an error if the request was not successful. return the result
#'           tryCatch({
#'             httr::stop_for_status(res)
#'           }, error = function(e){
#'             stop("Error in GET request: ", e$message)
#'             res
#'           })
#'
#'           records_df = tryCatch({
#'             readr::read_csv(httr::content(res, as='text', encoding = "UTF-8"),
#'                             show_col_types = FALSE)
#'           }, error = function(e){
#'             stop("Error reading request content: ", e$message)
#'             res
#'           })
#'
#'           callback(metadata = records_df,
#'                    data = NULL,
#'                    storr = storr,
#'                    additional_args)
#'
#'         },
#'         globals = list(
#'           url = paste(self$url, "export", sep = "/"),
#'           header = self$header,
#'           params = as.list.ParamsList(self$params),
#'           callback = callback,
#'           storr = self$storr,
#'           additional_args = additional_args
#'         ),
#'         packages = c("futile.logger", "httr", "readr")
#'       )
#'     }
#'   ),
#'   # private variables here. Recommend putting private variations with '.'
#'   # and then configuring getter/setter in the `active` section
#'   private = list(),
#'   active = list(),
#'   lock_objects = TRUE,
#'   class = TRUE,
#'   portable = TRUE,
#'   lock_class = FALSE,
#'   cloneable = TRUE,
#'   parent_env = parent.frame()
#' )
