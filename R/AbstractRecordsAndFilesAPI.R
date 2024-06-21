#' @title An Abstract R6 Class for Endpoints with a `file` field
#'
#' @description An R6 class to interact with both the records, and the
#'   data stored in the `file` field. The return for this class must be
#'   records, against the `/export` endpoint when `retrieve_files` is FALSE.
#'   When `retrieve_files` is true, the storr cache should be checked first.
#'   If the file doesn't exist there, it should be retrieved from the database
#'   against the `/record_table_and_files` endpoint. The file should be a
#'   tarball with the metadata.csv and the file associated with the record,
#'   where the file is named according to the `id` field in metadata.csv.
#'   data files should be `.csv.gz`
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
#' @return A AbstractRecordsAndFilesAPI object
AbstractRecordsAndFilesAPI <- R6::R6Class(
  classname = "AbstractRecordsAndFilesAPI",
  inherit = AbstractAPI,
  public = list(

    #' @description Initialize the AbstractRecordsAndFilesAPI object.
    #'   This will serve as an interface to an endpoint that can serve
    #'   both records and files, and cache the file/retrieve from the cache
    #'   if it exists
    #'
    #' @param ... parameters to pass to \code{\link{AbstractAPI}}.
    #'   See the new() method section.
    initialize = function(...) {
      super$initialize(...)
    },

    #' @description Retrieve data from the endpoint according to the
    #'   `retrieve_files` parameter. If `retrieve_files` is FALSE, the
    #'   records will be returned as a dataframe. If `retrieve_files` is TRUE,
    #'   the files associated with the records will be retrieved either from
    #'   the local cache or from the database
    #' @param callback a function to call with the metadata from the Rank
    #'   Response API. The first parameter of the callback must be `metadata`.
    #'   The callback will be called with the metadata from the Rank Response
    #'   API as the first parameter, and any additional parameters passed to
    #'   this function as additional parameters.
    #' @param retrieve_files Boolean. Whether to retrieve the files associated
    #'   with the records retrieved by the GET request according to the params.
    #'   Defaults to FALSE.
    #' @param ... additional parameters to pass to the callback
    read = function(callback = function(metadata, data, storr, ...) {
      list(metadata, data)
    }, retrieve_files = FALSE, ...) {
      if (!is.function(callback) ||
          length(setdiff(
            c("metadata", "data", "storr"),
            names(formals(callback))
          )) > 0) {
        stop(
          "The callback must be a function with ",
          "`metadata`, `data`, and `storr` as parameters"
        )
      }

      additional_args <- list(...)

      future::future(
        {
          # get the queryset as a dataframe first
          res = tryCatch({
            httr::GET(
              url,
              header,
              query = params)
          }, error = function(e){
            stop("Error in GET request: ", e$message)
          })

          # raise an error if the request was not successful. return the result
          tryCatch({
            httr::stop_for_status(res)
          }, error = function(e){
            stop("Error in GET request: ", e$message)
            res
          })

          records_df = tryCatch({
            readr::read_csv(httr::content(res, as='text', encoding = "UTF-8"),
                            show_col_types = FALSE)
          }, error = function(e){
            stop("Error reading request content: ", e$message)
            res
          })

          if (!retrieve_files){
            callback(metadata = records_df, data = NULL)
          } else{
            data_list = purrr::pmap(records_df, private$.retrieve_files)
            names(data_list) = records_df$id
            callback(metadata = records_df, data = data_list, additional_args)
          }
        },
        globals = list(
          url = paste(self$url, "export", sep = "/"),
          header = self$header,
          params = as.list.ParamsList(self$params),
          callback = callback,
          storr = self$storr,
          additional_args = additional_args
        ),
        packages = c("futile.logger", "httr", "jsonlite",
                     "dplyr", "readr", "purrr")
      )
    }
  ),
  # private variables here. Recommend putting private variations with '.'
  # and then configuring getter/setter in the `active` section
  private = list(
    .retrieve_files = function(id, ...){
      # try to get the data from the storr first
      tryCatch({
        # if the record can be extracted from the storr, do so. Return
        # as a dataframe
        df = dplyr::as_tibble(jsonlite::fromJSON(super$.storr_get(id)))
        futile.logger::flog.info(paste(class(self)[1], 'id:', id,
                                       'retrieved from the storr.'))
        # return the dataframe
        df
      }, error = function(e){
        futile.logger::flog.info(paste(class(self)[1], 'id:', id,
                                       'does not exist in the storr.',
                                       'Retrieving from the database.',
                                       'storr msg: ', e$message))

        # create a temporary file for the tarball, which will have
        # metadata.csv and <id>.csv files
        tar_file = tempfile(fileext = '.tar.gz')
        on.exit(unlink(tar_file), add = TRUE)

        params = list(id = id)
        res = httr::GET(paste(self$url, 'record_table_and_files', sep = '/'),
                        self$header,
                        query = params)
        writeBin(httr::content(res, as = 'raw'), tar_file)

        # List the contents of the tar file
        tar_contents <- untar(tar_file, list = TRUE)
        futile.logger::flog.debug(paste("tar_contents: ", tar_contents))

        # Check if the specified filename is in the tar file
        filename = paste0(id, ".csv.gz")
        if (!filename %in% tar_contents) {
          stop(paste(filename, "not found in tarball.",
                     "tarball contents:",
                     paste(tar_contents, collapse = ", ")))
        }

        # create a tempfile path for the extracted CSV file
        # and ensure that it is destroyed on exit
        csv_dir <- tempfile()
        dir.create(csv_dir)
        on.exit(unlink(csv_dir, recursive = TRUE), add = TRUE)

        # untar the specified CSV file to the tempfile path
        untar(tar_file, files = filename, exdir = csv_dir)

        # read in the CSV file and return it as a dataframe
        df = readr::read_csv(file.path(csv_dir,
                                       filename),
                             show_col_types = FALSE)

        # store the data in the storr
        tryCatch({
          futile.logger::flog.info(paste('Storing', class(self)[1],
                                         'file for record:',
                                         id,
                                         'in storr'))
          super$.storr_set(id, jsonlite::toJSON(df))
        }, error = function(e){
          futile.logger::flog.error(paste('Error storing',
                                          class(self)[1],
                                          'file for record',
                                          id,
                                          'in storr:', e$message))
        })

        # return the dataframe
        df
      })
    }
  ),
  active = list(),
  lock_objects = TRUE,
  class = TRUE,
  portable = TRUE,
  lock_class = FALSE,
  cloneable = TRUE,
  parent_env = parent.frame()
)
