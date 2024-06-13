#' RankResponseAPI R6 Class
#'
#' An R6 class to interact with the Rank Response API.
#'
#' @description Retrieves rank response data from the database.
#'
#' @note the parent .is_valid_url is overwritten in this class, and
#'   the Sys.getenv("PROMOTERSETSIG_URL") is used to validate that the
#'   url works and the token is valid. The RankResponse endpoint, as it is
#'   currently written, does not return a response object unless parameters
#'   are passed
#'
#' @family API
#'
#' @importFrom R6 R6Class
#' @importFrom futile.logger flog.debug
#'
#' @return A RankResponseAPI object
# RankResponseAPI R6 Class
RankResponseAPI <- R6::R6Class(
  "RankResponseAPI",
  inherit = AbstractAPI,
  public = list(

    #' @description Initialize the RankResponseAPI object. This will serve
    #'   as an interface to the RankResponse endpoint of both the database
    #'   and the application cache
    #'
    #' @note Because of the way the rankresponse endpoint is currently set
    #'   up, to check whether the header/token is valid, the PROMOTERSETSIG_URL
    #'   is used in .is_valid_url(). This should be fixed in the database
    #'   endpoint eventually.
    #'
    #' @note RANK_BIN_SIZE by default set at 5 -- it is possible to change
    #'   this through this class's paramslist right now, but it isn't
    #'   tracked in the cache
    #'
    #' @param url the URL of the Rank Response API
    #' @param ... additional parameters to pass to \code{\link{AbstractAPI}}.
    #'   See the new() method section.
    initialize = function(url = Sys.getenv("RANKRESPONSE_URL", ""), ...) {
      if (url == "") {
        stop("A valid Rank Response URL must be provided")
      }
      if (Sys.getenv("PROMOTERSETSIG_URL") == "") {
        stop("No PROMOTERSETSIG_URL in .Renviron -- contact the maintainer")
      }
      valid_param_keys <- c(
        "promotersetsig_id",
        "expression_id"
      )
      super$initialize(url, valid_param_keys = valid_param_keys, ...)
    },

    #' @description Retrieve data from the Rank Response API. At least the
    #'   `promotersetsig_id` must be set in the params. If the `expression_id`
    #'   is not also set, then all data associated with that
    #'   `promotersetsig_id`, given any other parameters, will be pulled. The
    #'   data will be cached with the <promotersetsig_id>_<experiment_id> as
    #'   a key and the table as an R object as a value in this instance's
    #'   cache. The metadata from the tarball will be returned, which can
    #'   be used to access the data from the cache. It will always be fastest
    #'   to check whether the promotersetsig_id/experiment_id sets already
    #'   exist in the application cache before calling this function. If they
    #'   do, extract that data and only call this on what is missing.
    #' @param callback a function to call with the metadata from the Rank
    #'   Response API. The first parameter of the callback must be `metadata`.
    #'   The callback will be called with the metadata from the Rank Response
    #'   API as the first parameter, and any additional parameters passed to
    #'   this function as additional parameters.
    #' @param ... additional parameters to pass to the callback
    read = function(callback = function(metadata, data, storr, ...) {
                      list(metadata, data)
                    }, ...) {
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

      if (length(setdiff(c(
        "promotersetsig_id",
        "expression_id"
      ), names(private$.params))) > 0) {
        stop("`promotersetsig_id` and `expression_id` must be in params")
      }

      additional_args <- list(...)

      cached_result <- private$.storr_get(paste(
        self$params[["promotersetsig_id"]],
        self$params[["expression_id"]],
        sep = "_"
      ))

      if (!is.null(cached_result)) {
        do.call(callback, c(
          list(
            metadata = cached_result$metadata,
            data = cached_result$data,
            storr = self$storr
          ),
          additional_args
        ))
      } else {
        future::future(
          {
            print(params)
            output <- get_data_from_api(
              url,
              header,
              params
            )

            # Call the callback with the response
            do.call(callback, c(
              list(
                metadata = output$metadata,
                data = output$data,
                storr = storr
              ),
              additional_args
            ))
          },
          globals = list(
            url = self$url,
            header = self$header,
            params = as.list(self$params),
            get_data_from_api = .get_data_from_api,
            callback = callback,
            storr = self$storr,
            additional_args = additional_args
          ),
          packages = c("futile.logger", "httr", "jsonlite", "dplyr")
        )
      }
    }
  ),
  private = list(
    .is_valid_url = function(url) {
      response <- tryCatch(
        {
          httr::HEAD(Sys.getenv("PROMOTERSETSIG_URL"), self$header)
        },
        error = function(e) {
          paste0(
            "httr::HEAD failure -- could not call function with url: ",
            url, " and header: ", private$.header
          )
        }
      )
      if (!is.null(response) && httr::status_code(response) == 200) {
        return(TRUE)
      } else {
        stop("Invalid URL or token provided: ", httr::content(response))
      }
    }
  )
)

.get_data_from_api <- function(url, header, params) {
  # Log API call details
  futile.logger::flog.debug(paste0(
    "rankresponse_url: ", url,
    "; params: ", as.character(params)
  ))

  # Create a new tempfile in the instance's tempdir
  tar_file <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tar_file), add = TRUE)

  # Make the API call
  response <- httr::GET(url,
    header,
    query = params
  )

  # write response content to tar_file
  writeBin(httr::content(response, "raw"), tar_file)

  # Check if the response is successful
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve data: ", httr::content(response, "text"))
  }
  # extract the metadata json file
  metadata <- .extract_rr_tarball_metadata(tar_file)
  metadata$cache_key <- paste(
    metadata$promotersetsig_id,
    metadata$expression_id,
    sep = "_"
  )

  data_list <- lapply(
    metadata$filename,
    function(x) .extract_rr_tarball_csv(tar_file, x)
  )
  names(data_list) <- metadata$cache_key

  list(metadata = metadata, data = data_list)
}

#' Read in the Metadata from a Rank Response Tarball
#'
#' @param tar_file A tarball file containing, at this point, only one
#'   CSV file
#'
#' @importFrom futile.logger flog.debug
#'
#' @return A dataframe of metadata with information on the contents of the
#'   Rank Response tarball
.extract_rr_tarball_metadata <- function(tar_file,
                                         metadata_filename = "metadata.json") {
  # List the contents of the tar file
  tar_contents <- untar(tar_file, list = TRUE)
  futile.logger::flog.debug(paste("tar_contents: ", tar_contents))

  # Check if 'metadata.json' is in the tar file
  if (!metadata_filename %in% tar_contents) {
    stop("metadata.json not found in tarball")
    # if metadata.json does exist, read it in as a dataframe and return
  }

  # create a tempfile path for the extracted json file
  # and ensure that it is destroyed on exit
  metadata_dir <- tempfile()
  dir.create(metadata_dir)
  on.exit(unlink(metadata_dir, recursive = TRUE), add = TRUE)

  # untar the metadata.json file to the tempfile path
  untar(tar_file, files = metadata_filename, exdir = metadata_dir)

  # read in the json file, convert to a tibble
  dplyr::bind_rows(
    jsonlite::fromJSON(file.path(
      metadata_dir,
      metadata_filename
    )),
    .id = "expression_id"
  )
}

#' Read in a CSV File from a Rank Response Tarball
#'
#' @param tar_file A tarball file containing, at this point, only one
#'   CSV file
#' @param filename The name of the CSV file to read from the tarball
#'
#' @importFrom futile.logger flog.debug
#' @importFrom readr read_csv
#'
#' @return A dataframe with the contents of the specified
#'   CSV file from the tarball
.extract_rr_tarball_csv <- function(tar_file, filename) {
  # List the contents of the tar file
  tar_contents <- untar(tar_file, list = TRUE)
  futile.logger::flog.debug(paste("tar_contents: ", tar_contents))

  # Check if the specified filename is in the tar file
  if (!filename %in% tar_contents) {
    stop(paste(filename, "not found in tarball"))
  }

  # create a tempfile path for the extracted CSV file
  # and ensure that it is destroyed on exit
  csv_dir <- tempfile()
  dir.create(csv_dir)
  on.exit(unlink(csv_dir, recursive = TRUE), add = TRUE)

  # untar the specified CSV file to the tempfile path
  untar(tar_file, files = filename, exdir = csv_dir)

  # read in the CSV file and return it as a dataframe
  readr::read_csv(file.path(csv_dir, filename))
}
