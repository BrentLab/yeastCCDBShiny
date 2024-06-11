#' RankResponseAPI R6 Class
#'
#' An R6 class to interact with the Rank Response API.
#'
#' @description
#' Retrieves rank response data from the database.
#'
#' @family API
#'
#' @importFrom R6 R6Class
#' @importFrom futile.logger flog.debug
#'
#' @return A RankResponseAPI object
RankResponseAPI <- R6::R6Class("RankResponseAPI",
  inherit = AbstractAPI,
  public = list(
    metadata = list(),
    redis = NULL, # Redis connection
    #' @description
    #' Initializes the RankResponseAPI client with a URL, token, and optional parameters.
    #' @inheritParams AbstractAPI$initialize
    initialize = function(url = Sys.getenv("RANK_RESPONSE_URL"),
                          ...) {
      super$set_temp_dir(temp_dir)

      super$set_redis(redis_host, redis_port)

      super$initialize(url, token, params)
    },

    #' @description
    #' Asynchronously retrieve rank response data from the database.
    #' @param callback A function to call when the data has been retrieved. The first argument of the callback should be `self`.
    #' @return A promise that resolves when the data has been retrieved and the metadata updated.
    read = function(callback = NULL) {
      if (!"promotersetsig_id" %in% private$params) {
        stop("`promotersetsig_id` must be set in the params")
      }

      # Create a unique hash of the current parameter set including the URL
      param_hash <- hash(c(private$params, list(url = private$url, caller = private$id)))
      callback_id <- UUIDgenerate()

      # Check if this parameter set has been requested before
      cached_result <- self$redis$GET(param_hash)
      if (!is.null(cached_result)) {
        cached_result <- fromJSON(cached_result)
        if (cached_result$status == "completed") {
          futile.logger::flog.debug("Using cached result for params: ", private$params)
          if (!is.null(callback) && is.function(callback)) {
            callback(self, cached_result$tar_file)
          }
          return(invisible(cached_result$metadata))
        } else {
          # Add callback to Redis to be executed later
          self$redis$RPUSH(callback_id, toJSON(list(class_name = class(self), tar_file = cached_result$tar_file)))
          return(invisible(NULL))
        }
      }

      tar_file <- tempfile(tmpdir = private$temp_dir, fileext = ".tar")

      # Mark the entry in Redis as in progress and store the callback
      self$redis$SET(param_hash, toJSON(list(tar_file = tar_file, metadata = NULL, status = "in-progress", callback_id = callback_id)))
      self$redis$RPUSH(callback_id, toJSON(list(class_name = class(self), tar_file = tar_file)))

      # Execute the HTTP GET request asynchronously
      future({
        tryCatch(
          {
            # Log API call details
            futile.logger::flog.debug(paste0(
              "rankresponse_url: ",
              private$url,
              "; params: ",
              paste(names(private$params),
                private$params,
                sep = ": ",
                collapse = ", "
              )
            ))
            # Make the API call
            httr::GET(private$url,
              private$header,
              httr::write_disk(tar_file, overwrite = TRUE),
              query = private$params
            )
          },
          error = function(e) {
            stop("Error in Rank Response table request: ", e$message)
          }
        )
      }) %...>% (function(response) {
        if (is.null(response) || httr::status_code(response) != 200) {
          stop(
            "Error getting rank response table: ",
            httr::status_code(response), "; message: ",
            httr::content(response, "text")
          )
        }

        self$metadata$tar_file <- private$params

        # Update the Redis cache with the result and metadata, marking it as completed
        self$redis$SET(param_hash, toJSON(list(tar_file = tar_file, metadata = self$metadata, status = "completed", callback_id = callback_id)))

        # Execute the callback
        callback_data <- self$redis$LRANGE(callback_id, 0, -1)
        for (cb in callback_data) {
          cb <- fromJSON(cb)
          if (!is.null(callback) && is.function(callback)) {
            callback(self, cb$tar_file)
          }
        }
        self$redis$DEL(callback_id) # Clean up callback storage

        return(self$metadata)
      })
    }
  )
)

#' Read in the Metadata from a Rank Response Tarball
#'
#' @param tar_file A tarball file containing, at this point, only one
#'   CSV file
#'
#' @importFrom futile.logger flog.debug
#'
#' @return A dataframe of metadata with information on the contents of the
#'   Rank Response tarball
.extract_rr_tarball_metadata <- function(tar_file, metadata_filename = "metadata.json") {
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
  dplyr::bind_rows(jsonlite::fromJSON(file.path(metadata_dir, metadata_filename)),
    .id = "expression_id"
  )
}
