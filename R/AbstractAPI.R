#' @title AbstractAPI R6 Class
#'
#' @description This class provides a template for creating API clients that
#'   require token authentication. It additionally provides a method of
#'   connecting to a redis server for caching API responses. API params are
#'   validated against a list of valid keys.
#'
#' @family API
#'
#' @importFrom R6 R6Class
#' @importFrom httr add_headers HEAD status_code
#' @importFrom redux hiredis
#' @importFrom futile.logger flog.info flog.debug
#' @importFrom uuid UUIDgenerate
#'
#' @examples
#' api_instance <- AbstractAPI$new(url = "https://api.example.com/resource")
AbstractAPI <- R6::R6Class("AbstractAPI",
  public = list(
    #' @description Initializes the API client
    #' @param url The API endpoint URL.
    #' @param token The authentication token. Defaults to the `TOKEN`
    #'   environment variable.
    #' @param params A ParamsList object containing parameters for the
    #'   API request.
    #' @param tmpdir A path to the temporary directory in which the instance
    #'   will create a subdirectory to store downloaded data.
    #' @param redis_host The host for the Redis connection.
    #' @param redis_port The port for the Redis connection.
    #'
    #' @return A new `AbstractAPI` object.
    initialize = function(url = "",
                          token = Sys.getenv("TOKEN"),
                          params = ParamsList(),
                          tmpdir = tempdir(),
                          redis_host = "localhost",
                          redis_port = 6379) {
      # log the instances unique ID
      futile.logger::flog.info(class(self)[1], " instance ID: ", private$.id)

      # create a temporary directory for the instance in the session tempdir.
      # This will be used to store any files downloaded from the API
      private$.set_temp_dir()

      # set private attributes
      if (!is.null(token)) {
        self$x <- token
      }
      if (!is.null(url)) {
        self$url <- url
      }
      if (!is.null(params)) {
        self$push_params(params)
      }
      if (!is.null(redis_host) && !is.null(redis_port)) {
        self$redis <- list(host = redis_host, port = redis_port)
      } else if (is.null(redis_host) != is.null(redis_port)) {
        stop("Both `redis_host` and `redis_port` must be provided, or neither.")
      }

      # register a finalizer. This is called when the object is garbage
      # collected, or prior to the end of the session. To manually call this
      # function, `rm(instance); gc()` will trigger the finalizer.
      reg.finalizer(self,
        function(e) {
          futile.logger::flog.info(
            "Removing all data for ",
            class(self)[1], ", ID: ", private$.id
          )
          unlink(private$.temp_dir, recursive = TRUE)
        },
        onexit = TRUE
      )
    },

    #' @description Adds or updates parameters in the ParamsList.
    #' @param params A list of key-value pairs to add or update.
    push_params = function(params) {
      private$.validate_params(params)
      private$.params <- push(private$.params, params)
    },

    #' @description Removes parameters from the ParamsList.
    #' @param keys A vector of keys to remove. If NULL, all parameters
    #'   are cleared.
    pop_params = function(keys) {
      if (is.null(keys)) {
        private$.params <- clear(private$.params)
      } else {
        private$.params <- pop(private$.params, keys)
      }
    },

    #' @description Placeholder for the create method. Raises an error if
    #'   not implemented.
    #' @param data The data to create.
    create = function(data) {
      stop(paste("`create()` is not implemented for", class(self)[1]))
    },

    #' @description Placeholder for the read method. Raises an error if
    #'    implemented.
    #' @param id The ID of the resource to read.
    read = function() {
      stop(paste("`read()` is not implemented for", class(self)[1]))
    },

    #' @description Placeholder for the update method. Raises an error if
    #'   not implemented.
    #' @param id The ID of the resource to update.
    #' @param data The data to update.
    update = function(id, data) {
      stop(paste("`update()` is not implemented for", class(self)[1]))
    },

    #' @description Placeholder for the delete method. Raises an error if
    #'   not implemented.
    #' @param id The ID of the resource to delete.
    delete = function(id) {
      stop(paste("`delete()` is not implemented for", class(self)[1]))
    }
  ),
  active = list(
    #' @field id A unique identifier for the instance
    id = function() {
      private$.id
    },

    #' @field token The authentication token for the API
    token = function(value) {
      if (missing(value)) {
        private$.token
      } else {
        private$.token <- value
      }
    },

    #' @field header The http authorization header
    header = function() {
      httr::add_headers(
        "Authorization" = paste("token", private$.token, sep = " "),
        "Content-Type" = "application/json"
      )
    },

    #' @field url The URL for the API. When this is set,
    #'   it is checked for validity using the current token/header
    url = function(value) {
      if (missing(value)) {
        private$.url
      } else {
        tryCatch(
          {
            private$.is_valid_url(url)
            private$.url <- url
          },
          error = function(e) {
            stop(
              "Check the URL. If token authentication is required, check ",
              "that, too. You may need to set the token with `set_token()` ",
              "before setting the url.\n\n", e$message
            )
          }
        )
      }
    },

    #' @field redis When a redis host AND port are passed, a `hiredis`
    #'   connection is created. To set this property, pass in a list with
    #'   structure `list(host = "localhost", port = "6379")`
    redis = function(value) {
      if (missing(value)) {
        private$.redis
      } else {
        if (length(value) == 2 && !is.null(value$host) && !is.null(value$port)) {
          tryCatch(
            {
              private$.redis <- redux::hiredis(
                host = value$host,
                port = value$port
              )
            },
            error = function(e) {
              stop("Error setting redis connection: ", e$message)
            }
          )
        } else {
          stop("Both `host` and `port` must be provided. in a named list")
        }
      }
    },

    #' @field params The ParamsList object containing parameters for the
    #'   API request.
    params = function() {
      private$.params
    },

    #' @field valid_param_keys A character vector of valid parameter keys
    #'   for the API.
    valid_param_keys = function(value) {
      if (missing(value)) {
        private$.valid_param_keys
      } else {
        if (!is.character(value)) {
          stop("valid_param_keys must be a character vector")
        }
        private$.valid_param_keys <- value
      }
    },

    #' @field temp_dir The temporary directory in which the instance will
    #'   create a subdirectory to store downloaded data. If the instance is
    #'   `rm()`'d, the temp_dir will be removed before the garbage collector
    #'   is called. This can be forced with `rm(instance); gc()`. The temp_dir
    #'   will also be automatically removed if the session ends.
    temp_dir = function(value) {
      if (missing(value)) {
        private$.temp_dir
      } else {
        # Create the private$.temp_dir directory
        if (!dir.exists(value)) {
          stop("The directory provided does not exist: ", value)
        }
        temp_dir <- tempfile(pattern = private$.id, tmpdir = value)
        tryCatch(
          {
            futile.logger::flog.debug(
              paste0("Creating temp_dir at: ", temp_dir)
            )
            dir.create(temp_dir)
            private$.temp_dir <- temp_dir
          },
          error = function(e) {
            stop(
              "Error creating temp directory at: ",
              temp_dir, ". Error message: ", e$message
            )
          }
        )
      }
    }
  ),
  private = list(
    .id = uuid::UUIDgenerate(),
    .url = NULL,
    .token = NULL,
    .header = NULL,
    .params = NULL,
    .temp_dir = NULL,
    .redis = NULL,
    .valid_param_keys = c(),

    # Validates that the value being passed into the params attribute is a
    # list, and that the keys are a subset of the valid_param_keys
    .validate_params = function(params) {
      if (!is.list(params)) {
        stop("params must be a list")
      }

      invalid_keys <- setdiff(names(params), private$.valid_param_keys)
      if (length(invalid_keys) > 0) {
        stop(
          "Invalid parameter keys provided: ",
          paste(invalid_keys, collapse = ", ")
        )
      }
    },

    # confirms that the url is valid and that the header authorization is
    # appropriate
    .is_valid_url = function(url) {
      response <- tryCatch(
        {
          httr::HEAD(url, self$header)
        },
        error = function(e) {
          paste0(
            "httr::HEAD failure -- could not call function with url: ",
            url, " and header: ", self$header
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
