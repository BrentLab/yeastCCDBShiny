#' @title AbstractAPI R6 Class
#'
#' @description This class provides a template for creating API clients that
#'   require token authentication. It additionally provides a method of
#'   connecting to a storr object for caching API responses, validating params
#'   against a list of valid keys, and provides an interface for CRUD
#'   operations
#'
#' @family API
#'
#' @importFrom R6 R6Class
#' @importFrom httr add_headers HEAD status_code
#' @importFrom redux hiredis
#' @importFrom futile.logger flog.debug flog.info flog.error
#' @importFrom uuid UUIDgenerate
#' @importFrom storr storr driver_rds
#'
AbstractAPI <- R6::R6Class("AbstractAPI",
  public = list(
    #' @description Initializes the API client
    #'
    #' @param url The API endpoint URL.
    #' @param token The authentication token. Defaults to the `TOKEN`
    #'   environment variable.
    #' @param params A ParamsList object containing parameters for the
    #'   API request.
    #' @param queue_name This will be used to store data pulled from the
    #'   database in a cache for fast retrieval. Any instantiation with the
    #'   same `queue_name` will have access to the same cache. By default,
    #'   it will be set to the instances unique ID
    #' @param storr The storr object for caching API responses.
    #'
    #' @return A new `AbstractAPI` object.
    initialize = function(url = Sys.getenv("BASE_URL"),
                          token = Sys.getenv("TOKEN"),
                          valid_param_keys = NULL,
                          params = list(),
                          queue_name = NULL,
                          storr = NULL) {
      # log the instance's unique ID
      futile.logger::flog.info(paste0(
        class(self)[1],
        " instance ID: ",
        private$.id
      ))

      # initialize default values for certain private attributes
      private$.id <- uuid::UUIDgenerate()
      private$.params <- ParamsList()
      private$.valid_param_keys <- NULL

      if (!is.null(queue_name)) {
        self$queue_name <- queue_name
      } else {
        self$queue_name <- private$.id
      }

      if (!is.null(valid_param_keys)) {
        self$valid_param_keys <- valid_param_keys
      }

      if (token != "") {
        self$token <- token
      }
      if (url != "") {
        self$url <- url
      }
      if (!is.null(params)) {
        self$push_params(params)
      }

      if (!is.null(storr)) {
        self$storr <- storr
      } else {
        .tmpstor_local <- tempfile(paste0("storr_", self$queue_name))
        self$storr <- storr::storr(driver = storr::driver_rds(.tmpstor_local))
      }

      # register a finalizer. This is called when the object is garbage
      # collected, or prior to the end of the session. To manually call this
      # function, `rm(instance); gc()` will trigger the finalizer.
      reg.finalizer(self,
        function(e) {
          futile.logger::flog.info(
            paste("Garbage Collecting Object",
              class(self)[1], ",", "ID:", self$id,
              sep = " "
            )
          )
          # try to delete the local storr cache, if one was instantiated by
          # this class, when the gc() is called. Ignore errors, such as if
          # the .tmpstor_local variable doesn't exist.
          tryCatch(
            {
              unlink(.tmpstor_local, recursive = TRUE)
              futile.logger::flog.info(
                paste("storr tmpfile deleted for",
                  class(self)[1], ",", "ID:", self$id,
                  sep = " "
                )
              )
            },
            error = function(e) {}
          )
        },
        onexit = TRUE
      )
    },

    #' @description Adds or updates parameters in the ParamsList.
    #' @param params A list of key-value pairs to add or update.
    push_params = function(params) {
      private$.validate_params(params)
      private$.params[names(params)] = params
    },

    #' @description Removes parameters from the ParamsList.
    #' @param keys A vector of keys to remove. If NULL, the params are set to
    #'   an empty ParamsList()
    pop_params = function(keys) {
      if (missing(keys)) {
        private$.params <- ParamsList()
      } else {
        if (!is.character(keys)) {
          stop("keys must be a character vector")
        }
        for (k in keys){
          private$.params[k] <- NULL
        }
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
            private$.is_valid_url(value)
            private$.url <- value
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

    #' @field queue_name The name of the queue for storing data in the storr
    #'    cache. This is used to retrieve data from the cache. By default, it
    #'    is set to the instance's unique ID.
    queue_name = function(value) {
      if (missing(value)) {
        private$.queue_name
      } else {
        private$.queue_name <- value
      }
    },

    #' @field storr The storr object for caching API responses.
    #'   This can be set to a user-provided storr object, or a default
    #'   storr object will be created using the rds driver.
    storr = function(value) {
      if (missing(value)) {
        private$.storr
      } else {
        private$.storr <- value
      }
    },

    #' @field params The ParamsList object containing parameters for the
    #'   API request.
    params = function(value) {
      if (missing(value)) {
        private$.params
      } else {
        private$.params[value]
      }
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
    }
  ),
  private = list(
    .id = NULL,
    .url = NULL,
    .token = NULL,
    .header = NULL,
    .params = NULL,
    .queue_name = NULL,
    .storr = NULL,
    .valid_param_keys = NULL,

    # Validates that the value being passed into the params attribute is a
    # list, and that the keys are a subset of the valid_param_keys
    .validate_params = function(params) {
      if (!is.list(params)) {
        stop("params must be a list")
      }
      # test if params is a named list
      if (length(params) > 0 && is.null(names(params))) {
        stop("params must be a named list")
      }

      invalid_keys <- setdiff(names(params), private$.valid_param_keys)
      if (length(invalid_keys) > 0) {
        stop(
          "Invalid parameter keys provided: ",
          paste(invalid_keys, collapse = ", ")
        )
      }
      TRUE
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
    },

    # Get a value from the storr cache if storr is configured. Else, return
    # NULL. NULL is also returned if there is an error, but the error is
    # logged at the error level
    # the `default` is the value returned if the key is not found. NULL by
    # default
    .storr_get = function(key, default = NULL) {
      if (is.null(self$storr)) {
        futile.logger::flog.debug(".storr_get(): storr not configured")
        NULL
      } else {
        tryCatch(
          {
            self$storr$get(key, namespace = self$queue_name)
          },
          KeyError = function(e) {
            futile.logger::flog.error(paste(key, "does not exist in storr"))
            default
          },
          HashError = function(e) {
            futile.logger::flog.error(paste(
              "Underlying data has been deleted",
              " from the storr cache, likely by storage engine",
              "expire settings. Deleting the key.",
              e$message,
              sep = " "
            ))
            private$.storr_delete(key)
            default
          }
        )
      }
    },

    # Set a value in the storr cache if storr is configured. Else, return
    # NULL. NULL is also returned if there is an error, but the error is
    # logged at the error level
    .storr_set = function(key, value) {
      if (self$storr == NULL) {
        futile.logger::flog.debug(".storr_set(): storr not configured")
        NULL
      } else {
        tryCatch(
          {
            self$storr$set(key, value, namespace = self$queue_name)
          },
          error = function(e) {
            futile.logger::flog.error(paste("Error setting key: ",
              key,
              " in storr: ",
              e$message,
              sep = " "
            ))
            NULL
          }
        )
      }
    },
    .storr_list = function() {
      if (self$storr == NULL) {
        futile.logger::flog.debug(".storr_list(): storr not configured")
        NULL
      } else {
        tryCatch(
          {
            self$storr$list(namespace = self$queue_name)
          },
          error = function(e) {
            futile.logger::flog.error(paste("Error listing keys in storr: ",
              e$message,
              sep = " "
            ))
            NULL
          }
        )
      }
    },
    .storr_delete = function(key) {
      if (self$storr == NULL) {
        futile.logger::flog.debug(".storr_delete(): storr not configured")
        NULL
      } else {
        tryCatch(
          {
            self$storr$delete(key, namespace = self$queue_name)
            self$st$gc()
          },
          error = function(e) {
            futile.logger::flog.error(e$message)
            NULL
          }
        )
      }
    }
  )
)
