#' ParamsList S3 Class
#'
#' An S3 class to manage a list of parameters with methods to push, pop,
#'   print, and clear parameters.
#' @rdname ParamsList
#'
#' @param params A list of parameters to initialize the ParamsList object.
#'
#' @return An object of class `ParamsList`.
#' @export
#' @examples
#' # Create an instance of ParamsList
#' params <- ParamsList()
#'
#' # Push a single key/value pair
#' params <- push(params, "key1", "value1")
#' print(params)
#'
#' # Push multiple key/value pairs
#' params <- push(params, list(key2 = "value2", key3 = "value3"))
#' print(params)
#'
#' # Pop a single key/value pair
#' popped_value <- pop(params, "key1")
#' print(popped_value)
#' print(params)
#'
#' # Pop multiple key/value pairs
#' popped_values <- pop(params, c("key2", "key3"))
#' print(popped_values)
#' print(params)
#'
#' # Clear all parameters
#' params <- clear(params)
#' print(params)
ParamsList <- function(params = list()) {
  if (!is.list(params)) {
    stop("params must be a list")
  }
  structure(list(params = params), class = "ParamsList")
}

#' Push Parameters into ParamsList
#'
#' Adds a list of key/value pairs to the ParamsList object. If a key already exists, its value is updated.
#' @family ParamsList-methods
#' @rdname ParamsList
#'
#' @param object An object of class `ParamsList`.
#' @param params A list of key/value pairs to be added or updated in the ParamsList object.
#'
#' @return The updated `ParamsList` object.
#' @export
push.ParamsList <- function(object, params) {
  if (!is.list(params)) {
    stop("params must be a list of key/value pairs")
  }
  for (k in names(params)) {
    object$params[[k]] <- params[[k]]
  }
  object
}

#' Pop Parameters from ParamsList
#'
#' Removes and returns the value(s) associated with the provided key(s) from
#'   the ParamsList object.
#' @family ParamsList-methods
#' @rdname ParamsList
#'
#' @param object An object of class `ParamsList`.
#' @param keys A single key as a string or a list of keys.
#'
#' @return A list of key/value pairs that were removed.
#' @export
pop.ParamsList <- function(object, keys) {
  if (missing(keys)) {
    stop("Keys must be provided")
  }
  if (is.character(keys)) {
    keys <- list(keys)
  }
  if (!is.list(keys)) {
    stop("Keys must be a character vector or a list of keys")
  }
  values <- list()
  for (key in keys) {
    values[[key]] <- object$params[[key]]
    object$params[[key]] <- NULL
  }
  values
}

#' Print ParamsList
#'
#' Prints the current state of the ParamsList object.
#' @family ParamsList-methods
#' @rdname ParamsList
#'
#' @param object An object of class `ParamsList`.
#'
#' @export
print.ParamsList <- function(object) {
  cat("Current params:\n")
  for (key in names(object$params)) {
    cat(sprintf("%s: %s\n", key, object$params[[key]]))
  }
}

#' Clear ParamsList
#'
#' Clears all parameters from the ParamsList object.
#' @family ParamsList-methods
#' @rdname ParamsList
#'
#' @param object An object of class `ParamsList`.
#'
#' @return The cleared `ParamsList` object.
#' @export
clear.ParamsList <- function(object) {
  object$params <- list()
  object
}

