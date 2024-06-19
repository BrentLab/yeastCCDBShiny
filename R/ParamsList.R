#' ParamsList S3 Class
#'
#' An S3 class to manage a list of parameters with methods to print and clear parameters.
#' @rdname ParamsList
#'
#' @param params A list of parameters to initialize the ParamsList object.
#'
#' @return An object of class `ParamsList`, sorted by name.
#' @examples
#' # Create an instance of ParamsList
#' params <- ParamsList(list(a = 1, b = 2))
#' print(params)
#'
#' # Use [ and [[ to extract elements
#' print(params['a'])
#' print(params[['a']])
#'
#' # Use [<- and [[<- to assign elements
#' params['c'] <- 3
#' print(params)
#'
#' params[c('d', 'e', 'f')] <- list(4, 5, 6)
#' print(params)
#'
#' params[['g']] <- 7
#' print(params)
ParamsList <- function(params = list()) {
  if (!is.list(params)) {
    stop("params must be a named list")
  }
  if (length(params) > 0 && is.null(names(params))) {
    stop("params must be a named list")
  }
  sorted_params <- params[sort(names(params))]
  structure(list(params = sorted_params), class = "ParamsList")
}

#' Extract element from ParamsList using `[`
#'
#' @family ParamsList-methods
#'
#' @param x An object of class `ParamsList`.
#' @param i The name(s) of the parameter(s) to extract.
#' @param ... Additional arguments (not used).
#'
#' @return A ParamsList object with the subset of parameters.
`[.ParamsList` <- function(x, i, ...) {
  ParamsList(x$params[i])
}

#' Extract element from ParamsList using `[[`
#'
#' @family ParamsList-methods
#'
#' @param x An object of class `ParamsList`.
#' @param i The name of the parameter to extract.
#' @param ... Additional arguments (not used).
#'
#' @return The value of the parameter.
`[[.ParamsList` <- function(x, i, ...) {
  x$params[[i]]
}

#' Assign element to ParamsList using `[<-`
#'
#' @family ParamsList-methods
#'
#' @param x An object of class `ParamsList`.
#' @param i The name(s) of the parameter(s) to assign.
#' @param value The value(s) to assign to the parameter(s).
#'
#' @return The modified ParamsList object.
`[<-.ParamsList` <- function(x, i, value) {
  if (!is.null(value) && length(i) != length(value)) {
    stop("Length of names and values must match")
  }
  for (k in seq_along(i)) {
    if (is.null(value[[k]])) {
      x$params[[i[k]]] <- NULL
    } else {
      x$params[[i[k]]] <- value[[k]]
    }
  }
  x$params <- x$params[sort(names(x$params))]
  x
}

#' Assign element to ParamsList using `[[<-`
#'
#' @family ParamsList-methods
#'
#' @param x An object of class `ParamsList`.
#' @param i The name of the parameter to assign.
#' @param value The value to assign to the parameter.
#'
#' @return The modified ParamsList object.
`[[<-.ParamsList` <- function(x, i, value) {
  if (!is.character(i) || length(i) != 1) {
    stop("Only a single character string can be used as an index")
  }
  x$params[[i]] <- value
  x$params <- x$params[sort(names(x$params))]
  x
}

#' return the names of the parameters
#'
#' @family ParamsList-methods
#'
#' @param object An object of class `ParamsList`.
#'
#' @return A character vector of the sorted names of the parameters in the
#'   same order as the input `object`
names.ParamsList <- function(object) {
  names(object$params)
}

#' Print ParamsList
#'
#' Prints the current state of the ParamsList object.
#'
#' @family ParamsList-methods
#'
#' @param object An object of class `ParamsList`.
print.ParamsList <- function(object) {
  cat("Current params:\n")
  for (key in names(object$params)) {
    cat(sprintf("%s: %s\n", key, object$params[[key]]))
  }
}

#' Convert ParamsList to character
#'
#' Converts the current state of the ParamsList object to a character string.
#' @family ParamsList-methods
#'
#' @param x An object of class `ParamsList`.
#' @param ... Additional arguments (not used).
#'
#' @return A string representing the ParamsList object.
as.character.ParamsList <- function(x, ...) {
  paste(names(x$params),
        x$params,
        sep = ": ",
        collapse = ", ")
}

# Define the as.list method for ParamsList
#' Convert ParamsList to list
#'
#' Converts the current state of the ParamsList object to a list.
#'
#' @family ParamsList-methods
#'
#' @param x An object of class `ParamsList`.
#' @param ... Additional arguments (not used).
#'
#' @return A name sorted list representing the ParamsList object.
as.list.ParamsList <- function(x, ...) {
  x$params
}

#' Sort a ParamsList by name
#'
#' @family ParamsList-methods
#'
#' @param x An object of class `ParamsList`
#' @param decreasing A logical value indicating whether to sort
#'   in decreasing order
#'
#' @return A ParamsList object with the parameters sorted by name
sort.ParamsList <- function(x, decreasing = FALSE, ...) {
  if (!is.logical(decreasing) || length(decreasing) != 1) {
    stop("`decreasing` must be a single logical value")
  }

  sorted_names <- sort(names(x$params), decreasing = decreasing)
  x$params <- x$params[sorted_names]
  x
}
