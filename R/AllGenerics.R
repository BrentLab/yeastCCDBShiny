#' Generic pop function
#'
#' This is the generic pop function.
#' @param x An object.
#' @param ... Additional arguments.
pop <- function(object, ...) {
  UseMethod("pop")
}

#' Generic push function
#'
#' This is the generic push function.
#' @param x An object.
#' @param ... Additional arguments.
push <- function(object, ...) {
  UseMethod("push")
}

#' Generic clear function
#'
#' This is the generic clear function.
#' @param x An object.
#' @param ... Additional arguments.
clear <- function(object, ...) {
  UseMethod("clear")
}
