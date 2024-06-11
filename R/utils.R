#' Validate a value as an Integer
#'
#' This function validates a value as an integer. If the value is numeric, it
#'   checks that it is an integer with no decimal values. If the value is a
#'   character, it checks that it is a numeric string with no decimal values.
#'   If the value is not numeric or character, it throws an error.
#'
#' @param x A numeric or character value representing an integer.
#'
#' @return An integer value.
#'
#' @examples
#' 5L == validate_integer_value(5)   ## TRUE
#' 5L == validate_integer_value("5") ## TRUE
#' validate_integer_value("5.0")     ## Error: value must be an integer with no decimal values.
#'
#' @export
validate_integer_value <- function(x){
  if (is.numeric(x)) {
    if (x %% 1 != 0) {
      stop("x must be an integer with no decimal values.")
    }
  } else if (is.character(x)) {
    if (!grepl("^[0-9]+$", x)) {
      stop("x must be a numeric string with no decimal values.")
    }
    x <- as.numeric(x)
  } else {
    stop("x must be a numeric or character value representing an integer.")
  }
  as.integer(x)
}

#' On Disk GET
#'
#' This function is a wrapper around httr::GET that writes the response to disk
#'
#' @param url the URL to GET
#' @param headers the headers to include in the request
#' @param params the query parameters to include in the request
#' @param tar_file the file to write the response to
#'
#' @return the response object
#'
#' @importFrom httr GET write_disk
#'
#' @note this will stop/raise an error if the tar_file directory doesn't exist,
#'   or the GET request fails
on_disk_get <- function(url, headers, tar_file, params = list()) {
  if(!dir.exists(dirname(tar_file))) {
    stop("The directory containing the `tar_file`: ", dirname(tar_file), " does not exist")
  }
  futile.logger::flog.debug(paste0('url: ', url,
                                   "; params: ",
                                   paste(names(params), params, sep = ": ", collapse = ", ")))
  tryCatch({
    httr::GET(url, headers, httr::write_disk(tar_file, overwrite = TRUE), query = params)
    }, error = function(e) {
      stop("Error getting rank response table: ", e$message)
    })
}

#' A helper function for httr::GET
#'
#' This function is a wrapper around httr::GET that writes the response to
#'   memory
#'
#' @param url the full url to the API endpoint
#' @param headers the headers to include in the request. Should be an
#'   httr::headers object
#' @param params the query parameters to include in the request. As a list
#'   where the name is the parameter name and the value is the parameter
#'   value
#'
in_memory_get <- function(url, headers, params = list()){
  futile.logger::flog.debug(paste0('url: ', url,
                                   "; params: ",
                                   paste(names(params), params, sep = ": ", collapse = ", ")))
  tryCatch({
    httr::GET(url, headers, query = params)
    }, error = function(e) {
      stop("Error getting rank response table: ", e$message)
    })

}

