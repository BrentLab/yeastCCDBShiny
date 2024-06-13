#' @importFrom foreach registerDoSEQ
#' @importFrom utils packageVersion
#' @importFrom futile.logger flog.threshold flog.info INFO WARN
.onLoad <- function(libname, pkgname) {
  # set default sequential backend for foreach %dopar%
  foreach::registerDoSEQ()
  # set the future plan to multisession
  future::plan(future::multisession)

  # # Set the log threshold to INFO, which means that INFO, WARNING, and ERROR
  # # messages will be logged, but DEBUG messages will not
  futile.logger::flog.threshold(futile.logger::DEBUG)
  #
  # # Log a message at the INFO level to indicate that the package has been loaded
  futile.logger::flog.debug(
    sprintf(
      "Package '%s' version %s loaded",
      pkgname,
      utils::packageVersion(pkgname)
    )
  )
}
