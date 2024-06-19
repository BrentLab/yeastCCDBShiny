#' #'
#' #' @importFrom R6 R6Class
#' #'
#' ExpressionAPI <- R6::R6Class(
#'   classname = "ExpressionAPI",
#'   inherit = AbstractAPI,
#'   public = list(
#'     initialize = function() {
#'       # initialization code here
#'       #
#'       # if this inherits, you can call parent functions
#'       # super$initialize(...)
#'     }
#'   ),
#'   # private variables here. Recommend putting private variations with '.'
#'   # and then configuring getter/setter in the `active` section
#'   private = list(),
#'   active = list(),
#'   lock_objects = TRUE,
#'   class = TRUE,
#'   portable = TRUE,
#'   lock_class = FALSE,
#'   cloneable = TRUE,
#'   parent_env = parent.frame()
#' )
