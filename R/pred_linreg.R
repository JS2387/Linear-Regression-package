#' @title Generic Method for pred()
#'
#' @param object input containing model data
#'
#' @export
pred <- function(object, ...) UseMethod("pred")

#' @title linreg method for pred()
#'
#' @param object input of the class \code{linreg} containing model data
#'
#' @export
pred.linreg <- function(object, ...) {
                        as.vector(object$Fittedvalues)
}



