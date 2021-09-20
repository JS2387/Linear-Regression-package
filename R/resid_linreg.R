#' @title Generic Method for resid()
#'
#' @param object input containing model data
#'
#' @export
resid <- function(object, ...) UseMethod("resid")

#' @title linreg method for resid()
#'
#' @param object input of the class \code{linreg} containing model data
#'
#' @export
resid.linreg <- function(object, ...) {
  as.vector(object$Residuals)
}



