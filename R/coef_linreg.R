#' @title Generic Method for coef()
#' @param object input containing model data
#' @param ... additional input to the coef function
#' @export
coef <- function(object, ...) UseMethod("coef")

#' @title linreg method for coef()
#' @param object input of the class \code{linreg} containing model data
#' @param ... additional input to the coef function
#' @export
coef.linreg <- function(object, ...) {
  coeffs <- object$Regressioncoeff
  coeffs
}
