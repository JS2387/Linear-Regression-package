#' @title Generic Method for coef()
#'
#' @param object input containing model data
#'
#' @export
coef <- function(object, ...) UseMethod("coef")

#' @title linreg method for coef()
#'
#' @param object input of the class \code{linreg} containing model data
#'
#' @export
coef.linreg <- function(object, ...) {
  coeffs <- object$Regressioncoeff
  coeffs
}


# CINSDSADSADASDASDBHASD GJKASHMDASB HJDMAS dbjkahda
# TEST INGSDFSDBHJSAD
# New comment
