#' @title Generic Method for summary()
#'
#' @param object input containing model data
#'
#' @export
summary <- function(object, ...) UseMethod("summary")

#' @title linreg method for summary()
#'
#' @param object input of the class \code{linreg} containing model data
#'
#' @export
summary.linreg <- function(object, ...) {
                          summary_result <- data.frame(object$Regressioncoeff,
                                                            object$Residuals,
                                                            object$tvals,
                                                            object$p_vals)
                          print(summary_result)
                          cat("\n Degrees of Freedom: ", object$DegFreedom)
                          cat("\n Residual Variance: ", object$ResidualVar)
}
