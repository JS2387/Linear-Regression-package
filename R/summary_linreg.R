#' @title Generic Method for summary()
#' @param object input containing model data
#' @param ... other inputs to summary function
#' @export
summary <- function(object, ...) UseMethod("summary")

#' @title linreg method for summary()
#' @param object input of the class \code{linreg} containing model data
#' @param ... other inputs to summary function
#' @export
summary.linreg <- function(object, ...) {
                          summary_result <- data.frame(object$Regressioncoeff,
                                                            object$VarofRC,
                                                            object$tvals,
                                                            object$p_vals,
                                                            "***")
                          colnames(summary_result) <- c("Estimate", "Std. Error", "t value", "p value","")
                          cat("Coefficients: \n")
                          print(summary_result)
                          cat("\n Degrees of Freedom: ", object$DegFreedom)
                          cat("\n Residual Variance: ", object$ResidualVar)
}
