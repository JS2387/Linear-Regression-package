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
                          summary_result <- data.frame(round(object$Regressioncoeff,5),
                                                            round(sqrt(diag(object$VarofRC)),5),
                                                            round(object$tvals,5),
                                                            round(object$p_vals,5),
                                                            sapply(object$p_vals,
                                                                    function(x) if(x < 0.001) {"***"}
                                                                    else if (x < 0.01) {"**"}
                                                                    else if (x < 0.05) {"*"}
                                                                    else if (x < 0.1) {"."}
                                                                    else {" "})
                                                      )
                          colnames(summary_result) <- c("Estimate", "Std. Error", "t value", "p value","")
                          cat("Coefficients: \n")
                          print(summary_result)
                          cat("\nResidual standard error:", sqrt(object$ResidualVar), "on", object$DegFreedom, "degrees of freedom")
}
