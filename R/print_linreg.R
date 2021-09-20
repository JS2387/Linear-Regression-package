#' @title Generic Method for print()
#'
#' @param x input to be printed
#'
#' @export
print <- function(x, ...) UseMethod("print")

#' @title linreg method for print()
#'
#' @param x input of the class \code{linreg}
#'
#' @export
print.linreg <- function(x, ...) {
    if (is.null(x$Call)) stop("Model not defined")
    if (is.null(x$Regressioncoeff)) stop("Coefficients missing")

    cat("Call:", "\n", "linreg(formula = ", as.character(x$Call)[2], as.character(x$Call)[1], as.character(x$Call)[3], ")", "\n", "\n",
        "Coefficients: ", "\n", "\n",
        unclass(t(x$Regressioncoeff)))

}
