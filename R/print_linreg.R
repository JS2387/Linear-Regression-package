#' @title Generic Method for print()
#' @param x input to be printed of the class linreg
#' @param ... other inputs to be printed
#' @export
print <- function(x, ...) UseMethod("print")

#' @title linreg method for print()
#' @param x input of the class \code{linreg}
#' @param ... other inputs to be printed
#' @export
print.linreg <- function(x, ...) {
    #if (is.null(x$Call)) stop("Model not defined")
    #if (is.null(x$Regressioncoeff)) stop("Coefficients missing")
    cat("Call: \n")
    print(x$Call)
    cat("\n", "Coefficients: ", "\n", "\n")
    print(t(x$Regressioncoeff))
}
