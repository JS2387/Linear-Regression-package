#' @title Generic Method for plot()
#'
#' @param x object containing model data to be used for plotting
#' @param y the y coordinates for plotting
#'
#' @importFrom ggplot2
#'             latex2exp
#'
#' @export
plot <- function (x, y, ...) UseMethod("plot")

#' @title linreg Method for plot()
#'
#' @param x object containing model data to be used for plotting
#' @param y the y coordinates for plotting
#'
#' @importFrom ggplot2
#'             latex2exp
#'
#' @export
plot.linreg <- function (x, y, ...) {
                        # Create the DF used for plotting
                        library(latex2exp)
                        library(ggplot2)
                        error_hat <- x$Residuals
                        y_hat <- x$Fittedvalues
                        df_1 <- data.frame(error_hat , y_hat)

                        # Smooth variable to create the desired plot
                        smoothed <- stats::lowess(x = y_hat, y = error_hat)

                        # Create the first plot
                        plot_res_vs_fitted <- ggplot(df_1 , aes(x=y_hat , y=error_hat) )+
                          geom_point(shape = 1 , size=4) +
                          geom_line(aes(x = smoothed$x, y = smoothed$y), color = "red") +
                          ggtitle("Residuals vs Fitted") +
                          theme(plot.title = element_text(hjust = 0.5))+
                          ylab("Residuals")+
                          xlab("Fitted values")+
                          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                          theme(plot.margin=unit(c(2,2,2.5,1.2),"cm"))
                        plot_res_vs_fitted

                        # SECOND PLOT THING SCALE LOCATION

                        # Calculate standardised residuals

                        standard_res <- error_hat / sd(error_hat)
                        standard_res_sqrt <- sqrt(abs(standard_res))
                        smoothed_2 <- stats::lowess(x = y_hat, y = standard_res_sqrt)


                        #Second plotting DF

                        df_2 <- data.frame(y_hat , standard_res_sqrt )


                        plot_scale_location  <- ggplot(df_2 , aes(x=y_hat , y= standard_res_sqrt ))+
                          geom_point(shape = 1 , size=4) +
                          geom_line(aes(x = smoothed_2$x, y = smoothed_2$y), color = "red") +
                          ggtitle("Scale-Location") +
                          theme(plot.title = element_text(hjust = 0.5))+
                          ylab(TeX(r"(\sqrt{Standardized residuals})"))+
                          xlab("Fitted values")+
                          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                          theme(plot.margin=unit(c(2,2,2.5,1.2),"cm"))

                        plot_scale_location

}








