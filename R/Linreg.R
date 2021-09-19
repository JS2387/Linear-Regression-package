#' @title Linear Regression
#'
#' @description Runs a simple linear regression over a formula and data set
#'
#' @param formula object of class "formula"
#' @param data a data table or data frame with the variables of the model
#'
#' @return returns an object of class "linreg"
#'
#'
# Save the formula for the desired regression
formula <- Petal.Length ~ Species
# Load the Data we use
data <- iris

# Create the matrix
mm <- model.matrix(formula, data)
# The variables
vars <- all.vars(formula)

# The Y values
y <- data[, vars[1]]
# Get the Beta estimations
betas <- solve(t(mm) %*% mm) %*% t(mm) %*% y

# Get the Yhat values
y_hat <- mm %*% betas

#Error estimations
error_hat <- y - y_hat

#Calulate the degrees of freedom for further calculations
degrees_freedom <- length(y) - (dim(mm)[2] - 1)

#Calulate the Sigma^2_hat
res_variance <-  (t(error_hat ) %*% error_hat) / degrees_freedom

# Calulate Var(Beta^hat)
var_beta_hat <-  diag(( as.vector(res_variance) * solve(t(mm) %*% mm) ) / degrees_freedom )

# Calulate the T-values for each Coefficient
t_beta <- betas / sqrt(var_beta_hat)

#Calculate the p-values for each regression coefficient
p_vals <- pt(betas, degrees_freedom)

#classes for the output
setClass("linreg", slots = c(Regressioncoeff = "matrix",
                             Fittedvalues = "matrix",
                             Residuals = "matrix",
                             DegFreedom = "numeric",
                             ResidualVar = "matrix",
                             VarofRC = "numeric",
                             tvals = "matrix",
                             p_vals = "matrix"))

Reg_result <- new("linreg",
                  Regressioncoeff = betas,
                  Fittedvalues = y_hat,
                  Residuals = error_hat,
                  DegFreedom = degrees_freedom,
                  ResidualVar = res_variance,
                  VarofRC = var_beta_hat,
                  tvals = t_beta,
                  p_vals = p_vals)




# Create the DF used for plotting
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

# SECOND PLOT THING SCALE LOCATIOn

# Calulate standardised residuals

standard_res <- error_hat / sqrt(res_variance)


#Second plotting DF

df_2 <- data.frame(y_hat ,  )


plot_scale_location  <- ggplot(df_1 , aes(x=y_hat , y= sqrt(error_hat)) )+
  geom_point(shape = 1 , size=4) +
  geom_line(aes(x = smoothed$x, y = smoothed$y), color = "red") +
  ggtitle("Scale-Location") +
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Sqrt(Standardized residuals)")+
  xlab("Fitted values")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(plot.margin=unit(c(2,2,2.5,1.2),"cm"))

plot_scale_location
