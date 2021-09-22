#' @title Linear Regression
#' @description Runs a simple linear regression over a formula and data set
#' @usage linreg(formula, data)
#' @param formula object of class \code{formula}
#' @param data a data table or \code{data.frame} containing the variables of the model
#' @return returns an object of \code{class linreg}, which contains all the regression outputs and mimics a list
#' @examples
#' linreg(Petal.Length ~ Species, iris)
#' linreg(speed ~ dist, cars)
#' @export
linreg <- function(formula, data) {

  # Create the matrix
  mm <- model.matrix(formula, data)
  # The variables
  vars <- all.vars(formula)

  #fml for printing
  fml <- match.call()

  # The Y values
  y <- data[, vars[1]]

  # With solve.QR
  # solve.qr(qr(X), y)

  # With finding Q and R , a bit cooler! Use this
  Q = qr.Q(qr(mm))
  R = qr.R(qr(mm))

  # Get the Beta estimations
  betas <- solve(R) %*% t(Q) %*% y

  # Get the Yhat values
  y_hat <- mm %*% betas

  #Error estimations
  error_hat <- y - y_hat

  #Calculate the degrees of freedom for further calculations
  degrees_freedom <- nrow(mm) - ncol(mm)

  #Calculate the Sigma^2_hat
  res_variance <-  as.numeric(t(error_hat ) %*% error_hat) / degrees_freedom

  # Calculate Var(Beta^hat)
  var_beta_hat <-  res_variance * solve(t(mm) %*% mm)

  # Calculate the T-values for each Coefficient
  t_beta <- betas / sqrt(diag(var_beta_hat))

  #Calculate the p-values for each regression coefficient
  p_vals <- pt(-abs(t_beta), degrees_freedom)

  #store the result of the regression in 'linreg' class object
  Reg_result <- list(Call = fml,
                    Regressioncoeff = betas,
                    Fittedvalues = y_hat,
                    Residuals = error_hat,
                    DegFreedom = degrees_freedom,
                    ResidualVar = res_variance,
                    VarofRC = var_beta_hat,
                    tvals = t_beta,
                    p_vals = p_vals)


  return(class_constructor(Reg_result))
  }

