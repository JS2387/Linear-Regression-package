% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linreg.R
\name{linreg}
\alias{linreg}
\title{Linear Regression}
\usage{
linreg(formula, data)
}
\arguments{
\item{formula}{object of class \code{formula}}

\item{data}{a data table or \code{data.frame} containing the variables of the model}
}
\value{
returns an object of \code{class linreg}, which contains all the regression outputs and mimics a list
}
\description{
Runs a simple linear regression over a formula and data set
}
\examples{
linreg(Petal.Length ~ Species, iris)
linreg(speed ~ dist, cars)
}
