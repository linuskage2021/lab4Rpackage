
#' Coefficeients of the estimated regression model
#'
#' @param object is a linreg object
#' @param ... Extra stuff
#' @return Returns the estimated coefficients of the fitted linear model
#' @export
#'
#' @examples
#' model <- linreg(Sepal.Length ~ Species, data = iris)
#' coef(model)
coef.linreg <- function(object, ...){


  return(object[["beta"]])

}
