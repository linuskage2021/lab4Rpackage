#' Pred
#'
#' @param x is a linreg object
#'
#' @return Returns the fitted values of the regression model on the training data
#' @export
#'
#' @examples
#' model <- linreg(Sepal.Length ~ Species, data = iris)
#' pred(model)
#'
pred.linreg <- function(x){

  return(x[["fits"]])

}
