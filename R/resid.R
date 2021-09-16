#' Residuals of a linereg class model
#'
#' @param object is a linreg object
#' @param ... extra stuff
#' @return The function returns a vector with residuals of the fitted model
#' @export
#'
#' @examples
#' model <- linreg(Sepal.Length ~ Species, data = iris)
#' resid(model)

residuals.linreg <- function(object, ...){

  return(object[["residuals"]])


}
