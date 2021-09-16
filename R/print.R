#' Print method for linreg classes
#'
#' @param x is a linreg object
#' @param ... extra stuff
#'
#' @return Returns the coefficients and the formula of the fitted model.
#' @export
#'
#' @examples
#' model <- linreg(Sepal.Length ~ Species, data = iris)
#' print(model)
print.linreg <- function(x, ...){


  cat("Call: \n", "linreg(formula = ",
      as.character(x[["formula"]]),
      ", data = ", as.character(x[["data"]]), ")", "\n", "\n", sep = "")


  cat("Coefficients: \n")
  print(x[["beta"]])

}
