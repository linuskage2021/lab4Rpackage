#' Print method for linreg classes
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.linreg <- function(x, ...){


  cat("Call: \n", "linreg(formula = ",
      as.character(x[["formula"]]),
      ", data = ", as.character(x[["data"]]), ")", "\n", "\n", sep = "")


  cat("Coefficients: \n")
  print(x[["beta"]])

}
