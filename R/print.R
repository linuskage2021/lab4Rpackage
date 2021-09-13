#' Print method for linreg classes
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
print.linreg <- function(object){


  cat("Call: \n", "linreg(formula = ",
      as.character(object[["formula"]]),
      ", data = ", as.character(object[["data"]]), ")", "\n", "\n", sep = "")


  cat("Coefficients: \n")
  print(object[["beta"]])

}
