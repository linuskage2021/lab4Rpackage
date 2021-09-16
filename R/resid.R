#' Residuals of a linereg class model
#'
#' @param object
#'
#' @return Vector with residuals
#' @export
#'

residuals.linreg <- function(object, ...){

  return(object[["residuals"]])


}
