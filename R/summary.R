#' Summary printout of the fitted regression model
#'
#' @param object is a linreg object
#' @param ... Extra stuff
#' @return The function returns summary statistics about the fitted models
#' @export
#'
#' @examples
#' model <- linreg(Sepal.Length ~ Species, data = iris)
#' summary(model)
summary.linreg <- function(object, ...){

  placehold_df <- data.frame(Estimate = object[["beta"]],
                             Std_Error = object[["error_betas"]],
                             t_value = object[["beta"]] / object[["error_betas"]])


  p_val <- (1 - stats::pt(abs(placehold_df$t_value), df = object[["df"]]))*2
  placehold_df$p_val <- p_val
  for(i in 1:nrow(placehold_df)){
    if (p_val[i] < 0.001){
      placehold_df$signif[i] <- "***"
    }else if(p_val[i] >= 0.001 & p_val[i] < 0.01){
      placehold_df$signif[i] <- "**"
    }else if(p_val[i] >= 0.01 & p_val[i] < 0.05){
      placehold_df$signif[i] <- "*"
    }else if(p_val[i] >= 0.05 & p_val[i] < 0.1){
      placehold_df$signif[i] <- "."
    }else{
      placehold_df$signif[i] <- ""
    }
  }

  names(placehold_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", " ")
  print(placehold_df)

  cat("\n")
  cat("Residual standard error:", sqrt(object[["res_var"]]), "on", object[["df"]], "degrees of freedom \n")


}
