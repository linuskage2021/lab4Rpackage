#' Show two scatter plots.The first one fitted values against Residuals.
#' The second scatter plot show square root of standardized residuals against fitted values
#'
#' @param x is a linreg object
#' @param ... extra stuff
#' @return Produces two scatter plots, one for residuals against fitted values and standardized residuals against fits
#' @export
#'
#' @examples
#' model <- linreg(Sepal.Length ~ Species, data = iris)
#' plot(model)
plot.linreg = function(x, ...){


  fits <- x[["fits"]]
  res <- x[["residuals"]]
  standardized_residual <- sqrt(abs(x[["residuals"]]/stats::sd(x[["residuals"]])))


  data_package <- as.data.frame(cbind(fits,res,standardized_residual))
  base::colnames(data_package) <- c("fits", "res", "standardized_residual")

  index <- base::order(base::abs(x[["residuals"]]), decreasing = TRUE)[1:3]


  outlier_df <- data_package[index,]


  p1<-ggplot2::ggplot(data_package,ggplot2::aes(x=fits, y=res)) +
    ggplot2::geom_point(shape=21) +
    ggplot2::ggtitle("Residual vs Fitted") +
    ggplot2::xlab(paste("Fitted values \n lm(", x[["formula"]], ")", sep = "")) +
    ggplot2::ylab("Residuals") +
    ggplot2::geom_text(data = outlier_df, ggplot2::aes(x = fits, y = res, label = rownames(outlier_df)), hjust = -0.4, vjust = 0) +
    ggplot2::geom_point(data = outlier_df, ggplot2::aes(x = fits, y= res), shape = 21) +
    ggplot2::theme_bw()


  p2<-ggplot2::ggplot(data_package,ggplot2::aes(x=fits,
                                       y=standardized_residual)) +
    ggplot2::geom_point(shape=21) +
    ggplot2::ggtitle("Scale - Location") +
    ggplot2::xlab(paste("Fitted values \n lm(", x[["formula"]], ")", sep = ""))+
    ggplot2::ylab(expression(sqrt("|Standaridized residuals|"))) +
    ggplot2::geom_text(data = outlier_df, ggplot2::aes(x = fits, y = standardized_residual, label = rownames(outlier_df)), hjust = -0.4, vjust = 0) +
    ggplot2::geom_point(data = outlier_df, ggplot2::aes(x = fits, y= standardized_residual), shape = 21) +
    ggplot2::theme_bw()

  p1<-p1 + ggplot2::stat_summary(fun.y=stats::median, colour="red", geom="line") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p2<-p2 + ggplot2::stat_summary(fun.y= stats::median, colour="red", geom="line")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  print(p1)
  print(p2)
}
