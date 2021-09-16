#' Show two scatter plots.The first one fitted values against Residuals.
#' The second scatter plot show square root of standardized residuals against fitted values
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
plot.linreg = function(object){

  data_package<-data.frame(fits=object[["fits"]],
                           res=object[["residuals"]],
                           standardized_residual = sqrt(abs(object[["residuals"]]/stats::sd(object[["residuals"]]))))

  p1<-ggplot2::ggplot(data_package,ggplot2::aes(x=object[["fits"]], y=object[["residuals"]])) +
    ggplot2::geom_point(shape=21) +
    ggplot2::ggtitle("Residual vs Fitted") +
    ggplot2::xlab(paste("Fitted values \n lm(Petal.Length ~ Species)")) +
    ggplot2::ylab("Residuals")

  p2<-ggplot2::ggplot(data_package,ggplot2::aes(x=object[["fits"]],
                                       y=sqrt(abs(object[["residuals"]]/stats::sd(object[["residuals"]]))))) +
    ggplot2::geom_point(shape=21) +
    ggplot2::ggtitle("Scale - Location") +
    ggplot2::xlab(paste("Fitted values \n lm(Petal.Length ~ Species)"))+
    ggplot2::ylab("|Standaridized residuals|")

  p1<-p1 + ggplot2::stat_summary(fun.y=stats::median, colour="red", geom="line") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  p2<-p2 + ggplot2::stat_summary(fun.y= stats::median, colour="red", geom="line")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  print(p1)
  print(p2)
}
