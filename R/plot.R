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
  data_package<-data.frame(fits=object[["fits"]], res=object[["residuals"]], standardized_residual = sqrt(abs(object[["residuals"]]/sd(object[["residuals"]]))))

  p1<-ggplot2::ggplot(data_package,aes(x=object[["fits"]], y=object[["residuals"]]))+
    geom_point(shape=21)+ggtitle("Residual vs Fitted")+ xlab(paste("Fitted values \n lm(Petal.Length ~ Species)"))+
    ylab("Residuals")
  p2<-ggplot2::ggplot(data_package,aes(x=object[["fits"]], y=sqrt(abs(object[["residuals"]]/sd(object[["residuals"]])))))+
    geom_point(shape=21)+ggtitle("Scale - Location")+ xlab(paste("Fitted values \n lm(Petal.Length ~ Species)"))+
    ylab("|Standaridized residuals|")

  p1<-p1+ggplot2::stat_summary(fun.y=median, colour="red", geom="line")+
    theme(plot.title = element_text(hjust = 0.5))
  p2<-p2+ggplot2::stat_summary(fun.y=median, colour="red", geom="line")+
    theme(plot.title = element_text(hjust = 0.5))

  print(p1)
  print(p2)
}
