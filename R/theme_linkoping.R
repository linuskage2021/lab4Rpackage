
#' ggplot theme for Linköping university
#'
#' @return Adds linkoping theme to ggplot2's plot
#' @export

theme_linkoping<-function(){
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#00b9e7", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis.text = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(colour = "black"),
    panel.grid.major = ggplot2::element_line(colour = "grey"),
    plot.title = ggplot2::element_text(hjust = .5)
  )

}
