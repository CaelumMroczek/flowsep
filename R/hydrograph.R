#' Plot a Hydrograph
#'
#' @param data data table -
#'
#' @return a plotted hydrograph
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' hydrograph(GreenRiver)
hydrograph <- function(data){
  p <- ggplot2::ggplot(data, ggplot2::aes(x = data[,1], y = data[,2]))+
    ggplot2::geom_line()+
    ggplot2::theme_classic()
  p
  }
