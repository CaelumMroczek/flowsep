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
#' t <- filterLH(GreenRiver)
#' hydrograph(t)
hydrograph <- function(data) {
  p <- ggplot2::ggplot(data, ggplot2::aes(x = data[,1])) +
    ggplot2::geom_line(ggplot2::aes(y = data[,2]), color = "black") +
    ggplot2::geom_line(ggplot2::aes(y = data[,3]), color = "red") +
    ggplot2::theme_classic()+
    ggplot2::xlab("Date")+
    ggplot2::ylab("Discharge")

  return(p)
}
