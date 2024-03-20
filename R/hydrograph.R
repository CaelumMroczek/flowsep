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
  p <- ggplot(data, aes(x = data[,1])) +
    geom_line(aes(y = data[,2]), color = "black") +
    geom_line(aes(y = data[,3]), color = "red") +
    theme_classic()+
    xlab("Date")+
    ylab("Discharge")

  return(p)
}
