#' Returns a dartboard plot made from dartboard::draw_dartboard() showing targets.
#'
#' @param xpt vector of "x" coordinates
#' @param ypt vector of "y" coordinates
#' @param db_plot dartboard::draw_dartboard() object
#' @param color is the shot's color
#'
#' @return dartboard::dart_dartboard() object showing shots
#'
#' @export
plot_shot <- function(xpt, ypt, db_plot, color) {

  # Build a data.table of the points to submit to ggplot2 object.
  d <- data.table::data.table("xpt" = xpt,
                              "ypt" = ypt)

  # Add the points to the plot.
  db_plot <- db_plot +
    ggplot2::geom_point(data = d, aes(x = xpt, y = ypt),
                        color = color, size = 2)

  # Return the dartboard plot.
  db_plot

}
