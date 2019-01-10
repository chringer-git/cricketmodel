#' From the target name set the coordinates to shoot
#'
#' @param target name of the target
#' @param target_dt data.table of targets created from translate_targets
#'
#' @return cartesian coordiates of the target
#'
#' @export
set_target <- function(target, target_dt) {

  # Load libraries
  library(data.table)

  # Find shot in the "targets" data.table
  shot <- target_dt[target_name == target]

  # Build coordinates to target.
  coords <- list()
  coords$x <- shot$target_x
  coords$y <- shot$target_y

  # Return coordinates.
  coords

}
