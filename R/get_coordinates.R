#' Find the cartesian coordinates of a shot give the name of the target.
#'
#' @param db dartboard object created from dartboard::create_dartboard()
#' @param bed_name name of the bed (the number of the bed or "Bull")
#' @param bed_mlt either "Single", "Double", or "Treble"
#'
#' @return target coordinates given the name of the target
#'
#' @export
get_coordinates <- function(db, bed_name, bed_mlt) {

  # If target is not in 1 thru 20, then it has to be the Bullseye.
  if (!bed_name %in% c(1:20)) {
    radius <- 0
    theta  <- 0
  } else {
    # If not the Bullseye, then set the target coordinates.
    radius <- max(db$bed_values[bed_multiplier == bed_mlt]$aim_distance)
    theta  <- db$bed_angles[bed_value == bed_name]$bed_mid_angle
  }

  # Build the data list.
  target_coords <- list()
  target_coords$x <- pol_to_cart(radius, theta)$x
  target_coords$y <- pol_to_cart(radius, theta)$y
  target_coords$radius <- radius
  target_coords$theta <- theta

  # Return coordinates
  target_coords

}
