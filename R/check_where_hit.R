#' Checks where the coordinates of a dart lands on the dartboard in terms of
#' which scoring wedge it hits.
#'
#' @param x is the 'x' coordinate of the shot
#' @param y is the 'y' coordinate of the shot
#' @param db is a dartbaord object created from the 'dartboard' package.
#'
#' @return the name of the location of where the shot landed (e.g., Treble 20)
#'
#' @export
check_where_hit <- function(x, y, db) {

  # Get the polar coordinates of the shot.
  pol <- cart_to_pol(x, y)

  # Find the mark given the polar coordinates.
  # First check if the shot is in the Bullseye.
  if(pol$radius < max(db$bull_values$max_distance)) {
    bed_name <- db$bull_values[pol$radius < max_distance &
                               pol$radius >= min_distance]$bull_label
  } else {
    # If not the Bullseye, then find the bed, and if it hit a single, double,
    # or treble.
    bed_name <- db$bed_angles[pol$theta < bed_upper_angle &
                              pol$theta >= bed_lower_angle]$bed_labels
    bed_mlt <- db$bed_values[pol$radius < max_distance &
                             pol$radius >= min_distance]$bed_multiplier_label

    # If we return nothing, then we missed the board.
    if (length(bed_mlt) == 0) {
      return("Missed the board")
      }

    # Format name of where dart hit.
    bed_name <- paste(bed_mlt, bed_name, sep = " ")
  }

  # Return the bed name
  bed_name

}
