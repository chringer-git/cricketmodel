#' Translates dartboard into data.table of targets, coordinates, and values.
#'
#' @param db dartboard object
#'
#' @return data.table of target coordinates
#'
#' @export
translate_targets <- function(db) {

  # Build vectors of bed data to place in data.table
  bmul_lab <- rep(db$bed_values$bed_multiplier_label[2:4], 20)
  bmul <- rep(db$bed_values$bed_multiplier[2:4], 20)
  bmag_min <- rep(db$bed_values$min_distance[2:4], 20)
  bmag_max <- rep(db$bed_values$max_distance[2:4], 20)
  bmag <- rep(db$bed_values$aim_distance[2:4], 20)
  bval <- rep(db$bed_angles$bed_value, 3)
  bang <- rep(db$bed_angles$bed_mid_angle, 3)
  bang_min <- rep(db$bed_angles$bed_lower_angle, 3)
  bang_max <- rep(db$bed_angles$bed_upper_angle, 3)

  # Create cartesian coordiates to target given a target name.
  target_x <- c()
  target_y <- c()
  for (i in 1:length(bmul)) {
    x <- get_coordinates(db = db, bed_name = bval[i], bed_mlt = bmul[i])$x
    y <- get_coordinates(db = db, bed_name = bval[i], bed_mlt = bmul[i])$y
    target_x <- c(target_x, x)
    target_y <- c(target_y, y)
  }

  # Join all data for beds into a data.table
  targets <- data.table::data.table(
    "target_name" = paste(bmul_lab, bval, sep = " "),
    "target_value" = bmul * bval,
    "target_multiplier" = bmul,
    "target_bed_value" = bval,
    "target_x" = target_x,
    "target_y" = target_y,
    "magnitude_min" = bmag_min,
    "magnitude_max" = bmag_max,
    "angle_min" = bang_min,
    "angle_max" = bang_max
    )
  targets <- targets[order(targets$target_bed_value, targets$target_value)]

  # Add data for bullseye rings.
  targets <- rbind(
    targets,
    data.table::data.table(
      "target_name" = c("Single Bull", "Double Bull"),
      "target_value" = c(25, 50),
      "target_bed_value" = c(25, 25),
      "target_multiplier" = c(1, 2),
      "target_x" = c(0, 0),
      "target_y" = c(0, 0),
      "magnitude_min" = c(db$bull_values$min_distance),
      "magnitude_max" = c(db$bull_values$max_distance),
      "angle_min" = c(-pi / 20, -pi / 20),
      "angle_max" = c(2 * pi - pi / 20, 2 * pi - pi / 20)
      )
    )

  # Add data for when a shot misses the board.
  targets <- rbind(
    targets,
    data.table::data.table(
      "target_name" = "Missed the board",
      "target_value" = 0,
      "target_bed_value" = 0,
      "target_multiplier" = 1,
      "target_x" = NA,
      "target_y" = NA,
      "magnitude_min" = 6 + 7/8,
      "magnitude_max" = Inf,
      "angle_min" = -pi / 20,
      "angle_max" = 2 * pi - pi / 20
      )
    )

  # Return the targets.
  targets

}
