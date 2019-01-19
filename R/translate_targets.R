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
  bradius_min <- rep(db$bed_values$min_radius[2:4], 20)
  bradius_max <- rep(db$bed_values$max_radius[2:4], 20)
  bradius <- rep(db$bed_values$aim_radius[2:4], 20)
  bval <- rep(db$bed_thetas$bed_value, 3)
  btheta <- rep(db$bed_thetas$bed_mid_theta, 3)
  btheta_min <- rep(db$bed_thetas$bed_lower_theta, 3)
  btheta_max <- rep(db$bed_thetas$bed_upper_theta, 3)

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
    "radius_min" = bradius_min,
    "radius_max" = bradius_max,
    "theta_min" = btheta_min,
    "theta_max" = btheta_max
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
      "radius_min" = c(db$bull_values$min_radius),
      "radius_max" = c(db$bull_values$max_radius),
      "theta_min" = c(-pi / 20, -pi / 20),
      "theta_max" = c(2 * pi - pi / 20, 2 * pi - pi / 20)
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
      "radius_min" = 6 + 7/8,
      "radius_max" = Inf,
      "theta_min" = -pi / 20,
      "theta_max" = 2 * pi - pi / 20
      )
    )

  # Return the targets.
  targets

}
