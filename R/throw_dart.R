#' Given a target, finds the actual shot.
#'
#' @param target_x x coordinate of the target
#' @param target_y y coordinate of the target
#' @param sd_factor standard deviation
#' @param random_seed random seed
#'
#' @return coordinates of shot hit
#'
#' @export
throw_dart <- function(target_x, target_y, sd_factor, random_seed) {

  # Load libraries
  library(data.table)

  # Set random seed if you want to reproduce an exmaple.
  if(!missing(random_seed)){
    set.seed(random_seed)
    }

  # Given the target, find the actual placement of the shot.
  if (missing(sd_factor)) {
    sd_factor <- 1
    }
  x <- target_x + (8.875/3) * rnorm(1, mean = 0, sd = sd_factor)
  y <- target_y + (8.875/3) * rnorm(1, mean = 0, sd = sd_factor)

  # Build a list for the placement of the shot.
  coords <- list()
  coords$x <- x
  coords$y <- y

  # Return coordinates.
  coords

}
