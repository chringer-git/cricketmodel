#' Create a player object given a description of the shooter's accruacy and
#' shot selection strategy
#'
#' @param mpr numeric value of a shooter's average marks per round
#' @param total_darts_501 numeric value of average total darts shot in a full game of 501
#' @param select_shot function that decides which target to shoot
#'
#' @return player object consisting of accuracy and target selection
#'
#' @export
make_player <- function(mpr, total_darts_501, select_shot, min_marks_ahead) {

  # If shot selection function is missing use the basic strategy
  if (missing(select_shot)) {
    select_shot <- select_target
  }

  # This is basically a place holder for setting the standard deviation by the
  # number of total dart to complete a 501 game.
  if (missing(total_darts_501)) {
    sd_factor <- 0.5
  }

  # Setting the standard deviation by the marks per turn.
  if (!missing(mpr)) {
    sd_factor <- 0.7682 * (1 / mpr ^ (1/2)) - 0.2118
  } else {
    sd_factor <- 0.5
  }

  # Build a list that describes the player's accuracy and strategy.
  player <- list()
  player$sd_factor <- sd_factor
  player$select_shot <- select_shot
  player$min_marks_ahead <- min_marks_ahead

  # Return player
  player

}
