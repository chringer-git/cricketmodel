#' Create a player object given a description of the shooter's accruacy and
#' shot selection strategy
#'
#' @param average_marks_per_turn numeric value of a shooter's average marks shot in one turn
#' @param total_darts_501 numeric value of average total darts shot in a full game of 501
#' @param select_shot function that decides which target to shoot
#'
#' @return player object consisting of accuracy and target selection
#'
#' @export
make_player <- function(average_marks_per_turn, total_darts_501, select_shot) {

  # If shot selection function is missing use the basic strategy
  if (missing(select_shot)) {
    select_shot <- select_target_basic_cricket
  }

  # This is basically a place holder for setting the standard deviation by the
  # number of total dart to complete a 501 game.
  if (missing(total_darts_501)) {
    sd_factor <- 0.91
  }

  # Setting the standard deviation by the marks per turn.
  if (!missing(average_marks_per_turn)) {
    sd_factor <- 1.00871 * (1 / average_marks_per_turn) - 0.09929
  } else {
    sd_factor <- 0.91
  }

  # Build a list that describes the player's accuracy and strategy.
  player <- list()
  player$sd_factor <- sd_factor
  player$select_shot <- select_shot

  # Return player
  player

}
