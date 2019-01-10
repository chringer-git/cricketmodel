#' Forces an update to a player's score without simulating a throw.
#'
#' This is useful if you want to stage a specific scenario to simulate.
#'
#' @param gm a cricket game object initialized by set_cricket_game
#' @param player_id either 1 or 2 to signify which shooter's score will be updated
#' @param bed_val vector signifying which bed (20, 19, etc.). Bull = 25 in this case
#' @param num_marks vector aligned with bed_val that has the number of marks for the bed
#' @param points scalar of the number of point to apply to the shooter
#'
#' @return updated gm, a cricket game object, with the new score
#'
#' @export
force_score_cricket <- function(gm, player_id, bed_val, num_marks, points) {

  # Set player id
  sidx <- set_player_id(player_id)

  # Set the marks for each bed. The vectors should be
  # bed_val   = c(20, 19, 18, 17, 16, 15, 25)
  # num_marks = c(x1, x2, x3, x4, x5, x6, x7)
  if (!missing(num_marks)) {
    for (i in 1:length(num_marks)) {
      if (num_marks[i] >= 3) {
        num_marks[i] == 3
        gm[[sidx]]$marks[bed_value == bed_val[i]]$open <- FALSE
        }
      gm[[sidx]]$marks[bed_value == bed_val[i]]$count <- num_marks[i]
    }
  }

  # Set number of points
  if (!missing(points)) {
    gm[[sidx]]$points<- points
  }

  # Add some symbols better than the actual number of marks.
  gm[[sidx]]$marks$symbol <- set_symbol(gm[[sidx]]$marks$count)

  # Retun new game state.
  gm

}
