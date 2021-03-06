#' Reasons which shot to select given basic cricket strategy.
#'
#' Reasons which shot the shooter will select based on a basic cricket strategy.
#' The shooter
#'    (1) checks if behind in points
#'    (2) if not behind, then shoots the highest bed or bull
#'    (3) if behind, then shooter the highest open bed of the opponent
#'
#' @param gm object created from the set_cricket_game
#' @param shooter_player_id either 1 or 2 given the shooter
#' @param opponent_player_id either 1 or 2, but should not be the shooter's id
#'
#' @return name of the target
#'
#' @export
select_target_basic_cricket <- function(gm,
                                        shooter_player_id,
                                        opponent_player_id,
                                        point_opponent_at_one_turn = TRUE) {

  # If game has a winner, stop.
  if (!is.na(gm$winner)) {
    stop
  }

  # Set if shooter should point opponent if the opponent is within one turn.
  #point_opponent_at_one_turn <- FALSE

  # Set player indicies
  sidx <- set_player_id(shooter_player_id)
  oidx <- set_player_id(opponent_player_id)

  # Extract the state of each player's marks.
  sh_marks <- gm[[sidx]]$marks
  op_marks <- gm[[oidx]]$marks

  # Extract the state of each player's points.
  sh_points <- gm[[sidx]]$points
  op_points <- gm[[oidx]]$points

  # Extract the state of each player's turns to finish
  sh_turns_to_finish <- gm[[sidx]]$turns_to_finish
  op_turns_to_finish <- gm[[oidx]]$turns_to_finish

  # Extract the state of each player's turn differential
  sh_turns_ahead <- gm[[sidx]]$turns_ahead
  op_turns_ahead <- gm[[oidx]]$turns_ahead


  # Detect whether the shooter is trailing in points.
  is_trailing_points <- sh_points < op_points

  # Detect whether the shooter is trailing in points.
  shoot_points <- sh_points < op_points

  # If the opponent is within one turn of winning, should we shoot points to
  # increase the opponent's number of turns.
  if (point_opponent_at_one_turn) {
    if (op_turns_to_finish == 1 & nrow(op_marks[open == TRUE]) > 0) {
      shoot_points <- TRUE
    }
  }

  # If shooter is not trailing then shoot the highest open bed, then bull if
  # no bed available.
  if (!shoot_points) {
    for (i in 1:nrow(sh_marks)) {
      if(sh_marks$open[i]) {
        if (sh_marks$bed_value[i] != 25) {
          if (sh_marks$count[i] == 2 & op_marks$count[i] == 3) {
            # If the shooter needs only a single, then shoot only a single
            return(target_name <- paste("Single", sh_marks$bed_name[i],
                                        sep = " "))
          } else {
            # Alway shoot a treble if the opponent is open.
            return(target_name <- paste("Treble", sh_marks$bed_name[i],
                                        sep = " "))
          }
        } else {
          # Shoot Bullseye if everything else is closed.
          return(target_name <- "Double Bull")
        }
      }
    }
  } else {
    # If the shooter is trailing in points, then find the next highest open
    # bed of the opponent.
    for (i in 1:nrow(op_marks)) {
      if (op_marks$open[i]) {
        if (op_marks$bed_value[i] != 25) {
          # Alway shoot a treble if the opponent is open.
          return(target_name <- paste("Treble", op_marks$bed_name[i],
                                      sep = " "))
        } else {
          # Shoot Bullseye if everything else is closed.
          return(target_name <- "Double Bull")
        }
      }
    }
  }

}
