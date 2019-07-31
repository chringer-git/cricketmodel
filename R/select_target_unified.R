#' Reasons which shot to select given a pointing strategy.
#'
#' Reasons which shot the shooter will select based on a pointing strategy.
#' The shooter
#'    (1) checks if behind in points (this is modified so that the shooter must be three mark's worth of points ahead)
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
select_target <- function(gm,
                          shooter_player_id,
                          opponent_player_id,
                          point_opponent_at_one_turn = FALSE,
                          min_marks_ahead = 0) {

  # If game has a winner, stop.
  if (!is.na(gm$winner)) {
    stop
  }

  # Record reason for shot.
  reason <- ""

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

  # Keep one dart's value above opponent before moving on to close another
  # number.
  if (nrow(op_marks[open == TRUE]) > 0) {
    point_pad <- 0
    if (nrow(op_marks[open == TRUE & bed_name != "Bull"]) > 0) {
      point_pad <- max(point_pad,
                       min_marks_ahead * max(op_marks[open == TRUE & bed_name != "Bull"]$bed_value)
      )
    } else if (nrow(op_marks[open == TRUE & bed_name == "Bull"]) > 0) {
      point_pad <- max(point_pad,
                       round(min_marks_ahead/2 + 0.01, 0) * max(op_marks[open == TRUE & bed_name == "Bull"]$bed_value)
      )
    }
  } else {
    point_pad <- 0
  }

  # Detect whether the shooter is trailing in points.
  is_trailing_points <- sh_points < op_points

  # Detect whether the shooter is trailing in points.
  shoot_points <- sh_points < op_points + point_pad

  # If the opponent is within one turn of winning, should we shoot points to
  # increase the opponent's number of turns.
  if (point_opponent_at_one_turn) {
    if (op_turns_to_finish == 1 & nrow(op_marks[open == TRUE]) > 0) {
      shoot_points <- TRUE
    }
  }

  if (is_trailing_points & shoot_points) {
    reason <- "offense--close to shoot points"
  } else if (is_trailing_points & !shoot_points) {
    reason <- "offense--close bed"
  } else if (!is_trailing_points & shoot_points) {
    reason <- "defense--increase point lead"
  } else if (!is_trailing_points & !shoot_points) {
    reason <- "offense--close bed"
  }

  # If shooter is not trailing (given the desired pad) then shoot the highest
  # open bed, then bull if no bed available.
  if (!shoot_points) {
    for (i in 1:nrow(sh_marks)) {
      if(sh_marks$open[i]) {
        if (sh_marks$bed_value[i] != 25) {
          if (sh_marks$count[i] == 2 & op_marks$count[i] == 3) {
            # If the shooter needs only a single, then shoot only a single
            target_name <- paste("Single", sh_marks$bed_name[i], sep = " ")
            break
          } else {
            # Alway shoot a treble if the opponent is open.
            target_name <- paste("Treble", sh_marks$bed_name[i], sep = " ")
            break
          }
        } else {
          # Shoot Bullseye if everything else is closed.
          target_name <- "Double Bull"
          break
        }
      }
    }
  } else {
    # If the shooter is trailing in points, then find the next highest open
    # bed of the opponent.
    for (i in 1:nrow(op_marks)) {
      if (op_marks$open[i]) {
        if (sh_marks$open[i]) reason <- "offense--close bed"
        if (op_marks$bed_value[i] != 25) {
          # Alway shoot a treble if the opponent is open.
          target_name <- paste("Treble", op_marks$bed_name[i], sep = " ")
          break
        } else {
          # Shoot Bullseye if everything else is closed.
          target_name <- "Double Bull"
          break
        }
      }
    }
  }

  return(list("reason" = reason,
              "target_name" = target_name))

}
