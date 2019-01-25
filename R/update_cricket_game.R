#' Updates cricket object
#'
#' @param gm cricket game object
#' @param shooter_player_id number either 1 or 2
#' @param opponent_player_id number either 1 or 2, but not shooter id
#' @param shot_hit where the shot landed
#' @param targets_dt data.table of dartboard coordiantes
#'
#' @return udpated game object
#'
#' @export
update_cricket_game <- function(gm, shooter_player_id, opponent_player_id,
                        shot_hit, targets_dt, dart_num) {

  # Extract the bed name and shot details
  # bed_name <- str_split_fixed(string = shot_hit, pattern = " ", n = 2)[[2]]
  shot     <- targets_dt[target_name == shot_hit]

  # Set player indicies
  sidx <- set_player_id(shooter_player_id)
  oidx <- set_player_id(opponent_player_id)

  # Extract each player's marks that are associate with the actual shot.
  sh_marks <- gm[[sidx]]$marks
  op_marks <- gm[[oidx]]$marks
  op_marks <- op_marks[bed_value == shot$target_bed_value]
  sh_marks <- sh_marks[bed_value == shot$target_bed_value]

  # If no targets exist in the cricket game, then the shot missed and return the
  # state of the game as it existed prior to the shot.
  if (nrow(op_marks) == 0) {
    # Update the state of the game
    #gm$game_state <- update_game_state(gm, sidx, dart_num)

    return(gm)
  }

  # If the target is within the cricket game, then look at the following
  # scenarios:
  # 1. If the (sh)ooter and the (op)ponent are closed
  # 2. If (sh)ooter is open and (op)ponent is closed, then update the the
  #    (sh)ooter's marks to no more than 3.
  # 3. If the (op) is open, then all marks count. Make sure you don't go over
  #    3 marks, and any additional marks will be turned into points.

  # Shooter and opponent are closed
  if (!sh_marks$open & !op_marks$open) {
    # Update the state of the game
    # gm$game_state <- update_game_state(gm, sidx, dart_num)

    # Return state of game.
    return(gm)

  } else if (sh_marks$open & !op_marks$open) {
    # Shooter is open, but Opponent is closed, find all marks that apply.
    marks_hit <- shot$target_multiplier
    marks_cur <- sh_marks$count
    new_marks_cur <- min(3, marks_cur + marks_hit)

    # Apply new marks amd if applicable marks = 3, then close the bed.
    gm[[sidx]]$marks[bed_value == shot$target_bed_value]$count <- new_marks_cur
    if (new_marks_cur == 3) {
      gm[[sidx]]$marks[bed_value == shot$target_bed_value]$open <- FALSE
    }

    # Update the state of the game
    #gm$game_state <- update_game_state(gm, sidx, dart_num)

    # Check to see if all the shooter's marks are closed and is not behind in
    # points. If the shooter has completed the game, set game winner.
    if (!any(gm[[sidx]]$marks$open) & gm[[sidx]]$points >= gm[[oidx]]$points) {
      gm$winner <- sidx
    }

    # Set all correct symbols.
    gm[[sidx]]$marks$symbol <- set_symbol(gm[[sidx]]$marks$count)

    gm$scoreboard <- data.table::data.table(
      "Player Red" = c(gm$p1$marks$symbol, gm$p1$points),
      " " = c(gm$p1$marks$bed_name, "Points"),
      "Player Black" = c(gm$p2$marks$symbol, gm$p2$points)
    )

    # Return state of game.
    return(gm)

  } else if (op_marks$open) {
    # The opponent's bed is open, so all marks count for close and/or points.
    marks_hit <- shot$target_multiplier
    marks_cur <- sh_marks$count
    new_marks_cur <- min(3, marks_cur + marks_hit)

    # If the shots marks and the current hit are greater than 3, then take the
    # difference as points and close the number.
    points <- max(0, (marks_hit - (3 - marks_cur)) * shot$target_bed_value)
    gm[[sidx]]$marks[bed_value == shot$target_bed_value]$count <- new_marks_cur
    gm[[sidx]]$points <- gm[[sidx]]$points + points
    if (new_marks_cur == 3) {
      gm[[sidx]]$marks[bed_value == shot$target_bed_value]$open <- FALSE
    }

    # Update the state of the game
    #gm$game_state <- update_game_state(gm, sidx, dart_num)

    # Check to see if all the shooter's marks are closed and is not behind in
    # points. If the shooter has completed the game, set game winner.
    if (!any(gm[[sidx]]$marks$open) & gm[[sidx]]$points >= gm[[oidx]]$points) {
      gm$winner <- sidx
    }

    # Set all correct symbols.
    gm[[sidx]]$marks$symbol <- set_symbol(gm[[sidx]]$marks$count)

    gm$scoreboard <- data.table::data.table(
      "Player Red" = c(gm$p1$marks$symbol, gm$p1$points),
      " " = c(gm$p1$marks$bed_name, "Points"),
      "Player Black" = c(gm$p2$marks$symbol, gm$p2$points)
    )

    # Return state of game.
    return(gm)

  }

}
