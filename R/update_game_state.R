#' Update the state of the game in the cricket game object.
#'
#' @param gm cricket game object
#' @param shooter_id shooter id either player 1 or palyer 2
#' @param dart_num the dart that is thrown
#'
#' @return a data.table with the state of the game
#'
#' @export
update_game_state <- function(gm, shooter_id, dart_num) {

  game_state <- data.table::data.table(
    "p1_20_count" = gm$p1$marks[bed_name == "20"]$count,
    "p1_19_count" = gm$p1$marks[bed_name == "19"]$count,
    "p1_18_count" = gm$p1$marks[bed_name == "18"]$count,
    "p1_17_count" = gm$p1$marks[bed_name == "17"]$count,
    "p1_16_count" = gm$p1$marks[bed_name == "16"]$count,
    "p1_15_count" = gm$p1$marks[bed_name == "15"]$count,
    "p1_bull_count" = gm$p1$marks[bed_name == "Bull"]$count,
    "p1_20_open" = gm$p1$marks[bed_name == "20"]$open,
    "p1_19_open" = gm$p1$marks[bed_name == "19"]$open,
    "p1_18_open" = gm$p1$marks[bed_name == "18"]$open,
    "p1_17_open" = gm$p1$marks[bed_name == "17"]$open,
    "p1_16_open" = gm$p1$marks[bed_name == "16"]$open,
    "p1_15_open" = gm$p1$marks[bed_name == "15"]$open,
    "p1_bull_open" = gm$p1$marks[bed_name == "Bull"]$open,
    "p1_points" = gm$p1$points,
    "p1_marks_to_finish" = gm$p1$marks_to_finish,
    "p1_marks_ahead" = gm$p1$marks_ahead,
    "p1_darts_to_finish" = gm$p1$darts_to_finish,
    "p1_darts_ahead" = gm$p1$darts_ahead,
    "p1_turns_to_finish" = gm$p1$turns_to_finish,
    "p1_turns_ahead" = gm$p1$turns_ahead,
    "p2_20_count" = gm$p2$marks[bed_name == "20"]$count,
    "p2_19_count" = gm$p2$marks[bed_name == "19"]$count,
    "p2_18_count" = gm$p2$marks[bed_name == "18"]$count,
    "p2_17_count" = gm$p2$marks[bed_name == "17"]$count,
    "p2_16_count" = gm$p2$marks[bed_name == "16"]$count,
    "p2_15_count" = gm$p2$marks[bed_name == "15"]$count,
    "p2_bull_count" = gm$p2$marks[bed_name == "Bull"]$count,
    "p2_20_open" = gm$p2$marks[bed_name == "20"]$open,
    "p2_19_open" = gm$p2$marks[bed_name == "19"]$open,
    "p2_18_open" = gm$p2$marks[bed_name == "18"]$open,
    "p2_17_open" = gm$p2$marks[bed_name == "17"]$open,
    "p2_16_open" = gm$p2$marks[bed_name == "16"]$open,
    "p2_15_open" = gm$p2$marks[bed_name == "15"]$open,
    "p2_bull_open" = gm$p2$marks[bed_name == "Bull"]$open,
    "p2_points" = gm$p2$points,
    "p2_marks_to_finish" = gm$p2$marks_to_finish,
    "p2_marks_ahead" = gm$p2$marks_ahead,
    "p2_darts_to_finish" = gm$p2$darts_to_finish,
    "p2_darts_ahead" = gm$p2$darts_ahead,
    "p2_turns_to_finish" = gm$p2$turns_to_finish,
    "p2_turns_ahead" = gm$p2$turns_ahead,
    "current_turns" = gm$turns,
    "shooter_id" = shooter_id,
    "dart_num" = dart_num
    )

  game_state <- data.table::data.table(
    data.table::rbindlist(
      gm$game_state,
      game_state
      )
    )

  # Return game_state
  game_state

}
