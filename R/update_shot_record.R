udpate_shot_record <- function(gm) {

  shot_record <- data.table::data.table(
    "player_1_20_count" = gm$p1$marks[bed_name == "20"]$count,
    "player_1_19_count" = gm$p1$marks[bed_name == "19"]$count,
    "player_1_18_count" = gm$p1$marks[bed_name == "18"]$count,
    "player_1_17_count" = gm$p1$marks[bed_name == "17"]$count,
    "player_1_16_count" = gm$p1$marks[bed_name == "16"]$count,
    "player_1_15_count" = gm$p1$marks[bed_name == "15"]$count,
    "player_1_bull_count" = gm$p1$marks[bed_name == "Bull"]$count,
    "player_1_20_open" = gm$p1$marks[bed_name == "20"]$open,
    "player_1_19_open" = gm$p1$marks[bed_name == "19"]$open,
    "player_1_18_open" = gm$p1$marks[bed_name == "18"]$open,
    "player_1_17_open" = gm$p1$marks[bed_name == "17"]$open,
    "player_1_16_open" = gm$p1$marks[bed_name == "16"]$open,
    "player_1_15_open" = gm$p1$marks[bed_name == "15"]$open,
    "player_1_bull_open" = gm$p1$marks[bed_name == "Bull"]$open,
    "player_1_points" = gm$p1$points,
    "player_1_marks_to_finish" = gm$p1$marks_to_finish,
    "player_1_marks_ahead" = gm$p1$marks_ahead,
    "player_1_darts_to_finish" = gm$p1$darts_to_finish,
    "player_1_darts_ahead" = gm$p1$darts_ahead,
    "player_1_turns_to_finish" = gm$p1$turns_to_finish,
    "player_1_turns_ahead" = gm$p1$turns_ahead,
    "player_2_20_count" = gm$p2$marks[bed_name == "20"]$count,
    "player_2_19_count" = gm$p2$marks[bed_name == "19"]$count,
    "player_2_18_count" = gm$p2$marks[bed_name == "18"]$count,
    "player_2_17_count" = gm$p2$marks[bed_name == "17"]$count,
    "player_2_16_count" = gm$p2$marks[bed_name == "16"]$count,
    "player_2_15_count" = gm$p2$marks[bed_name == "15"]$count,
    "player_2_bull_count" = gm$p2$marks[bed_name == "Bull"]$count,
    "player_2_20_open" = gm$p2$marks[bed_name == "20"]$open,
    "player_2_19_open" = gm$p2$marks[bed_name == "19"]$open,
    "player_2_18_open" = gm$p2$marks[bed_name == "18"]$open,
    "player_2_17_open" = gm$p2$marks[bed_name == "17"]$open,
    "player_2_16_open" = gm$p2$marks[bed_name == "16"]$open,
    "player_2_15_open" = gm$p2$marks[bed_name == "15"]$open,
    "player_2_bull_open" = gm$p2$marks[bed_name == "Bull"]$open,
    "player_2_points" = gm$p2$points,
    "player_2_marks_to_finish" = gm$p2$marks_to_finish,
    "player_2_marks_ahead" = gm$p2$marks_ahead,
    "player_2_darts_to_finish" = gm$p2$darts_to_finish,
    "player_2_darts_ahead" = gm$p2$darts_ahead,
    "player_2_turns_to_finish" = gm$p2$turns_to_finish,
    "player_2_turns_ahead" = gm$p2$turns_to_finish,
    "current_turns" = gm$turns
    )

  shot_record <- data.table::data.table(
    rbind(gm$shot_record,
          shot_record)
  )

  # Return shot record

}
