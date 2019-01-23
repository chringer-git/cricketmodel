#' Create a cricket game object
#'
#' @param p1 player object
#' @param p2 another player object
#'
#' @return cricket game object
#'
#' @export
set_cricket_game <- function(p1, p2) {

  # Initialize all marks to zero and apply a record of the marks to each player.
  marks <- data.table::data.table(
    "bed_value" = c(20, 19, 18, 17, 16, 15, 25),
    "bed_name" = c(20, 19, 18, 17, 16, 15, "Bull"),
    "open" = rep(TRUE, 7),
    "count" = rep(0, 7),
    "symbol" = rep("", 7)
    )
  p1$marks <- marks
  p2$marks <- marks

  # Initialize points to zero and apply a record to each player.
  p1$points <- 0
  p2$points <- 0

  # Initialize marks remaining to finish to 21 and apply a record to
  # each player.
  p1$marks_to_finish <- 21
  p2$marks_to_finish <- 21

  # Initialize difference in marks between each player to zero.
  p1$marks_ahead <- 0
  p2$marks_ahead <- 0

  # Initialize darts remaining to finish to 21 and apply a record to
  # each player.
  p1$darts_to_finish <- 8
  p2$darts_to_finish <- 8

  # Initialize difference in darts between each player to zero.
  p1$darts_ahead <- 0
  p2$darts_ahead <- 0

  # Initialize turns remaining to finish to 21 and apply a record to
  # each player.
  p1$turns_to_finish <- 3
  p2$turns_to_finish <- 3

  # Initialize difference in turns between each player to zero.
  p1$turns_ahead <- 0
  p2$turns_ahead <- 0

  # Initialize game winner
  game_winner <- NA

  # Initialize the total number of turns
  turns <- 0

  # Create game shot record
  game_state <- data.table::data.table(
    "p1_20_count" = p1$marks[bed_name == "20"]$count,
    "p1_19_count" = p1$marks[bed_name == "19"]$count,
    "p1_18_count" = p1$marks[bed_name == "18"]$count,
    "p1_17_count" = p1$marks[bed_name == "17"]$count,
    "p1_16_count" = p1$marks[bed_name == "16"]$count,
    "p1_15_count" = p1$marks[bed_name == "15"]$count,
    "p1_bull_count" = p1$marks[bed_name == "Bull"]$count,
    "p1_20_open" = p1$marks[bed_name == "20"]$open,
    "p1_19_open" = p1$marks[bed_name == "19"]$open,
    "p1_18_open" = p1$marks[bed_name == "18"]$open,
    "p1_17_open" = p1$marks[bed_name == "17"]$open,
    "p1_16_open" = p1$marks[bed_name == "16"]$open,
    "p1_15_open" = p1$marks[bed_name == "15"]$open,
    "p1_bull_open" = p1$marks[bed_name == "Bull"]$open,
    "p1_points" = p1$points,
    "p1_marks_to_finish" = p1$marks_to_finish,
    "p1_marks_ahead" = p1$marks_ahead,
    "p1_darts_to_finish" = p1$darts_to_finish,
    "p1_darts_ahead" = p1$darts_ahead,
    "p1_turns_to_finish" = p1$turns_to_finish,
    "p1_turns_ahead" = p1$turns_ahead,
    "p2_20_count" = p2$marks[bed_name == "20"]$count,
    "p2_19_count" = p2$marks[bed_name == "19"]$count,
    "p2_18_count" = p2$marks[bed_name == "18"]$count,
    "p2_17_count" = p2$marks[bed_name == "17"]$count,
    "p2_16_count" = p2$marks[bed_name == "16"]$count,
    "p2_15_count" = p2$marks[bed_name == "15"]$count,
    "p2_bull_count" = p2$marks[bed_name == "Bull"]$count,
    "p2_20_open" = p2$marks[bed_name == "20"]$open,
    "p2_19_open" = p2$marks[bed_name == "19"]$open,
    "p2_18_open" = p2$marks[bed_name == "18"]$open,
    "p2_17_open" = p2$marks[bed_name == "17"]$open,
    "p2_16_open" = p2$marks[bed_name == "16"]$open,
    "p2_15_open" = p2$marks[bed_name == "15"]$open,
    "p2_bull_open" = p2$marks[bed_name == "Bull"]$open,
    "p2_points" = p2$points,
    "p2_marks_to_finish" = p2$marks_to_finish,
    "p2_marks_ahead" = p2$marks_ahead,
    "p2_darts_to_finish" = p2$darts_to_finish,
    "p2_darts_ahead" = p2$darts_ahead,
    "p2_turns_to_finish" = p2$turns_to_finish,
    "p2_turns_ahead" = p2$turns_ahead,
    "current_turns" = turns,
    "shooter_id" = NA,
    "dart_num" = 0
  )

  # Make scoreboard
  scoreboard <- data.table::data.table(
    "Player 1" = c(p1$marks$symbol, p1$points),
    " " = c(p1$marks$bed_name, "Points"),
    "Player 2" = c(p2$marks$symbol, p2$points)
  )

  # Build the list of game attributes
  cricket_game <- list()
  cricket_game$p1 <- p1
  cricket_game$p2 <- p2
  cricket_game$game_state <- game_state
  cricket_game$scoreboard <- scoreboard
  cricket_game$turns  <- turns
  cricket_game$winner <- game_winner

  # Return state of the cricket game.
  cricket_game

}
