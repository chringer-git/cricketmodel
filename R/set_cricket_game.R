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
  shot_record <- data.table::data.table(
    "player_1_20_count" = 0,
    "player_1_19_count" = 0,
    "player_1_18_count" = 0,
    "player_1_17_count" = 0,
    "player_1_16_count" = 0,
    "player_1_15_count" = 0,
    "player_1_bull_count" = 0,
    "player_1_20_open" = FALSE,
    "player_1_19_open" = FALSE,
    "player_1_18_open" = FALSE,
    "player_1_17_open" = FALSE,
    "player_1_16_open" = FALSE,
    "player_1_15_open" = FALSE,
    "player_1_bull_open" = FALSE,
    "player_1_points" = 0,
    "player_1_marks_to_finish" = 21,
    "player_1_marks_ahead" = 0,
    "player_1_darts_to_finish" = 8,
    "player_1_darts_ahead" = 0,
    "player_1_turns_to_finish" = 3,
    "player_1_turns_ahead" = 0,
    "player_2_20_count" = 0,
    "player_2_19_count" = 0,
    "player_2_18_count" = 0,
    "player_2_17_count" = 0,
    "player_2_16_count" = 0,
    "player_2_15_count" = 0,
    "player_2_bull_count" = 0,
    "player_2_20_open" = FALSE,
    "player_2_19_open" = FALSE,
    "player_2_18_open" = FALSE,
    "player_2_17_open" = FALSE,
    "player_2_16_open" = FALSE,
    "player_2_15_open" = FALSE,
    "player_2_bull_open" = FALSE,
    "player_2_points" = 0,
    "player_2_marks_to_finish" = 21,
    "player_2_marks_ahead" = 0,
    "player_2_darts_to_finish" = 8,
    "player_2_darts_ahead" = 0,
    "player_2_turns_to_finish" = 3,
    "player_2_turns_ahead" = 0,
    "current_turns" = 0
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
  cricket_game$shot_record <- shot_record
  cricket_game$scoreboard <- scoreboard
  cricket_game$turns  <- turns
  cricket_game$winner <- game_winner

  # Return state of the cricket game.
  cricket_game

}
