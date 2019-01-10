#' Set the marks and differentials for each player
#'
#' @param gm cricket game object
#'
#' @return updated cricket game object
#'
#' @export
set_marks <- function(gm) {

  # Set the point differential with respect to the first shooter in the game.
  # This difference will be used to calculate the minimum number of marks needed
  # to close the game.
  p1_point_diff <- gm$p1$points - gm$p2$points

  # Using the point differential, calculate the number of marks for each player
  # and the relative difference in marks between the players. In this case,
  # we take the highest value of single bed values including including the bull
  # from the player that is behind in points.
  max_single_point <- 25
  if (is.na(gm$winner)) {
    if (p1_point_diff > 0) {
      max_single_point <- max(gm$p1$marks[open == TRUE]$bed_value)
    } else if (p1_point_diff < 0) {
      max_single_point <- max(gm$p2$marks[open == TRUE]$bed_value)
    } else {
      # Since this will be a divisor we need a value, although p1_point_diff
      # will be 0, so additional marks will be zero.
      max_single_point <- 25
    }
  }
  gm$p1$marks_to_finish <- 21 - sum(gm$p1$marks$count) +
    ifelse(p1_point_diff > 0, 0, ceiling(-p1_point_diff / max_single_point))
  gm$p2$marks_to_finish <- 21 - sum(gm$p2$marks$count) +
    ifelse(-p1_point_diff > 0, 0, ceiling(p1_point_diff / max_single_point))

  gm$p1$marks_ahead <- -(gm$p1$marks_to_finish - gm$p2$marks_to_finish)
  gm$p2$marks_ahead <- gm$p1$marks_to_finish - gm$p2$marks_to_finish

  # Using the point differential, calculate the number of darts for each player
  # and the relative difference in darts between the players.
  max_point <- 50
  if (is.na(gm$winner)) {
    if (p1_point_diff > 0) {
      # First we set the highest triple value
      if (nrow(gm$p1$marks[open == TRUE & bed_name != "Bull"]) != 0) {
        max_point <- max(3 * gm$p1$marks[open == TRUE &
                                         bed_name != "Bull"]$bed_value,
                         na.rm = TRUE
                         )
      }
      if (nrow(gm$p1$marks[open == TRUE & bed_name == "Bull"]) != 0) {
        max_point <- max(max_point,
                         2 * gm$p1$marks[open == TRUE]$bed_value,
                         na.rm = TRUE
                         )
      }
    } else if (p1_point_diff < 0) {
      if (nrow(gm$p2$marks[open == TRUE & bed_name != "Bull"]) != 0) {
        max_point <- max(3 * gm$p2$marks[open == TRUE &
                                         bed_name != "Bull"]$bed_value,
                         na.rm = TRUE
                         )
      }
      if (nrow(gm$p2$marks[open == TRUE & bed_name != "Bull"]) != 0) {
        max_point <- max(max_point,
                         2 * gm$p2$marks[open == TRUE]$bed_value,
                         na.rm = TRUE
                         )
      }
    } else {
      # Since this will be a divisor we need a value, although p1_point_diff
      # will be 0, so additional marks will be zero.
      max_point <- 50
    }
  }
  gm$p1$darts_to_finish <-
    nrow(gm$p1$marks[open == TRUE & bed_name != "Bull"]) +
    ifelse(round(gm$p1$marks[bed_name == "Bull"]$count) <= 0,
           2,
           ifelse(round(gm$p1$marks[bed_name == "Bull"]$count) %in% c(1, 2), 1, 0)
           ) +
    max(0, ceiling(-p1_point_diff / max_point))
  gm$p2$darts_to_finish <-
    nrow(gm$p2$marks[open == TRUE & bed_name != "Bull"]) +
    ifelse(round(gm$p2$marks[bed_name == "Bull"]$count) <= 0, 2,
           ifelse(round(gm$p2$marks[bed_name == "Bull"]$count) %in% c(1, 2), 1, 0)
           ) +
    max(0, ceiling(p1_point_diff / max_point))

  gm$p1$darts_ahead <- -(gm$p1$darts_to_finish - gm$p2$darts_to_finish)
  gm$p2$darts_ahead <- gm$p1$darts_to_finish - gm$p2$darts_to_finish

  # Using the marks differential, calculate the number of darts for each player
  # and the relative difference in darts between the players.
  gm$p1$turns_to_finish <- ceiling(gm$p1$darts_to_finish / 3)
  gm$p2$turns_to_finish <- ceiling(gm$p2$darts_to_finish / 3)

  gm$p1$turns_ahead <- -(gm$p1$turns_to_finish - gm$p2$turns_to_finish)
  gm$p2$turns_ahead <- gm$p1$turns_to_finish - gm$p2$turns_to_finish

  # Return "marks" calculation of the game.
  gm

}
