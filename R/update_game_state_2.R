#' Update the state of the game in the cricket game object.
#'
#' @param gm cricket game object
#' @param shooter_id shooter id either player 1 or palyer 2
#' @param dart_num the dart that is thrown
#'
#' @return a data.table with the state of the game
#'
#' @export
update_game_state_matrix <- function(gm, shooter_id, dart_num, target, reason, hit, iteration = 1, aim_x, aim_y, hit_x, hit_y) {

  row_entry <- unname(c(
    gm$sim_id,
    gm$seed,
    iteration,
    gm$p1$min_marks_ahead,
    unlist(gm$p1$marks$count),
    unlist(gm$p1$marks$open),
    gm$p1$points,
    gm$p1$marks_to_finish,
    gm$p1$marks_ahead,
    gm$p1$darts_to_finish,
    gm$p1$darts_ahead,
    gm$p1$turns_to_finish,
    gm$p1$turns_ahead,
    gm$p2$min_marks_ahead,
    unlist(gm$p2$marks$count),
    unlist(gm$p2$marks$open),
    gm$p2$points,
    gm$p2$marks_to_finish,
    gm$p2$marks_ahead,
    gm$p2$darts_to_finish,
    gm$p2$darts_ahead,
    gm$p2$turns_to_finish,
    gm$p2$turns_ahead,
    gm$turns,
    shooter_id,
    dart_num
    ))
  row_entry <- as.character(row_entry)
  row_entry <- c(row_entry, target, as.character(aim_x), as.character(aim_y), reason, hit, as.character(hit_x), as.character(hit_y))

  game_matrix <- c(gm$game_matrix, row_entry)

  game_matrix

}
