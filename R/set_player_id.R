#' Set player id given 1 or 2
#'
#' @param id number for player id, only 1 or 2
#'
#' @return player_id
#'
#' @export
set_player_id <- function(id) {

  # Set player id
  idx <- paste0("p", id)

  # Return id
  idx

}
