#' Simulates a cricket game
#'
#' @param db dartboard object from dartboard::create_dartboard
#' @param gm game object from set_cricket_game
#' @param plot_game dartboard plot from dartboard::draw_dartboard
#' @param verbose logic to write out data via print statement
#'
#' @return results contain final state of the game and all shots
#'
#' @export
simulate_game <- function(db, gm, plot_game, verbose = FALSE) {

  # Load libraries
  library(data.table)

  targets <- translate_targets(db)
  count <- 0
  #verbose <- TRUE

  if (plot_game) plot_db <- draw_dartboard(db)

  while (is.na(gm$winner)){

  #for (i in 1:1000000) {

    if (!is.na(gm$winner)) break

    count <- count + 1
    if (count %% 2 != 0) {
      shooter <- 1
      opponent <- 2
      color <- "red"
    } else {
      shooter <- 2
      opponent <- 1
      color <- "black"
    }
    gm$turns <- round(count / 2, 1)
    sidx <- paste0("p", shooter)

    for (j in 1:3) {
      target_shot <- gm[[sidx]]$select_shot(gm, shooter, opponent)
      aim_shot    <- set_target(target = target_shot, target_dt = targets)
      throw_shot  <- throw_dart(target_x = aim_shot$x, target_y = aim_shot$y, sd_factor = gm[[sidx]]$sd_factor)
      shot_hit    <- check_where_hit(x = throw_shot$x, throw_shot$y, db)
      #if (verbose) print(paste(sidx, target_shot, shot_hit, sep = ", "))
      if (plot_game) plot_db <- plot_shot(xpt = throw_shot$x, throw_shot$y, plot_db, color = color)
      gm <- update_game(gm = gm,
                        shooter_player_id = shooter,
                        opponent_player_id = opponent,
                        shot_hit = shot_hit,
                        targets = targets)
      gm <- set_marks(gm)
      if (!is.na(gm$winner)) break

    }

    #if (verbose) View(gm)
    if (!is.na(gm$winner)) break

    gm$scoreboard <- data.table::data.table(
      "Player Red" = c(gm$p1$marks$symbol, gm$p1$points),
      " " = c(gm$p1$marks$bed_name, "Points"),
      "Player Black" = c(gm$p2$marks$symbol, gm$p2$points)
      )
    if (verbose) print(gm$scoreboard)

  }

  gm$scoreboard <- data.table::data.table(
    "Player Red" = c(gm$p1$marks$symbol, gm$p1$points),
    " " = c(gm$p1$marks$bed_name, "Points"),
    "Player Black" = c(gm$p2$marks$symbol, gm$p2$points)
  )

  sim_results <- list()
  if (plot_game) sim_results$plot_db <- plot_db
  sim_results$gm <- gm

  sim_results

}
