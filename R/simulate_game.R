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
simulate_game <- function(db, gm, targets, iteration, plot_game = FALSE, verbose = FALSE, seed) {

  # Set random seed.
  if (!missing(seed)) {
    set.seed(seed = seed)
  }

  # Prepare target data
  count <- 0

  # Initialize plot if needed
  if (plot_game) plot_db <- draw_dartboard(db)

  # Players take turns until there is a winner
  while (is.na(gm$winner)){

    # For each turn alternate players
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

    # Have the shooter:
    #   1. figure out the target
    #   2. set the target
    #   3. throw the dart
    #   4. see where the dart actually hit
    #   5. update the dartboard and update the game
    #   6. reset all the marks and differentials with new data
    #   7. check to see if the shooter won
    for (j in 1:3) {
      target_shot <- gm[[sidx]]$select_shot(
        gm,
        shooter,
        opponent,
        FALSE,
        gm[[sidx]]$min_marks_ahead
        )
      target_name <- target_shot$target_name
      reason <- target_shot$reason
      aim_shot <- set_target(
        target = target_name,
        target_dt = targets
        )
      throw_shot <- throw_dart(
        target_x = aim_shot$x,
        target_y = aim_shot$y,
        sd_factor = gm[[sidx]]$sd_factor,
        r = db$outer_ring$max_radius
        )
      shot_hit <- check_where_hit(
        x = throw_shot$x,
        y = throw_shot$y,
        db = db
        )
      if (plot_game) plot_db <- plot_shot(
        xpt = throw_shot$x,
        ypt = throw_shot$y,
        db_plot = plot_db,
        color = color
        )
      gm <- update_cricket_game(
        gm = gm,
        shooter_player_id = shooter,
        opponent_player_id = opponent,
        shot_hit = shot_hit,
        targets = targets,
        dart_num = j
        )
      gm <- set_marks(gm)
      gm$game_matrix <- update_game_state_matrix(
        gm = gm,
        shooter_id = shooter,
        dart_num = j,
        target = target_name,
        reason = reason,
        hit = shot_hit,
        iteration = iteration,
        aim_x = aim_shot$x,
        aim_y = aim_shot$y,
        hit_x = throw_shot$x,
        hit_y = throw_shot$y
        )
      if (!is.na(gm$winner)) break
    }

    # Update the scoreboard of the game
    gm$scoreboard <- data.table::data.table(
      "Player Red" = c(gm$p1$marks$symbol, gm$p1$points),
      " " = c(gm$p1$marks$bed_name, "Points"),
      "Player Black" = c(gm$p2$marks$symbol, gm$p2$points)
      )
    if (verbose) print(gm$scoreboard)
  }

  gm$game_matrix <- matrix(
    data = gm$game_matrix,
    ncol = 57,
    byrow = TRUE
    )
  gm$game_matrix <- as.data.frame(gm$game_matrix)
  colnames(gm$game_matrix) <- c("sim_id", "seed", "iteration",
                                "p1_min_marks_ahead",
                                "p1_20_marks", "p1_19_marks", "p1_18_marks", "p1_17_marks", "p1_16_marks", "p1_15_marks", "p1_bb_marks",
                                "p1_20_open", "p1_19_open", "p1_18_open", "p1_17_open", "p1_16_open", "p1_15_open", "p1_bb_open",
                                "p1_points",
                                "p1_marks_to_finish", "p1_marks_ahead",
                                "p1_darts_to_finish", "p1_darts_ahead",
                                "p1_turns_to_finish", "p1_turns_ahead",
                                "p2_min_marks_ahead",
                                "p2_20_marks", "p2_19_marks", "p2_18_marks", "p2_17_marks", "p2_16_marks", "p2_15_marks", "p2_bb_marks",
                                "p2_20_open", "p2_19_open", "p2_18_open", "p2_17_open", "p2_16_open", "p2_15_open", "p2_bb_open",
                                "p2_points",
                                "p2_marks_to_finish", "p2_marks_ahead",
                                "p2_darts_to_finish", "p2_darts_ahead",
                                "p2_turns_to_finish", "p2_turns_ahead",
                                "turns",
                                "shooter_id",
                                "dart_number",
                                "intended_target",
                                "intended_target_x",
                                "intended_target_y",
                                "reason",
                                "actual_hit",
                                "actual_hit_x",
                                "actual_hit_y")

  # Build results data object
  sim_results <- list()
  if (plot_game) sim_results$plot_db <- plot_db
  sim_results$gm <- gm

  readr::write_csv(x = gm$game_matrix, paste0("output/games/sim_", gm$sim_id, "_", str_pad(iteration, width = 5, side = "left", pad = "0"), ".csv"))

  # Return results of the simulated game
  sim_results

}
