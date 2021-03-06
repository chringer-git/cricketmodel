
<!-- README.md is generated from README.Rmd. Please edit that file -->
cricketmodel
============

The goal of `cricketmodel` is to model the decisions of which targets to throw at and the error in making contact with the targets. The game is simulated until a winner emerges.

`cricketmodel` should be used in conjuction with my `dartboard` package

Installation
------------

You can install a development version of `cricketmodel` from [GitHub](https://github.com) with:

``` r
devtools::install_github("chringer-git/cricketmodel")
```

You can install a development version of `dartboard` from GitHub with:

``` r
devtools::install_github("chringer-git/dartboard")
```

Walkthough
----------

### Example 1: Set up cricket game with `set_cricket_game`

You can create a couple of players by setting a couple of parameters, then initialize a game.

``` r
library(cricketmodel)
#> Loading required package: data.table
#> Loading required package: ggplot2
#> Loading required package: dartboard

# Making a player you only need to know how many mark MPR the player averages.
# A good player.
player_1 <- make_player(mpr = 3.2)

# A player who is not-as good
player_2 <- make_player(mpr = 2.2)

# From here you can set a cricket match between the two players
gm <- set_cricket_game(p1 = player_1, p2 = player_2)

# To show a scoreboard we use the knitr package.
knitr::kable(gm$scoreboard, align = rep("c", 3))
```

| Player 1 |        | Player 2 |
|:--------:|--------|:--------:|
|          | 20     |          |
|          | 19     |          |
|          | 18     |          |
|          | 17     |          |
|          | 16     |          |
|          | 15     |          |
|          | Bull   |          |
|     0    | Points |     0    |

### Example 2: Add shot selection to players

Within the package, there are a few methods of selecting shots to choose from when making a player. Examine the code and come up with a new shot selection method to apply to players.

``` r
# Basic strategy
player_1 <- make_player(mpr = 3, 
                        select_shot =  select_target_basic_cricket)

# A pro-closing strategy
player_1 <- make_player(mpr = 3, 
                        select_shot =  select_target_closing_cricket)

# A pro-pointing strategy
player_1 <- make_player(mpr = 3, 
                        select_shot =  select_target_pointing_cricket)

# I hope to allow an easy way to construct target selection strategies to attach to players.
```

### Example 3: Apply a score straight to the game with `force_score_cricket`

Once a game has been created you can force a score update to examine.

``` r
# If you want to update player 1 with 3-20s, 2-19s and 20 points you can submit this to the cricket game object
new_gm <- force_score_cricket(gm = gm, 
                              player_id = 1,
                              bed_val = c(20, 19),
                              num_marks = c(3, 2),
                              points = 20)

# To show the new scoreboard we can look at the 
knitr::kable(new_gm$scoreboard, align = rep("c", 3))
```

| Player 1 |        | Player 2 |
|:--------:|--------|:--------:|
|     0    | 20     |          |
|     X    | 19     |          |
|          | 18     |          |
|          | 17     |          |
|          | 16     |          |
|          | 15     |          |
|          | Bull   |          |
|    20    | Points |     0    |

### Example 4: Target shot for the next player

Given the scoreboard from Example 3, what may be the first shot for player 2?

``` r
# Player 2 is using basic strategy, so the shooter will look to hit 19s to close,
# then point.
shot_selection <- new_gm$p2$select_shot(gm = new_gm, 
                                        shooter_player_id = 2, 
                                        opponent_player_id = 1)

print(shot_selection)
#> [1] "Treble 19"

# Take aim at the shot, show the coordinates
library(dartboard)
db       <- create_dartboard()
aim_shot <- set_target(target = shot_selection, 
                       target_dt = translate_targets(db))

print(paste0("x coordinate: ", aim_shot$x, ", y coordinate: ", aim_shot$y))
#> [1] "x coordinate: -1.24572475857401, y coordinate: -3.83394658131484"
```

### Example 5: Throw dart at `shot_selection`, see where it lands

Given the shot selection from Example 4, throw the dart with added error and check to see where it lands on the board.

``` r
# Given the players accuracy shoot the shot and see where get coordinates of where it lands
throw_shot  <- throw_dart(target_x = aim_shot$x, 
                          target_y = aim_shot$y, 
                          sd_factor = gm$p2$sd_factor,
                          r = db$outer_ring$max_distance)

print(paste0("x coordinate: ", throw_shot$x, ", y coordinate: ", throw_shot$y))
#> [1] "x coordinate: -1.72611486890448, y coordinate: -5.1334302392265"

# Check to see where it hit the board
shot_hit <- check_where_hit(x = throw_shot$x, throw_shot$y, db)
print(shot_hit)
#> [1] "Single 19"
```

### Example 6: Throw three darts (Examples 4 & 5 combined) and update the scoreboard.

A simple loop to throw three darts in a turn and update the scoreboard.

``` r
# Run a loop to throw three darts, log the targets and update the scorebaord.
for (i in 1:3) {
  shot_selection <- new_gm$p2$select_shot(gm = new_gm, 
                                        shooter_player_id = 2, 
                                        opponent_player_id = 1)
  aim_shot <- set_target(target = shot_selection, 
                       target_dt = translate_targets(db))
  throw_shot  <- throw_dart(target_x = aim_shot$x, 
                          target_y = aim_shot$y, 
                          sd_factor = gm$p2$sd_factor,
                          r = db$outer_ring$max_distance)
  shot_hit <- check_where_hit(x = throw_shot$x, throw_shot$y, db)
  print(paste0("Player 2 shoots: ", shot_selection, " and hits: ", shot_hit))
  
  new_gm <- update_game(gm = new_gm,
                        shooter_player_id = 2,
                        opponent_player_id = 1,
                        shot_hit = shot_hit,
                        targets = translate_targets(db))
  
}
#> [1] "Player 2 shoots: Treble 19 and hits: Single 19"
#> [1] "Player 2 shoots: Treble 19 and hits: Single 19"
#> [1] "Player 2 shoots: Treble 19 and hits: Single 7"
knitr::kable(new_gm$scoreboard, align = rep("c", 3))
```

| Player Red |        | Player Black |
|:----------:|--------|:------------:|
|      0     | 20     |              |
|      X     | 19     |       X      |
|            | 18     |              |
|            | 17     |              |
|            | 16     |              |
|            | 15     |              |
|            | Bull   |              |
|     20     | Points |       0      |

### Example 7: Simulate an entire game with `simulate_game`

Give the two players, you can simulate an entire game by looping the logic in Example 6 with `simulate_game`.

``` r
# Simulate game with one function.
sim_gm <- simulate_game(db = db,
                        gm = gm,
                        plot_game = TRUE,
                        verbose = FALSE)
```

Show the scoreboad

``` r
# Show scoreboard
knitr::kable(sim_gm$gm$scoreboard)
```

| Player Red |        | Player Black |
|:-----------|--------|:-------------|
| 0          | 20     | 0            |
| 0          | 19     | 0            |
| 0          | 18     | 0            |
| 0          | 17     | 0            |
| 0          | 16     |              |
| 0          | 15     | /            |
| 0          | Bull   | /            |
| 190        | Points | 122          |

Show all shots on the dartboard

``` r
# Show dartboard
sim_gm$plot_db
```

<img src="man/figures/README-dartboard_entire_game-1.png" width="100%" />
