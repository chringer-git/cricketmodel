
<!-- README.md is generated from README.Rmd. Please edit that file -->
cricketmodel
============

The goal of cricketmodel is to model the decisions of which targets to throw at and the error in making contact with the targets. The game is simulated until a winner emerges.

cricketmodel should be used in conjuction with my dartboard package

Installation
------------

You can install a development version of cricketmodel from [GitHub](https://github.com) with:

``` r
devtools::install_github("chringer-git/cricketmodel")
```

You can install a development version of dartboard from GitHub with:

``` r
devtools::install_github("chringer-git/dartboard")
```

Example
-------

You can create a couple of players by setting a couple of parameters, then initialize a game.

``` r
library(cricketmodel)
#> Loading required package: data.table
#> Loading required package: dartboard

# Making a player you only need to know how many mark MPR the player averages.
player_1 <- make_player(average_marks_per_turn = 3)
player_2 <- make_player(average_marks_per_turn = 3.2)

# From here you can set a cricket match between the two players
gm <- set_cricket_game(p1 = player_1, p2 = player_2)

# To show a scoreboard we can look at the 
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

There are a few strategies to choose from when making a player

``` r

# Basic strategy
player_1 <- make_player(average_marks_per_turn = 3, 
                        select_shot =  select_target_basic_cricket)

# A pro-closing strategy
player_1 <- make_player(average_marks_per_turn = 3, 
                        select_shot =  select_target_closing_cricket)

# A pro-pointing strategy
player_1 <- make_player(average_marks_per_turn = 3, 
                        select_shot =  select_target_pointing_cricket)

# Or you can make your own strategy to submit to the player
```

Once a game has been created you can force a score update to examine.

``` r

# If you want to update player 1 with 3-20s, 2-19s and 20 points you can submit
# this to the cricket game object

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
