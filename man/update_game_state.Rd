% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_game_state.R
\name{update_game_state}
\alias{update_game_state}
\title{Update the state of the game in the cricket game object.}
\usage{
update_game_state(gm, shooter_id, dart_num)
}
\arguments{
\item{gm}{cricket game object}

\item{shooter_id}{shooter id either player 1 or palyer 2}

\item{dart_num}{the dart that is thrown}
}
\value{
a data.table with the state of the game
}
\description{
Update the state of the game in the cricket game object.
}
