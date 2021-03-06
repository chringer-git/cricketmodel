% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_player.R
\name{make_player}
\alias{make_player}
\title{Create a player object given a description of the shooter's accruacy and
shot selection strategy}
\usage{
make_player(mpr, total_darts_501, select_shot)
}
\arguments{
\item{mpr}{numeric value of a shooter's average marks shot in one turn}

\item{total_darts_501}{numeric value of average total darts shot in a full game of 501}

\item{select_shot}{function that decides which target to shoot}
}
\value{
player object consisting of accuracy and target selection
}
\description{
Create a player object given a description of the shooter's accruacy and
shot selection strategy
}
