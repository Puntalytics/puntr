% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mini2.R
\name{by_punter_games}
\alias{by_punter_games}
\title{Summarize data for player-games}
\usage{
by_punter_games(punts, ..., threshold = 1)
}
\arguments{
\item{punts}{The play-by-play punting data to be summarized}

\item{threshold}{The minimum number of punts for a week to be included, defaults to 1}
}
\value{
A tibble \code{miniG} where each row is a punter-week and each column is a stat
}
\description{
This function is essentially a convenient wrapper for \code{dplyr::summarise}
which includes all of the relevant columns from a \code{puntr}-style data frame.
This function differs from \code{puntr::create_mini} and \code{puntr::create_miniY}
in that it groups by \code{punter_player_name}, \code{season}, and \code{week}
(and adds a convenient \code{weekid} column to uniquely identify each row).
It is unfortunately not customizable beyond the setting of a minimum number of punts to be included;
if you have additional parameters you'd like to be summarised, the easiest thing would be to call \code{summarise} yourself.
}
\examples{
\dontrun{
create_miniG(punts)
}
}
