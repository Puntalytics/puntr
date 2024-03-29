% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mini2.R
\name{by_punter_seasons}
\alias{by_punter_seasons}
\title{Summarize data for player-seasons}
\usage{
by_punter_seasons(punts, ..., threshold = 32)
}
\arguments{
\item{punts}{The play-by-play punting data to be summarized}

\item{threshold}{The minimum number of punts for a season to be included, defaults to 32}
}
\value{
A tibble \code{miniY} where each row is a punter-season and each column is a stat
}
\description{
This function is essentially a convenient wrapper for \code{dplyr::summarise}
which includes all of the relevant columns from a \code{puntr}-style data frame.
This function differs from \code{puntr::create_mini}
in that it groups by both \code{punter_player_name} and \code{season} (and adds a convenient \code{seasonid} column to uniquely identify each row).
It is unfortunately not customizable beyond the setting of a minimum number of punts to be included;
if you have additional parameters you'd like to be summarised, the easiest thing would be to call \code{summarise} yourself.
}
\examples{
\dontrun{
create_miniY(punts)
}
}
