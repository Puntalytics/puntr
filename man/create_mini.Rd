% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mini.R
\name{create_mini}
\alias{create_mini}
\title{Summarize data for players}
\usage{
create_mini(punts, threshold = 64)
}
\arguments{
\item{punts}{The play-by-play punting data to be summarized}

\item{threshold}{The minimum number of career punts needed to be included, defaults to 64}
}
\value{
A tibble \code{mini} where each row is a punter and each column is a stat
}
\description{
This function is essentially a convenient wrapper for \code{dplyr::summarise}
which includes all of the relevant columns from a \code{puntr}-style data frame. It is unfortunately not customizable beyond the setting of a
minimum number of punts to be included; if you have additional parameters you'd like to be summarised, the easiest thing would be to call \code{summarise} yourself.
For comparison of punter seasons rather than punter careers, see \code{puntr::create_miniY}
}
\examples{
\dontrun{
create_mini(punts)
}
}
