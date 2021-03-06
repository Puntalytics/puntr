% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{import_seasons}
\alias{import_seasons}
\title{Import play-by-play data}
\usage{
import_seasons(years)
}
\arguments{
\item{years}{A year or range of years between 1999 and 2020, inclusive}
}
\value{
A tibble \code{pbp} containing play-by-play data for the specified years
}
\description{
Grab all play-by-play data, not just punts. This function pulls data directly from the \code{nflfastR-data} repo, and
is purely a wrapper for ease of use.
}
\examples{
\dontrun{
import_seasons(1999:2019)
}
}
