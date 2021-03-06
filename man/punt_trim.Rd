% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing.R
\name{punt_trim}
\alias{punt_trim}
\title{Filter columns relevant to punting}
\usage{
punt_trim(punts, columns = NULL)
}
\arguments{
\item{punts}{The play-by-play punting data}

\item{columns}{Defaults to \code{NULL} indicating trimming down to the default columns. If \code{columns} is a list of other columns, those will be additionally included}
}
\value{
A tibble \code{punts} containing only the specified columns
}
\description{
This function is exported in case you would like include custom columns; if you would NOT, you can just
use [\code{puntr::trust_the_process}].
If you do use \code{puntr::punt_trim} yourself, be sure to include the \code{trim=FALSE} flag in \code{puntr::trust_the_process}.
}
\examples{
\dontrun{
punt_trim(punts)
punt_trim(punts, columns=c("solo_tackle", "assist_tackle"))
}
}
