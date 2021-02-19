###|
###| Importing.
###|
###|

#' Import punting data
#' @description Easily import punting data for completed seasons.
#' This data has been pre-scraped and compiled \href{https://github.com/Puntalytics/puntr-data/tree/master/data}{here}.
#' The only cleaning performed on this data before storing is \code{filter(play_type == 'punt')}.
#' @param years A year or range of years between 1999 and 2019, inclusive
#' @return A tibble \code{punts} containing play-by-play punting data for the specified years
#' @examples
#' \dontrun{
#' import_punts(1999:2019)
#' }
#' @export
import_punts <- function(years) {
  punts <- years %>%
    purrr::map_df(import_one_season, 'https://raw.githubusercontent.com/Puntalytics/puntr-data/master/data/punts_')
  return(punts)
}

#' Import play-by-play data
#' @description Grab all play-by-play data, not just punts. This function pulls data directly from the \code{nflfastR-data} repo, and
#' is purely a wrapper for ease of use.
#' @param years A year or range of years between 1999 and 2020, inclusive
#' @return A tibble \code{pbp} containing play-by-play data for the specified years
#' @examples
#' \dontrun{
#' import_seasons(1999:2019)
#' }
#' @export
import_seasons <- function(years) {
  pbp <- years %>%
    purrr::map_df(import_one_season, 'https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_')
  return(pbp)
}

# Helper function for a single season (works for all plays or just punts, depending on the url given)
import_one_season <- function(year, url) {
  pbp <- glue::glue('{url}{year}.rds') %>%
    url() %>%
    readr::read_rds()
  return(pbp)
}

#' #' Add metadata to punts
#' #'
#' #' @description This function adds game metadata to your play-by-play dataframe (should work for dataframes containing non-punting data too, fwiw)
#' #' @param punts A tibble containing the punts to which metadata will be added
#' #' @return A tibble \code{punts} with metadata added
#' #' @examples
#' #' \dontrun{
#' #' add_metadata(punts)
#' #' }
#' #' @export
#' add_metadata <- function(punts) {
#'   metadata <- import_metadata()
#'   punts <- left_join(punts, metadata, by = c("season", "week", "home_team", "away_team"))
#'   return(punts)
#' }
#'
#' #' Import metadata
#' #'
#' #' This work piggybacks on that of greerre on github, see here: (https://github.com/Puntalytics/pfr_metadata_pull)
#' #' The pfr_metadata_pull package contains code for scraping the metadata yourself, if you're so inclined
#' #' @return A tibble \code{metadata} containing metadata. Each row is a game.
#' #' @examples
#' #' metadata <- import_metadata()
#' #' @export
#' import_metadata <- function() {
#'   metadata <- url('https://raw.githubusercontent.com/Puntalytics/puntr-data/master/data/game_meta_data_ready_to_merge.csv') %>%
#'     readr::read_csv()
#'   return(metadata)
#' }

