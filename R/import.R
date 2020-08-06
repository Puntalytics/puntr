###|
###| Importing.
###|
###|

## Essentially all of the data preparation and cleaning has been done already
# You can download a .rds file containing all of the punts here:
#' Import punting data
#'
#' @param years A year or range of years between 1999 and 2019, inclusive
#' @return A tibble \code{punts} containing play-by-play punting data for the specified years
#' @examples
#' punts <- import_punts(1999:2019)
#' @export
import_punts <- function(years) {
  punts <- years %>%
    purrr::map_df(import_one_season, 'https://raw.githubusercontent.com/Puntalytics/puntr/master/data/punts_')
  return(punts)
}

## You might decide you want all of the play-by-play data yourself, so you can clean/filter it as you see fit
# Call this function with a range of years, e.g. import_seasons(1999:2019)
# Data goes back to 1999, and should be updated week-by-week during whatever the current season is
#' Import play-by-play data
#'
#' @param years A year or range of years between 1999 and 2019, inclusive
#' @return A tibble \code{pbp} containing play-by-play data for the specified years
#' @examples
#' pbp <- import_seasons(1999:2019)
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

# Here's the associated metadata that you might want (see the associated scraping in the pfr_metadata_pull repo)
#' Import metadata
#'
#' @return A tibble \code{metadata} containing metadata
#' @examples
#' metadata <- import_metadata()
#' @export
import_metadata <- function() {
  metadata <- url('https://raw.githubusercontent.com/Puntalytics/puntr/master/data/game_meta_data_ready_to_merge.csv') %>%
    readr::read_csv()
  return(metadata)
}

