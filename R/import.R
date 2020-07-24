###|
###| Importing.
###|
###|

## Essentially all of the data preparation and cleaning has been done already
# You can download a .rds file containing all of the punts here:
import_punts <- function() {
  punts <- url('https://raw.githubusercontent.com/Puntalytics/puntr/master/data/punts_all_seasons.rds') %>%
    readr::read_rds()
  return(punts)
}

# Here's the associated metadata that you might want (see the associated scraping in the pfr_metadata_pull repo)
import_metadata <- function() {
  metadata <- url('https://raw.githubusercontent.com/Puntalytics/puntr/master/data/game_meta_data_ready_to_merge.csv') %>%
    readr::read_rds()
  return(metadata)
}

## You might decide you want all of the play-by-play data yourself, so you can clean/filter it as you see fit
# Call this function with a range of years, e.g. import_seasons(2000:2019)
# Data goes back to 2000, and should be updated week-by-week during whatever the current season is
import_seasons <- function(years) {
  pbp <- years %>%
    purrr::map_df(import_one_season, 'https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_')
  return(pbp)
}

# Helper function for a single season
import_one_season <- function(year, url) {
  pbp <- glue::glue('{url}{year}.rds') %>%
    url() %>%
    readr::read_rds()
  return(pbp)
}

# This function is what we use for converting a 'plays' df to a 'punts' df
# The punts_all_seasons file in our repo is the output of this function with the defaults
#
# If you'd like additional columns, include them as a second argument.
# If you'd like to keep all columns, include the flag columns = "ALL"
punt_trim <- function(plays, columns="STANDARD") {

  punts <- plays %>% dplyr::filter(play_type=="punt")

  if(columns=="ALL"){
    return(punts)

  } else {
    punt_columns <- c("play_id", "game_id", "home_team", "away_team", "posteam", "defteam", "game_date",
                      "yardline_100", "yrdln", "desc", "play_type", "ydstogo", "touchback",
                      "kick_distance", "ep", "epa", "wp", "wpa", "punt_blocked", "punt_inside_twenty",
                      "punt_in_endzone","punt_out_of_bounds", "punt_downed", "punt_fair_catch",
                      "punt_attempt", "punter_player_id", "punter_player_name", "punt_returner_player_id",
                      "punt_returner_player_name", "return_yards", "season", "season_type", "week", "game_key",
                      "game_time_local", "site_id", "site_city", "site_fullname", "site_state", "roof_type")
  }

  if(columns != "STANDARD") {
    punt_columns <- c(punt_columns, columns)
  }

  punts <- punts %>% dplyr::select(punt_columns)
  return(punts)
}
