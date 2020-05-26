###|
###| Importing.
###|
###|

## To start, make sure you grab the data as .rds files right from the NFLfastR-data GitHub repository:
##      https://github.com/guga31bb/nflfastR-data.git
## This is convenient because (we think??) this will be updated every week, and a 'git pull origin master'
## will get you the updated data

# With that done, easily import whatever seasons you'd like:
# example call: import_seasons(2000:2019, path='stuff/football/play_by_play_')
import_seasons <- function(years, path='~/github/nflfastR-data/data/play_by_play_') {
  years %>% purrr::map_df(import_one_season, path=path)
}

# This function grabs just one season, but the above function will also work for one season
import_one_season <- function(year, path='~/github/nflfastR-data/data/play_by_play_') {
  pbp <- glue::glue('{path}{year}.rds') %>% readr::read_rds()
  return(pbp)
}

# We will (hopefully!) be updating the punting-specific data on our GitHub each week, but if we're
# lagging behind, you might have a plays file that you'd like to 'trim' into a punts file;
# You also might like to include custom columns

# Punt Trim
# Inputs a dataframe "plays"
# Outputs a dataframe "punts", containing only the rows that contain punts,
# and only the columns relevant to puntalytics.
# The default columns will pretty much always do the trick.
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
