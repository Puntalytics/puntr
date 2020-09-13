###|
###| Processing.  Functions are usable individually, but it is usually easier to use
###| the "trust the process" function to perform all of the standard steps in one go.
###|

# Perform standard processing of a punt data frame
# Inputs and outputs a dataframe "punts"
#
# Read on for details about the functions individually, or don't!  This function will usually do the trick
#' Clean and process play-by-play punting data
#' @description This function handles all of the standard data cleaning necessary/preferable for puntalytics.
#' @param punts The play-by-play punting data to be cleaned and processed
#' @param trim Specify \code{trim=FALSE} if you would like to use \code{puntr::punt_trim} to create custom columns (or not use \code{puntr::punt_trim} at all and include all columns)
#' @param seasontype One of "REG" (default) or "POST" to filter data, or NULL to include all data
#' @return A tibble \code{punts} with cleaned and processed play-by-play punting data
#' @examples
#' \dontrun{
#' trust_the_process(punts)
#' trust_the_process(punts_custom_trimmed_already, trim=FALSE)
#' }
#' @export
trust_the_process <- function(punts, seasontype="REG", trim=TRUE) {

  if(!is.null(seasontype)) {
    punts <- punts %>% dplyr::filter(season_type==seasontype)
  }
  if(trim) {
    punts <- punts %>% punt_trim()
  }
  punts <- punts %>% add_GrossYards()
  punts <- punts %>% fix_na()
  punts <- punts %>% fix_touchbacks()
  punts <- punts %>% calculate_net()
  punts <- punts %>% add_YFOEZ()
  punts <- punts %>% label_type()
  punts <- punts %>% add_logos()

  return(punts)
}

# Trim down to only the columns you're likely to need for puntalytics
# If you'd like additional columns, include them as a second argument.
# If you'd like to keep all columns, include the flag columns = "ALL"
#' Filter columns relevant to punting
#' @description This function is exported in case you would like include custom columns; if you would NOT, you can just
#' use [\code{puntr::trust_the_process}].
#' If you do use \code{puntr::punt_trim} yourself, be sure to include the \code{trim=FALSE} flag in \code{puntr::trust_the_process}.
#' @param punts The play-by-play punting data
#' @param columns Defaults to \code{NULL} indicating trimming down to the default columns. If \code{columns} is a list of other columns, those will be additionally included
#' @return A tibble \code{punts} containing only the specified columns
#' @examples
#' \dontrun{
#' punt_trim(punts)
#' punt_trim(punts, columns=c("solo_tackle", "assist_tackle"))
#' }
#' @export
punt_trim <- function(punts, columns=NULL) {

  punt_columns <- c("play_id", "game_id", "home_team", "away_team", "posteam", "defteam", "game_date",
                      "yardline_100", "yrdln", "desc", "play_type", "ydstogo", "touchback",
                      "kick_distance", "ep", "epa", "wp", "wpa", "punt_blocked", "punt_inside_twenty",
                      "punt_in_endzone","punt_out_of_bounds", "punt_downed", "punt_fair_catch",
                      "punt_attempt", "punter_player_id", "punter_player_name", "punt_returner_player_id",
                      "punt_returner_player_name", "return_yards", "season", "season_type", "week",
                      "touchdown", "td_team", "temp", "roof")

  if(!is.null(columns))  {
    punt_columns <- c(punt_columns, columns)
  }

  punts <- punts %>% dplyr::select(all_of(punt_columns))

  return(punts)
}

# Add GrossYards to data frame
# Inputs and outputs a dataframe "punts"
#
# Rename the ugly kick_distance to GrossYards so it looks nice as an axis label
add_GrossYards <- function(punts) {
  punts <- punts %>% dplyr::rename(GrossYards = kick_distance)
  return(punts)
}

# Fix NA
# Inputs and outputs a dataframe "punts"
# Sometimes, the punter's name isn't properly parsed from the play description, so let's do that manually
fix_na <- function(punts) {
  punts$punter_player_name[is.na(punts$punter_player_name)] <-
    punts$desc[is.na(punts$punter_player_name)] %>% stringr::str_extract('[:upper:].[:upper:][:lower:]*')

  punts$GrossYards[is.na(punts$GrossYards)] <-
    punts$desc[is.na(punts$GrossYards)] %>% stringr::str_extract('[:digit:]+ yard') %>%
    stringr::str_extract('[:digit:]+') %>% as.numeric()

  punts$return_yards[is.na(punts$return_yards)] <-
    punts$desc[is.na(punts$return_yards)] %>% stringr::str_extract('for [:digit:]+') %>%
    stringr::str_extract('[:digit:]+') %>% as.numeric()

  punts <- punts %>% dplyr::filter(!is.na(GrossYards))
  punts <- punts %>% dplyr::filter(!is.na(return_yards))
  punts <- punts %>% dplyr::filter(!is.na(punter_player_name))

  return(punts)
}

# Fix NA value for touchback kick distance
# Inputs and outputs a dataframe "punts"
#
# In the nflscrapr data, touchbacks carry a value of NA for kick_distance
# This function assigns those punts a value for kick_distance as if they had gone to the 20
# If you'd like touchbacks to be treated as if they went elsewhere (say, the goalline, as is standard)
# you can do so by setting correction=0
# We recommend using this default value of correction=20, representing the actual field-position
# result of a touchback
fix_touchbacks <- function(punts, correction=20) {

  punts$GrossYards[punts$touchback==1] <- punts$yardline_100[punts$touchback==1] - correction

  return(punts)
}

# Calculate normal net, which annoyingly isn't in the dataframe to begin with
# Inputs and outputs a dataframe "punts"
calculate_net <- function(punts) {
  punts <- punts %>% dplyr::mutate(NetYards = punts$GrossYards - punts$return_yards)
  return(punts)
}

# Label each punt as open field (OF) or pin deep (!OF)
# Inputs and outputs a dataframe "punts"
#
# We at puntalytics inherited this OF v. PD threshold from Chuck Zodda
# http://insidethepylon.com/football-101/glossary-football-101/2016/01/22/itp-glossary-pin-deep-punt/
# Feel free to use your own threshold
# Note that none of our actual metrics depend on this threshold; it's just a way to categorize situations and
# performance in those situations
label_type <- function(punts, threshold=41) {
  punts <- punts %>% dplyr::mutate(PD = dplyr::if_else(punts$YardsFromOwnEndZone>=threshold, 1, 0))
  return(punts)
}

# Add YardsFromOwnEndZone to data frame
# Inputs and outputs a dataframe "punts"
#
# Useful such that on a graph with YFOEZ as the x-axis, the offensive team goes "left-to-right"
# Note - this function can be used on a dataframe that contains plays other than punts
add_YFOEZ <- function(punts) {

  punts <- punts %>% dplyr::mutate(YardsFromOwnEndZone = 100 - punts$yardline_100)

  return(punts)
}

add_logos <- function(punts) {
  if(!"team_logo_espn" %>% is.element(colnames(punts))) {
    logos <- nflfastR::teams_colors_logos %>%
      dplyr::select(team_abbr, team_logo_espn, team_color, team_color2)
    punts <- punts %>% dplyr::left_join(logos, by = c("posteam" = "team_abbr"))
  }
  return(punts)
}

# # Note scoring plays and muffs, but *don't* remove them.
# # Inputs and outputs a dataframe "punts"
# note_scores <- function(punts) {
#   # All TDs
#   punts <- punts %>% tibble::add_column(TDany = dplyr::if_else(stringr::str_detect(punts$desc, 'TOUCHDOWN'), 1, 0))
#   # muffs
#   punts <- punts %>% tibble::add_column(punt_fumble = dplyr::if_else(stringr::str_detect(punts$desc, 'RECOVERED'), 1, 0))
#   # TD returns
#   punts <- punts %>%
#     tibble::add_column(TDreturn = dplyr::if_else(punts$TDany==1 & punts$punt_fumble==0 & !stringr::str_detect(punts$desc, 'NULLIFIED'),
#                                   1, 0))
#
#   return(punts)
# }
