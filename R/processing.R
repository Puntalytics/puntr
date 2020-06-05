###|
###| Processing.  Functions are usable individually, but it is usually easier to use
###| the "trust the process" function to perform all of the standard steps in one go.
###|

# Perform standard processing of a punt data frame
# Inputs and outputs a dataframe "punts"
#
# Read on for details about the functions individually, or don't!  This function will usually do the trick
trust_the_process <- function(punts) {
  punts <- punts %>% add_GrossYards()
  punts <- punts %>% fix_na()
  #punts <- punts %>% remove_blocks()
  # punts <- punts %>% add_season()
  #punts <- punts %>% remove_scores()
  punts <- punts %>% fix_touchbacks()
  punts <- punts %>% calculate_net()
  punts <- punts %>% add_YFOEZ()
  punts <- punts %>% label_type()

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

# Add Season
# Inputs and outputs a dataframe "punts"
#
# Data frame comes pre-loaded with a date for each game, in the form of "game_date", not used here,
# and in the form of game_id = YYYYMMDDII, where "II" is a two-digit identifier for the particular
# game. This function extracts and adds to the data frame the season in which each punt occurred.
# This also accounts for January/February games in different calendar years than the rest of the season
# add_season <- function(punts) {
#
#   punts <- punts %>% add_column(Year=NA_integer_)
#
#   # jan is true if game is in january, else false
#   jan <- floor(((punts$game_id - ((floor(punts$game_id/1000000))*1000000))/10000)) == 1
#
#   # likewise for Febuary
#   feb <- floor(((punts$game_id - ((floor(punts$game_id/1000000))*1000000))/10000)) == 2
#
#   punts$Year[!jan & !feb] <- floor(punts$game_id[!jan & !feb] / 1000000)
#   punts$Year[jan | feb] <- floor(punts$game_id[jan | feb] / 1000000 - 1)
#
#   return(punts)
# }

# Remove blocked punts
# Inputs and outputs a dataframe "punts"
remove_blocks <- function(punts) {
  punts <- punts %>% dplyr::filter(punt_blocked==0)
  return(punts)
}

# Note scoring plays and muffs, but *don't* remove them.  *NOT* wrapped into TrustTheProcess
# Inputs and outputs a dataframe "punts"
note_scores <- function(punts) {
  # All TDs
  punts <- punts %>% tibble::add_column(TDany = dplyr::if_else(stringr::str_detect(punts$desc, 'TOUCHDOWN'), 1, 0))
  # muffs
  punts <- punts %>% tibble::add_column(punt_fumble = dplyr::if_else(stringr::str_detect(punts$desc, 'RECOVERED'), 1, 0))
  # TD returns
  punts <- punts %>%
    add_column(TDreturn = dplyr::if_else(punts$TDany==1 & punts$punt_fumble==0 & !stringr::str_detect(punts$desc, 'NULLIFIED'),
                                  1, 0))

  return(punts)
}

# Remove scoring plays and muffs
# Inputs and outputs a dataframe "punts"
remove_scores <- function(punts) {

  punts <- punts %>% note_scores()  # note scores but don't remove them yet
  punts <- punts %>% dplyr::filter(punt_fumble==0 & TDany==0) # remove scores

  return(punts)
}

# Fix NA value for touchback kick distance
# Inputs and outputs a dataframe "punts"
#
# In the nflscrapr data, touchbacks carry a value of NA for kick_distance
# This function assigns those punts a value for kick_distance as if they had gone to the 20
# If you'd like touchbacks to be treated as if they went elsewhere (say, the goalline, as is standard)
# you can do so by setting correction=0
# We recommend using this default value of correcton=20, representing the actual field-position
# result of a touchback
fix_touchbacks <- function(punts, correction=20) {

   punts <- punts %>% add_column(tb = if_else(str_detect(punts$desc, 'Touchback'), 1, 0))
   punts$GrossYards[punts$tb==1] <- punts$yardline_100[punts$tb==1] - correction

   return(punts)
 }

# Add YardsFromOwnEndZone to data frame
# Inputs and outputs a dataframe "punts"
#
# Useful such that on a graph with YFOEZ as the x-axis, the offensive team goes "left-to-right"
# Note - this function can be used on a dataframe that contains plays other than punts
add_YFOEZ <- function(punts) {

  punts <- punts %>% tibble::add_column(YardsFromOwnEndZone = 100 - punts$yardline_100)

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

# Calculate normal net, which annoyingly isn't in the dataframe to begin with
# Inputs and outputs a dataframe "punts"
calculate_net <- function(punts) {
  punts <- punts %>% tibble::add_column(NetYards = punts$GrossYards - punts$return_yards)
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
  punts <- punts %>% tibble::add_column(PD = if_else(punts$YardsFromOwnEndZone>=threshold, 1, 0))
  return(punts)
}
