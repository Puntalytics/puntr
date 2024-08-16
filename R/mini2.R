###|
###| "Mini2" section (NEW VERSION)
###| Create a dataframe where each punter appears once
###|

#' Summarize data for player careers
#' @description This function is essentially a convenient wrapper for \code{dplyr::summarise}
#' which provides a dataframe with the following columns: NumPunts, pEPA, Gross, Net, RERUN, SHARP_RERUN_OF, SHARP_RERUN_PD,
#' first_year, last_year, team_logo_espn, team_color, team_color2. Additional columns may be added via valid calls to \code{dplyr::summarise}
#' as additional arguments.
#' For comparison of punter seasons, see \code{puntr::by_punter_seasons}; for comparison of punter games, see \code{puntr::by_punter_games}.
#' @param punts The play-by-play punting data to be summarized
#' @param threshold The minimum number of career punts needed to be included, defaults to 64
#' @param ... Any additional arguments will be passed through to \code{dplyr::summarise}
#' @return A tibble where each row is a punter and each column is a stat
#' @examples
#' \dontrun{
#' punters <- by_punters(punts)
#' punters_custom <- by_punters(punts, longest_punt = max(GrossYards))
#' more_punters <- by_punters(punts, threshold=16)
#' }
#' @export
by_punters <- function(punts, ..., threshold=64) {

  punters <- punts %>%
    dplyr::group_by(punter_player_name) %>%
    dplyr::filter(dplyr::n() > threshold) %>%
    custom_summary(...,
                   first_year = min(season),
                   last_year = max(season))

  return(punters)
}

#' Summarize data for player-seasons
#' @description This function is essentially a convenient wrapper for \code{dplyr::summarise}
#' which includes all of the relevant columns from a \code{puntr}-style data frame.
#' This function differs from \code{puntr::create_mini}
#' in that it groups by both \code{punter_player_name} and \code{season} (and adds a convenient \code{seasonid} column to uniquely identify each row).
#' It is unfortunately not customizable beyond the setting of a minimum number of punts to be included;
#' if you have additional parameters you'd like to be summarised, the easiest thing would be to call \code{summarise} yourself.
#' @param punts The play-by-play punting data to be summarized
#' @param threshold The minimum number of punts for a season to be included, defaults to 32
#' @return A tibble where each row is a punter-season and each column is a stat
#' @examples
#' \dontrun{
#' by_punter_seasons(punts)
#' }
#' @export
by_punter_seasons <- function(punts, ..., threshold=32) {

  seasons <- punts %>%
    dplyr::group_by(punter_player_name, season) %>%
    dplyr::filter(dplyr::n() > threshold) %>%
    custom_summary(...)

  seasons <- seasons %>%
    dplyr::mutate(seasonid = purrr::map2_chr(punter_player_name, season, glue::glue, .sep=" "))

  return(seasons)
}

#' Summarize data for player-games
#' @description This function is essentially a convenient wrapper for \code{dplyr::summarise}
#' which includes all of the relevant columns from a \code{puntr}-style data frame.
#' This function differs from \code{puntr::create_mini} and \code{puntr::create_miniY}
#' in that it groups by \code{punter_player_name}, \code{season}, and \code{week}
#' (and adds a convenient \code{weekid} column to uniquely identify each row).
#' It is unfortunately not customizable beyond the setting of a minimum number of punts to be included;
#' if you have additional parameters you'd like to be summarised, the easiest thing would be to call \code{summarise} yourself.
#' @param punts The play-by-play punting data to be summarized
#' @param threshold The minimum number of punts for a week to be included, defaults to 1
#' @return A tibble where each row is a punter-week and each column is a stat
#' @examples
#' \dontrun{
#' by_punter_games(punts)
#' }
#' @export
by_punter_games <- function(punts, ..., threshold=1) {

  games <- punts %>%
    dplyr::group_by(punter_player_name, season, week) %>%
    dplyr::filter(dplyr::n() >= threshold) %>%
    custom_summary(...)

  games <- games %>%
    dplyr::mutate(weekid = purrr::pmap_chr(list(punter_player_name, ' ', season, ' w', week), glue::glue))

  return(games)
}

custom_summary <- function(data, ...) {
  .summary <- data %>%
    dplyr::summarize(NumPunts = dplyr::n(),
                     pEPA = mean(pEPA),
                     Gross = mean(GrossYards),
                     Net = mean(NetYards),
                     RERUN = mean(RERUN),
                     SHARP_RERUN_OF = mean(SHARP_RERUN_OF, na.rm = TRUE),
                     SHARP_RERUN_PD = mean(SHARP_RERUN_PD, na.rm = TRUE),
                     ...,
                     team = puntr::getmode(posteam),
                     team_logo_espn = puntr::getmode(team_logo_espn),
                     team_color = puntr::getmode(team_color),
                     team_color2 = puntr::getmode(team_color2),
  )
  return(.summary)
}
#' Find the mode of a column
#' @description Hilariously, R does not have a built-in method for finding the mode of a column. \code{puntr} has included an internal
#' helper function for this purpose for a while, and now it can be yours too!
#' @param column The dataframe column (or any other list) from which you would like to extract the most frequently occuring item
#' @return The mode of \code{column}
#' @examples
#' \dontrun{
#' players_most_common_team <- getmode(punts_by_punter$posteam)
#' }
#' @export
getmode <- function(column) {
  uniqv <- unique(column)
  uniqv[which.max(tabulate(match(column, uniqv)))]
}
