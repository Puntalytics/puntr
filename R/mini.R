###|
###| "Mini" section (OLD VERSION)
###| Create a dataframe where each punter appears once
###|

#' Summarize data for players
#' @description This function is essentially a convenient wrapper for \code{dplyr::summarise}
#' which includes all of the relevant columns from a \code{puntr}-style data frame.
#' NOTE: \code{puntr::create_mini}, \code{puntr::create_miniY}, and \code{puntr::create_miniG} are being phased out
#' in favor of \code{puntr::by_punters}, \code{puntr::by_punter_seasons}, and \code{puntr::by_punter_games}
#' @param punts The play-by-play punting data to be summarized
#' @param threshold The minimum number of career punts needed to be included, defaults to 64
#' @param ... Any additional arguments will be passed through to \code{dplyr::summarise}
#' @return A tibble \code{mini} where each row is a punter and each column is a stat
#' @examples
#' \dontrun{
#' create_mini(punts)
#' }
#' @export
create_mini <- function(punts, ..., threshold=64) {

  mini <- punts %>%
    dplyr::group_by(punter_player_name) %>%
    dplyr::filter(dplyr::n() > threshold) %>%
    dplyr::summarize(NumPunts = dplyr::n(),
                     Gross = mean(GrossYards),
                     Net = mean(NetYards),
                     RERUN = mean(RERUN),
                     SHARP = mean(SHARP),
                     SHARP_OF = mean(SHARP_OF, na.rm = TRUE),
                     SHARP_PD = mean(SHARP_PD, na.rm = TRUE),
                     SHARPnet = mean(SHARPnet),
                     SHARPnet_OF = mean(SHARPnet_OF, na.rm = TRUE),
                     SHARPnet_PD = mean(SHARPnet_PD, na.rm = TRUE),
                     SHARP_RERUN = mean(SHARP_RERUN),
                     SHARP_RERUN_OF = mean(SHARP_RERUN_OF, na.rm = TRUE),
                     SHARP_RERUN_PD = mean(SHARP_RERUN_PD, na.rm = TRUE),
                     Punt_eaepaae_avg = mean(pEPA),
                     pEPA = mean(pEPA),
                     Punt_eaepaae_tot = sum(pEPA),
                     returnpct = mean(returned),
                     first_year = min(season),
                     last_year = max(season),
                     team_logo_espn = getmode_local(team_logo_espn),
                     team_color = getmode_local(team_color),
                     team_color2 = getmode_local(team_color2),
                     ...
    )
  return(mini)
}

#' Summarize data for player-seasons
#' @description This function is essentially a convenient wrapper for \code{dplyr::summarise}
#' which includes all of the relevant columns from a \code{puntr}-style data frame.
#' This function differs from \code{puntr::create_mini}
#' in that it groups by both \code{punter_player_name} and \code{season} (and adds a convenient \code{seasonid} column to uniquely identify each row).
#' NOTE: \code{puntr::create_mini}, \code{puntr::create_miniY}, and \code{puntr::create_miniG} are being phased out
#' in favor of \code{puntr::by_punters}, \code{puntr::by_punter_seasons}, and \code{puntr::by_punter_games}
#' @param punts The play-by-play punting data to be summarized
#' @param threshold The minimum number of punts for a season to be included, defaults to 32
#' @param ... Any additional arguments will be passed through to \code{dplyr::summarise}
#' @return A tibble \code{miniY} where each row is a punter-season and each column is a stat
#' @examples
#' \dontrun{
#' create_miniY(punts)
#' }
#' @export
create_miniY <- function(punts, ..., threshold=32) {

  mini <- punts %>%
    dplyr::group_by(punter_player_name, season) %>%
    dplyr::filter(dplyr::n() > threshold) %>%
    dplyr::summarize(NumPunts = dplyr::n(),
                     Gross = mean(GrossYards),
                     Net = mean(NetYards),
                     RERUN = mean(RERUN),
                     SHARP = mean(SHARP),
                     SHARP_OF = mean(SHARP_OF, na.rm = TRUE),
                     SHARP_PD = mean(SHARP_PD, na.rm = TRUE),
                     SHARPnet = mean(SHARPnet),
                     SHARPnet_OF = mean(SHARPnet_OF, na.rm = TRUE),
                     SHARPnet_PD = mean(SHARPnet_PD, na.rm = TRUE),
                     SHARP_RERUN = mean(SHARP_RERUN),
                     SHARP_RERUN_OF = mean(SHARP_RERUN_OF, na.rm = TRUE),
                     SHARP_RERUN_PD = mean(SHARP_RERUN_PD, na.rm = TRUE),
                     Punt_eaepaae_avg = mean(pEPA),
                     pEPA = mean(pEPA),
                     Punt_eaepaae_tot = sum(pEPA),
                     returnpct = mean(returned),
                     team_logo_espn = getmode_local(team_logo_espn),
                     team_color = getmode_local(team_color),
                     team_color2 = getmode_local(team_color2),
                     ...
    )

  mini <- mini %>%
    dplyr::mutate(seasonid = purrr::map2_chr(punter_player_name, season, glue::glue, .sep=" "))

  return(mini)
}

#' Summarize data for player-games
#' @description This function is essentially a convenient wrapper for \code{dplyr::summarise}
#' which includes all of the relevant columns from a \code{puntr}-style data frame.
#' This function differs from \code{puntr::create_mini} and \code{puntr::create_miniY}
#' in that it groups by \code{punter_player_name}, \code{season}, and \code{week}
#' (and adds a convenient \code{weekid} column to uniquely identify each row).
#' NOTE: \code{puntr::create_mini}, \code{puntr::create_miniY}, and \code{puntr::create_miniG} are being phased out
#' in favor of \code{puntr::by_punters}, \code{puntr::by_punter_seasons}, and \code{puntr::by_punter_games}
#' @param punts The play-by-play punting data to be summarized
#' @param threshold The minimum number of punts for a week to be included, defaults to 1
#' @param ... Any additional arguments will be passed through to \code{dplyr::summarise}
#' @return A tibble \code{miniG} where each row is a punter-week and each column is a stat
#' @examples
#' \dontrun{
#' create_miniG(punts)
#' }
#' @export
create_miniG <- function(punts, ..., threshold=1) {

  mini <- punts %>%
    dplyr::group_by(punter_player_name, season, week) %>%
    dplyr::filter(dplyr::n() >= threshold) %>%
    dplyr::summarize(NumPunts = dplyr::n(),
                     Gross = mean(GrossYards),
                     Net = mean(NetYards),
                     RERUN = mean(RERUN),
                     SHARP = mean(SHARP),
                     SHARP_OF = mean(SHARP_OF, na.rm = TRUE),
                     SHARP_PD = mean(SHARP_PD, na.rm = TRUE),
                     SHARPnet = mean(SHARPnet),
                     SHARPnet_OF = mean(SHARPnet_OF, na.rm = TRUE),
                     SHARPnet_PD = mean(SHARPnet_PD, na.rm = TRUE),
                     SHARP_RERUN = mean(SHARP_RERUN),
                     SHARP_RERUN_OF = mean(SHARP_RERUN_OF, na.rm = TRUE),
                     SHARP_RERUN_PD = mean(SHARP_RERUN_PD, na.rm = TRUE),
                     Punt_eaepaae_avg = mean(pEPA),
                     pEPA = mean(pEPA),
                     Punt_eaepaae_tot = sum(pEPA),
                     returnpct = mean(returned),
                     team_logo_espn = getmode_local(team_logo_espn),
                     team_color = getmode_local(team_color),
                     team_color2 = getmode_local(team_color2),
                     ...
    )

  mini <- mini %>%
    dplyr::mutate(weekid = purrr::pmap_chr(list(punter_player_name, ' ', season, ' w', week), glue::glue))

  return(mini)
}

getmode_local <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
