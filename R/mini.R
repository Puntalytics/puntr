###|
###| "Mini" section
###| Create a dataframe where each punter appears once
###|

#' Summarize data for players
#'
#' @param punts The play-by-play punting data to be summarized
#' @param threshold The minimum number of career punts needed to be included, defaults to 64
#' @return A tibble \code{mini} where each row is a punter and each column is a stat
#' @examples
#' mini <- create_mini(punts)
#' @export
create_mini <- function(punts, threshold=64) {

    mini <- punts %>%
      dplyr::group_by(punter_player_name) %>%
      dplyr::filter(n() > threshold) %>%
      dplyr::summarize(NumPunts = n(),
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
                       Punt_epa_avg = mean(punt_epa),
                       Punt_epa_tot = sum(punt_epa),
                       returnpct = mean(returned),
                       #returnpctpd = mean(returned_pd, na.rm = TRUE),
                       #returnpctof = mean(returned_of, na.rm = TRUE),
                       first_year = min(season),
                       last_year = max(season)
                       )
    return(mini)
}

#' Summarize data for player-seasons
#'
#' @param punts The play-by-play punting data to be summarized
#' @param threshold The minimum number of punts for a season to be included, defaults to 32
#' @return A tibble \code{miniY} where each row is a punter-season and each column is a stat
#' @examples
#' miniY <- create_miniY(punts)
#' @export
create_miniY <- function(punts, threshold=32) {

  mini <- punts %>%
    dplyr::group_by(punter_player_name, season) %>%
    dplyr::filter(n() > threshold) %>%
    dplyr::summarize(NumPunts = n(),
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
                     Punt_epa_avg = mean(punt_epa),
                     Punt_epa_tot = sum(punt_epa),
                     #Punter_coach_epa = mean(epa - punt_epa),
                     returnpct = mean(returned),
                     #returnpctpd = mean(returned_pd, na.rm = TRUE),
                     #returnpctof = mean(returned_of, na.rm = TRUE),
                     #PEAR = mean(pear),
                     team_logo_espn = getmode_local(team_logo_espn),
                     team_color = getmode_local(team_color),
                     team_color2 = getmode_local(team_color2)
    )

  mini <- mini %>%
    dplyr::mutate(seasonid = purrr::map2_chr(punter_player_name, season, glue::glue, .sep=" "))

  return(mini)
}

getmode_local <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
