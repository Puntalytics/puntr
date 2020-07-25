###|
###| "Mini" section
###| Create a dataframe where each punter appears once
###|

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
                       PEAR = mean(pear),
                       first_year = min(season),
                       last_year = max(season)
                       )
    return(mini)
}


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
                     PEAR = mean(pear)
    )
  return(mini)
}
