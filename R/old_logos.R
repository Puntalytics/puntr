# install.packages("ggimage")
#library(ggimage)

add_logos_miniY <- function(miniY, punts) {

  punts <- punts %>% add_logos()

    for(i in 1:length(miniY$punter_player_name)) {
      miniY$team_abbr[i] <- punts_temp$posteam[punts_temp$punter_player_name==miniY$punter_player_name[i] & punts_temp$season==miniY$season[i]]
    }

  miniY <- miniY %>% dplyr::left_join(logos)

  return(miniY)
}

add_logos <- function(punts) {
  if(!"team_logo_espn" %>% is.element(colnames(miniY))) {
    logos <- nflfastR::teams_colors_logos %>%
      select(team_abbr, team_logo_espn, team_color, team_color2)
    punts <- punts %>% dplyr::left_join(logos, by = c("posteam" = "team_abbr"))
  }
  return(punts)
}
