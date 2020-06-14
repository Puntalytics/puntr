# install.packages("ggimage")
# library(ggimage)

add_logos_miniY <- function(miniY, punts) {

  if(!"team_logo_espn" %in% colnames(miniY)) {
    logos <- nflfastR::teams_colors_logos %>% select(team_abbr, team_logo_espn)
    punts_temp <- punts %>% dplyr::left_join(logos, by = c("posteam" = "team_abbr"))

    for(i in 1:length(miniY$Punter)) {
      miniY$team_code[i] <- punts_temp$posteam[punts_temp$punter_player_name==miniY$Punter[i] & punts_temp$Year==miniY$year[i]]
    }

    miniY <- miniY %>% dplyr::left_join(logos, by = c("team_code" = "team_abbr"))
  }
  return(miniY)
}

add_logos <- function(punts) {
  if(!"team_logo_espn" %in% colnames(punts)) {
    logos <- nflfastR::teams_colors_logos %>% select(team_abbr, team_logo_espn)
    punts <- punts %>% dplyr::left_join(logos, by = c("posteam" = "team_abbr"))
  }
  return(punts)
}
