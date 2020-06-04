install.packages("ggimage")
library(ggimage)
logos <- teams_colors_logos %>% select(team_abbr, team_logo_espn)
punts <- punts %>% left_join(logos, by = c("posteam" = "team_abbr"))
for(i in 1:length(miniY$Punter)) {
  miniY$team_code[i]<-punts$posteam[punts$punter_player_name==miniY$Punter[i]&punts$Year==miniY$year[i]]
}
miniY <- miniY %>% left_join(logos, by = c("team_code" = "team_abbr"))