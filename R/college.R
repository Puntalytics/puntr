#' Import college punting data
#' @description Import college punting data for seasons in the scope of \code{cfbfastR} (back to 2014).
#' This function is a wrapper around \code{cfbfastR::load_cfb_pbp}.
#' @param years A year or range of years to be scraped
#' @return A tibble \code{punts} of punts in the \code{cfbfastR} format
#' @examples
#' \dontrun{
#' import_college_punts(2018:2021)
#' }
#' @export
import_college_punts <- function(years) {
  punts <- purrr::map_df(years, function(x){
    cfbfastR::load_cfb_pbp(x) %>%
      dplyr::filter(punt == 1) %>%
      dplyr::mutate(season = x)
  })
  return(punts)
}

#' Convert college data to \code{puntr} format
#'
#' @description Rename columns and process data such that the output can be plugged directly into \code{puntr::calculate_all},
#' and the output of that can be plugged directly into \code{puntr::create_mini} (or \code{puntr::create_miniY}).
#' @param punts A data frame containing punts in the cfbfastR format
#' @param power_five Logical, defaults to TRUE to include only punters from Power 5 teams
#' @return A tibble \code{punts} in a format usable for \code{puntr::calculate_all}
#' @examples
#' \dontrun{
#' college_to_pro(punts)
#' }
#' @export
college_to_pro <- function(punts, power_five = TRUE) {

  punts <- punts %>%
    #dplyr::rename(season = year) %>%
    dplyr::mutate(GrossYards = play_text %>%
             stringr::str_extract("punt for [:digit:]+") %>%
             stringr::str_extract("[:digit:]+") %>%
             as.numeric()) %>%
    dplyr::filter(!is.na(GrossYards)) %>%
    dplyr::mutate(return_yards = as.numeric(yds_punt_return),
                  return_yards = ifelse(is.na(return_yards),0,return_yards),
                  return_yards = ifelse(stringr::str_detect(play_text,"loss"),-1*return_yards,return_yards)) %>%
    #dplyr::filter(!is.na(return_yards)) %>%
    dplyr::mutate(punter_player_name = play_text %>%
             stringr::str_extract(".+(?= punt for)")) %>%
    dplyr::mutate(YardsFromOwnEndZone = as.integer(100 - yards_to_goal)) %>%
    dplyr::filter(YardsFromOwnEndZone <= 70) %>%
    dplyr::mutate(touchback = play_text %>% stringr::str_detect("ouchback")) %>%
    dplyr::mutate(return_yards = dplyr::if_else(touchback, 0, return_yards)) %>%
    dplyr::mutate(NetYards = GrossYards - return_yards) %>%
    dplyr::mutate(GrossYards = dplyr::if_else(touchback, as.numeric(GrossYards-20), as.numeric(GrossYards))) %>%
    dplyr::mutate(punt_out_of_bounds = play_text %>% stringr::str_detect("out.of.bounds")) %>%
    dplyr::mutate(punt_fair_catch = play_text %>% stringr::str_detect("air catch")) %>%
    dplyr::mutate(punt_downed = play_text %>% stringr::str_detect("downed")) %>%
    dplyr::mutate(PD = dplyr::if_else(YardsFromOwnEndZone >=41, 1, 0)) %>%
    #rename columns to avoid breaking calculate_all()
    dplyr::rename(ep_before_cfb = ep_before,
                  ep_after_cfb = ep_after)



  # Pull from data-repo to avoid requiring cfbd API key
  #team_info <- cfbfastR::cfbd_team_info()
  team_info <- readRDS(url("https://github.com/saiemgilani/cfbfastR-data/blob/master/team_info/rds/cfb_team_info_2020.rds?raw=true"))

  if(power_five) {
    team_info <- team_info %>%
      dplyr::filter(conference %in% c("Pac-12","SEC","Big Ten","Big 12","ACC"))
  }

  team_info <- team_info %>%
    dplyr::mutate(logo = purrr::map(logos,magrittr::extract2,1),
           logo = as.character(logo)) %>%
    dplyr::select(school, logo, color, alt_color) %>%
    dplyr::rename(team_abbr = school,
           team_logo_espn = logo,
           team_color = color,
           team_color2 = alt_color)

  punts <- punts %>% dplyr::inner_join(team_info, by = c("pos_team" = "team_abbr"))

  return(punts)
}
