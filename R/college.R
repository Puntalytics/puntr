# # here we go!
# devtools::install_github(repo = "saiemgilani/cfbscrapR")
# library(tidyverse)
# library(cfbscrapR)
# library(ggimage)
# library(ggrepel)

#' Import college punting data
#'
#' @param years A year or range of years to be scraped
#' @return A tibble \code{punts} of punts in the \code{cfbscrapR} format
#' @examples
#' \dontrun {
#' import_college_punts(2018:2019)
#' }
#' @export
import_college_punts <- function(years) {
  punts <- tidyr::tibble(week = 1:15) %>% tidyr::expand(week, year=years) %>%
    dplyr::mutate(pbp = purrr::map2(year, week, cfbscrapR::cfb_pbp_data, season_type='both')) %>%
    tidyr::unnest(cols = pbp) %>%
    filter(play_type %>% str_detect("Punt"))
  return(punts)
}

#' Convert college data to \code{puntr} format
#'
#' @description Rename columns to play nicely with puntr functions
#' @param punts A data frame containing punts in the cfbscrapR format
#' @param power_five Logical, defaults to TRUE to include only punters from Power 5 teams
#' @return A tibble \code{punts} in a format usable for \code{puntr::calculate_all()}
#' @examples
#' \dontrun {
#' college_to_pro(punts)
#' }
#' @export
college_to_pro <- function(punts, power_five = TRUE) {

  punts <- punts %>%
    dplyr::rename(season = year) %>%
    dplyr::mutate(GrossYards = play_text %>%
             stringr::str_extract("punt for [:digit:]+") %>%
             stringr::str_extract("[:digit:]+") %>%
             as.numeric()) %>%
    dplyr::filter(!is.na(GrossYards)) %>%
    dplyr::mutate(return_yards = as.numeric(yards_gained)) %>%
    dplyr::filter(!is.na(return_yards)) %>%
    dplyr::mutate(NetYards = GrossYards - return_yards) %>%
    dplyr::mutate(punter_player_name = play_text %>%
             stringr::str_extract(".+(?= punt for)")) %>%
    dplyr::mutate(YardsFromOwnEndZone = as.integer(100 - yards_to_goal)) %>%
    dplyr::filter(YardsFromOwnEndZone <= 70) %>%
    dplyr::mutate(touchback = play_text %>% stringr::str_detect("ouchback")) %>%
    dplyr::mutate(GrossYards = dplyr::if_else(touchback, as.numeric(GrossYards-20), as.numeric(GrossYards))) %>%
    dplyr::mutate(punt_out_of_bounds = play_text %>% stringr::str_detect("out.of.bounds")) %>%
    dplyr::mutate(punt_fair_catch = play_text %>% stringr::str_detect("air catch")) %>%
    dplyr::mutate(punt_downed = play_text %>% stringr::str_detect("downed")) %>%
    dplyr::mutate(PD = dplyr::if_else(YardsFromOwnEndZone >=41, 1, 0))

  team_info <- cfbscrapR::cfb_team_info()

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

  punts <- punts %>% dplyr::inner_join(team_info, by = c("offense_play" = "team_abbr"))

  return(punts)
}
