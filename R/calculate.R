###|
###| "Calculate" section
###| Add one or more columns do df with some new parameter
###|

# Calculate All
# Inputs and outputs a dataframe "punts"
#' Calculate metrics
#'
#' @param punts The already-processed play-by-play punting data to which metrics will be added
#' @return A tibble \code{punts} with the calculated metrics added
#' @examples
#' punts <- calculate_all(punts)
#' @export
calculate_all <- function(punts) {

  punts <- punts %>%
    calculate_rerun() %>%
    calculate_sharp() %>%
    dplyr::mutate(returned_pd = dplyr::if_else(PD==1, returned, NA_real_)) %>%
    dplyr::mutate(returned_of = dplyr::if_else(PD==0, returned, NA_real_)) %>%
    calculate_punt_epa()

  return(punts)
}

# Calculate SHARP - Scrimmage Help/Hurt Adjusted Real Punting
# Inputs and outputs a dataframe "punts"

calculate_sharp <- function(punts) {

  sharp_model <- function(input) {
    loess(formula = GrossYards ~ yardline_100, data = input, span=0.8, na.action = na.exclude)
  }

  sharpnet_model <- function(input) {
    loess(formula = NetYards ~ yardline_100, data = input, span=0.9, na.action = na.exclude)
  }

  sharprerun_model <- function(input) {
    loess(formula = RERUN ~ yardline_100, data = input, span=0.9, na.action = na.exclude)
  }

  punts <- punts %>%
    dplyr::group_by(season) %>%
    tidyr::nest() %>%
    dplyr::mutate(model = purrr::map(data, sharp_model)) %>%
    dplyr::mutate(yard_smooth = purrr::map(model, predict)) %>%

    dplyr::mutate(model_net = purrr::map(data, sharpnet_model)) %>%
    dplyr::mutate(yard_smooth_net = purrr::map(model_net, predict)) %>%

    dplyr::mutate(model_rerun = purrr::map(data, sharprerun_model)) %>%
    dplyr::mutate(yard_smooth_rerun = purrr::map(model_rerun, predict)) %>%

    tidyr::unnest(c(data, yard_smooth, yard_smooth_net, yard_smooth_rerun)) %>%
    subset(select = -c(model, model_net, model_rerun)) %>%

    dplyr::mutate(SHARP = GrossYards/yard_smooth * 100) %>%
    dplyr::mutate(SHARP_PD = purrr::pmap_dbl(list(PD==1, SHARP, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(SHARP_OF = purrr::pmap_dbl(list(PD==0, SHARP, NA_real_), dplyr::if_else)) %>%

    dplyr::mutate(SHARPnet = NetYards/yard_smooth_net * 100) %>%
    dplyr::mutate(SHARPnet_PD = purrr::pmap_dbl(list(PD==1, SHARPnet, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(SHARPnet_OF = purrr::pmap_dbl(list(PD==0, SHARPnet, NA_real_), dplyr::if_else)) %>%

    dplyr::mutate(SHARP_RERUN = RERUN/yard_smooth_rerun * 100) %>%
    dplyr::mutate(SHARP_RERUN_PD = purrr::pmap_dbl(list(PD==1, SHARP_RERUN, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(SHARP_RERUN_OF = purrr::pmap_dbl(list(PD==0, SHARP_RERUN, NA_real_), dplyr::if_else))

  punts <- punts %>% dplyr::ungroup()

  return(punts)
}

# calculate RERUN - Return-Edited Real Unbiased Net
# Inputs and outputs a dataframe "punts"
calculate_rerun <- function(punts) {

  rerun_model <- function(input) {
    loess(formula = return_yards_r ~ kick_distance_r, data = input, span=0.65, na.action = na.exclude)
  }

  # new column with actual returns
  # column is NA for punts that aren't returned; those will be omitted from the regression
  punts <- punts %>%
    dplyr::mutate(returned = purrr::pmap_dbl(list(punt_out_of_bounds==0 &
                                                    punt_downed==0 &
                                                    punt_fair_catch==0 &
                                                    touchback==0, 1, 0), dplyr::if_else)) %>%
    dplyr::mutate(return_yards_r = purrr::pmap_dbl(list(returned==1, return_yards, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(kick_distance_r = purrr::pmap_dbl(list(returned==1, GrossYards, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(return_smooth = loess(formula = return_yards_r ~ kick_distance_r,
                                        data = punts,
                                        span=.65,
                                        na.action = na.exclude) %>% predict) %>%
    dplyr::mutate(RERUN = purrr::pmap_dbl(list(returned==1, kick_distance_r - return_smooth, GrossYards), dplyr::if_else))

  return(punts)
}

# calculate EAEPAAE/P - Era-adjusted EPA above expected / punt

calculate_punt_epa <- function(punts) {

  ep_ref <- url('https://raw.githubusercontent.com/Puntalytics/puntr/master/data/fourth_down_ep_reference.csv') %>%
    readr::read_csv(col_types = 'id')
  ep_ref_after <- url('https://raw.githubusercontent.com/Puntalytics/puntr/master/data/first_down_ep_reference.csv') %>%
    readr::read_csv(col_types = 'id') %>%
    dplyr::rename(YardLineAfter_For_Opponent = YardsFromOwnEndZone)

  punts <- punts %>%
    dplyr::left_join(ep_ref) %>%
    dplyr::rename(ep_before = fourth_down_ep) %>%
    dplyr::mutate(YardLineAfter_For_Opponent = 100 - round((YardsFromOwnEndZone + RERUN))) %>%
    dplyr::left_join(ep_ref_after) %>%
    dplyr::rename(ep_after = first_down_ep) %>%
    dplyr::mutate(ep_after = -ep_after) %>%
    dplyr::mutate(punt_epa = ep_after - ep_before)

  epa_model <- function(input) {
    loess(formula = punt_epa ~ YardsFromOwnEndZone, data = input, na.action = na.exclude)
  }

  punts <- punts %>%
    dplyr::mutate(punt_expected_epa = predict(epa_model(punts))) %>%
    dplyr::mutate(punt_epa_above_expected = punt_epa - punt_expected_epa)

  punts <- punts %>%
    dplyr::group_by(season) %>%
    tidyr::nest() %>%
    dplyr::mutate(model = purrr::map(data, epa_model)) %>%
    dplyr::mutate(ea_punt_expected_epa = purrr::map(model, predict))

  punts <- punts %>%
    tidyr::unnest(c(data, ea_punt_expected_epa)) %>%
    subset(select = -c(model))

  punts <- punts %>%
    dplyr::mutate(ea_punt_epa_above_expected = punt_epa - ea_punt_expected_epa)

  punts <- punts %>%
    dplyr::ungroup()

  return(punts)
}
