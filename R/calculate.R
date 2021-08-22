###|
###| "Calculate" section
###| Add one or more columns do df with some new parameter
###|

# Calculate All
# Inputs and outputs a dataframe "punts"
#' Calculate metrics
#'
#' @description This function adds all Puntalytics metrics to your punting data: RERUN, SHARP, SHARPnet and SHARP_RERUN
#' (plus open-field and pin-deep versions of the 3 SHARPs) as well as average and cumulative EPA (our custom EPA metric.)
#' This function will continue to be updated as new metrics are added.
#' Currently, functions for calculating individual metrics are for internal use only, and not exported.
#' @param punts The already-processed play-by-play punting data to which metrics will be added. Must contain at least three seasons and 1000 punts.
#' @return A tibble \code{punts} with the calculated metrics added
#' @examples
#' \dontrun{
#' calculate_all(punts)
#' }
#' @export
calculate_all <- function(punts) {

  # Check the size of the data and throw and error and/or warning
  if (nrow(punts) < 1000) {
    stop(
"Calculating with fewer than 1000 punts is no longer supported by `puntr`.
Add more punts and/or seasons to your dataframe for accurate calculation")
  } else if (max(punts$season) - min(punts$season) <= 1) {
    stop(
"Calculating statistics using only one or two seasons is no longer supported by `puntr`.
Use at least three seasons to your dataframe for best results."
    )
  }

  # Do all pre-nesting processing ####
  # RERUN-related processing
  message("puntr::calculate_all - Preparing data for calculations")
  punts <- punts %>%
    dplyr::mutate(returned = purrr::pmap_dbl(list(punt_out_of_bounds==0 &
                                                    punt_downed==0 &
                                                    punt_fair_catch==0 &
                                                    touchback==0, 1, 0), dplyr::if_else)) %>%
    dplyr::mutate(return_yards_r = purrr::pmap_dbl(list(returned==1, return_yards, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(kick_distance_r = purrr::pmap_dbl(list(returned==1, GrossYards, NA_real_), dplyr::if_else))

  # EPA-related processing
  ep_ref <- url('https://raw.githubusercontent.com/Puntalytics/puntr-data/master/data/fourth_down_ep_reference.csv') %>%
    readr::read_csv(col_types = 'id')
  ep_ref_after <- url('https://raw.githubusercontent.com/Puntalytics/puntr-data/master/data/first_down_ep_reference.csv') %>%
    readr::read_csv(col_types = 'id') %>%
    dplyr::rename(YardLineAfter_For_Opponent = YardsFromOwnEndZone)

  message("puntr::calculate_all - Declaring models")

  # declare models
  rerun_model <- function(input) {
    loess(formula = return_yards_r ~ kick_distance_r, data = input, span=0.65, na.action = na.exclude)
  }
  sharp_model <- function(input) {
    loess(formula = GrossYards ~ YardsFromOwnEndZone, data = input, span=0.8, na.action = na.exclude)
  }
  sharpnet_model <- function(input) {
    loess(formula = NetYards ~ YardsFromOwnEndZone, data = input, span=0.9, na.action = na.exclude)
  }
  sharprerun_model <- function(input) {
    loess(formula = RERUN ~ YardsFromOwnEndZone, data = input, span=0.9, na.action = na.exclude)
  }
  epa_model <- function(input) {
    loess(formula = punt_epa ~ YardsFromOwnEndZone, data = input, na.action = na.exclude)
  }

  message("puntr::calculate_all - Calculating RERUN")
  tictoc::tic("Calculated RERUN")

  # roll_join and compute rerun
  punts_roll <- punts %>%
    roll_join() %>%
    # compute return_smooth and RERUN
    dplyr::mutate(r_model = purrr::map(data_all$data, rerun_model)) %>%
    dplyr::mutate(return_smooth = purrr::pmap(list(r_model, data), predict)) %>%

    tidyr::unnest(c(data, return_smooth)) %>%
    dplyr::select(-c(roll1, roll2, data_roll1, data_roll2, data_all, r_model)) %>%

    dplyr::mutate(RERUN = purrr::pmap_dbl(list(returned==1,
                                               kick_distance_r - return_smooth, GrossYards),
                                          dplyr::if_else))

  tictoc::toc()
  message("puntr::calculate_all - Calculating SHARP")
  tictoc::tic("Calculated SHARP")

    # roll_join again and compute SHARP and variants
  punts_roll <- punts_roll %>%
    roll_join() %>%
    dplyr::mutate(model = purrr::map(data_all$data, sharp_model)) %>%
    dplyr::mutate(yard_smooth = purrr::pmap(list(model, data), predict)) %>%

    dplyr::mutate(model_net = purrr::map(data_all$data, sharpnet_model)) %>%
    dplyr::mutate(yard_smooth_net = purrr::pmap(list(model_net, data), predict)) %>%

    dplyr::mutate(model_rerun = purrr::map(data_all$data, sharprerun_model)) %>%
    dplyr::mutate(yard_smooth_rerun = purrr::pmap(list(model_rerun, data), predict)) %>%

    tidyr::unnest(c(data, yard_smooth, yard_smooth_net, yard_smooth_rerun)) %>%
    dplyr::select(-c(roll1, roll2, data_roll1, data_roll2, data_all, model, model_net, model_rerun)) %>%

    dplyr::mutate(SHARP = GrossYards/yard_smooth * 100) %>%
    dplyr::mutate(SHARP_PD = purrr::pmap_dbl(list(PD==1, SHARP, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(SHARP_OF = purrr::pmap_dbl(list(PD==0, SHARP, NA_real_), dplyr::if_else)) %>%

    dplyr::mutate(SHARPnet = NetYards/yard_smooth_net * 100) %>%
    dplyr::mutate(SHARPnet_PD = purrr::pmap_dbl(list(PD==1, SHARPnet, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(SHARPnet_OF = purrr::pmap_dbl(list(PD==0, SHARPnet, NA_real_), dplyr::if_else)) %>%

    dplyr::mutate(SHARP_RERUN = RERUN/yard_smooth_rerun * 100) %>%
    dplyr::mutate(SHARP_RERUN_PD = purrr::pmap_dbl(list(PD==1, SHARP_RERUN, NA_real_), dplyr::if_else)) %>%
    dplyr::mutate(SHARP_RERUN_OF = purrr::pmap_dbl(list(PD==0, SHARP_RERUN, NA_real_), dplyr::if_else)) %>%

    dplyr::left_join(ep_ref, by = "YardsFromOwnEndZone") %>%
    dplyr::rename(ep_before = fourth_down_ep) %>%
    dplyr::mutate(YardLineAfter_For_Opponent = 100 - round((YardsFromOwnEndZone + RERUN))) %>%
    dplyr::left_join(ep_ref_after, by = "YardLineAfter_For_Opponent") %>%
    dplyr::rename(ep_after = first_down_ep) %>%
    dplyr::mutate(ep_after = -ep_after) %>%
    dplyr::mutate(punt_epa = ep_after - ep_before)

  tictoc::toc()
  message("puntr::calculate_all - Calculating pEPA")
  tictoc::tic("Calculated pEPA")

  # roll_join a third time and compute EPA
  punts_roll <- punts_roll %>%
    roll_join() %>%
    dplyr::mutate(model_e = purrr::map(data_all$data, epa_model)) %>%
    dplyr::mutate(ea_punt_expected_epa = purrr::pmap(list(model_e, data), predict)) %>%

    tidyr::unnest(c(data, ea_punt_expected_epa)) %>%
    dplyr::select(-c(roll1, roll2, data_roll1, data_roll2, data_all, model_e)) %>%

    dplyr::mutate(ea_punt_epa_above_expected = punt_epa - ea_punt_expected_epa) %>%
    dplyr::mutate(pEPA = ea_punt_epa_above_expected) %>%

    dplyr::ungroup()

  tictoc::toc()

  return(punts_roll)
}

roll_join <- function(punts) {

  min_season <- min(punts$season)

  # assign associated seasons for rolling averages
  punts <- punts %>%
    dplyr::group_by(season) %>%
    tidyr::nest()

  punts <- punts %>%
    dplyr::mutate(roll1 = dplyr::case_when(season == min_season ~ season+1,
                                           season == (min_season + 1) ~ season+1,
                                           season > (min_season + 1) ~ season-1)) %>%
    dplyr::mutate(roll2 = dplyr::case_when(season == min_season ~ season+2,
                                           season == (min_season + 1) ~ season-1,
                                           season > (min_season + 1) ~ season-2)) %>%
    dplyr::left_join(punts, by = c('roll1' = 'season'), suffix = c('', '_roll1')) %>%
    dplyr::left_join(punts, by = c('roll2' = 'season'), suffix = c('', '_roll2')) %>%
    # bind seasons with their rolling buddies
    dplyr::mutate(data_all =
                    dplyr::bind_rows(list(data, data_roll1, data_roll2)) %>%
                    tidyr::nest(data=dplyr::everything()))

  return(punts)
}
#
# # calculate RERUN - Return-Edited Real Unbiased Net
# # Inputs and outputs a dataframe "punts"
# calculate_rerun <- function(punts) {
#
#   rerun_model <- function(input) {
#     loess(formula = return_yards_r ~ kick_distance_r, data = input, span=0.65, na.action = na.exclude)
#   }
#
#   # new column with actual returns
#   # column is NA for punts that aren't returned; those will be omitted from the regression
#   punts <- punts %>%
#     dplyr::mutate(returned = purrr::pmap_dbl(list(punt_out_of_bounds==0 &
#                                                     punt_downed==0 &
#                                                     punt_fair_catch==0 &
#                                                     touchback==0, 1, 0), dplyr::if_else)) %>%
#     dplyr::mutate(return_yards_r = purrr::pmap_dbl(list(returned==1, return_yards, NA_real_), dplyr::if_else)) %>%
#     dplyr::mutate(kick_distance_r = purrr::pmap_dbl(list(returned==1, GrossYards, NA_real_), dplyr::if_else))
#
#
#   punts <- punts %>%
#     dplyr::group_by(season) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(r_model = purrr::map(data, rerun_model)) %>%
#     dplyr::mutate(return_smooth = purrr::map(r_model, predict)) %>%
#     tidyr::unnest(c(data, return_smooth)) %>%
#     subset(select = -c(r_model)) %>%
#     dplyr::mutate(RERUN = purrr::pmap_dbl(list(returned==1,
#                                                kick_distance_r - return_smooth, GrossYards),
#                                           dplyr::if_else)) %>%
#     dplyr::ungroup()
#
#   # dplyr::mutate(return_smooth = loess(formula = return_yards_r ~ kick_distance_r,
#   #                                     data = punts,
#   #                                     span=.65,
#   #                                     na.action = na.exclude) %>% predict) %>%
#   # dplyr::mutate(RERUN = purrr::pmap_dbl(list(returned==1, kick_distance_r - return_smooth, GrossYards), dplyr::if_else))
#
#   return(punts)
# }
#
# # Calculate SHARP - Scrimmage Help/Hurt Adjusted Real Punting
# # Inputs and outputs a dataframe "punts"
#
# calculate_sharp <- function(punts) {
#
#   sharp_model <- function(input) {
#     loess(formula = GrossYards ~ YardsFromOwnEndZone, data = input, span=0.8, na.action = na.exclude)
#   }
#
#   sharpnet_model <- function(input) {
#     loess(formula = NetYards ~ YardsFromOwnEndZone, data = input, span=0.9, na.action = na.exclude)
#   }
#
#   sharprerun_model <- function(input) {
#     loess(formula = RERUN ~ YardsFromOwnEndZone, data = input, span=0.9, na.action = na.exclude)
#   }
#
#   punts <- punts %>%
#     dplyr::group_by(season) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(model = purrr::map(data, sharp_model)) %>%
#     dplyr::mutate(yard_smooth = purrr::map(model, predict)) %>%
#
#     dplyr::mutate(model_net = purrr::map(data, sharpnet_model)) %>%
#     dplyr::mutate(yard_smooth_net = purrr::map(model_net, predict)) %>%
#
#     dplyr::mutate(model_rerun = purrr::map(data, sharprerun_model)) %>%
#     dplyr::mutate(yard_smooth_rerun = purrr::map(model_rerun, predict)) %>%
#
#     tidyr::unnest(c(data, yard_smooth, yard_smooth_net, yard_smooth_rerun)) %>%
#     subset(select = -c(model, model_net, model_rerun)) %>%
#
#     dplyr::mutate(SHARP = GrossYards/yard_smooth * 100) %>%
#     dplyr::mutate(SHARP_PD = purrr::pmap_dbl(list(PD==1, SHARP, NA_real_), dplyr::if_else)) %>%
#     dplyr::mutate(SHARP_OF = purrr::pmap_dbl(list(PD==0, SHARP, NA_real_), dplyr::if_else)) %>%
#
#     dplyr::mutate(SHARPnet = NetYards/yard_smooth_net * 100) %>%
#     dplyr::mutate(SHARPnet_PD = purrr::pmap_dbl(list(PD==1, SHARPnet, NA_real_), dplyr::if_else)) %>%
#     dplyr::mutate(SHARPnet_OF = purrr::pmap_dbl(list(PD==0, SHARPnet, NA_real_), dplyr::if_else)) %>%
#
#     dplyr::mutate(SHARP_RERUN = RERUN/yard_smooth_rerun * 100) %>%
#     dplyr::mutate(SHARP_RERUN_PD = purrr::pmap_dbl(list(PD==1, SHARP_RERUN, NA_real_), dplyr::if_else)) %>%
#     dplyr::mutate(SHARP_RERUN_OF = purrr::pmap_dbl(list(PD==0, SHARP_RERUN, NA_real_), dplyr::if_else))
#
#   punts <- punts %>% dplyr::ungroup()
#
#   return(punts)
# }
#
# # calculate EAEPAAE/P - Era-adjusted EPA above expected / punt
#
# calculate_punt_epa <- function(punts) {
#
#   ep_ref <- url('https://raw.githubusercontent.com/Puntalytics/puntr-data/master/data/fourth_down_ep_reference.csv') %>%
#     readr::read_csv(col_types = 'id')
#   ep_ref_after <- url('https://raw.githubusercontent.com/Puntalytics/puntr-data/master/data/first_down_ep_reference.csv') %>%
#     readr::read_csv(col_types = 'id') %>%
#     dplyr::rename(YardLineAfter_For_Opponent = YardsFromOwnEndZone)
#
#   punts <- punts %>%
#     dplyr::left_join(ep_ref, by = "YardsFromOwnEndZone") %>%
#     dplyr::rename(ep_before = fourth_down_ep) %>%
#     dplyr::mutate(YardLineAfter_For_Opponent = 100 - round((YardsFromOwnEndZone + RERUN))) %>%
#     dplyr::left_join(ep_ref_after, by = "YardLineAfter_For_Opponent") %>%
#     dplyr::rename(ep_after = first_down_ep) %>%
#     dplyr::mutate(ep_after = -ep_after) %>%
#     dplyr::mutate(punt_epa = ep_after - ep_before)
#
#   epa_model <- function(input) {
#     loess(formula = punt_epa ~ YardsFromOwnEndZone, data = input, na.action = na.exclude)
#   }
#
#   punts <- punts %>%
#     dplyr::mutate(punt_expected_epa = predict(epa_model(punts))) %>%
#     dplyr::mutate(punt_epa_above_expected = punt_epa - punt_expected_epa)
#
#   punts <- punts %>%
#     dplyr::group_by(season) %>%
#     tidyr::nest() %>%
#     dplyr::mutate(model = purrr::map(data, epa_model)) %>%
#     dplyr::mutate(ea_punt_expected_epa = purrr::map(model, predict))
#
#   punts <- punts %>%
#     tidyr::unnest(c(data, ea_punt_expected_epa)) %>%
#     subset(select = -c(model))
#
#   punts <- punts %>%
#     dplyr::mutate(ea_punt_epa_above_expected = punt_epa - ea_punt_expected_epa)
#
#   punts <- punts %>%
#     dplyr::ungroup()
#
#   return(punts)
# }
