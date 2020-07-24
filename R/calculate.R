###|
###| "Calculate" section
###| Add one or more columns do df with some new parameter
###|

# Calculate All
# Inputs and outputs a dataframe "punts"
calculate_all <- function(punts) {

  punts <- punts %>% calculate_rerun()
  punts <- punts %>% calculate_sharp()
  # punts < - calculate_pear(punts, plays)
  # punts <- calculate_pear(punts, plays, parameter = "net")
  # punts <- calculate_pear(punts, plays, parameter = "RERUN")

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



# calculate PEAR - Punter points Expected Above Replacement

calculate_pear <- function(punts) {

  ep_ref <- url('https://raw.githubusercontent.com/Puntalytics/puntr/master/data/first_down_ep_reference.csv') %>%
    readr::read_csv()
  ep_ref_after<- ep_ref %>%
    dplyr::rename(YardLineAfter_For_Opponent = YardsFromOwnEndZone)

  punts <- punts %>%
    dplyr::left_join(ep_ref) %>%
    dplyr::rename(ep_before = first_down_ep) %>%
    dplyr::mutate(YardLineAfter_For_Opponent = 100 - round((YardsFromOwnEndZone + RERUN))) %>%
    dplyr::left_join(ep_ref_after) %>%
    dplyr::rename(ep_after = first_down_ep) %>%
    dplyr::mutate(ep_after = -ep_after) %>%
    dplyr::mutate(punt_epa = ep_after - ep_before)

  pear_model <- loess(formula = punt_epa ~ YardsFromOwnEndZone, data = punts, span=0.8, na.action = na.exclude)

  punts <- punts %>% dplyr::mutate(pear = predict(pear_model))



  # if(is.na(parameter)) {
  #
  #   # add ep_dest to punts. This is the EP for the yard line at which the punt lands
  #   punts$ep_dest <- ep_table$ep_smooth[punts$YardsFromOwnEndZone + punts$PuntDistance]
  #
  #   # now smooth this in punt world
  #   punts$ep_dsmooth <- predict(loess(formula = punts$ep_dest ~ punts$YardsFromOwnEndZone),
  #                                              data = punts, model=T, span=.5)
  #   # Calculate PEAR
  #   # subtract ep_dsmooth from each punt's ep_dest and adjust by 0.2 to get replacement instead of average
  #   #arbitrary
  #   punts$PEAR = punts$ep_dest - punts$ep_dsmooth + 0.2
  #
  # } else if(parameter=="net") {
  #   # no comments in these two sections, because it's all the same
  #   punts$ep_dest_net <- ep_table$ep_smooth[punts$YardsFromOwnEndZone + punts$net]
  #   punts$ep_dsmooth_net <- predict(loess(formula = punts$ep_dest_net ~ punts$YardsFromOwnEndZone),
  #                                              data = punts, model=T, span=.5)
  #   punts$PEARnet = punts$ep_dest_net - punts$ep_dsmooth_net
  # # } else if(parameter=="RERUN") {
  # #   # no comments in these two sections, because it's all the same
  # #   punts$ep_dest_rerun <- ep_table$ep_smooth[punts$YardsFromOwnEndZone + as.integer(round(punts$RERUN, digits=0))]
  # #   punts$ep_dsmooth_rerun <- predict(loess(formula = punts$ep_dest_rerun ~ punts$YardsFromOwnEndZone),
  # #                                   data = punts, model=T, span=.5)
  # #   punts$PEAR_RERUN = punts$ep_dest_rerun - punts$ep_dsmooth_rerun + 0.2
  # }

  return(punts)
}
