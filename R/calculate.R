###|
###| "Calculate" section
###| Add one or more columns do df with some new parameter
###|

# Calculate All
# Inputs and outputs a dataframe "punts"
calculate_all <- function(punts
                          #  , plays
) {

  punts <- punts %>% calculate_rerun()
  punts <- punts %>% calculate_sharp()
  punts <- punts %>% calculate_sharp(parameter = "net")
  punts <- punts %>% calculate_sharp(parameter = "RERUN")
  # punts < - calculate_pear(punts, plays)
  # punts <- calculate_pear(punts, plays, parameter = "net")
  # punts <- calculate_pear(punts, plays, parameter = "RERUN")

  return(punts)
}

# Calculate SHARP - Scrimmage Help/Hurt Adjusted Real Punting
# Inputs and outputs a dataframe "punts"
#
# Calculate for gross (no parameter), parameter = "net", or parameter = "RERUN"

calculate_sharp <- function(punts) {

  sharp_model <- function(input) {
    loess(formula = GrossYards ~ yardline_100, data = input, model=T, span=.75, na.action = na.exclude)
  }

  punts <- punts %>%
    dplyr::group_by(season) %>%
    tidyr::nest() %>%
    dplyr::mutate(model = purrr::map(data, sharp_model))

  punts <- punts %>%
    dplyr::mutate(yard_smooth = purrr::map2(data, model, predict))
#
#   punts <- punts %>% tidyr::unnest(data)
#
#   punts <- punts %>%
#     dplyr::mutate(yard_smooth = purrr::map(predict(model)))

  return(punts)
}

calculate_sharp_old <- function(punts, parameter=NA) {
  if(is.na(parameter)) {
    punts <- punts %>% tibble::add_column(yard_smooth =
                                    loess(formula = GrossYards ~ yardline_100, data = punts, model=T, span=.75, na.action = na.exclude)
                                  %>% predict())

    # center punts around 100
    punts <- punts %>% tibble::add_column(SHARP = punts$GrossYards / punts$yard_smooth * 100)

    punts <- punts %>% tibble::add_column(SHARP_PD = ifelse(punts$PD==1, punts$SHARP, NA))
    punts <- punts %>% tibble::add_column(SHARP_OF = ifelse(punts$PD==0, punts$SHARP, NA))

  } else if(parameter=="net") {

    punts <- punts %>% tibble::add_column(yard_smooth_net =
                                    loess(formula = NetYards ~ yardline_100, data = punts, model=T, span=.75, na.action = na.exclude)
                                  %>% predict())

    # center punts
    punts <- punts %>% tibble::add_column(SHARPnet = punts$NetYards / punts$yard_smooth_net * 100)

    punts <- punts %>% tibble::add_column(SHARPnet_PD = ifelse(punts$PD==1, punts$SHARPnet, NA))
    punts <- punts %>% tibble::add_column(SHARPnet_OF = ifelse(punts$PD==0, punts$SHARPnet, NA))

  } else if(parameter=="RERUN") {

    punts <- punts %>% tibble::add_column(yard_smooth_rerun =
                                    loess(formula = RERUN ~ yardline_100, data = punts, model=T, span=.75, na.action = na.exclude)
                                  %>% predict())

    # center punts
    punts <- punts %>% tibble::add_column(SHARP_RERUN = punts$RERUN / punts$yard_smooth_rerun * 100)

    punts <- punts %>% tibble::add_column(SHARP_RERUN_PD = ifelse(punts$PD==1, punts$SHARP_RERUN, NA))
    punts <- punts %>% tibble::add_column(SHARP_RERUN_OF = ifelse(punts$PD==0, punts$SHARP_RERUN, NA))
  }

  return(punts)
}

# calculate RERUN - Return-Edited Real Unbiased Net
# Inputs and outputs a dataframe "punts"
calculate_rerun <- function(punts) {

  # new column with actual returns
  # column is NA for punts that aren't returned; those will be omitted from the regression
  punts <- punts %>% tibble::add_column(returned =
                                  dplyr::if_else(punts$punt_out_of_bounds==0 & punts$punt_downed==0 &
                                            punts$punt_fair_catch==0 & punts$touchback==0, 1, 0))

  punts <- punts %>% tibble::add_column(return_yards_r =
                                  ifelse(punts$returned==1, punts$return_yards, NA))

  punts <- punts %>% tibble::add_column(kick_distance_r =
                                  ifelse(punts$returned==1, punts$GrossYards, NA))

  # create smoothed average return for each punt distance
  # only using returned punts, so the curve isn't biased by all the 0s

  punts <- punts %>% tibble::add_column(return_smooth =
                                  loess(formula = return_yards_r ~ kick_distance_r, data = punts,
                                        model=T, span=.65, na.action = na.exclude)
                                %>% predict())

  # punts <- cbind(punts, return_smooth=NA_integer_)
  # smooth <- predict(loess(formula = Returns ~ kick_distance_R, data = punts, model=T, span=.65))
  # punts$return_smooth[!is.na(punts$Returns)] <- smooth
  #
  # add RERUN
  # RERUN = kick_distance for non-returned punts
  # RERUN = kick_distance - return_smooth for returned punts
  punts <- punts %>% tibble::add_column(RERUN =
                                  dplyr::if_else(punts$returned==1,
                                          punts$kick_distance_r - punts$return_yards_r,
                                          punts$GrossYards))
  # punts <- cbind(punts, RERUN <- NA_integer_)
  # punts$RERUN[!is.na(punts$Returns)] <- punts$kick_distance_R[!is.na(punts$Returns)] -
  #   punts$return_smooth[!is.na(punts$Returns)]
  # punts$RERUN[is.na(punts$Returns)] <- punts$GrossYards[is.na(punts$Returns)]
  #
  # punts$RERUN[punts$RERUN < 1] <- 1

  return(punts)
}

# # pear helper function, not useful alone
# reference_ep <- function(yardline, plays) {
#
# }

# calculate PEAR - Punter points Expected Above Replacement
# Inputs: Two dataframes, "punts" and "plays"
# Outputs: Dataframe "punts" with PEAR added
#
# Calculate for gross (no parameter), parameter = "net", or parameter = "RERUN"
# calculate_pear <- function(punts, plays, parameter=NA) {
#
#   # This first section does not depend on the parameter
#   # filter "plays" to just 1st downs
#   firsts <- plays %>% subset(down==1) %>% subset(play_type != "qb_kneel")
#
#   # make df of reference EP value for each LOS on 1st down
#   # (eliminate duplicate LOS values in the YardsFromOwnEndZone column)
#   firsts <- cbind(firsts, ep_smooth = predict(loess(formula = ep ~ YardsFromOwnEndZone,
#                                                     data = firsts, model=T, span=.5)))
#   ep_table <- subset(firsts, select=c(YardsFromOwnEndZone, ep_smooth))
#   ep_table <- ep_table[!duplicated(ep_table$YardsFromOwnEndZone),]
#   ep_table <- ep_table[order(ep_table$YardsFromOwnEndZone),]
#   # done with "plays"!!
#
#   if(is.na(parameter)) {
#
#     # add ep_dest to punts. This is the EP for the yard line at which the punt lands
#     punts$ep_dest <- ep_table$ep_smooth[punts$YardsFromOwnEndZone + punts$PuntDistance]
#
#     # now smooth this in punt world
#     punts$ep_dsmooth <- predict(loess(formula = punts$ep_dest ~ punts$YardsFromOwnEndZone),
#                                                data = punts, model=T, span=.5)
#     # Calculate PEAR
#     # subtract ep_dsmooth from each punt's ep_dest and adjust by 0.2 to get replacement instead of average
#     #arbitrary
#     punts$PEAR = punts$ep_dest - punts$ep_dsmooth + 0.2
#
#   } else if(parameter=="net") {
#     # no comments in these two sections, because it's all the same
#     punts$ep_dest_net <- ep_table$ep_smooth[punts$YardsFromOwnEndZone + punts$net]
#     punts$ep_dsmooth_net <- predict(loess(formula = punts$ep_dest_net ~ punts$YardsFromOwnEndZone),
#                                                data = punts, model=T, span=.5)
#     punts$PEARnet = punts$ep_dest_net - punts$ep_dsmooth_net
#   # } else if(parameter=="RERUN") {
#   #   # no comments in these two sections, because it's all the same
#   #   punts$ep_dest_rerun <- ep_table$ep_smooth[punts$YardsFromOwnEndZone + as.integer(round(punts$RERUN, digits=0))]
#   #   punts$ep_dsmooth_rerun <- predict(loess(formula = punts$ep_dest_rerun ~ punts$YardsFromOwnEndZone),
#   #                                   data = punts, model=T, span=.5)
#   #   punts$PEAR_RERUN = punts$ep_dest_rerun - punts$ep_dsmooth_rerun + 0.2
#   }
#
#   return(punts)
# }
