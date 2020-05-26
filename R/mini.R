###|
###| "Mini" section
###| Create a dataframe where each punter appears once
###|

create_mini <- function(punts, parameter=NA, threshold=32) {

  if(is.na(parameter)) {

    # define "mini" dataframe where each player appears once
    mini <- punts$punter_player_name %>% unique() %>% dplyr::tibble(Punter = ., small=0)

    # filter punters based on some minimum number of punts (sorry Ben)
    #mini <- cbind(mini, small=0)
    for(i in 1:length(mini$Punter)) {
      if(length(punts$punter_player_name[punts$punter_player_name==mini$Punter[i]]) < threshold) {
        mini$small[i] <- 1
      }
    }
    mini <- mini %>% dplyr::filter(small==0) %>% dplyr::select(Punter)

  } else if(parameter=="Year") {

    # miniY - comparing punter years to each other
    punter_temp <- unique(punts$punter_player_name)
    year_temp <- unique(punts$Year)
    mini <- dplyr::tibble(Punter = NA, year = NA)

    # did a punter punt that year?  If so, make a row for that year of that punter
    for(i in 1:length(year_temp)) {
      for(j in 1:length(punter_temp)) {
        if(length(punts$Year[punts$Year==year_temp[i] & punts$punter_player_name==punter_temp[j]]) > threshold) {
          mini <- mini %>% dplyr::add_row(Punter = punter_temp[j], year = year_temp[i])
        }
      }
    }
    # remove that original NA row
    mini <- mini %>% dplyr::filter(!is.na(Punter))

  }
  return(mini)
}

# add_logos <- function(mini, )

calculate_all_mini <- function(mini, punts, parameter=NA, digits=2) {
  if(is.na(parameter)) {

    # add stats to mini
    for(i in 1:length(mini$Punter)) {

      mini$NumPunts[i] <- length(punts$X1[punts$punter_player_name == mini$Punter[i]])
      mini$Gross[i] <- round(mean(punts$GrossYards[punts$punter_player_name == mini$Punter[i]]), digits=digits)
      mini$Net[i] <- round(mean(punts$NetYards[punts$punter_player_name == mini$Punter[i]]), digits=digits)
      mini$RERUN[i] <- round(mean(punts$RERUN[punts$punter_player_name == mini$Punter[i]]), digits=digits)

      mini$SHARP[i] <- round(mean(punts$SHARP[punts$punter_player_name == mini$Punter[i]]), digits=digits)
      mini$SHARPnet[i] <- round(mean(punts$SHARPnet[punts$punter_player_name == mini$Punter[i]]), digits=digits)
      mini$SHARP_RERUN[i] <- round(mean(punts$SHARP_RERUN[punts$punter_player_name == mini$Punter[i]]), digits=digits)

      mini$SHARP_OF[i] <- round(mean(punts$SHARP_OF[punts$punter_player_name == mini$Punter[i]], na.rm=TRUE), digits=digits)
      mini$SHARP_PD[i] <- round(mean(punts$SHARP_PD[punts$punter_player_name == mini$Punter[i]], na.rm=TRUE), digits=digits)
      mini$SHARPnet_OF[i] <- round(mean(punts$SHARPnet_OF[punts$punter_player_name == mini$Punter[i]], na.rm=TRUE), digits=digits)
      mini$SHARPnet_PD[i] <- round(mean(punts$SHARPnet_PD[punts$punter_player_name == mini$Punter[i]], na.rm=TRUE), digits=digits)
      mini$SHARP_RERUN_OF[i] <- round(mean(punts$SHARP_RERUN_OF[punts$punter_player_name == mini$Punter[i]], na.rm=TRUE), digits=digits)
      mini$SHARP_RERUN_PD[i] <- round(mean(punts$SHARP_RERUN_PD[punts$punter_player_name == mini$Punter[i]], na.rm=TRUE), digits=digits)

      #mini$PEAR[i] <- round(mean(punts$PEAR[punts$punter_player_name == mini$Punter[i]]), digits=digits)
      #mini$PEARnet[i] <- round(mean(punts$PEARnet[punts$punter_player_name == mini$Punter[i]]), digits=digits)
      # # mini$PEAR_RERUN[i] <- round(mean(punts$PEAR_RERUN[punts$punter_player_name == mini$Punter[i]]), digits=digits)
    }
    return(mini)

  } else if((parameter=="Year")) {

    # add stats to mini
    for(i in 1:length(mini$Punter)) {

      mini$NumPunts[i] <- length(punts$X1[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]])
      mini$Gross[i] <- round(mean(punts$GrossYards[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)
      mini$Net[i] <- round(mean(punts$NetYards[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)
      mini$RERUN[i] <- round(mean(punts$RERUN[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)

      mini$SHARP[i] <- round(mean(punts$SHARP[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)
      mini$SHARPnet[i] <- round(mean(punts$SHARPnet[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)
      mini$SHARP_RERUN[i] <- round(mean(punts$SHARP_RERUN[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)

      mini$SHARP_OF[i] <- round(mean(punts$SHARP_OF[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]], na.rm=TRUE), digits=digits)
      mini$SHARP_PD[i] <- round(mean(punts$SHARP_PD[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]], na.rm=TRUE), digits=digits)
      mini$SHARPnet_OF[i] <- round(mean(punts$SHARPnet_OF[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]], na.rm=TRUE), digits=digits)
      mini$SHARPnet_PD[i] <- round(mean(punts$SHARPnet_PD[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]], na.rm=TRUE), digits=digits)
      mini$SHARP_RERUN_OF[i] <- round(mean(punts$SHARP_RERUN_OF[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]], na.rm=TRUE), digits=digits)
      mini$SHARP_RERUN_PD[i] <- round(mean(punts$SHARP_RERUN_PD[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]], na.rm=TRUE), digits=digits)

      # mini$PEAR[i] <- round(mean(punts$PEAR[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)
      # mini$PEARnet[i] <- round(mean(punts$PEARnet[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)
      # mini$PEAR_RERUN[i] <- round(mean(punts$PEAR_RERUN[punts$punter_player_name == mini$Punter[i] & punts$Year==mini$year[i]]), digits=digits)
    }
    return(mini)
  }
}
