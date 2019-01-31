#________________________________________________________
# require libraries
  library(tidyverse)
  library(lubridate)
  library(dplyr)
#________________________________________________________

#________________________________________________________
# check for outliers
  is_outlier <- function(x) {
    
    out <- (x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)) | 
           (x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
    
  # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# filter data for time periods of interest only
# requires df with time windows (id, start, end)
# df with id, time
# appends rep variable
filter_times <- function(times, df){
    
    rows <- nrow(times)
    
    # loop idsx
    for(i in 1:rows){
      # filter by date and time
      tmp <- dplyr::filter(df, date == times$date[i]) %>%
             dplyr::filter(hh_id == times$hh_id[i],
                           time >= times$start[i],
                           time <= times$end[i])
      
      # if first match
      if(exists("out", inherits = FALSE) == FALSE & nrow(tmp) > 0){
        out <- tmp
      }
      
      # if not first match with data
      if(exists("out", inherits = FALSE) == TRUE & nrow(tmp) > 0){
        out <- rbind(out, tmp)
      }
      # end for loop
    }
    
    # return
    return(out)
}
#________________________________________________________

#________________________________________________________
# Calculate the molecular weight of study pollutants
# Molecuar weights are calculated using the average
# standard atomic weights of each individual elements
#
# Atomic weights are from the NIST Physical Reference Data Website
calc_mw <- function(tbl){

  dplyr::mutate(tbl, mw = (num_c * 12.0106) +
                          (num_h * 1.007975) +
                          (num_o * 15.9994),
                       mw = ifelse(other == "S" & !is.na(other),
                                   mw + 32.0675, mw))

}
#________________________________________________________

#________________________________________________________
# return the molecular weight of carbon
mw_c <- function() {12.0106}
#________________________________________________________

#________________________________________________________
# convert ppmv to ug/m^3
# mw = molecular weight g/mol
# t = temperature oC
# p = pressure kPa
convert_ppmv_mgm3 <- function(ppmv, mw, t = 25, p = 101325){

  (ppmv * mw)*(p /(8.3144 * (t + 273.15))) * (1 / 1000)
  
}
#________________________________________________________

#________________________________________________________
get_lm_eqn <- function(m){

  eq <- substitute(~~R^2~"="~r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 3)))

  as.character(as.expression(eq))

}
#________________________________________________________

#________________________________________________________
# apply kirchstetter loading correction to microaeth data
ma_loading_corr <- function(data) {
  a <- 0.88
  b <- 0.12
  data %>% 
    dplyr::mutate(Tr = exp(-atn / 100)) %>%
    dplyr::mutate(rK = (a * Tr + b)) %>%
    dplyr::mutate(bc_corr = bc / (0.6 * rK))
  
}
#________________________________________________________

stacking_stovedays_fun <- function(ok_cooking_events_padded){
  
  # Initialize stacking_stove_days dataframe
  stacking_stovedays = data.frame(group=character(),stove_descriptions=character(),HHID=character(),
                                  percent_stovedays=character())
  grouplist <- unique(ok_cooking_events_padded$group)
  
  
  for (g in 1:length(unique(grouplist))){
    ok_cooking_events_padded_group <- dplyr::filter(ok_cooking_events_padded, group==grouplist[g]) #Subset of hhid j
    HH_list <- unique(ok_cooking_events_padded_group$HHID)
    
    #Loop through households, then through first stove, then through second. Get percent stove days in second loop.
    for (j in 1:length(unique(HH_list))) {
      temp_events <- dplyr::filter(ok_cooking_events_padded_group, HHID==HH_list[j]) #Subset of hhid j
      temp_stoves <- unique(temp_events$stove_descriptions)
      length_temp_stoves <- length(unique(temp_events$stove_descriptions))
      
      for (jj in 1:length_temp_stoves) { #For the first stove
        temp_events_overlap1 <- dplyr::filter(temp_events, stove_descriptions==temp_stoves[jj]) %>% #Subset of stove jj
          dplyr::group_by(day_month_year) %>% #Get data from unique days only
          dplyr::select(group,stove_descriptions,HHID,use_flag,day_month_year,start_time,end_time) #Keep all data, for the time overlap calc.
        temp_events_stove1 <- dplyr::filter(temp_events_overlap1,row_number(day_month_year) == 1) #Keep only first row, for the stove-days calc
        
        
        #Get the percent stovedays for the solo stoves.
        percent_stovedays <- sum(temp_events_stove1$use_flag)/dim(temp_events_stove1)[1] *100
        stacking_stovedays <- rbind(stacking_stovedays,
                                    data.frame(group=temp_events_stove1$group[1],
                                               stove_descriptions=temp_events_stove1$stove_descriptions[1],
                                               HHID=temp_events_stove1$HHID[1], percent_stovedays))
        
        if (length_temp_stoves>1 && jj<length_temp_stoves) {
          for (jjj in (jj+1):length_temp_stoves) { #For the second stove.
            
            temp_events_stove2 <- dplyr::filter(temp_events, stove_descriptions==temp_stoves[jjj]) %>% #Subset of stove jj
              dplyr::group_by(day_month_year) %>% #Get data from unique days only
              dplyr::select(group,stove_descriptions,HHID,use_flag,day_month_year,start_time,end_time) %>%
              dplyr::filter(row_number(day_month_year) == 1) %>%
              dplyr::inner_join(temp_events_stove1,by = "day_month_year")
            
            #How is this going to join - every combo of day-month-year?
            temp_events_overlap2 <- dplyr::filter(temp_events, stove_descriptions==temp_stoves[jjj]) %>% #Subset of stove jj
              dplyr::group_by(day_month_year) %>% #Get data from unique days only
              dplyr::select(group,stove_descriptions,HHID,use_flag,day_month_year,start_time,end_time) %>%
              dplyr::full_join(temp_events_overlap1,by = "day_month_year")
            
            percent_stovedays2 <- sum(temp_events_stove2$use_flag.x==TRUE & temp_events_stove2$use_flag.y==TRUE)/
              dim(temp_events_stove1)[1] *100
            
            #Get the percent stove days for the stove combos
            stacking_stovedays <- rbind(stacking_stovedays,
                                        data.frame(group=temp_events_stove2$group.x[1], 
                                                   stove_descriptions=paste(temp_events_stove2$stove_descriptions.x[1],'+',
                                                                            temp_events_stove2$stove_descriptions.y[1]),
                                                   HHID=temp_events_stove2$HHID.x[1], percent_stovedays = percent_stovedays2))
            
          }
        }
      }
    }
  }
  stacking_stovedays
}



#________________________________________________________
#Get counts of times when multiple stoves were in use at once. %of cooking events that were simultaneous?
#%of mimi events that had another stove at the same time (for days when they were both monitored).
#use the padded data set - keep all data that has a corresponding stove's data from the same dates, and use the useflag to ignore non-simul events.
simul_events_fun <- function(ok_cooking_events_padded){
  
  # Initialize stacking_stove_days dataframe
  simul_by_stove_combos = data.frame(group=character(),stove_descriptions=character(),HHID=character(),
                                  simul_counts=numeric(), days_simuldata_avail=numeric(),
                                  fraction_days_simul=numeric(),percent_days_simul=numeric())
  HH_list <- unique(ok_cooking_events_padded$HHID)
  #Loop through households, then through first stove, then through a second one. Get percent stove days in second loop.
  for (j in 1:length(unique(HH_list))) {#Loop through households
    temp_events <- dplyr::filter(ok_cooking_events_padded, HHID==HH_list[j]) #Subset of hhid j
    temp_stoves <- unique(temp_events$stove_descriptions)
    length_temp_stoves <- length(unique(temp_events$stove_descriptions))
    
    #Worry about simulcooking if there are more than two stoves only.
    if (length_temp_stoves>1) { 
      
        for (jj in 1:(length_temp_stoves-1)) { #Loop through stoves.
          
            for (jjj in (jj+1):length_temp_stoves) { #For the second stove.
              #Keep data for days in which stove jj and stove jjj both have data contributing.
              temp_events_both_have_data <- dplyr::group_by(temp_events,day_month_year) %>%
                dplyr::filter(length(unique(stove_descriptions))==2) %>%
                dplyr::ungroup()
              
              #How to deal with use_flag = false?
              temp_events_stovejj <- as.data.table(dplyr::filter(temp_events_both_have_data, stove_descriptions==temp_stoves[jj])) #Subset of stove jj
              temp_events_stovejj<-setkey(temp_events_stovejj,start_time,end_time)
              
              temp_events_stovejjj <- as.data.table(dplyr::filter(temp_events_both_have_data, stove_descriptions==temp_stoves[jjj])) #Subset of stove jjj
         
              simul_counts <- dim(asdf<-foverlaps(temp_events_stovejjj,temp_events_stovejj,type="any") %>% dplyr::filter(!is.na(start_time)))[1]
              days_simuldata_avail = length(unique(temp_events_both_have_data$day_month_year))
              fraction_days_simul = simul_counts/days_simuldata_avail
              
              #Get the percent simultaneous cooking events for the stove combos
              simul_by_stove_combos <- rbind(simul_by_stove_combos,
                                          data.frame(group=temp_events_stovejjj$group[1], 
                                                     stove_descriptions=paste(temp_events_stovejj$stove_descriptions[1],'+',
                                                                              temp_events_stovejjj$stove_descriptions[1]),
                                                     HHID=temp_events_stovejj$HHID[1], simul_counts = simul_counts,
                                                     days_simuldata_avail = days_simuldata_avail, fraction_days_simul=fraction_days_simul,
                                                     percent_days_simul=100*fraction_days_simul))
  
        }
      }
    }
  }
  simul_by_stove_combos
}
