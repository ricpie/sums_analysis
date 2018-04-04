#________________________________________________________
# load relevant libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)




#________________________________________________________
# load sumsarized data and convert each column to appropriate R class
# Load files of Sumsarizer output for the thermocouple data analyzed by RP
load_sumsarized <- function(){
  
 asdf<- lapply(list.files(paste0("../../SUMSARIZED", collapse=NULL),
                    pattern = ".csv",
                    full.names = TRUE,recursive = TRUE),
         function(x)
           readr::read_csv(x, 
                           skip = 1,
                           col_names = c("filename", "datetime", "stove_temp", "state","datapoint_id","dataset_id"),
                           col_types = 
                             cols(
                               filename = col_character(),
                               datetime = col_character(),
                               stove_temp = col_double(),
                               state = col_logical(),
                               datapoint_id = col_character(),
                               dataset_id = col_character()
                             ),
                           na = c("", "NA")
           ) %>%
           dplyr::mutate(logging_duration_days = as.numeric(difftime(max(datetime),min(datetime),units = "days")))  

  ) %>%
    dplyr::bind_rows() %>%
    # convert time to secs in day and fix file problems
    dplyr::mutate(datetime = parse_date_time(gsub("/00", "/16", datetime),orders = c("y-m-d HMS", "m/d/y HMS"))) %>%
    dplyr::mutate(filename = if_else(grepl("DL4",filename,ignore.case=TRUE),gsub(".csv", "_DL4.csv", filename),filename)) %>% #Special case for importing DL4 and DL5, because they were not renamed properly.  
    dplyr::mutate(filename = if_else(grepl("DL5",filename,ignore.case=TRUE),gsub(".csv", "_DL5.csv", filename),filename))%>%
    dplyr::mutate(fullfilename = filename) %>%
    dplyr::mutate(filename = substring(filename, 
                                       sapply(filename, function(x) unlist(gregexpr('/',x,perl=TRUE))[1])+1,100)) %>%
   dplyr::mutate(filename = gsub("KE", "_KE", filename,ignore.case = TRUE)) %>%
   dplyr::mutate(filename = gsub("CC", "_CC", filename,ignore.case = TRUE)) %>%
    dplyr::mutate(filename = gsub("LPG", "_LPG", filename,ignore.case = TRUE)) %>%
    dplyr::mutate(filename = gsub("AMB", "_AMB", filename,ignore.case = TRUE)) %>%
    dplyr::mutate(filename = gsub("__","_",filename)) %>% 
    dplyr::mutate(filename = gsub(" ","_",filename)) 
  
}


#________________________________________________________
#Load tracking meta data download sheet(s)
#________________________________________________________

load_meta_download <- function(xx){
        
  # Text names for the v1 version of the tracking sheet    
  # varname_text <- c("stove_type","datetime_removal","time_removal","datetime_download","logger_id",
  # "max_temp","min_temp","filename","logger_new_id_number",
  # "new_placement_on_stove","datetime_placed","location")
  
  # Text names for the v2 version of the tracking sheet
  varname_text <- c("stove_type","datetime_placed","time_placed","datetime_removal",
                    "time_removal","datetime_download","logger_id",
                    "max_temp","min_temp","filename","logger_new_id_number",
                    "new_placement_on_stove","time_replaced","location")

  variers <- c( paste('S1.',varname_text,sep=''),
                paste('S2.',varname_text,sep=''),
                paste('S3.',varname_text,sep=''),
                paste('S4.',varname_text,sep=''))
  column_names <- c("HHID","deployment","enumerator","number_loggers_placed_home",
    variers,
    "amb.logged","amb.logger_id","amb.datetime_placed",
    "amb.time_placed","amb.location","comments")
  
  asdf<- read_excel(paste0("../../",xx, collapse=NULL),
          sheet = 'AllData',skip = 1,
          col_names = column_names,
          
          col_types = c("text","text","text","numeric",
                        "text","date","date","date","date","date","text",
                        "numeric","numeric","text","text","text","date","text",
                        "text","date","date","date","date","date","text",
                        "numeric","numeric","text","text","text","date","text",
                        "text","date","date","date","date","date","text",
                        "numeric","numeric","text","text","text","date","text",
                        "text","date","date","date","date","date","text",
                        "numeric","numeric","text","text","text","date","text",
                        "text","text","date",
                        "date","text","text")
                      )  %>%

    
           # convert time to secs in day and fix file problems
          dplyr::mutate(S1.datetime_placed =  as.POSIXct(paste(S1.datetime_placed, strftime(S1.time_placed,"%H:%M:%S")),
                                                    format = "%Y-%m-%d %H:%M:%S",
                                                    origin = "1970-01-01")) %>%
          dplyr::mutate(S1.datetime_removal =  as.POSIXct(paste(S1.datetime_removal, strftime(S1.time_removal,"%H:%M:%S")),
                                                          format = "%Y-%m-%d %H:%M:%S",
                                                          origin = "1970-01-01")) %>%
          dplyr::mutate(S1.datetime_download =  as.POSIXct(paste(S1.datetime_removal, strftime(S1.datetime_download,"%H:%M:%S")),
                                                         format = "%Y-%m-%d %H:%M:%S",
                                                         origin = "1970-01-01")) %>%
          dplyr::mutate(S2.datetime_placed =  as.POSIXct(paste(S2.datetime_placed, strftime(S2.time_placed,"%H:%M:%S")),
                                                         format = "%Y-%m-%d %H:%M:%S",
                                                         origin = "1970-01-01")) %>%
          dplyr::mutate(S2.datetime_removal =  as.POSIXct(paste(S2.datetime_removal, strftime(S2.time_removal,"%H:%M:%S")),
                                                        format = "%Y-%m-%d %H:%M:%S",
                                                        origin = "1970-01-01")) %>%
          dplyr::mutate(S2.datetime_download =  as.POSIXct(paste(S2.datetime_removal, strftime(S2.datetime_download,"%H:%M:%S")),
                                                         format = "%Y-%m-%d %H:%M:%S",
                                                         origin = "1970-01-01")) %>%
          dplyr::mutate(S3.datetime_placed =  as.POSIXct(paste(S3.datetime_placed, strftime(S3.time_placed,"%H:%M:%S")),
                                                         format = "%Y-%m-%d %H:%M:%S",
                                                         origin = "1970-01-01")) %>%
          dplyr::mutate(S3.datetime_removal =  as.POSIXct(paste(S3.datetime_removal, strftime(S3.time_removal,"%H:%M:%S")),
                                                        format = "%Y-%m-%d %H:%M:%S",
                                                        origin = "1970-01-01")) %>%
          dplyr::mutate(S3.datetime_download =  as.POSIXct(paste(S3.datetime_removal, strftime(S3.datetime_download,"%H:%M:%S")),
                                                         format = "%Y-%m-%d %H:%M:%S",
                                                         origin = "1970-01-01")) %>%
          dplyr::mutate(S4.datetime_placed =  as.POSIXct(paste(S4.datetime_placed, strftime(S4.time_placed,"%H:%M:%S")),
                                                         format = "%Y-%m-%d %H:%M:%S",
                                                         origin = "1970-01-01")) %>%
          dplyr::mutate(S4.datetime_removal =  as.POSIXct(paste(S4.datetime_removal, strftime(S4.time_removal,"%H:%M:%S")),
                                                        format = "%Y-%m-%d %H:%M:%S",
                                                        origin = "1970-01-01")) %>%
          dplyr::mutate(S4.datetime_download =  as.POSIXct(paste(S4.datetime_removal, strftime(S4.datetime_download,"%H:%M:%S")),
                                                         format = "%Y-%m-%d %H:%M:%S",
                                                    origin = "1970-01-01")) %>%
          dplyr::mutate(amb.datetime_placed =  as.POSIXct(paste(amb.datetime_placed, strftime(amb.time_placed,"%H:%M:%S")),
                                                         format = "%Y-%m-%d %H:%M:%S",
                                                         origin = "1970-01-01")) %>%
          dplyr::mutate(amb.logged = if_else(grepl("N/A",amb.logged,ignore.case=TRUE),"NA","YES")) %>%
    dplyr::filter(!is.na(number_loggers_placed_home)) %>%
    dplyr::filter(!is.na(deployment)) %>%
    dplyr::filter(deployment!=0) %>%
    as.data.frame()
        

long_metadata <-  reshape(asdf, direction='long', 
        varying=variers, 
        timevar='stove_use_category',
        v.names=varname_text,
        times=c('S1','S2','S3','S4'),
        idvar=c('HHID','deployment')) %>%
        dplyr::select(-new_placement_on_stove,-time_replaced,-location)
  
#Column orders changed, can't figure out why...manually correcting them.
colnames(long_metadata) <- c("HHID",                        "deployment"     ,  "enumerator" ,                
                             "number_loggers_placed_home", "amb.logged"     ,   "amb.logger_id",         
                             "amb.datetime_placed"       ,  "amb.time_placed",   "amb.location",               
                             "comments"                  ,  "stove_use_category"           ,   "datetime_download"  ,               
                             "datetime_placed"          ,  "datetime_removal"           ,   "filename",          
                             "location"            ,  "logger_id"            ,   "un2"    ,               
                             "maxtemp"                  ,  "mintemp",   "un5",     
                             "stove_type")


#time_removal datetime_removal stove_type max_temp min_temp logger_new_id_number new_placement_on_stove
#Need to add these back in when ready.
long_metadata <- dplyr::select(long_metadata,-un5,-un2,-amb.time_placed)  
  
}


#________________________________________________________
#Load tracking meta data download sheet(s)
#________________________________________________________

load_meta_json <- function(xx){
  xx <- "SUMS Tracking data/Kigeme_SUMs_V1_results (2).json"
  xx<-paste0("../",xx, collapse=NULL)

  
  
data <- stream_in(file(xx)) 
for (ii in 1:dim(data)[1]) {
data$sum_repeat[[ii]] = data$sum_repeat[[ii]][-6]
}

data <- tidyr::unnest(data,sum_repeat) %>%
  dplyr::mutate(comments = paste(B10_notes,B21_notes)) %>%
  dplyr::rename(visitdate = A1_date,visittime = A2_visittime,HHID = A3_HHID,enumerator = A4_survinitials,logger_id = B3_sumid,stove_type = B4_stove,
                filename = B15_filename,maxtemp = B16_maxtemp,mintemp = B17_mintemp) %>%
  dplyr::mutate(visitstartdatetimeUTC = as.POSIXct(paste(visitdate, visittime),
                                                   format = "%Y-%m-%d %H:%M:%S",
                                                   origin = "1970-01-01")) %>%
  dplyr::mutate(visitenddatetimeUTC = as.POSIXct(paste(visitdate, B7_installationtime),
                                                   format = "%Y-%m-%d %H:%M:%S",
                                                   origin = "1970-01-01")) %>%
  dplyr::select(-A_V1_note,-instanceID,-A5_cookname,-B10_notes,-B21_notes,-B4_stove_other,-B7_installationtime,-visitdate,-visittime,
                -D_visitendtime,-B12_stoveimage,-B13_removaltime) %>%
  dplyr::mutate(deployment = substring(filename, 
                                                sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),1)[1])+1, 
                                                sapply(filename, function(x) tail(unlist(gregexpr('_',x,perl=TRUE)),1)[2])-1)) 

stove_codes <- data.frame(stove = as.factor(c(1,2,3,4,5,6,7)),
                          stove_descriptions = as.factor(c("Mimi-moto","Traditional metal wire","Simple wood","Local improved charocal","High efficiency wood","LPG","Other")))

stove_group_codes <- data.frame(group = as.factor(c(1,2)),  #Use these if there are different study arms.
                                stove_groups = as.factor(c("Baseline","Mimi-moto"))) #group variable in filter_sumsarized.R

install_codes <- data.frame(group = as.factor(c(1,2)),  #Use these if there are different study arms.
                                stove_groups = as.factor(c("Installing","Downloading"))) #group variable in filter_sumsarized.R


}


