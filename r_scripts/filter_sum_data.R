#________________________________________________________
# load relevant libraries
library(tidyverse)
library(lubridate)
library(dplyr)
#________________________________________________________

#________________________________________________________
# load field metadata and convert each column to appropriate R class
filter_sumsarized <- function(sumsarized,metadata,bad_files,HHID_remove,project_name,stove_codes){
  
  #join with the meta data to get the logger id, field_site, and stove_type
  sumsarized$project_name <- project_name
  unlistfun_slash <- function(x) {unlist(gregexpr('/',x,perl=TRUE))[1]}
  unlistfun_1 <- function(x) {unlist(gregexpr('_',x,perl=TRUE))[1]}
  unlistfun_2 <- function(x) {unlist(gregexpr('_',x,perl=TRUE))[2]}
  unlistfun_3 <- function(x) {unlist(gregexpr('_',x,perl=TRUE))[3]}
  metadata_sub<-dplyr::select(metadata, filename, logger_id, location,comments,datetime_placed,datetime_removal,
                              deployment,enumerator,number_loggers_placed_home,stove_type)
  stove_codes_sub <- dplyr::select(stove_codes,stove,stove_descriptions)
  lazy_sumsarized <- lazy_dt(sumsarized)
  
  sumsarized_filtered <- mutate(lazy_sumsarized,HHID = if_else(!grepl("Nigeria_AfDB",project_name,ignore.case = TRUE),
                                                               substring(filename,sapply(filename, unlistfun_1)+1, 
                                                                         sapply(filename, unlistfun_2)-1), #Else
                                                               substring(filename, 1, sapply(filename, unlistfun_2)-1))) %>%  
    mutate(HHID = if_else(grepl("\\(B\\)",filename,ignore.case=TRUE),gsub("\\(B\\)","B",HHID),HHID)) %>% #Special case for the B round of households  
    left_join(metadata_sub, by = "filename") %>%
    mutate(HHID = as.factor(HHID)) %>% 
    mutate(filename = substring(filename,sapply(filename, unlistfun_slash)+1,100)) %>%
    mutate(region = substring(filename,sapply(filename, unlistfun_3)+1,100)) %>%
    mutate(file_indices = match(fullsumsarizer_filename, unique(fullsumsarizer_filename))) %>%
    mutate(group = substring(filename,
                             1, sapply(filename, unlistfun_1)-1)) %>%
    mutate(stove = substring(filename,
                             sapply(filename, unlistfun2)+1
                             ,sapply(filename, unlistfun3)-1))  %>%
    group_by(filename) %>%
    distinct(filename,datetime,stove_temp, .keep_all = TRUE)  %>% #Get rid of duplicate data points from the same file being imported/saved twice
    ungroup() %>%
    mutate(qc = if_else(grepl(bad_files,fullsumsarizer_filename,ignore.case=TRUE),"bad","ok")) %>%
    mutate(qc = if_else(grepl(HHID_remove,HHID),"bad",qc)) %>%
    mutate(stove = as.factor(stove)) %>%
    mutate(stove_temp = ifelse(stove_temp < -300,NA,stove_temp)) %>%
    left_join(stove_codes_sub,by = "stove") 
  sumsarized_filtered <- as.data.frame(sumsarized_filtered)
  
  
  
}
#________________________________________________________
