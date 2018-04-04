#________________________________________________________
# load relevant libraries
library(tidyverse)
library(lubridate)
library(dplyr)
#________________________________________________________

#________________________________________________________
# load field metadata and convert each column to appropriate R class
filter_sumsarized <- function(sumsarized,metadata,bad_files,HHID_remove){

  
  #join with the meta data to get the logger id, field_site, and stove_type
  sumsarized_all <-  dplyr::mutate(sumsarized,HHID = substring(filename, 
                                    1, sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[1])+2)) %>%       
    dplyr::mutate(HHID = if_else(grepl("\\(B\\)",filename,ignore.case=TRUE),paste0(HHID,"B"),HHID)) %>% #Special case for the B round of households  
    dplyr::left_join(dplyr::select(metadata, filename, logger_id, location,comments,datetime_placed,datetime_removal,
                                   deployment,enumerator,number_loggers_placed_home,stove_type,stove_use_category),
                                    by = "filename") %>%
    dplyr::mutate(HHID = as.factor(HHID)) %>%# 
    dplyr::mutate(filename = substring(filename, 
                                       sapply(filename, function(x) unlist(gregexpr('/',x,perl=TRUE))[1])+1,100)) %>%
    dplyr::mutate(file_indices = match(filename, unique(filename))) %>%
    dplyr::mutate(site =  substring(filename, 1, 3)) %>%
    dplyr::mutate(stove = substring(filename, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])+1
                                    ,sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])+2))  %>%
    dplyr::group_by(filename) %>%
    dplyr::distinct(filename,datetime,stove_temp, .keep_all = TRUE)  %>% #Get rid of duplicate data points from the same file being imported/saved twice
    dplyr::ungroup() %>%
    dplyr::mutate(qc = if_else(grepl(bad_files,fullfilename,ignore.case=TRUE),"bad","ok")) %>%
    dplyr::mutate(qc = if_else(grepl(HHID_remove,HHID),"bad",qc))

  }
#________________________________________________________
