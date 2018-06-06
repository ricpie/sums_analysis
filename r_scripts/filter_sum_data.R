#________________________________________________________
# load relevant libraries
library(tidyverse)
library(lubridate)
library(dplyr)
#________________________________________________________

#________________________________________________________
# load field metadata and convert each column to appropriate R class
filter_sumsarized <- function(sumsarized,metadata,bad_files,HHID_remove,project_name,stove_codes,logging_duration_minimum){
  
  #join with the meta data to get the logger id, field_site, and stove_type
  sumsarized$project_name <- project_name
  sumsarized_filtered <-     dplyr::mutate(sumsarized,filename = gsub("KE\\_MUS","MUS",filename)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = substring(filename,1, sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])-1)) %>%  
    dplyr::mutate(filename = if_else(grepl("Nigeria_AfDB",project_name,ignore.case = TRUE) & grepl("ke",HHID,ignore.case=TRUE),
                                     gsub("ke","_KE",filename,ignore.case = T),filename)) %>% #Special case for the AfDB households 
    dplyr::mutate(HHID = substring(filename,1, sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])-1)) %>%  
    dplyr::mutate(HHID = if_else(!grepl("Nigeria_AfDB",project_name,ignore.case = TRUE),
                                 substring(filename,sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[1])+1, 
                                           sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])-1),
                                 HHID)) %>%  
    dplyr::mutate(HHID = if_else(grepl("\\_KE\\(B\\)",filename,ignore.case=TRUE),gsub("\\_KE\\(B\\)","B\\_KE",HHID),HHID)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = if_else(grepl("\\_KE1\\(B\\)",filename,ignore.case=TRUE),gsub("\\_KE1\\(B\\)","B\\_KE",HHID),HHID)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = if_else(grepl("\\_KE2\\(B\\)",filename,ignore.case=TRUE),gsub("\\_KE2\\(B\\)","B\\_KE",HHID),HHID)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = if_else(grepl("\\_KE\\(1\\)\\(B\\)",filename,ignore.case=TRUE),gsub("\\_KE\\(1\\)\\(B\\)","B_KE",HHID),HHID)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = if_else(grepl("\\_CC\\(B\\)",filename,ignore.case=TRUE),gsub("\\_CC\\(B\\)","B\\_CC",HHID),HHID)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = if_else(grepl("\\(B\\)",filename,ignore.case=TRUE),gsub("\\(B\\)","B",HHID),HHID)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = if_else(grepl("\\(B\\)",filename,ignore.case=TRUE),paste0(HHID,"B"),HHID)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = gsub("BB","B",HHID)) %>% #Special case for the B round of households  
    dplyr::mutate(HHID = gsub("Ke","",HHID,ignore.case = T)) %>% #Special case for the B round of households  
    dplyr::left_join(dplyr::select(metadata, filename, logger_id, location,comments,datetime_placed,datetime_removal,
                                   deployment,enumerator,number_loggers_placed_home,stove_type),
                     by = "filename") %>%
    dplyr::mutate(HHID = as.factor(HHID)) %>%# 
    dplyr::mutate(filename = substring(filename, 
                                       sapply(filename, function(x) unlist(gregexpr('/',x,perl=TRUE))[1])+1,100)) %>%
    dplyr::mutate(file_indices = match(fullsumsarizer_filename, unique(fullsumsarizer_filename))) %>%
    dplyr::mutate(group = substring(filename, 
                                    1, sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[1])-1)) %>% 
    dplyr::mutate(stove = substring(filename, 
                                    sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[2])+1
                                    ,sapply(filename, function(x) unlist(gregexpr('_',x,perl=TRUE))[3])-1))  %>%
    dplyr::group_by(filename) %>%
    dplyr::distinct(filename,datetime,stove_temp, .keep_all = TRUE)  %>% #Get rid of duplicate data points from the same file being imported/saved twice
    {if (logging_duration_minimum>0) filter(.,day_month_year > min(day_month_year) & day_month_year < max(day_month_year))} %>% #Remove data (filter it out) from the first and last day of logging, to help get around the issue of placement times.
    dplyr::ungroup() %>%
    dplyr::mutate(qc = if_else(grepl(bad_files,fullsumsarizer_filename,ignore.case=TRUE),"bad","ok")) %>%
    dplyr::mutate(qc = if_else(grepl(HHID_remove,HHID),"bad",qc)) %>%
    dplyr::mutate(stove = as.factor(stove)) %>%
    dplyr::left_join(dplyr::select(stove_codes,stove,stove_descriptions),by = "stove") 
  
}
#________________________________________________________
