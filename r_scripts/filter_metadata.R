#________________________________________________________
# load relevant libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(gsubfn)


#________________________________________________________
#Load metadata and fix any issues with it.
filter_metadata_fun <- function(metadata){

metadata <- dplyr::mutate(metadata, HHID = gsub('54asdf5','74162',HHID)) %>%
  # dplyr::mutate_all(funs(toupper))  %>%
  # dplyr::mutate(logger_id = gsub('Dot','',logger_id)) %>%
  # dplyr::filter(nchar(HHID)>3) %>%
  # dplyr::filter(nchar(HHID)<8) %>%
  # dplyr::mutate(Group = substring(HHID, 2,4)) %>%
  dplyr::mutate(dot_name = logger_id)
  


}