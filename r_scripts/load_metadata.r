#________________________________________________________
# load relevant libraries
  library(tidyverse)
  library(lubridate)
  library(readxl)
#________________________________________________________

#________________________________________________________
# load temp metadata and convert each column to appropriate R class
load_field_temp_meta <- function(){
  asdf<- read_excel("../AfBb_SUM_Tracking Data_oct31.xlsx",sheet = "First Visit",
                    col_names = c("hhid", "enumerator", "num_buttons_placed", "stovetype","sum_id","date_placed"),
                    skip = 0) 
  %>%
                 # convert time to secs in day and fix file problems
            dplyr::mutate(start_date = mdy(start_date))  %>%
            dplyr::mutate(end_date = mdy(end_date))
}
#________________________________________________________


