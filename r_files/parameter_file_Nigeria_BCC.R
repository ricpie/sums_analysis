#Parameter file for analysis
#Set path to tracking sheet relative to 'SUMS processing' directory
# path_tracking_sheet <- "Nigeria_BCC_SUMS tracking form database_NEW_1st Download.xlsx"
path_tracking_sheet <- "Nigeria_BCC_SUMS tracking form database_NEW_1st Download_1.xlsx"

project_name <- "BCC_Nigeria"
#Text associating a given stove code with the full name to use in figures.
stove_codes <- data.frame(stove = as.factor(c("KER","ELE","LPG","TCHAR","LPG2","AMB","KER2")),
                          stove_descriptions = as.factor(c("Kerosene","Electric","LPG","Traditional Charcoal","LPG Secondary","Ambient","Kerosene Secondary")))

stove_group_codes <- data.frame(group = as.factor(c("EP","NEP","NENP","ENP")),  #Use these if there are different study arms.
                                stove_groups = as.factor(c("EP","NEP","NENP","ENP"))) #group variable in filter_sumsarized.R

cooking_group <- 30 # x minute window for grouping events together.
cooking_duration_minimum <- 9  #Minutes
cooking_duration_maximum <- 1440 #Minutes
logging_duration_minimum <- 1 #days.  Single file duration must be this long to be included in the analysis.
#If set to zero, it will not trim out any data, instead leaving the complete data sets available for analysis.  Trimming is done
#to account for placement times/dates.
total_days_logged_minimum <- 5 #Must have at least this many days for a households's stove to be included in the analysis. Default value is 5.



#Not yet implemented.
start_date_range <- "2018-2-1" #Year-Month-Day format.  Do not include data before this in the analysis
end_date_range <- "2018-10-1" #Do not include data after this in the analysis


# Remove data from files selected as bad (they match the following strings, so they should be specific to the 
#given file, not generic enough to remove more files than intended)
bad_files <- paste(c("0_0_3_P83601_IndiaDMYminave","|0_0_3_P46673_Indiaminaves","|AKOKA DOWNLOAD 5_CC/"
                     ,"|Shomolu fourth download LPG/"),collapse="")

# Exclude data from the following households from the entire analysis.
HHID_remove <- paste(c("^SHO_03$","|^SHO_04$","|^SHO_08$","|^SHO_10$","|^MUS_01$"),collapse="")