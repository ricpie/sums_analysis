#Parameter file for analysis for Boston College Smokeless Village study
#Set path to tracking sheet relative to 'SUMS processing' directory
path_tracking_sheet <- "SUMs_tracking form Smokeless Village"

project_name <- "SmokelessVillage"
#Text associating a given stove code with the full name to use in figures.
stove_codes <- data.frame(stove = as.factor(c("BG","TS","LPG","AMB")),
                          stove_descriptions = as.factor(c("Biogas",
                                                           "Three stone fire",
                                                           "LPG",
                                                           "Ambient")))

stove_group_codes <- data.frame(group = as.factor(c("Pre","Post")),  #Use these if there are different study arms.
                                stove_groups = as.factor(c("Pre LPG Intervention","Post LPG Intervention"))) #group variable in filter_sumsarized.R

cooking_group <- 40 # x minute window for grouping events together.
cooking_duration_minimum <- 9  #Minutes
cooking_duration_maximum <- 1440 #Minutes
logging_duration_minimum <- 1 #days.  Single file duration must be this long to be included in the analysis.
#If set to zero, it will not trim out any data, instead leaving the complete data sets available for analysis.  Trimming is done
#to account for placement times/dates.
total_days_logged_minimum <- 5 #Must have at least this many days for a households's stove to be included in the analysis. Default value is 5.
metadata_date_ignore <- 1

start_date_range <- "2018-5-1" #Year-Month-Day format.  Do not include data before this in the analysis
end_date_range <- "2020-12-1" #Do not include data after this in the analysis


# Remove data from files selected as bad (they match the following strings, so they should be specific to the 
#given file, not generic enough to remove more files than intended)
bad_files <- paste(c("Exclude","|Pre_Pilot-04_TMS_DL-01","|Pre_Pilot-04_TMS_DL-01"
                     ,"|Pre_Pilot-04_TMS_DL-01","|AMB"),collapse="")

# Exclude data from the following households from the entire analysis. e.g.SHO_03 is removed
HHID_remove <- paste(c("^SHO_03$","|^SHO_04$","|^MUS_01$","|^AMB$"),collapse="")



