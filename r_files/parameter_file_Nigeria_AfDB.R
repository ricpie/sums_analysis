#Parameter file

project_name <- "Nigeria_AfDB"

#Text associating a given stove code with the full name to use in figures.
stove_codes <- data.frame(stove = as.factor(c("CC","LP","LPG","KE","KE(1)","KE(2)","AMB")),
                          stove_descriptions = as.factor(c("CleanCook","LPG","LPG","Kerosene","Kerosene","Kerosene","Ambient")))

stove_group_codes <- data.frame(group = as.factor(c("AKO","SHO","MUS")),  #Use these if there are different study arms.
                                stove_groups = as.factor(c("AKO","SHO","MUS"))) #group variable in filter_sumsarized.R

cooking_group <- 30 # x minute window for grouping events together.
cooking_duration_minimum <- 9  #Minutes
cooking_duration_maximum <- 1440 #Minutes
logging_duration_minimum <- 1 #days.  Single file duration must be this long to be included in the analysis.
total_days_logged_minimum <- 5 #Must have at least this many days for a households's stove to be included in the analysis.

#Set path to tracking sheet relative to 'SUMS processing' directory
# path_tracking_sheet <- "SUMS Tracking data/AfBb_SUM_Tracking Data_Dec7_v2.xlsx"
path_tracking_sheet <- "AfBb_SUM_Tracking Data_Jan23.xlsx"

#Not yet implemented.
start_date_range <- "2017-6-1" #Year-Month-Day format.  Do not include data before this in the analysis
end_date_range <- "2018-4-1" #Do not include data after this in the analysis


# Remove data from files selected as bad (they match the following strings, so they should be specific to the 
#given file, not generic enough to remove more files than intended)
bad_files <- paste(c("0_0_3_P83601_IndiaDMYminave","|0_0_3_P46673_Indiaminaves","|AKOKA DOWNLOAD 5_CC/"
                     ,"|AKOKA DOWNLOAD"
                     ,"|AKOKA DOWNLOAD 5_KE/" ,"|AKOKA DOWNLOAD 5_CC/" ,"|AKOKA DOWNLOAD 5_LPG/"
                     ,"|MUSHIN DOWNLOAD 5_CC/","|MUSHIN DOWNLOAD 5","|MUSHIN DOWNLOAD 5_KE/"
                     ,"|SHOMOLU DOWNLOAD 5","|SHOMOLU DOWNLOAD 5_CC/","|SHOMOLU DOWNLOAD 5_KE/"
                     ,"|SHOMOLU DOWNLOAD 5_LPG/","|Akoka fourth download CC/","|Akoka fouth download"
                     ,"|Akoka fourth download","|Akoka fourth download LPG/","|Mushin fourth download CC/"
                     ,"|Mushin fourth download KE/","|Shomolu fourth download","|Shomolu fourth download CC/"
                     ,"|Shomolu fourth download KE/","|Shomolu fourth download LPG/"),collapse="")

# Exclude data from the following households from the entire analysis.
HHID_remove <- paste(c("^SHO_03$","|^SHO_04$","|^SHO_08$","|^SHO_10$","|^MUS_01$",
                       "|^AKO_01$","|^AKO_02$","|^AKO_03$","|^AKO_05$","|^AKO_06$","|^AKO_07$","|^AKO_08$"),collapse="")