#Parameter file

project_name <- "Nigeria_AfDB"

#This is a vraible associating a given stove code with the full name to use in figures. Make sure they correspond correctly (the first
#stove code should be the same as the first stove_description)
stove_codes <- data.frame(stove = as.factor(c("CC","CC(B)","LP","LPG","KE","KE1(B)","KE2(B)","KE(B)",  #The stove codes used as short-hand
                                              "AMB","AMB(B)","KE1","KE1","KE2", "KE(1)", "KE(2)")),
                          stove_descriptions = as.factor(c("CleanCook","CleanCook","LPG","LPG","Kerosene","Kerosene","Kerosene", #The full names used for plotting
                                                           "Kerosene","Ambient","Ambient","Kerosene","Kerosene","Kerosene","Kerosene","Kerosene")))

#This variable defines the 'group' that is used in the analysis.  It could be Control and Intervention, or Before and After, or different
#cities as shown here.
stove_group_codes <- data.frame(group = as.factor(c("AKO","SHO","MUS")),  #Use these if there are different study arms.
                                stove_groups = as.factor(c("AKO","SHO","MUS"))) #group variable in filter_sumsarized.R

cooking_group <- 60 # x minute window for grouping events together.  This is a common approach
cooking_duration_minimum <- 5  #Minutes.  Is it possible or likely that there will be 5 to 10 minute cooking events.  We will remove shorter ones to reduce noise
cooking_duration_maximum <- 1440 #Minutes.  Will stove uses last up to a day ever?  We will remove longer ones, assuming it is due to an error.
logging_duration_minimum <- 1 #days.  Single file duration must be this long to be included in the analysis.
#If set to zero, it will not trim out any data, instead leaving the complete data sets available for analysis.  Trimming is done
#to account for placement times/dates.
total_days_logged_minimum <- 5 #Must have at least this many days for a households's stove to be included in the analysis. Default value is 5.
metadata_date_ignore <- 0

#Set path to tracking sheet relative to 'SUMS processing' directory
path_tracking_sheet <- "AfBb_SUM_Tracking Data_Jan23.xlsx"

#This is the study start and end date, to help correct errors in date entries
start_date_range <- "2018-3-31"#"2017-01-01" #"2018-3-31" #Year-Month-Day format.  Do not include data before this in the analysis
end_date_range <- "2018-5-12"#"2018-02-17" #"2018-5-12" #Do not include data after this in the analysis


# Remove data from files selected as bad (they match the following strings, so they should be specific to the 
#given file, not generic enough to remove more files than intended).  Make sure the "|" symbol is before the name.
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
# Example: if household name is SHO_03, the full string should be in quotes as follows: "^SHO_03$"
# All of the households, except the first should have the "|" symbol before the carat.  For example, if SHO_03 was the second
# household in the list it should be written :  "|^SHO_03$"
# HHIDs to remove for the main data set analysis
# HHID_remove <- paste(c("^SHO_03$","|^SHO_04$","|^SHO_08$","|^SHO_10$","|^MUS_01$","|^MUS_03$",
                       # "|^AKO_01$","|^AKO_02$","|^AKO_03$","|^AKO_05$","|^AKO_06$","|^AKO_07$","|^AKO_08$","|^AKO_08B$"),collapse="")

# HHIDs to remove for the pre/post purchase analysis.  These leave the purchaser homes intact
HHID_remove <- paste(c("MUS_01B$","|^MUS_02$","|^MUS_03$","|^MUS_04$","|^MUS_05$","|^MUS_06$","|^MUS_07$","|^MUS_08$","|^MUS_09$","|^SHO_02$",
                       "|^SHO_06$","|^SHO_07$","|^AKO_01B$","|^AKO_03B$","|^AKO_04$","|^AKO_05B$","|^AKO_06B$","|^AKO_07B$",
                       "|^SHO_03$","|^SHO_10B$","|^SHO_10$","|^SHO_04$","|^SHO_08$","|^MUS_01$","|^MUS_03$",
                       "|^AKO_01$","|^AKO_02$","|^AKO_03$","|^AKO_05$","|^AKO_06$","|^AKO_07$","|^AKO_08$","|^AKO_08B$"),collapse="")

# # HHIDs to remove for the pre/post purchase analysis.  These leave the non-purchaser homes intact
# HHID_remove <- paste(c("^SHO_03$","|^SHO_04$","|^SHO_08$","|^SHO_10$","|^MUS_01$","|^MUS_03$",
#                       "|^AKO_01$","|^AKO_02$","|^AKO_03$","|^AKO_05$","|^AKO_06$","|^AKO_07$","|^AKO_08$","|^AKO_08B$",
#                       "|^MUS_10$","|^SHO_01$","|^SHO_10$","|^SHO_03B$","|^SHO_04B$","|^SHO_05$","|^SHO_08B$","|^SHO_09$",
#                       "|^AKO_02B$","|^AKO_08$","|^AKO_09$","|^AKO_10$"),collapse="")

stoves_remove <-paste(c("xabc","|xxabc"),collapse="")
