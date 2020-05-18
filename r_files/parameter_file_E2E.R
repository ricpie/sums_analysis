#Parameter file for analysis for Boston College Smokeless Village study
#Set path to tracking sheet relative to 'SUMS processing' directory
path_tracking_sheet <- NA #Set to NA if none available
path_tracking_sheet_json <- NA #"~/Dropbox/Peru 2019 NAMA Internal/Analysis/SUMS Tracking data/Perú_NAMA_SUMs_v3_results.json"

project_name <- "Kenya E2E"

stove_codes <- data.frame(stove = as.factor(c("lpg","traditional_manufactured","traditional_non_manufactured","kerosene",
                                              "lpg","trad-manufactured","trad-non-manufactured","3-stone-fire",
                                                    "electric","microwave","other","tsf","charcoal jiko","charcoal_jiko","charcoal")),  #Use these if there are different study arms.
                          stove_descriptions = as.factor(c("lpg","trad-manufactured","trad-non-manufactured","kerosene",
                                                           "lpg","trad-manufactured","trad-non-manufactured","trad-non-manufactured",
                                                           "electric","microwave","charcoal","trad-non-manufactured","charcoal","charcoal","charcoal"))) #group variable in filter_sumsarized.R

stove_group_codes <- data.frame(group = as.factor(c("charcoal_unprocessed","lpg","biomass","agriculturalresidue","wood","kerosene","NA","charcoal")),  #Use these if there are different study arms.
                                stove_groups = as.factor(c("charcoal-primary","lpg-primary","biomass-primary","biomass-primary","biomass-primary","kerosene-primary","biomass-primary","charcoal-primary"))) #group variable in filter_sumsarized.R

region_codes <- data.frame(region_code = as.factor(c("01")),  #Use these if there are different study arms.
                                region = as.factor(c("North"))) #group variable in filter_sumsarized.R

campaign_name = "CAA-Kenya"


cooking_group <- 30 # x minute window for grouping events together.
cooking_duration_minimum <- 1  #Minutes
cooking_duration_maximum <- 1440 #Minutes
logging_duration_minimum <- 1 #days.  Single file duration must be this long to be included in the analysis.
#If set to zero, it will not trim out any data, instead leaving the complete data sets available for analysis.  Trimming is done
#to account for placement times/dates.
total_days_logged_minimum <- 5 #Must have at least this many days for a households's stove to be included in the analysis. Default value is 5.
metadata_date_ignore <- 1

start_date_range <- "2019-9-1" #Year-Month-Day format.  Do not include data before this in the analysis
end_date_range <- "2020-12-1" #Do not include data after this in the analysis

timezone = 3 #Timezone relative to GMT.  Shifts the time by this many hours.

# Remove data from files selected as bad (they match the following strings, so they should be specific to the 
#given file, not generic enough to remove more files than intended)
bad_files <- paste(c("Excl","|AMB","|Eliminar","|Pre_Pilot-04_TMS_DL-01"
                     ,"|Pre_Pilot-04_TMS_DL-01","|AMB"),collapse="")

# Exclude data from the following households from the entire analysis. e.g.SHO_03 is removed
HHID_remove <- paste(c("^SHO_03$","|^SHO_04$","|^NA$","|^MUS_01$","|^AMB$"),collapse="")

# Files must have this pattern to be retained in the analysis
HHID_keep <- paste(c("KE"))


