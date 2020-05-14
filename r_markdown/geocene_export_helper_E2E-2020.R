#Import Geocene Studies data, organize it so it looks like Sumsarizer data.
#***Data from other campaigns, stove types, and stove groups will be ignored!!***
rm(list = ls())


###***Enter parameter file name for data to be analyzed***###
parameter_filename <- "parameter_file_E2E.R" 


# Include libraries 
source('r_scripts/load.R')
# source('r_scripts/load_data.R')
source(paste0('r_files/',parameter_filename)) #OG Codebase

stove_types <- paste(stove_codes$stove,  collapse = "|")
stove_groups <-paste(stove_group_codes$group,collapse = "|")

# The name of the unzipped folder containing the Geocene Studies export
studies_export_folder = 'Geocene Studies Output'
save_folder = 'SUMSARIZED' #Data is saved to this folder, to be consistent with past codes.

dot_data_files = list.files('../Geocene Studies Output/metrics',pattern='.csv|.CSV', full.names = T,recursive = F)

# Define which files should be considered exclude raw data files from Dots
save_path = grep(dir(paste0('../',studies_export_folder)),pattern = 'mission|Icon\r|event',inv = T,value = T)

# Read in the missions and events CSVs
missions = fread(paste('..',studies_export_folder, 'missions.csv', sep = '/'))
events = fread(paste('..',studies_export_folder, 'events.csv', sep = '/'))
events$start_time = as.POSIXct(events$start_time, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
events$stop_time = as.POSIXct(events$stop_time, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
tags = fread(paste('..',studies_export_folder, 'tags.csv', sep = '/'))
tags$tag <- gsub( " ", "",tags$tag)

substitution_list <- stove_group_codes$stove_groups
substitution_list <- str_replace_all(substitution_list, "[[:punct:]]", "")
sub_list <- paste0("_",as.character(substitution_list)) #Create list for replacement
names(sub_list ) <- substitution_list
sub_list <- as.list(sub_list, use.names=FALSE)



# Define a function to read Dot raw data files
read_dot_data_file = function(dot_data_file) {
  if (file.info(dot_data_file)$size>100){
    tsdata = fread(dot_data_file,stringsAsFactors = F,header = T)
    tsdata$timestamp = as.POSIXct(tsdata$timestamp, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    tsdata = cbind(tsdata, filename = tools::file_path_sans_ext(dot_data_file))
  }else{tsdata=NULL}
  return(tsdata)
}

# Apply the function to read Dot data to all Dot data files and
# convert individual dataframes into a single data frame
dot_data = rbindlist(lapply(dot_data_files, read_dot_data_file), fill = T)
dot_data[, filename:=NULL]


# Combine mission and tag metadata.  Parse tags. Need Stove Type, Group variable, HHID form the tags
#Only keep data from campaign of interest
missions <- missions[grepl(campaign_name,campaign,ignore.case = TRUE),]

#Get HHID - every mission will be associated with an HHID. Mission x HHID
hhid_tags <- tags[grepl("ID:",tag,ignore.case = TRUE),]
setnames(hhid_tags, old=c("tag"), new=c("HHID"))
hhid_tags$HHID <- substr(hhid_tags$HHID,4,100)
stovetype_tags <- tags[grepl(stove_types,tag,ignore.case = TRUE),]
setnames(stovetype_tags, old=c("tag"), new=c("stove_type"))
stovetype_hhid_tags <- merge(stovetype_tags,hhid_tags,by="mission_id")

# Then get stove groups for each mission.  Not all missions will have a stove group.  Mission x stove_group
group_tags <- tags[grepl(stove_groups,tag,ignore.case = TRUE),]
setnames(group_tags, old=c("tag"), new=c("stove_group"))
group_hhid_tags <- merge(group_tags,hhid_tags,by="mission_id")
hhid_tags$region <- substr(hhid_tags$HHID,1,2)

#Subset of households for which we have both stove group and HHID.  Use this as key for merging later
#Mission x HHID and Stove_Group
hhid_group_tags <- merge(stovetype_hhid_tags,group_hhid_tags,by="HHID")
setnames(hhid_group_tags, old=c("mission_id.x"), new=c("mission_id"))

#Mission metadata x HHID
missionstags = merge(missions,hhid_group_tags, by = c('mission_id'))
missionstags$region <- substr(missionstags$HHID,1,2)

missionstags$stove_group<-mgsub::mgsub(missionstags$stove_group, stove_group_codes$group, stove_group_codes$stove_groups)
missionstags$stove_type<-mgsub::mgsub(missionstags$stove_type, stove_codes$stove, stove_codes$stove_descriptions)
missionstags$region<-mgsub::mgsub(missionstags$region, region_codes$region_code, region_codes$region)

tags_all<-tags[ , .(alltags = paste(tag, collapse=",")), by = mission_id]
missionstags = merge(missionstags, tags_all, by = c('mission_id'))
missionstags$HHID <- missionstags$HHID
missionstags[,filename:=paste(stove_group,HHID,stove_type,region,sep = "_")]
missionstags[, HHID:=NULL]
missionstags[, stove_group:=NULL]
missionstags[, stove_type:=NULL]


# Combine time series data and mission+tag metadata 
temp = merge(dot_data, as.data.table(missionstags), by = c('mission_id'))

# Join the events table with the time series data into one large wide table
temp[, helpTimestamp := timestamp]
events[, start_time := as.POSIXct(start_time)]
events[, stop_time := as.POSIXct(stop_time)]
setkey(events, mission_id, start_time, stop_time)
all_data=foverlaps(temp, events, by.x=c('mission_id','timestamp','helpTimestamp'),type='any')
all_data[, helpTimestamp := NULL ]
all_data[, start_time:=NULL]
all_data[, stop_time:=NULL]
setnames(all_data, old=c("meter_name"), new=c("logger_id"))

# all_data <- dplyr::mutate(all_data,DL = "DLdot") %>%
#   dplyr::mutate_all(funs(toupper))  
# dplyr::mutate(logger_id = gsub("Dot-","",dot_name)) %>%#First need to get the Dot ID in the right format.
# dplyr::mutate(logger_id =gsub('Dot_','',logger_id)) %>%
# dplyr::mutate(logger_id =gsub('DOT-','',logger_id)) %>%
#Join all_data with the jsonmetadata based on Dot ID.
# dplyr::left_join(dplyr::select(jsonmetadata,HHID,Group,stove_type,logger_id) %>%
# dplyr::distinct(logger_id,.keep_all = TRUE),by = "logger_id") %>%
# dplyr::mutate(stovegroup = Group) %>%
#Build a passable filename from the group, hhid, stovetype, DL#.


#Check to make sure all the dot Geocene entries have corresponding ODK entries.  Check for typos etc (manually)
# dot_name_check <- dplyr::as_data_frame(unique(all_data$logger_id)) %>%
#   dplyr::rename("logger_id" = "value") %>%
#   dplyr::full_join(jsonmetadata,"logger_id")


#Check to make sure all the dot ODK entries have corresponding Geocene entries.  Check for typos etc (manually)
# asdf<-anti_join(dplyr::as_data_frame(unique(jsonmetadata$logger_id)),dplyr::as_data_frame(unique(all_data$logger_id)) ,'value')

# Correct any Dot metadata entries (HHID, stove group, stove type, dot name) in the Dot file names using the jsonmetadata, that has been confirmed to be correct.
# unknowns <- dplyr::filter(all_data,"unknown"==HHID | "Unknown"==HHID)
# unknowns <- dplyr::filter(all_data,"11050"==HHID)
# unique_unknowns <- unique(unknowns$dot_name)
# unique_unknowns <- unique(unknowns$HHID)
# unknowns <- dplyr::filter(all_data,"EC01"==DotID)
# dplyr::filter(jsonmetadata,grepl('EC01',logger_id,fixed=TRUE))
# unknowns <- dplyr::filter(all_data,is.na(stovegroup))
# unique_unknowns <- unique(unknowns$HHID)
# uniquepattern <- paste(unique_unknowns, collapse = "|")
# uniquepattern <- gsub('Dot_','',unique_unknowns)
# dplyr::filter(jsonmetadata,grepl(uniquepattern[10],logger_id,fixed=TRUE))


# -----------------
# For SUMSarizer users, create a table that looks like SUMSarizer's old output
sumsarizer_output = data.table(filename = all_data$filename,
                               timestamp = all_data$timestamp,
                               value = all_data$value,
                               pred = all_data$event_kind,
                               datapoint_id = paste(all_data$processor_name,all_data$model_name,all_data$alltags),
                               dataset_id = all_data$meter_id
)
sumsarizer_output$pred[grepl('COOKING|cooking',sumsarizer_output$pred)] = TRUE
sumsarizer_output$pred[sumsarizer_output$pred == 'COOKING'] = TRUE
sumsarizer_output$pred[is.na(sumsarizer_output$pred)] = FALSE

uniquefiles <- unique(sumsarizer_output[,1])
#Save each file separately with the new correct name.  
for (i in 1:dim(uniquefiles)[1]) {
  output_temp <- sumsarizer_output[filename == uniquefiles[i]]
  write.csv(output_temp, file = paste0("../",save_folder,"/",output_temp$filename[1],".csv"),row.names = FALSE)
}

# Clean up the files
# rm(list=setdiff(ls(), c('all_data', 'dot_data', 'events', 'missions', 'sumsarizer_output')))



