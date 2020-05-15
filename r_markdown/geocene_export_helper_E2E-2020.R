#Import Geocene Studies data, organize it so it looks like Sumsarizer data.
#***Data from other campaigns, stove types, and stove groups will be ignored!!***
rm(list = ls())


###***Enter parameter file name for data to be analyzed***###
parameter_filename <- "parameter_file_E2E.R" 
processor ='FF_sensitive'


# Include libraries 
source('r_scripts/load.R')
# source('r_scripts/load_data.R')
source(paste0('r_files/',parameter_filename)) #OG Codebase

stove_types <- paste(stove_codes$stove,  collapse = "|")
stove_groups <-paste(stove_group_codes$group,collapse = "|")

# The name of the unzipped folder containing the Geocene Studies export
studies_export_folder = '~/Dropbox/UNOPS emissions exposure/Data/analysis-20200421/caa_dots_data'
save_folder = 'SUMSARIZED' #Data is saved to this folder, to be consistent with past codes.

dot_data_files = list.files(paste0(studies_export_folder,'/metrics'),pattern='.csv|.CSV', full.names = T,recursive = F)

# Define which files should be considered exclude raw data files from Dots
save_path = grep(dir(paste0('../',studies_export_folder)),pattern = 'mission|Icon\r|event',inv = T,value = T)

# Read in the missions and events CSVs
events = fread(paste(studies_export_folder, 'events.csv', sep = '/'))
events = events[processor_name==processor]
events$start_time = as.POSIXct(events$start_time, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
events$stop_time = as.POSIXct(events$stop_time, "%Y-%m-%dT%H:%M:%S", tz = "UTC")

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

make_tags = function(tags){
  tags<-tags[grep(":", tag)]  #Keep only tags with a colon.
  tags<-tags[str_count(tags$tag, ":")<2]  #Keep only tags with less than two colons.
  tag_dicts = strsplit(tags$tag, ":")
  tag_dicts = as.data.table(matrix(unlist(tag_dicts), ncol=2, byrow=TRUE))
  names(tag_dicts) = c('tag_category','tag_value')
  tag_dicts$tag_value <- gsub( " ", "",tag_dicts$tag_value)
  
  tags = cbind(tags,tag_dicts)[,c('mission_id','tag_category','tag_value')]
  wide_tags = dcast(unique(tags,by=c('mission_id','tag_category')), mission_id ~ tag_category, value.var = 'tag_value')
  return(wide_tags)
}

tags = make_tags(fread(paste(studies_export_folder, 'tags.csv', sep = '/')))

missions = fread(paste(studies_export_folder, 'missions.csv', sep = '/'))
missions <- missions[grepl(campaign_name,campaign,ignore.case = TRUE),]
missions = merge(missions,tags,by='mission_id')
missions = missions[,c("mission_id","mission_name","meter_name","meter_id","notes","campaign","creator_username",
                       "household_id","indoors","other_people_use","other_people_use_n","shared_cooking_area","stove_type","stove_type_other")]

missions$stove_type<-mgsub::mgsub(missions$stove_type, stove_codes$stove, stove_codes$stove_descriptions)


missions[,filename:=paste(shared_cooking_area,household_id,stove_type,indoors,sep = "_")]
missions[, household_id:=NULL]
missions[, shared_cooking_area:=NULL]
missions[, stove_type:=NULL]


# Combine time series data and mission+tag metadata 
temp = merge(dot_data, as.data.table(missions), by = c('mission_id'))

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
                               logger_id = all_data$logger_id
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



