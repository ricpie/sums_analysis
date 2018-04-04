iassist.split_chinadata <- function(x){
  start <- frames[rnum==x, start]
  end <- frames[rnum==x, end]
  this <- datapipes[start:end,]

  serial  <- this[prefix %like% "### SERIAL_NO", strsplit(prefix, ": ")[[1]][2],]
  prefix  <- this[10:100,as.numeric(unique(prefix))]
  button  <- this[10:100,as.numeric(unique(number))]
  session <- this[10:100,as.numeric(unique(session))]
  
  fileName <- paste(substring(serial, 9, nchar(serial)), prefix, button, session, sep="_")
  fileName <- file.path(lzFile, paste(datapipes$sheet[1],'_',hhid,fileName, ".csv", sep=""))
  
  header <- this[1:8, prefix]
  header <- c(header,"  ")
  
  datapoints <- this[11:nrow(this),]
  datapoints <- datapoints[,c('datetime', 'temp'), with=F]
  datapoints[, Unit:="C"]
  datapoints[,datetime:=as.POSIXct(as.numeric(datetime)/1000, origin="1970/1/1")]
  setnames(datapoints, c('Date.Time', 'Value', 'Unit'))
  setcolorder(datapoints, c(1,3,2))
  datapoints[, Value:=as.numeric(Value)]
  
  cat(paste(header, collapse="\n"), file=fileName)
  write.table(datapoints, fileName, sep=",", append=TRUE, row.names=F, quote=F)
}

iassist.import_chinadata <- function(file, zip=T){

  hhid <- file %>%
    basename %>%
    file_path_sans_ext %>%
    gsub("iButtons","",.) %>%
    assign("hhid", ., envir = .GlobalEnv) # works
    
  assign('lzFile', 'converted data', envir=.GlobalEnv)

  col_namestemp=c('sheet','prefix','number','session',"datetime",'sample','temp','humidity')
  
  datapipes <- file %>%
    excel_sheets() %>%
    set_names() %>% 
    map_df(~ read_excel(path = file, sheet = .x, range = cell_cols("A:G"),col_names=FALSE), .id = "sheet") %>%
    as.data.table() %>%
    set_names(col_namestemp) 
  
  print(datapipes, n = 20)
  assign('datapipes', datapipes, envir=.GlobalEnv)
  
  # create an index vector "sections" that will inform where the sections start and end
  sections <- which(datapipes$prefix %like% "### SERIAL_NO: ") 
  sections <- append(sections, (nrow(datapipes)))
  
  frames <- data.table(start=sections,end=c(sections[2:length(sections)]-1,NA))
  frames[,rnum:=1:nrow(frames)]
  frames <- frames[1:(nrow(frames)-1)]
  frames <- frames[!(end-start<13)]
  assign('frames',frames,envir=.GlobalEnv)
  
  dir.create(path=lzFile, showWarnings = FALSE)
  
  l_ply(frames$rnum, iassist.split_chinadata)
  
  if(zip){
    zip(paste(lzFile,'.zip',sep=""), list.files(lzFile, full.names=T))
    unlink(lzFile, recursive=T)
  }
  
  #rm stuff assigned globally. fix your environments!
  removeMe <- c('frames', 'datapipes', 'lzFile', 'sections')
  
  rm(list=removeMe)
}
