#Select a bunch of csv files, plot them, do some stats on them, save the figures and stats to the same folder
library(lubridate)
source('r_scripts/load.R')
library(tcltk)

files_plot <- tk_choose.files(default = "", caption = "Select files",
                multi = TRUE, filters = NULL, index = 1)

plot_names <- gsub(".csv",".png",files_plot)

for (i in 1:length(files_plot)){
  
  datas <- as.data.table(read.csv(files_plot[i], skip=19, colClasses=c('character', 'character', 'character')))
  setnames(datas, c('Date.Time', 'Unit', 'Value'))
  datas$Value<-as.numeric(datas$Value)
  
  testdate <- mdy_hms(datas$Date.Time[1])
  testdate2 <- mdy_hm(datas$Date.Time[1])
  datas$Date.Time <- parse_date_time(datas$Date.Time, orders = c("m/d/y HM", "m/d/y HMS"))#,"d/m/y HM","d-m-y HM"))

  
  # 
  # percentiles <- quantile(datas$Value,c(.05,.95))
  # quantile_ratio <- unname(percentiles[2]/percentiles[1])
  # cat_string <- paste("min T(C) = ",as.character(percentiles[1]),
  #                     ", max T(C) = ",as.character(percentiles[2]), 
  #                     "max/min = ", strtrim(as.character(quantile_ratio),5))
  # 
  # 
  cat_string <- ""
  png(filename=plot_names[i],width = 550, height = 480, res = 100)
  plot(datas$Date.Time, datas$Value, main=plot_names[i],xaxt = "n",
       type = "l",xlab = cat_string,ylab="Temp (C)",prob=TRUE,cex.main = .6,cex = .5)
  axis(1, datas$Date.Time, format(datas$Date.Time, "%m/%d"), cex.axis = .7)
  grid(nx = NULL, ny = 5, col = "lightgray", lty = "dotted",
       lwd = par("lwd"), equilogs = TRUE)
  dev.off()
  # 
}

