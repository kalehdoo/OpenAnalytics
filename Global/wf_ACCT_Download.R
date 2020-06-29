library(lubridate)
library(sqldf)
#all scripts must be executed in the order
#create global parameters

#ACCT_HOME to store path of home dir
assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)


#create parameters for year period
assign("var_current_date", today(), envir = .GlobalEnv)
assign("var_current_year", year(today()), envir = .GlobalEnv)
assign("var_last_year", (var_current_year-1), envir = .GlobalEnv)
assign("var_last_year_2", (var_current_year-2), envir = .GlobalEnv)
assign("var_last_year_3", (var_current_year-3), envir = .GlobalEnv)
assign("var_last_year_4", (var_current_year-4), envir = .GlobalEnv)
assign("var_last_year_5", (var_current_year-5), envir = .GlobalEnv)
assign("var_last_year_10", (var_current_year-10), envir = .GlobalEnv)

#execute script to download ACCT zip file from web and extract
path_ACCT_downloadLog<-paste(var_DIR_HOME, "Data/ACCT/DATA/logs/log_acct_download.txt", sep="")

#create file if does not exists and initialize
if(!file.exists(path_ACCT_downloadLog)) {
  file.create(path_ACCT_downloadLog)
  write(paste("EventName","EventValue", sep="|"),path_ACCT_downloadLog, append = TRUE)
  write(paste("UnzipComplete_ACCT",Sys.time()-86400, sep = "|"),path_ACCT_downloadLog, append = TRUE)
}

ACCT_downloadLog<-read.csv(path_ACCT_downloadLog, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#get max date when file downloaded
max_download_dt<-sqldf("select max(EventValue) from ACCT_downloadLog where EventName=='UnzipComplete_ACCT'")
max_download_dt<-as.Date.character(max_download_dt, "%Y-%m-%d %H:%M:%S")

#execute ACCT download only if max downloaded date is less than current date
if((var_current_date)>(max_download_dt)) {
  source(paste0(var_DIR_HOME,"ImportData/download_ACCT.R"))
} else {
  print("Do not download")
}
