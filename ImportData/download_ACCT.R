# download data files from ACCT website

library(lubridate)

#prepare write and append log
var_filename_log<-"download_log.txt"
dest_logfile<-paste("C:/ACCT/DATA/SourceFiles/",var_filename_log, sep="")

#prepare dynamic source filename and path for download from ACCT
var_today<-today()
var_today_fmt<-format(var_today, format="%Y%m%d")
var_filename_in<-paste(var_today_fmt,"_pipe-delimited-export.zip", sep="")
#url<-"https://aact.ctti-clinicaltrials.org/static/exported_files/daily/20200120_pipe-delimited-export.zip"
url_dynamic<-paste("https://aact.ctti-clinicaltrials.org/static/exported_files/daily/",var_filename_in, sep="")

#prepare target directory and filename for zip
var_filename_out<-"ACCT.zip"
destfile<-paste("C:/ACCT/DATA/SourceFiles/",var_filename_out, sep="")

#start writing to log file
write(paste("FilePath_ACCT",url_dynamic, sep="="), dest_logfile, append=TRUE)
write(paste("StartDownload_ACCT",now(), sep="="), dest_logfile, append=TRUE)
download.file(url_dynamic,destfile)
write(paste("DownloadComplete_ACCT",now(), sep="="), dest_logfile, append=TRUE)

#unzip the downloaded zip file
write(paste("UnzipStart_ACCT",now(), sep="="), dest_logfile, append=TRUE)
dest_unzip_dir<-"C:/ACCT/DATA/unzipSrcFiles"
unzip("C:/ACCT/DATA/SourceFiles/ACCT.zip", exdir = dest_unzip_dir)
write(paste("UnzipComplete_ACCT",now(), sep="="), dest_logfile, append=TRUE)
write(paste("-----------------------------"), dest_logfile, append=TRUE)




