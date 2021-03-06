# download data files from ACCT website

library(lubridate)

#prepare write and append log
var_filename_log<-"Data/ACCT/DATA/logs/log_acct_download.txt"
dest_logfile<-paste(var_DIR_HOME,var_filename_log, sep="")


#prepare dynamic source filename and path for download from ACCT
var_today<-today()
var_today_fmt<-format(var_today, format="%Y%m%d")
var_filename_in<-paste(var_today_fmt,"_pipe-delimited-export.zip", sep="")
#url<-"https://aact.ctti-clinicaltrials.org/static/exported_files/daily/20200120_pipe-delimited-export.zip"
url_dynamic<-paste("https://aact.ctti-clinicaltrials.org/static/exported_files/daily/",var_filename_in, sep="")

#prepare target directory and filename for zip
var_filename_out<-"ACCT.zip"
destdir<-paste(var_DIR_HOME, "Data/ACCT/DATA/SourceFiles/", sep="")
destfile<-paste(destdir, var_filename_out, sep="")

#start writing to log file
#write(paste("FilePath_ACCT",url_dynamic, sep="|"), dest_logfile, append=TRUE)
write(paste("StartDownload_ACCT",now(), sep="|"), dest_logfile, append=TRUE)
download.file(url_dynamic,destfile)
write(paste("DownloadComplete_ACCT",now(), sep="|"), dest_logfile, append=TRUE)

#unzip the downloaded zip file
write(paste("UnzipStart_ACCT",now(), sep="|"), dest_logfile, append=TRUE)

dest_unzip_dir<-paste(var_DIR_HOME,"Data/ACCT/DATA/unzipSrcFiles", sep="")
dest_unzip_filepath<-paste(var_DIR_HOME,"Data/ACCT/DATA/unzipSrcFiles", sep="")

unzip(destfile, exdir = dest_unzip_dir)

write(paste("UnzipComplete_ACCT",now(), sep="|"), dest_logfile, append=TRUE)





