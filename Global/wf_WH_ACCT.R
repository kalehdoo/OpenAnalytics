library(lubridate)
library(sqldf)

#all scripts must be executed in the order
#create global parameters

#HOME to store path of home dir
assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)

#set paths for data files
in_path_batch<-paste(var_DIR_HOME, "Global/BatchMaster.txt", sep="")

#create file if does not exists and initialize with id=1001
if(!file.exists(in_path_batch)) {
  file.create(in_path_batch)
  write("BatchId",in_path_batch, append = TRUE)
  write("1001",in_path_batch, append = TRUE)
}

#reads the data files into dataframes with header
batchid<-read.csv(in_path_batch, header = TRUE)

NextBatchId<-sqldf("select max(BatchId)+1 from batchid")
NextBatchId<-NextBatchId[[1]]
write(NextBatchId,in_path_batch, append = TRUE)

#################################################################
#create parameters for year period
assign("var_current_date", today(), envir = .GlobalEnv)
assign("var_current_year", year(today()), envir = .GlobalEnv)
assign("var_last_year", (var_current_year-1), envir = .GlobalEnv)
assign("var_last_year_2", (var_current_year-2), envir = .GlobalEnv)
assign("var_last_year_3", (var_current_year-3), envir = .GlobalEnv)
assign("var_last_year_4", (var_current_year-4), envir = .GlobalEnv)
assign("var_last_year_5", (var_current_year-5), envir = .GlobalEnv)
assign("var_last_year_10", (var_current_year-10), envir = .GlobalEnv)
assign("var_last_year_15", (var_current_year-15), envir = .GlobalEnv)

########################################################
#prepare write and append log for warehouse load
dest_whlogfile<-paste(var_DIR_HOME,"Data/ACCT/DATA/logs/log_warehouse.txt", sep="")
#create file if does not exists with header
if(!file.exists(dest_whlogfile)) {
  file.create(dest_whlogfile)
  write(paste("BatchId","EventName","EventTime", sep="|"),dest_whlogfile, append = TRUE)
}

####################################################################
#execute to create extract summary tables in extract folder
write(paste(NextBatchId,"DataMartStart",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_mv_year_Lst10Yr",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"Extracts/mv_year_Lst10Yr.R"))
write(paste(NextBatchId,"end_mv_year_Lst10Yr",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_mv_studies_recruiting",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"Extracts/mv_studies_recruiting.R"))
write(paste(NextBatchId,"end_mv_studies_recruiting",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_rec_conditions_list",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"Extracts/rec_conditions_list.R"))
write(paste(NextBatchId,"end_rec_conditions_list",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_sponsor_network",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"Extracts/sponsor_network.R"))
write(paste(NextBatchId,"end_sponsor_network",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"DataMartEnd",now(), sep="|"), dest_whlogfile, append=TRUE)

#########################################################################
#copy the required extracts to apps folder for final consumption
var_EXTRACT_FROM_DIR<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts", sep="")
var_EXTRACT_TOAPP01_DIR<-paste(var_DIR_HOME, "apps/app01/data", sep="")
var_EXTRACT_TOAPP02_DIR<-paste(var_DIR_HOME, "apps/app02/data/", sep="")

#get the list of files present in dir
#files_list<-list.files(var_EXTRACT_FROM_DIR)

#copy all the files in the extract folder for APP01
#file.copy(file.path(var_EXTRACT_FROM_DIR,files_list),var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)

#paths for the files required
var_path_agg_year<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_year_Lst10Yr.txt", sep="")
var_path_agg_month<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_month.txt", sep="")
var_path_agg_conditions_recruiting<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_conditions_recruiting.txt", sep="")
var_mv_recruiting_mini<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_studies_recruiting_mini.rds", sep="")
var_rec_conditions_list<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/rec_conditions_list.txt", sep="")
var_r_sponsor_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_conditions.txt", sep="")
var_r_sponsor_site<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_site.txt", sep="")
var_r_sponsor_collaborator<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_collaborator.txt", sep="")
var_r_sponsor_interventions<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_interventions.txt", sep="")

########################################################

##copy file extracts required for app02
file.copy(var_mv_recruiting_mini,var_EXTRACT_TOAPP02_DIR, overwrite = TRUE)
file.copy(var_rec_conditions_list,var_EXTRACT_TOAPP02_DIR, overwrite = TRUE)

#extract the timestamps of the files copied to app02
path_app02_mv_rec<-paste(var_EXTRACT_TOAPP02_DIR,"mv_studies_recruiting_mini.rds", sep="")
time_app02_mv_rec<-file.info(path_app02_mv_rec)$mtime

#prepare write and append log for app02
dest_app02logfile<-paste(var_EXTRACT_TOAPP02_DIR,"log_app02.txt", sep="")
#create file if does not exists with header
if(!file.exists(dest_app02logfile)) {
  file.create(dest_app02logfile)
  #write(paste("fileTime", sep="|"),dest_app02logfile, append = TRUE)
}

##write the file and file timestamp
write(paste(time_app02_mv_rec, sep="|"), dest_app02logfile, append=FALSE)

#copy the files for app01
file.copy(var_path_agg_year,var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)
file.copy(var_path_agg_month,var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)
file.copy(var_path_agg_conditions_recruiting,var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)
file.copy(var_r_sponsor_conditions,var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)
file.copy(var_r_sponsor_site,var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)
file.copy(var_r_sponsor_collaborator,var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)
file.copy(var_r_sponsor_interventions,var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)


##################################################################
#remove data in the memory and quit the session without saving the session
rm(list = ls())
#q("no", 1, FALSE)


