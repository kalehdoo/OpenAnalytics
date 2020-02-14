library(lubridate)
library(sqldf)

#all scripts must be executed in the order
#create global parameters

#ACCT_HOME to store path of home dir
assign("var_DIR_HOME", "C:/Users/ranamanohar/Documents/GitHub/OpenAnalytics/", envir = .GlobalEnv)

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
write(NextBatchId,in_path_batch, append = TRUE)


#create parameters for year period
assign("var_current_date", today(), envir = .GlobalEnv)
assign("var_current_year", year(today()), envir = .GlobalEnv)
assign("var_last_year", (var_current_year-1), envir = .GlobalEnv)
assign("var_last_year_2", (var_current_year-2), envir = .GlobalEnv)
assign("var_last_year_3", (var_current_year-3), envir = .GlobalEnv)
assign("var_last_year_4", (var_current_year-4), envir = .GlobalEnv)
assign("var_last_year_5", (var_current_year-5), envir = .GlobalEnv)
assign("var_last_year_10", (var_current_year-10), envir = .GlobalEnv)

#prepare write and append log for warehouse load
dest_whlogfile<-paste(var_DIR_HOME,"Data/ACCT/DATA/logs/log_warehouse.txt", sep="")
#create file if does not exists with header
if(!file.exists(dest_whlogfile)) {
  file.create(dest_whlogfile)
  write(paste("BatchId","EventName","EventTime", sep="|"),dest_whlogfile, append = TRUE)
}

#execute basic transformation scripts
write(paste(NextBatchId,"StageStart",now(), sep="|"), dest_whlogfile, append=TRUE)
write(paste(NextBatchId,"start_x_studies",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"TransformData/x_studies.R"))
write(paste(NextBatchId,"start_x_sponsors",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"TransformData/x_sponsors.R"))
write(paste(NextBatchId,"start_x_conditions",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"TransformData/x_conditions.R"))
write(paste(NextBatchId,"start_x_facilities",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"TransformData/x_facilities.R"))
write(paste(NextBatchId,"start_x_interventions",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"TransformData/x_interventions.R"))
write(paste(NextBatchId,"StageEnd",now(), sep="|"), dest_whlogfile, append=TRUE)


#execute to create integrated aggregate tables in warehouse
source(paste0(var_DIR_HOME,"IntegrateData/agg_conditions_study.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_facilities.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_facilities_study.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_interventions_study.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_studies.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_sponsors.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_year.R"))

#execute to create extract summary tables in extract folder
source(paste0(var_DIR_HOME,"Extracts/mv_year_Lst10Yr.R"))
source(paste0(var_DIR_HOME,"Extracts/mv_studies_recruiting.R"))


