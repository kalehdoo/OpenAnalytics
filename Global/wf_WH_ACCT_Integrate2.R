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

########################################################
#prepare write and append log for warehouse load
dest_whlogfile<-paste(var_DIR_HOME,"Data/ACCT/DATA/logs/log_warehouse.txt", sep="")
#create file if does not exists with header
if(!file.exists(dest_whlogfile)) {
  file.create(dest_whlogfile)
  write(paste("BatchId","EventName","EventTime", sep="|"),dest_whlogfile, append = TRUE)
}


#######################################################################
#execute to create integrated aggregate tables in warehouse
write(paste(NextBatchId,"IntegrationStart",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_conditions_study",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_conditions_study.R"))
write(paste(NextBatchId,"end_agg_conditions_study",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_studyfacilities",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_studyfacilities.R"))
write(paste(NextBatchId,"end_agg_studyfacilities",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_studyfacilities_recruiting",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_studyfacilities_recruiting.R"))
write(paste(NextBatchId,"end_agg_studyfacilities_recruiting",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_facilities_study",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_facilities_study.R"))
write(paste(NextBatchId,"end_agg_facilities_study",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_interventions_study",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_interventions_study.R"))
write(paste(NextBatchId,"end_agg_interventions_study",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_studies",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_studies.R"))
write(paste(NextBatchId,"end_agg_studies",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_studyconditions_study",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_studyconditions_study.R"))
write(paste(NextBatchId,"end_agg_studyconditions_study",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_conditions_all",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_conditions_all.R"))
write(paste(NextBatchId,"end_agg_studyconditions_facilities",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_conditions_recruiting",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_conditions_recruiting.R"))
write(paste(NextBatchId,"end_agg_conditions_recruiting",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_studyconditions_facilities",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_studyconditions_facilities.R"))
write(paste(NextBatchId,"end_agg_studyconditions_facilities",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_sponsors",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_sponsors.R"))
write(paste(NextBatchId,"end_agg_sponsors",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"start_agg_year",now(), sep="|"), dest_whlogfile, append=TRUE)
source(paste0(var_DIR_HOME,"IntegrateData/agg_year.R"))
write(paste(NextBatchId,"end_agg_year",now(), sep="|"), dest_whlogfile, append=TRUE)

write(paste(NextBatchId,"IntegrationEnd",now(), sep="|"), dest_whlogfile, append=TRUE)

##################################################################
#remove data in the memory and quit the session without saving the session
rm(list = ls())
#q("no", 1, FALSE)