#generate dummy patient log data based on measurements and number of observation req
library(dplyr)
library(stringr)


#create patient table
var_patid_st<-1000
var_pat_size<-10
var_age_min<-30
var_age_max<-40

df_patient<-data.frame(
  row_id=seq(from=var_patid_st+1, to=var_patid_st+var_pat_size, by=1),
  patient_id=paste0("P_",seq(from=var_patid_st+1,to=var_patid_st+var_pat_size,by=1)),
  #gender=sample(0:1,var_pat_size, replace=TRUE),
  gender=sample(c("M","F"),var_pat_size, replace=TRUE),
  age=sample(var_age_min:var_age_max, var_pat_size, replace=TRUE),
  stringsAsFactors=FALSE
)

#create measurement table
var_measure_st<-1000
var_measure_size<-4
#add the total readings

df_measurement<-data.frame(
  row_id=seq(from=var_measure_st+1, to=var_measure_st+var_measure_size, by=1),
  measure_id=paste0("M_",seq(from=var_measure_st+1,to=var_measure_st+var_measure_size,by=1)),
  total_readings=c(24,2,3,168),
  stringsAsFactors=FALSE
)

#Generate observations for measurements
out_path<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/resultsSimulator.txt", sep="")

#delete the previous output file if exists
if(file.exists(out_path)) {
  file.remove(out_path)
}

#create the output file
if(!file.exists(out_path)) {
  file.create(out_path)
  write(paste("measureId","obsReq","obsSeq","observationId", sep="|"),out_path, append = TRUE)
}

for (i in 1:nrow(df_measurement)) {
  begin <- 1
  readings<-df_measurement$total_readings[i]
  measurement<-df_measurement$measure_id[i]
  while(begin <= readings) {
    obs_id=paste(measurement,"_",begin, sep="")
    cat(paste(measurement,readings,begin,obs_id, sep="|"),fill =TRUE,file=out_path, append = TRUE)
    begin<-begin+1
  }
}

#set paths for data files
in_observations<-paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/resultsSimulator.txt", sep="")

#reads the data files into dataframes
df_observations<-read.csv(in_observations, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

observation_log<-merge.data.frame(df_patient,df_observations, by=NULL, sort = TRUE)
observation_log<-arrange(observation_log,(patient_id))
observation_log$row_id<-seq(from=1, to=nrow(observation_log), by=1)

#add observation unit and value
