#generate dummy patient log data based on measurements and number of observation req
library(dplyr)
library(stringr)
library(lubridate)


#create patient table
var_patid_st<-1000
var_pat_size<-50
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

#Randomize stratified patient sets using rsample
#keep gender ration proportionally same
library(rsample)
gender_dist<-table(df_patient$gender) %>%
  prop.table()
#gender_dist
var_random_ratio<-0.6
set.seed(123)
index<-initial_split(df_patient, prop=var_random_ratio, strata = "gender")
patient_treatment<-training(index)
patient_control<-testing(index)

#check gender distribution
gender_dist_treatment<-table(patient_treatment$gender) %>%
  prop.table()
gender_dist_treatment

gender_dist_control<-table(patient_control$gender) %>%
  prop.table()
gender_dist_control

#add indicator in respective datasets to identify randomization
patient_treatment$random<-"Treatment"
patient_control$random<-"Control"

#union two sets
df_patient<-union_all(patient_treatment,patient_control)

#########################################################################

#set paths for data files
in_path_measurement<-paste(var_DIR_HOME, "apps/app03/data/measurement.csv", sep="")

#reads the data files into dataframes
df_measurement<-read.csv(in_path_measurement, header=TRUE, sep = ",",na.strings = "NA", nrows = -100)


#Generate observations for measurements
out_path<-paste(var_DIR_HOME, "apps/app03/data/observations_log.txt", sep="")

#delete the previous output file if exists
if(file.exists(out_path)) {
  file.remove(out_path)
}

#create the output file
if(!file.exists(out_path)) {
  file.create(out_path)
  write(paste("measure_id","measurement_name","total_readings","obsSeq","obs_date","obs_id","measurement_unit","lower_limit","upper_limit","variance_normal", sep="|"),out_path, append = TRUE)
}

for (i in 1:nrow(df_measurement)) {
  begin <- 1
  total_readings<-df_measurement$total_readings[i]
  measure_id<-df_measurement$measure_id[i]
  measurement_name<-df_measurement$measurement_name[i]
  measurement_unit<-df_measurement$measurement_unit[i]
  lower_limit<-df_measurement$lower_limit[i]
  upper_limit<-df_measurement$upper_limit[i]
  variance_normal<-df_measurement$variance_normal[i]
  obs_frequency<-df_measurement$obs_frequency[i]
  while(begin <= total_readings) {
    obs_id=paste(measure_id,"_",begin, sep="")
    obs_date=if_else(obs_frequency=="Daily",Sys.time() + days(begin-1),
                     if_else(obs_frequency=="Weekly" && begin==1,Sys.time(),
                             if_else(obs_frequency=="Weekly" && begin>=1,Sys.time()+ days((begin-1)*7),
                                     if_else(obs_frequency=="Daily-2" && begin==1,Sys.time(),
                                             if_else(obs_frequency=="Daily-2" && begin>=1, Sys.time()+ hours((begin-1)*12),
                                                     if_else(obs_frequency=="Daily-3" && begin==1,Sys.time(),
                                                             if_else(obs_frequency=="Daily-3" && begin>=1, Sys.time()+ hours((begin-1)*8),
                     Sys.time()
                     )))))))
    cat(paste(measure_id,measurement_name,total_readings,begin,obs_date,obs_id,measurement_unit,lower_limit,upper_limit,variance_normal, sep="|"),fill =TRUE,file=out_path, append = TRUE)
    begin<-begin+1
  }
}


#set paths for data files
in_observations<-paste(var_DIR_HOME, "apps/app03/data/observations_log.txt", sep="")

#reads the data files into dataframes
observation_log<-read.csv(in_observations, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

observation_log<-merge.data.frame(df_patient,observation_log, by=NULL, sort = TRUE)
observation_log<-arrange(observation_log,(patient_id))
observation_log$row_id<-seq(from=1, to=nrow(observation_log), by=1)
###############################################################

observation_log_control<-observation_log %>%
  filter(random=="Control")

observation_log_treatment<-observation_log %>%
  filter(random=="Treatment")

###########################################################
#add observation unit and value for control group
#Generate observations for measurements
out_path_patient_control<-paste(var_DIR_HOME, "apps/app03/data/patients_control_log.txt", sep="")

#delete the previous output file if exists
if(file.exists(out_path_patient_control)) {
  file.remove(out_path_patient_control)
}

#create the output file
if(!file.exists(out_path_patient_control)) {
  file.create(out_path_patient_control)
  write(paste("row_id","actual_value", sep="|"),out_path_patient_control, append = TRUE)
}

for (i in 1:nrow(observation_log_control)) {
  row_id=observation_log_control$row_id[i]
  actual_value=sample(observation_log_control$lower_limit[i]:observation_log_control$upper_limit[i], 1, replace = TRUE)
  cat(paste(row_id,actual_value, sep="|"),fill =TRUE,file=out_path_patient_control, append = TRUE)
  
}

#set paths for data files
in_pat_obs_control<-paste(var_DIR_HOME, "apps/app03/data/patients_control_log.txt", sep="")

#reads the data files into dataframes
patient_obs_control<-read.csv(in_pat_obs_control, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)


##################################################################
#add observation unit and value for treatment group
#Generate observations for measurements
out_path_patient_treatment<-paste(var_DIR_HOME, "apps/app03/data/patients_treatment_log.txt", sep="")

#delete the previous output file if exists
if(file.exists(out_path_patient_treatment)) {
  file.remove(out_path_patient_treatment)
}

#create the output file
if(!file.exists(out_path_patient_treatment)) {
  file.create(out_path_patient_treatment)
  write(paste("row_id","actual_value", sep="|"),out_path_patient_treatment, append = TRUE)
}

for (i in 1:nrow(observation_log_treatment)) {
  row_id=observation_log_treatment$row_id[i]
  actual_value=(observation_log_treatment$variance_normal[i]*log(observation_log_treatment$obsSeq[i]))+(sample(observation_log_treatment$lower_limit[i]:observation_log_treatment$upper_limit[i], 1, replace = TRUE))
  cat(paste(row_id,actual_value, sep="|"),fill =TRUE,file=out_path_patient_treatment, append = TRUE)
  
}

#set paths for data files
in_pat_obs_treatment<-paste(var_DIR_HOME, "apps/app03/data/patients_treatment_log.txt", sep="")

#reads the data files into dataframes
patient_obs_treatment<-read.csv(in_pat_obs_treatment, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#Union control and treatment group observations patient_logs
patient_obs<-union_all(patient_obs_control,patient_obs_treatment)

#stich patient log to observation log
patient_observation_log<-left_join(observation_log,patient_obs, by="row_id")

#####################################################################

#write to csv file
write.table(patient_observation_log,paste(var_DIR_HOME, "apps/app03/data/patient_observations_log.txt", sep=""), sep = "|", row.names = FALSE)

#copy individual files from other folders to apps
var_path_obs_log<-paste(var_DIR_HOME, "apps/app03/data/patient_observations_log.txt", sep="")

var_EXTRACT_TOAPP01_DIR<-paste(var_DIR_HOME, "apps/app01/data", sep="")
#copy the files
file.copy(var_path_obs_log,var_EXTRACT_TOAPP01_DIR, overwrite = TRUE)


