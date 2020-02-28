#create aggregate at recruiting condition level
#import libraries
library(dplyr)
library(stringr)
library(data.table)

#set paths for data files
in_path_x_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep="")
in_path_agg_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies.txt", sep="")

#reads the data files into dataframes
x_conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)

#subset studies to get required columns
agg_studies<-subset.data.frame(agg_studies, select=c("nct_id","overall_status","anticipated_study_enrollment","lead_sponsor_name",
                                                 "register_to_start_days","agency_class","sponsor_type"))

#join studies to conditions
conditions_study<-left_join(x_conditions, agg_studies, by="nct_id")
#remove enrollments greater than 9999 as could be outliars
conditions_study<-mutate(conditions_study,
                             "anticipated_study_enrollment"=ifelse(anticipated_study_enrollment>=9999,0,anticipated_study_enrollment),
)

#aggregate recruiting conditions
agg_condition_study<-data.table(conditions_study)[,list( 
  flag_rarecondition = mean(flag_rare_disease, na.rm = TRUE),
  cnt_recruitingstudies = length(unique(nct_id)),
  cnt_sponsors = length(unique(lead_sponsor_name)),
  cnt_sponsors_ind = sum(ifelse(agency_class=="Industry",1,0)),
  cnt_sponsors_nonind = sum(ifelse(agency_class!="Industry",1,0)),
  cnt_sponsors_academic = sum(ifelse(sponsor_type=="Academic",1,0)),
  cnt_sponsors_hospital = sum(ifelse(sponsor_type=="Hospital",1,0)),
  cnt_anti_enrollment = sum(anticipated_study_enrollment, na.rm = TRUE),
  median_register_to_start_days = as.double(median(register_to_start_days))
), by='condition_name']

#write to txt file
write.table(agg_condition_study, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_conditions_all.txt", sep=""), sep = "|", row.names = FALSE, quote=FALSE)
