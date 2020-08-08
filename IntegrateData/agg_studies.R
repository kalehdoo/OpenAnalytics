#import libraries
library(dplyr)
library(stringr)
library(lubridate)

#set paths for data files
in_path_x_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_studies.txt", sep="")
in_path_x_lead_sponsors<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_lead_sponsors.txt", sep="")
in_path_agg_study_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_conditions_study.txt", sep="")
in_path_agg_study_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities_study.txt", sep="")
in_path_agg_study_interventions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_study_interventions.txt", sep="")
in_path_agg_collaborators_by_study<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_collaborators_by_study.txt", sep="")


#reads the data files into dataframes
x_studies<-read.csv(in_path_x_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
x_lead_sponsors<-read.csv(in_path_x_lead_sponsors, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
agg_study_conditions<-read.csv(in_path_agg_study_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
agg_study_facilities<-read.csv(in_path_agg_study_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
agg_study_interventions<-read.csv(in_path_agg_study_interventions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
agg_collaborators_by_study<-read.csv(in_path_agg_collaborators_by_study, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)


#left join lead_sponsors to studies - has 1-1 relationship
agg_studies_sponsors<-left_join(x_studies,x_lead_sponsors,by="nct_id")

#stich conditions at study level to study aggregate
agg_studies_sponsors$nct_id<-as.character(agg_studies_sponsors$nct_id)
agg_study_conditions$nct_id<-as.character(agg_study_conditions$nct_id)
agg_studies_conditions<-left_join(agg_studies_sponsors,agg_study_conditions,by="nct_id")

#stitch conditions at study level
agg_study_facilities$nct_id<-as.character(agg_study_facilities$nct_id)
agg_studies_facilities<-left_join(agg_studies_conditions,agg_study_facilities,by="nct_id")

#stitch facilities at study level
agg_study_interventions$nct_id<-as.character(agg_study_interventions$nct_id)
agg_studies_interventions<-left_join(agg_studies_facilities,agg_study_interventions,by="nct_id")

#stich collaborators
agg_collaborators_by_study$nct_id<-as.character(agg_collaborators_by_study$nct_id)
agg_collaborators_by_study<-left_join(agg_studies_interventions,agg_collaborators_by_study,by="nct_id")

#final set
agg_studies_combined<-agg_collaborators_by_study

#write combined results to agg_studies txt file
write.table(agg_studies_combined,paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies.txt", sep=""), sep = "|", row.names = FALSE)



