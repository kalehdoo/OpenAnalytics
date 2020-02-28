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

#subset recruiting studies
x_studies_recruiting<-subset.data.frame(agg_studies, subset = (overall_status=="Recruiting"),
                                        select=c("nct_id","anticipated_study_enrollment","lead_sponsor_name",
                                                 "register_to_start_days","agency_class","sponsor_type"))


#check if condition study is recruiting
x_conditions<- mutate(x_conditions,
                      flag_study_recruiting=if_else(casefold(nct_id) %in% casefold(x_studies_recruiting$nct_id),1,0)
)

#subset recruiting conditions
conditions_recruiting<-subset.data.frame(x_conditions, subset = (flag_study_recruiting>0))

#join studies to conditions
conditions_rec_study<-left_join(conditions_recruiting, x_studies_recruiting, by="nct_id")
#remove enrollments greater than 9999 as could be outliars
conditions_rec_study<-mutate(conditions_rec_study,
                             "anticipated_study_enrollment"=ifelse(anticipated_study_enrollment>=9999,0,anticipated_study_enrollment),
)

#aggregate recruiting conditions
agg_Studiesbyconditions<-data.table(conditions_rec_study)[,list( 
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
write.table(agg_Studiesbyconditions, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_conditions_recruiting.txt", sep=""), sep = "|", row.names = FALSE)
