#import libraries
library(dplyr)
library(sqldf)
library(stringr)

#HOME to store path of home dir
assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)

#set paths for data files
in_path_x_sponsors<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_lead_sponsors.txt", sep="")

#reads the data files into dataframes
sponsors<-read.csv(in_path_x_sponsors, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#set paths for data files
in_path_x_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_studies.txt", sep="")

#reads the data files into dataframes
studies<-read.csv(in_path_x_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

studies<-sqldf("select distinct nct_id, phase 
                  from studies")

sponsors<-left_join(sponsors, studies, by=c("nct_id"))

#set paths for data files
in_path_x_collaborators<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_collaborators.txt", sep="")

#reads the data files into dataframes
collaborators<-read.csv(in_path_x_collaborators, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)


r_sponsor_collaborator<- sqldf("select sponsors.lead_sponsor_name as 'Sponsor', collaborators.collaborator_name as 'Collaborator', count(distinct(sponsors.nct_id)) as cnt_studies
                                  from sponsors
                                  left join collaborators
                                  on sponsors.nct_id=collaborators.nct_id
                                  where length(collaborators.collaborator_name)>1
                               group by sponsors.lead_sponsor_name, collaborators.collaborator_name
                               having count(distinct(sponsors.nct_id))>=1")



#write to script for sponsor master node txt file

write.table(r_sponsor_collaborator,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_collaborator.txt", sep=""), sep = "|", row.names = FALSE)

#get facilities data
#set paths for data files
in_path_x_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facilities.txt", sep="")

#reads the data files into dataframes
facilities<-read.csv(in_path_x_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

r_sponsor_site<- sqldf("select sponsors.lead_sponsor_name as 'Sponsor', facilities.facility_name as 'Facility', sponsors.phase as studyPhase,count(distinct(sponsors.nct_id)) as cnt_studies
                                  from sponsors
                                  left join facilities
                                  on sponsors.nct_id=facilities.nct_id
                                  where length(facilities.facility_name)>1
                                  group by sponsors.lead_sponsor_name, facilities.facility_name, sponsors.phase
                                  having count(distinct(sponsors.nct_id))>=1")

write.table(r_sponsor_site,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_site.txt", sep=""), sep = "|", row.names = FALSE)


#get conditions data
#set paths for data files
in_path_x_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep="")

#reads the data files into dataframes
conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

r_sponsor_conditions<- sqldf("select sponsors.lead_sponsor_name as 'Sponsor', conditions.condition_name as 'Condition', sponsors.phase as studyPhase, count(distinct(sponsors.nct_id)) as cnt_studies
                                  from sponsors
                                  left join conditions
                                  on sponsors.nct_id=conditions.nct_id
                                  where length(conditions.condition_name)>1
                                  group by sponsors.lead_sponsor_name, conditions.condition_name, sponsors.phase
                                  having count(distinct(sponsors.nct_id))>=1")

write.table(r_sponsor_conditions,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_conditions.txt", sep=""), sep = "|", row.names = FALSE)

#get conditions data
#set paths for data files
in_path_x_intervention_other_names<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_intervention_other_names.txt", sep="")

#reads the data files into dataframes
interventions<-read.csv(in_path_x_intervention_other_names, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

r_sponsor_interventions<- sqldf("select sponsors.lead_sponsor_name as 'Sponsor', interventions.name as intervention, count(distinct(sponsors.nct_id)) as cnt_studies
                                  from sponsors
                                  left join interventions
                                  on sponsors.nct_id=interventions.nct_id
                                  where length(interventions.name)>1
                                  group by sponsors.lead_sponsor_name, interventions.name
                                  having count(distinct(sponsors.nct_id))>0")

write.table(r_sponsor_interventions,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_interventions.txt", sep=""), sep = "|", row.names = FALSE)
