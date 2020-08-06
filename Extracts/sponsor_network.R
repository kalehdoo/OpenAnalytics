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

studies<-sqldf("select distinct nct_id, phase, study_first_posted_year 
                  from studies")

sponsors<-left_join(sponsors, studies, by=c("nct_id"))

#set paths for data files
in_path_x_collaborators<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_collaborators.txt", sep="")

#reads the data files into dataframes
collaborators<-read.csv(in_path_x_collaborators, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)


r_sponsor_collaborator<- sqldf("select sponsors.lead_sponsor_name as 'Sponsor',
                                        sponsors.sponsor_type as 'sponsorType',
                                        (CASE when length(sponsors.phase)=0 THEN 'NULL VALUE' ELSE sponsors.phase END) as 'studyPhase',
                                        collaborators.sponsor_type as 'CollaboratorType',
                                        collaborators.collaborator_name as 'Collaborator',
                                        count(distinct(sponsors.nct_id)) as cnt_studies
                                  from sponsors
                                  left join collaborators
                                  on sponsors.nct_id=collaborators.nct_id
                                  where length(collaborators.collaborator_name)>1
                               group by sponsors.lead_sponsor_name, sponsors.sponsor_type, sponsors.phase,collaborators.sponsor_type,collaborators.collaborator_name
                               having count(distinct(sponsors.nct_id))>=1")



#write to script for sponsor master node txt file
write.table(r_sponsor_collaborator,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_collaborator.txt", sep=""), sep = "|", row.names = FALSE)

#get facilities data
#set paths for data files
in_path_x_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facilities.txt", sep="")

#reads the data files into dataframes
facilities<-read.csv(in_path_x_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

r_sponsor_site<- sqldf("select sponsors.lead_sponsor_name as 'Sponsor',
                            (CASE when length(sponsors.study_first_posted_year)=0 THEN 'NOT AVAILABLE' ELSE sponsors.study_first_posted_year END) as 'StudyYear',
                            --substr(facilities.country,1,1) as 'CountryBegin',
                            facilities.country as 'Country',
                            (CASE when (length(facilities.state)=0 OR (facilities.state) is null) THEN 'NOT AVAILABLE' ELSE facilities.state END) as 'State',
                            facilities.city as 'City',
                            (CASE when (length(facilities.site_type)=0 OR (facilities.site_type) is null) THEN 'Others' ELSE facilities.site_type END) as 'FacilityType',
                            (CASE when (length(sponsors.phase)=0 OR (sponsors.phase) is null) THEN 'NOT AVAILABLE' ELSE sponsors.phase END) as 'studyPhase',
                            facilities.facility_name as 'Facility',
                            count(distinct(sponsors.nct_id)) as cnt_studies
                                  from sponsors
                                  left join facilities
                                  on sponsors.nct_id=facilities.nct_id
                                  where length(facilities.facility_name)>1 AND sponsors.study_first_posted_year>=2015
                                  group by 
                                  sponsors.lead_sponsor_name,
                                  (CASE when length(sponsors.study_first_posted_year)=0 THEN 'NOT AVAILABLE' ELSE sponsors.study_first_posted_year END),
                                  --substr(facilities.country,1,1),
                                  facilities.country,
                                  (CASE when (length(facilities.state)=0 OR (facilities.state) is null) THEN 'NOT AVAILABLE' ELSE facilities.state END),
                                  facilities.city,
                                  (CASE when (length(facilities.site_type)=0 OR (facilities.site_type) is null) THEN 'Others' ELSE facilities.site_type END),
                                  (CASE when (length(sponsors.phase)=0 OR (sponsors.phase) is null) THEN 'NOT AVAILABLE' ELSE sponsors.phase END),
                                  substr(facilities.facility_name,1,1),
                                  facilities.facility_name
                                  having count(distinct(sponsors.nct_id))>=2")

write.table(r_sponsor_site,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/r_sponsor_site.txt", sep=""), sep = "|", row.names = FALSE)


#get conditions data
#set paths for data files
in_path_x_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep="")

#reads the data files into dataframes
conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#clean the column by replacing commas and special
conditions <- conditions %>% mutate(condition_name = gsub("[',-/\\;]", "", condition_name))

#check a column to extract first word
conditions<- mutate(conditions,
                      StartWord=word(condition_name,1)
)

r_sponsor_conditions<- sqldf("select sponsors.lead_sponsor_name as 'Sponsor',
                              (CASE when (length(sponsors.phase)=0 OR (sponsors.phase) is null) THEN 'NOT AVAILABLE' ELSE sponsors.phase END) as 'studyPhase',
                              (CASE when length(sponsors.study_first_posted_year)=0 THEN 'NOT AVAILABLE' ELSE sponsors.study_first_posted_year END) as 'StudyYear',
                              conditions.StartWord as 'StartWord',
                              conditions.condition_name as 'Condition', 
                              count(distinct(sponsors.nct_id)) as cnt_studies
                                  from sponsors
                                  left join conditions
                                  on sponsors.nct_id=conditions.nct_id
                                  where length(conditions.condition_name)>1
                                  AND sponsors.study_first_posted_year>=2015
                                  group by sponsors.lead_sponsor_name, 
                                  (CASE when (length(sponsors.phase)=0 OR (sponsors.phase) is null) THEN 'NOT AVAILABLE' ELSE sponsors.phase END),
                                  (CASE when length(sponsors.study_first_posted_year)=0 THEN 'NOT AVAILABLE' ELSE sponsors.study_first_posted_year END),
                                  conditions.StartWord,
                                  conditions.condition_name
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
