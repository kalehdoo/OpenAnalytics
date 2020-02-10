#import libraries
library(dplyr)
library(sqldf)

#set paths for data files
in_path_agg_studies<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_studies.txt", sep="")
in_path_agg_facilities<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_facilities.txt", sep="")

#reads the data files into dataframes
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
agg_facilities<-read.csv(in_path_agg_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#create new df with required columns and filter condition
mv_studies_recruiting1<-subset.data.frame(agg_studies, 
                                   subset=overall_status=="Recruiting" ,
                                   select=c(
                                     "nct_id",
                                     "start_date",
                                     "official_title",
                                     "phase",
                                     "enrollment",
                                     "has_dmc",
                                     "lead_sponsor_name",
                                     "condition_name",
                                     "flag_rare_condition"
                                   )
                                   
)

#create new df with required columns and filter condition
mv_facilities_recruiting<-subset.data.frame(agg_facilities, 
                                            status=="Recruiting" |status=="Not yet recruiting",
                                         select=c(
                                           "nct_id",
                                           "facility_name",
                                           "status",
                                           "City"="city",
                                           "state",
                                           "country",
                                           "zip",
                                           "latitude",
                                           "longitude"
                                           )
                                         
)

mv_studies_recruiting1$nct_id<-as.character(mv_studies_recruiting1$nct_id)
mv_facilities_recruiting$nct_id<-as.character(mv_facilities_recruiting$nct_id)
mv_studies_recruiting<-left_join(mv_studies_recruiting1, mv_facilities_recruiting, by="nct_id")
mv_studies_recruiting$urlid<-paste0("<a href=\'","https://clinicaltrials.gov/ct2/show/",mv_studies_recruiting$nct_id, "\' target=\'_blank\'>",mv_studies_recruiting$nct_id,"</a>")

mv_studies_recruiting_s<-sqldf("Select urlid as 'ID',
                                 official_title as 'Title', 
                                 start_date as 'StartDate',
                                 phase as 'StudyPhase',
                                 enrollment as 'Enrollment',
                                 case when has_dmc='t' THEN 'Yes' when has_dmc='f' THEN 'No' ELSE 'NA' END as 'DataMonitoring',
                                 lead_sponsor_name as 'Sponsor',
                                 condition_name as 'Condition',
                                 case when flag_rare_condition=1 THEN 'Yes' when flag_rare_condition=0 THEN 'No' ELSE 'NA' END as 'RareDisease',
                                 facility_name as 'Facility',
                                 status as 'RecruitingStatus',
                                 upper(city) as 'City',
                                 upper(state) as 'State',
                                 upper(country) as 'Country',
                                 zip as 'ZipCode'
                              from mv_studies_recruiting")


#write to txt file
write.table(mv_studies_recruiting,paste(var_DIR_ACCT_HOME, "DATA/extracts/mv_studies_recruiting.txt", sep=""), sep = "|", row.names = FALSE)

write.table(mv_studies_recruiting_s,paste(var_DIR_ACCT_HOME, "DATA/extracts/mv_studies_recruiting_s.txt", sep=""), sep = "|", row.names = FALSE)
