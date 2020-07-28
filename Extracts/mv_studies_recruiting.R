  #import libraries
  library(dplyr)
  library(sqldf)
  library(data.table)
  library(stringr)

  #HOME to store path of home dir
  assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)
  
  
  #set paths for data files
  in_path_agg_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies.txt", sep="")
  in_path_agg_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt", sep="")
  in_path_facility_contacts<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facility_contacts.txt", sep="")

#reads the data files into dataframes
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
agg_facilities<-read.csv(in_path_agg_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
facility_contacts<-read.csv(in_path_facility_contacts, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)


agg_studies$official_title<-str_remove_all(agg_studies$official_title, "[',|]")
agg_studies$condition_name<-str_remove_all(agg_studies$condition_name, "['|]")


#create new df with required columns and filter condition
mv_studies_recruiting<-subset.data.frame(agg_studies, 
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
                                     "cnt_match_rare_condition",
                                     "study_type",
                                     "study_first_posted_yrmonth",
                                     "start_yrmonth",
                                     "register_to_start_days",
                                     "agency_class",
                                     "sponsor_type",
                                     "cnt_conditions",
                                     "cnt_sites",
                                     "intervention_type"
                                   )
                                   
)


#facilities recruiting with required columns and filter condition
mv_facilities_recruiting<-subset.data.frame(agg_facilities, 
                                            status=="Recruiting",
                                         select=c(
                                           "nct_id",
                                           "facility_name",
                                           "facility_id",
                                           "status",
                                           "city",
                                           "state",
                                           "country",
                                           "region",
                                           "iso2",
                                           "iso3",
                                           "statecode",
                                           "zip",
                                           "latitude",
                                           "longitude"
                                           )
                                         
)


mv_studies_recruiting$nct_id<-as.character(mv_studies_recruiting$nct_id)
mv_studies_recruiting$urlid<-paste0("<a href=\'","https://clinicaltrials.gov/ct2/show/",mv_studies_recruiting$nct_id, "\' target=\'_blank\'>",mv_studies_recruiting$nct_id,"</a>")

mv_facilities_recruiting$nct_id<-as.character(mv_facilities_recruiting$nct_id)
#join recruiting facilities to recruiting studies
mv_studies_recruiting<-left_join(mv_facilities_recruiting,mv_studies_recruiting, by="nct_id")

mv_studies_recruiting<-sqldf("Select urlid as 'ID',
                                nct_id,
                                condition_name as 'Condition',
                                 official_title as 'Title', 
                                 start_date as 'StartDate',
                                 case when has_dmc='t' THEN 'Yes' when has_dmc='f' THEN 'No' ELSE 'NA' END as 'DataMonitoring',
                                 case when cnt_match_rare_condition>0 THEN 'Yes' when cnt_match_rare_condition=0 THEN 'No' ELSE 'NA' END as 'RareDisease',
                                 city,
                                 state,
                                 country,
                                 zip as 'ZipCode',
                                 phase as 'StudyPhase',
                                 lead_sponsor_name as 'Sponsor',
                                 facility_name as 'Facility',
                                 facility_id,
                                 region as 'Region',
                                 iso2,
                                 iso3,
                                 statecode,
                                 latitude,
                                 longitude,
                                 study_type as 'StudyType',
                                 study_first_posted_yrmonth as 'PostedYrMon',
                                 start_yrmonth as 'StartYrMon',
                                  register_to_start_days as 'RegToStartDays',
                                  agency_class as 'AgencyClass',
                                  sponsor_type as 'SponsorType',
                                  cnt_conditions as 'CntConditions',
                                  cnt_sites as 'CntSites',
                                  intervention_type as 'InterventionType',
                                  cnt_match_rare_condition
                              from mv_studies_recruiting
                             where nct_id is not null")

#create mini data set with important columns
mv_studies_recruiting_mini<-sqldf("Select ID,
                                nct_id,
                                Condition,
                                 city,
                                 state,
                                 country,
                                 ZipCode,
                                 case when length(StudyPhase)=0 THEN 'NULL VALUE' ELSE StudyPhase END as 'StudyPhase',
                                 Sponsor,
                                 Facility,
                                 Region,
                                 latitude,
                                 longitude,
                                 StudyType
                              from mv_studies_recruiting
                             where nct_id is not null")

#recruitment by location
#agg conditions at condition level
mv_studies_recruiting_loc<-data.table(mv_studies_recruiting)[,list(
  latitude=median(as.double(latitude)),
  longitude=median(as.double(longitude)),
  cnt_studies = length(unique(nct_id))
), by=c('iso2','iso3', 'state','statecode','city')]


#write to txt file
write.table(mv_studies_recruiting,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_studies_recruiting.txt", sep=""), sep = "|", row.names = FALSE)
saveRDS(mv_studies_recruiting, paste0(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_studies_recruiting.rds"))
#write_feather(mv_studies_recruiting, paste0(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_studies_recruiting.feather"))


write.table(mv_studies_recruiting_mini,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_studies_recruiting_mini.txt", sep=""), sep = "|", row.names = FALSE)
saveRDS(mv_studies_recruiting_mini, paste0(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_studies_recruiting_mini.rds"))


write.table(mv_studies_recruiting_loc,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_studies_recruiting_loc.txt", sep=""), sep = "|", row.names = FALSE)

#write to mongodb
#library(mongolite)
#create connection to mongodb cloud
#conn_mongo_mv_recruiting_studies <- mongo(collection = "mv_studies_recruiting", 
#                      db="openanalytics",
#                      url = "mongodb+srv://kalehdoo_user:Aquano139182@kalehdoo-gx7df.mongodb.net/test"
#)


#conn_mongo_mv_recruiting_studies$drop()
#conn_mongo_mv_recruiting_studies$insert(mv_studies_recruiting)

#facilities recruiting
mv_studies_recr_facility<-sqldf("Select 
                                nct_id,
                                 Sponsor,
                                 facility_id,
                                 Facility,
                                 country
                              from mv_studies_recruiting
                             where nct_id is not null")

#join recruiting facilities to recruiting studies
mv_rec_facility_contacts<-left_join(mv_studies_recr_facility, facility_contacts, by=c("nct_id","facility_id"))

mv_rec_facility_contacts<-sqldf("select
                                nct_id,
                                 Sponsor,
                                 facility_id,
                                 Facility,
                                 country,
                                 contact_type,
                                 name,
                                 email
                                from mv_rec_facility_contacts
                                where length(email) >=1")

write.table(mv_rec_facility_contacts,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_rec_facility_contacts.csv", sep=""), sep = ",", row.names = FALSE)

rm(list = c("mv_studies_recr_facility","mv_rec_facility_contacts","mv_studies_recruiting_loc","mv_studies_recruiting","mv_facilities_recruiting","agg_studies","agg_facilities"))


