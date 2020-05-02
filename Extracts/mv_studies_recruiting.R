  #import libraries
  library(dplyr)
  library(sqldf)
  library(data.table)
  library(stringr)

  #HOME to store path of home dir
  assign("var_DIR_HOME", "C:/Users/ranamanohar/Documents/GitHub/OpenAnalytics/", envir = .GlobalEnv)
  
  
  #set paths for data files
  in_path_agg_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies.txt", sep="")
  in_path_agg_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt", sep="")

#reads the data files into dataframes
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
agg_facilities<-read.csv(in_path_agg_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)

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
                                     "flag_rare_condition"
                                   )
                                   
)


#create new df with required columns and filter condition
mv_facilities_recruiting<-subset.data.frame(agg_facilities, 
                                            status=="Recruiting",
                                         select=c(
                                           "nct_id",
                                           "facility_name",
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
mv_studies_recruiting<-left_join(mv_studies_recruiting, mv_facilities_recruiting, by="nct_id")

mv_studies_recruiting<-sqldf("Select urlid as 'ID',
                                nct_id,
                                condition_name as 'Condition',
                                 official_title as 'Title', 
                                 start_date as 'StartDate',
                                 case when has_dmc='t' THEN 'Yes' when has_dmc='f' THEN 'No' ELSE 'NA' END as 'DataMonitoring',
                                 case when flag_rare_condition=1 THEN 'Yes' when flag_rare_condition=0 THEN 'No' ELSE 'NA' END as 'RareDisease',
                                 city,
                                 state,
                                 country,
                                 zip as 'ZipCode',
                                 phase as 'StudyPhase',
                                 lead_sponsor_name as 'Sponsor',
                                 facility_name as 'Facility',
                                 region as 'Region',
                                 iso2,
                                 iso3,
                                 statecode,
                                 latitude,
                                 longitude
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


write.table(mv_studies_recruiting_loc,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_studies_recruiting_loc.txt", sep=""), sep = "|", row.names = FALSE)


rm(list = c("mv_studies_recruiting_loc","mv_studies_recruiting","mv_facilities_recruiting","agg_studies","agg_facilities"))
