#import libraries
library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

#set paths for data files
in_path_agg_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt", sep="")

#reads the data files into dataframes
agg_facilities<-read.csv(in_path_agg_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#agg facilities at study level
agg_facilities_study1<-data.table(agg_facilities)[,list( 
  country_name  = paste(unique(country), collapse =", "),
  state_name  = paste(state, collapse =", "),
  city_name  = paste(city, collapse =", "),
  cnt_sites = length(facility_id),
  cnt_site_recruiting = sum(as.integer(flag_site_recruiting), na.rm = TRUE),
  flag_study_us_only = if_else(sum(as.integer(flag_site_US), na.rm = TRUE)>=1,1,0)
), by='nct_id']

agg_facilities_study2<-sqldf("select nct_id, count(distinct country) as cnt_site_countries,
                             count(distinct city) as cnt_site_cities
                             from agg_facilities
                             group by nct_id")

agg_facilities_study<-left_join(agg_facilities_study1, agg_facilities_study2, by="nct_id")


#write to txt file
write.table(agg_facilities_study, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities_study.txt", sep=""), sep = "|", row.names = FALSE)

#agg facilities at site level
agg_facilities_site1<-data.table(agg_facilities)[,list( 
  cnt_studies = length(nct_id),
  cnt_site_recruiting = sum(as.integer(flag_site_recruiting), na.rm = TRUE),
  flag_study_us_only = if_else(sum(as.integer(flag_site_US), na.rm = TRUE)>=1,1,0)
), by='facility_name']

agg_facilities_site2<-sqldf("select facility_name, count(distinct country) as cnt_site_countries,
                             count(distinct city) as cnt_site_cities
                             from agg_facilities
                             group by facility_name")

agg_facilities_site<-left_join(agg_facilities_site1, agg_facilities_site1, by="facility_name")

#write to txt file
write.table(agg_facilities_site, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities_site.txt", sep=""), sep = "|", row.names = FALSE)

