#import libraries
library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

#set paths for data files
in_path_x_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facilities.txt", sep="")

#reads the data files into dataframes
facilities<-read.csv(in_path_x_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#agg facilities at study level
agg_facilities_study<-data.table(facilities)[,list( 
  country_name  = paste(unique(country), collapse =", "),
  state_name  = paste(state, collapse =", "),
  city_name  = paste(city, collapse =", "),
  cnt_sites = length(facility_id),
  cnt_site_recruiting = sum(as.integer(flag_site_recruiting), na.rm = TRUE),
  cnt_sites_US = sum(as.integer(flag_site_US), na.rm = TRUE),
  cnt_sites_Hosp = sum(ifelse(site_type == "Hospital", 1L, 0L), na.rm = TRUE),
  cnt_sites_Acad = sum(ifelse(site_type == "Academic", 1L, 0L), na.rm = TRUE),
  cnt_countries = length(unique(country))
), by='nct_id']

#second iteration

agg_facilities_study<-sqldf("select nct_id,
                            country_name,
                            state_name,
                            city_name,
                            cnt_sites,
                            cnt_site_recruiting,
                            cnt_sites_US,
                            cnt_sites_Hosp,
                            cnt_sites_Acad,
                            (CASE when cnt_sites_US>0 THEN 1 ELSE 0 END) as 'flag_has_US_site',
                            cnt_countries,
                            (cnt_countries-(CASE when cnt_sites_US>0 THEN 1 ELSE 0 END)) as 'cnt_countries_nonUS',
                            (CASE when (cnt_countries=1 AND cnt_sites_US>0) THEN 1 ELSE 0 END) as 'flag_USonly'
                             from agg_facilities_study
                             group by nct_id")


#write to txt file
write.table(agg_facilities_study, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities_study.txt", sep=""), sep = "|", row.names = FALSE)

#agg facilities at site level
agg_facilities_site1<-data.table(facilities)[,list( 
  cnt_studies = length(nct_id),
  cnt_site_recruiting = sum(as.integer(flag_site_recruiting), na.rm = TRUE),
  flag_study_us_only = if_else(sum(as.integer(flag_site_US), na.rm = TRUE)>=1,1,0)
), by='facility_name']

agg_facilities_site2<-sqldf("select facility_name, count(distinct country) as cnt_site_countries,
                             count(distinct city) as cnt_site_cities
                             from facilities
                             group by facility_name")

agg_facilities_site<-left_join(agg_facilities_site1, agg_facilities_site1, by="facility_name")

#write to txt file
write.table(agg_facilities_site, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities_site.txt", sep=""), sep = "|", row.names = FALSE)

