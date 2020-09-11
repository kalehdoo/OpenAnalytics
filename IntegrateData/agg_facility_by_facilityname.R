#import libraries
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(sqldf)


#set paths for data files
in_path_agg_facilities <-
  paste(var_DIR_HOME,
        "Data/ACCT/DATA/warehouse/agg_facilities.txt",
        sep = "")

#reads the data files into dataframes
agg_facilities <-
  read.csv(
    in_path_agg_facilities,
    header = TRUE,
    na.strings = "NA",
    sep = "|",
    nrows = -100
  )

#clean the column by replacing commas and special
#agg_facilities <- agg_facilities %>% mutate(facility_name2 = gsub("[',.$&-/\\;()]", "", facility_name))
#agg_facilities <- agg_facilities %>% mutate(facility_name2 = casefold(facility_name2))



#set paths for data files
in_path_agg_studies <-
  paste(var_DIR_HOME,
        "Data/ACCT/DATA/warehouse/agg_studies.txt",
        sep = "")

#reads the data files into dataframes
agg_studies <-
  read.csv(
    in_path_agg_studies,
    header = TRUE,
    na.strings = "NA",
    sep = "|",
    nrows = -100
  )


facilityStudy<-left_join(agg_facilities, agg_studies, by=c("nct_id"))


facilityStudy<-subset.data.frame(facilityStudy,
                                 select = c("facility_name","city", "state", "country", "zip","nct_id", "overall_status", "study_type", "phase", 
                                           "enrollment", "enrollment_type",
                                           "study_first_posted_year", "sponsor_type"),
                                 subset = (study_first_posted_year>=var_last_year_15)
                                )


agg_facility_by_facilityname<- fn$sqldf("select distinct facility_name,
                    country, state, city,
                    count(distinct zip) as site_locations,
                    count(distinct nct_id) as studies,
                    min(study_first_posted_year) as first_study_year,
                    max(study_first_posted_year) as recent_study_year,
                    sum(case when phase='Phase 4' then 1 else 0 end) as cnt_phase4,
                    sum(case when phase='Phase 3' then 1 else 0 end) as cnt_phase3,
                    sum(case when sponsor_type='Academic' then 1 else 0 end) as Academic_sponsors,
                    sum(case when sponsor_type='Hospital' then 1 else 0 end) as Hospital_sponsors,
                    sum(case when sponsor_type='Industry' then 1 else 0 end) as Industry_sponsors,
                    sum(case when enrollment_type='Actual' then enrollment else 0 end) as Actual_Enrollment
                   from facilityStudy
                   where length(facility_name) >4
                   and facility_name NOT IN ('1 site','(Investigator site)')
                   group by facility_name,
                    country, state, city")


#agg_facility_by_facilityname <- setDT(facilityStudy)[, list(
#  conditions=paste(unique(condition_name), collapse =",")
#), by = 'facility_name2']

#write to txt file
write.table(agg_facility_by_facilityname, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facility_by_facilityname.txt", sep=""), sep = "|", row.names = FALSE)
saveRDS(agg_facility_by_facilityname, paste0(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facility_by_facilityname.rds"))

