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
agg_facilities <- agg_facilities %>% mutate(facility_name2 = gsub("[',.$&-/\\;()]", "", facility_name))
agg_facilities <- agg_facilities %>% mutate(facility_name2 = casefold(facility_name2))


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
                                subset = (study_first_posted_year>=var_last_year_10))



agg_facility_by_facilityname <- setDT(facilityStudy)[, list(
  countries=paste(unique(country), collapse =","),
  cnt_countries = length(unique(country)),
  cities=paste(unique(city), collapse =","),
  cnt_cities=length(unique(city)),
  sponsors=paste(unique(lead_sponsor_name), collapse =","),
  cnt_sponsors=length(unique(lead_sponsor_name)),
  conditions=paste(unique(condition_name), collapse =","),
  cnt_studies = length(unique(nct_id)),
  cnt_recruiting = sum(ifelse(status == "Recruiting", 1, 0), na.rm = TRUE)
), by = 'facility_name2']

#write to txt file
#write.table(agg_facility_by_facilityname, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facility_by_facilityname.txt", sep=""), sep = "|", row.names = FALSE)
saveRDS(agg_facility_by_facilityname, paste0(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facility_by_facilityname.rds"))

