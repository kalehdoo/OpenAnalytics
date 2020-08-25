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

agg_facilityStudy <- setDT(facilityStudy)[, list(
  countries=paste(unique(country, na.rm=TRUE), collapse =","),
  cnt_countries=length(unique(country, na.rm=TRUE)),
  states=paste(unique(ifelse(state=="","na",state), na.rm=TRUE), collapse =","),
  cnt_states=length(unique(state, na.rm=TRUE)),
  cities=paste(unique(city, na.rm=TRUE), collapse =","),
  cnt_cities=length(unique(city, na.rm=TRUE)),
  cnt_studies_registered = length(unique(nct_id))
), by = c('facility_name')]
