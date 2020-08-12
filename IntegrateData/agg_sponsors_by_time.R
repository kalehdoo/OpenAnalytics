#import libraries
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(sqldf)

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

agg_sponsors_registered <- setDT(agg_studies)[, list(
  agency_class=unique(agency_class),
  sponsor_type=unique(sponsor_type),
  cnt_studies_registered = length(unique(nct_id))
), by = c('lead_sponsor_name','study_first_posted_year')]

agg_sponsors_registered<-subset.data.frame(agg_sponsors_registered,
                                       subset = (is.na(study_first_posted_year)==FALSE
                                                 & study_first_posted_year>=var_last_year_15
                                                 & study_first_posted_year<=var_current_year))

colnames(agg_sponsors_registered)[colnames(agg_sponsors_registered)=="study_first_posted_year"]<-"common_year"


agg_sponsors_started <- setDT(agg_studies)[, list(
  agency_class=unique(agency_class),
  sponsor_type=unique(sponsor_type),
  cnt_studies_started = sum(if_else(flag_actual_started==1 ,1L,0L))
), by = c('lead_sponsor_name','start_year')]

agg_sponsors_started<-subset.data.frame(agg_sponsors_started,
                                           subset = (is.na(start_year)==FALSE
                                                     & start_year>=var_last_year_15
                                                     & start_year<=var_current_year))

colnames(agg_sponsors_started)[colnames(agg_sponsors_started)=="start_year"]<-"common_year"


agg_sponsors_completed <- setDT(agg_studies)[, list(
  agency_class=unique(agency_class),
  sponsor_type=unique(sponsor_type),
  cnt_studies_completed = sum(if_else(flag_completed_status==1 ,1L,0L))
), by = c('lead_sponsor_name','completed_year')]

agg_sponsors_completed<-subset.data.frame(agg_sponsors_completed,
                                        subset = (is.na(completed_year)==FALSE
                                                  & completed_year>=var_last_year_15
                                                  & completed_year<=var_current_year
                                                  ))

colnames(agg_sponsors_completed)[colnames(agg_sponsors_completed)=="completed_year"]<-"common_year"


agg_sponsors_results <- setDT(agg_studies)[, list(
  agency_class=unique(agency_class),
  sponsor_type=unique(sponsor_type),
  cnt_studies_results_posted = sum(if_else(flag_results_posted==1 ,1L,0L))
), by = c('lead_sponsor_name','results_first_posted_year')]

agg_sponsors_results<-subset.data.frame(agg_sponsors_results,
                                          subset = (is.na(results_first_posted_year)==FALSE
                                                    & results_first_posted_year>=var_last_year_15
                                                    & results_first_posted_year<=var_current_year))

colnames(agg_sponsors_results)[colnames(agg_sponsors_results)=="results_first_posted_year"]<-"common_year"


agg_sponsors_by_time<- agg_sponsors_registered %>%
  full_join(agg_sponsors_started, by=c("lead_sponsor_name","agency_class","sponsor_type","common_year")) %>%
  full_join(agg_sponsors_completed, by=c("lead_sponsor_name","agency_class","sponsor_type","common_year")) %>%
  full_join(agg_sponsors_results, by=c("lead_sponsor_name","agency_class","sponsor_type","common_year"))
  
#write to txt file
write.table(
  agg_sponsors_by_time,
  paste(
    var_DIR_HOME,
    "Data/ACCT/DATA/warehouse/agg_sponsors_by_time.txt",
    sep = ""
  ),
  sep = "|",
  row.names = FALSE
)
