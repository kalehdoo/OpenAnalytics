library(dplyr)
library(lubridate)
library(stringr)

#set paths for data files
in_path_studies<-paste(var_DIR_ACCT_HOME, "DATA/unzipSrcFiles/studies.txt", sep="")

#reads the data files into dataframes
studies<-read.csv(in_path_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#create subset df with selected columns
studies_1<-subset.data.frame(studies, select =c("nct_id",
                                                "study_first_posted_date",
                                                "start_date",
                                                "start_date_type",
                                                "primary_completion_date",
                                                "primary_completion_date_type",
                                                "completion_date",
                                                "completion_date_type",
                                                "results_first_posted_date",
                                                "results_first_posted_date_type",
                                                "overall_status",
                                                "study_type",
                                                "brief_title",
                                                "official_title",
                                                "phase",
                                                "enrollment",
                                                "enrollment_type",
                                                "source",
                                                "number_of_arms",
                                                "has_dmc"
)
)
#head(studies_1,10)
#view the structure of df
#str(studies_1)
#create new columns
studies_1<-mutate(studies_1,
                  "study_first_posted_year"=year(study_first_posted_date),
                  "study_first_posted_month"=month(study_first_posted_date),
                  "study_first_posted_yrmonth"=str_sub(study_first_posted_date,1,7),
                  "flag_study_first_posted"=if_else(str_length(study_first_posted_date)>0,1,0),
                  "flag_recruiting_status"=if_else (overall_status=="Recruiting",1,0),
                  
                  "start_year"=str_sub(start_date,1,4),
                  "start_month"=str_sub(start_date,6,7),
                  "start_yrmonth"=str_sub(start_date,1,7),
                  "flag_actual_started"=if_else(start_date_type=="Actual",1,0),
                  
                  "completed_year"=str_sub(completion_date,1,4),
                  "completed_month"=str_sub(completion_date,6,7),
                  "completed_yrmonth"=str_sub(completion_date,1,7),
                  "flag_completed_status"=if_else (overall_status=="Completed",1,0),
                  
                  "results_first_posted_year"=str_sub(results_first_posted_date,1,4),
                  "results_first_posted_month"=str_sub(results_first_posted_date,6,7),
                  "results_first_posted_yrmonth"=str_sub(results_first_posted_date,1,7),
                  "flag_results_posted"=if_else(parse_date_time(as.factor(results_first_posted_date), orders = c("ymd", "dmy", "mdy"))<=as.Date(var_current_date),1,0),
                  
                  "actual_study_enrollment"=ifelse(enrollment_type=="Actual",enrollment,NA),
                  "anticipated_study_enrollment"=ifelse(enrollment_type=="Anticipated",enrollment,NA),
                  "flag_has_dmc"=case_when(has_dmc=="t" ~ 1,
                                           has_dmc=="f" ~ 0,
                                           TRUE ~ NA_real_),
                  "start_to_complete_days"= difftime(parse_date_time(as.factor(completion_date), orders = c("ymd", "dmy", "mdy")), 
                                                       parse_date_time(as.factor(start_date), orders = c("ymd", "dmy", "mdy")), units = "days"
                                                       ),
                  "complete_to_results_days"= difftime(parse_date_time(as.factor(results_first_posted_date), orders = c("ymd", "dmy", "mdy")), 
                                                       parse_date_time(as.factor(completion_date), orders = c("ymd", "dmy", "mdy")), units = "days"
                                                        )
                    
)

#write to txt file
write.table(studies_1, paste(var_DIR_ACCT_HOME, "DATA/warehouse/x_studies.txt", sep=""), sep = "|", row.names = FALSE)



