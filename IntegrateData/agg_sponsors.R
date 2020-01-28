#import libraries
library(dplyr)
library(stringr)
library(lubridate)

#set paths for data files
in_path_agg_studies<-"C:/ACCT/DATA/warehouse/agg_studies.txt"

#reads the data files into dataframes
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#create agg at sponsor level
#create parameters
var_current_year<-year(today())
var_last_year<-(var_current_year-1)
var_last_year_2<-(var_current_year-2)
var_last_year_3<-(var_current_year-3)
var_last_year_4<-(var_current_year-4)
var_last_year_5<-(var_current_year-5)

agg_studies$flag_recruiting_status<-as.integer(agg_studies$flag_recruiting_status)
agg_studies$flag_results_posted<-as.integer(agg_studies$flag_results_posted)
agg_studies$flag_completed_status<-as.integer(agg_studies$flag_completed_status)
agg_studies$flag_actual_started<-as.integer(agg_studies$flag_actual_started)
agg_studies$flag_has_dmc<-as.integer(agg_studies$flag_has_dmc)

#create sponsor agg table with all attributes from lead sponsor
agg_sponsors<-agg_studies %>%
  group_by(lead_sponsor_id) %>%
  summarise(cnt_studies_registered=n(),##counts number of records
            
            cnt_started_actual=sum(flag_actual_started),
            cnt_recruiting_status=sum(flag_recruiting_status),
            cnt_results_submitted=sum(flag_results_posted),
            cnt_completed_status=sum(flag_completed_status),
            ratio_results_to_completed=100*(sum(flag_results_posted)/sum(flag_completed_status)),
            
            cnt_has_dmc=sum(flag_has_dmc),
            cnt_interventional=sum(ifelse(study_type=="Interventional",1,0)),
            cnt_observational=sum(ifelse(study_type=="Observational",1,0)),
            cnt_phase3=sum(ifelse(phase=="Phase 3",1,0)),
            cnt_phase4=sum(ifelse(phase=="Phase 4",1,0)),
            cnt_actual_study_enrollment=sum(actual_study_enrollment),
            cnt_anticipated_study_enrollment=sum(anticipated_study_enrollment),
            
            cnt_study_registered_lstyr=sum(ifelse(study_first_posted_year==var_last_year[1],1,0)),
            cnt_started_actual_lstyr=sum(ifelse(start_year==var_last_year[1],flag_actual_started,0)),
            cnt_completed_status_lstyr=sum(ifelse(completed_year==var_last_year[1],flag_completed_status,0)),
            cnt_results_submitted_lstyr=sum(ifelse(results_first_posted_year==var_last_year[1],flag_results_posted,0)),
            ratio_results_to_completed_lstyr=100*(sum(cnt_results_submitted_lstyr)/sum(cnt_completed_status_lstyr)),
            
            cnt_study_registered_lstyr2=sum(ifelse(study_first_posted_year==var_last_year_2[1],1,0)),
            cnt_started_actual_lstyr2=sum(ifelse(start_year==var_last_year_2[1],flag_actual_started,0)),
            cnt_completed_status_lstyr2=sum(ifelse(completed_year==var_last_year_2[1],flag_completed_status,0)),
            cnt_results_submitted_lstyr2=sum(ifelse(results_first_posted_year==var_last_year_2[1],flag_results_posted,0)),
            ratio_results_to_completed_lstyr2=100*(sum(cnt_results_submitted_lstyr2)/sum(cnt_completed_status_lstyr2)),
            
            cnt_study_registered_lstyr3=sum(ifelse(study_first_posted_year==var_last_year_3[1],1,0)),
            cnt_started_actual_lstyr3=sum(ifelse(start_year==var_last_year_3[1],flag_actual_started,0)),
            cnt_completed_status_lstyr3=sum(ifelse(completed_year==var_last_year_3[1],flag_completed_status,0)),
            cnt_results_submitted_lstyr3=sum(ifelse(results_first_posted_year==var_last_year_3[1],flag_results_posted,0)),
            ratio_results_to_completed_lstyr3=100*(sum(cnt_results_submitted_lstyr3)/sum(cnt_completed_status_lstyr3)),
            
            cnt_study_registered_lstyr4=sum(ifelse(study_first_posted_year==var_last_year_4[1],1,0)),
            cnt_started_actual_lstyr4=sum(ifelse(start_year==var_last_year_4[1],flag_actual_started,0)),
            cnt_completed_status_lstyr4=sum(ifelse(completed_year==var_last_year_4[1],flag_completed_status,0)),
            cnt_results_submitted_lstyr4=sum(ifelse(results_first_posted_year==var_last_year_4[1],flag_results_posted,0)),
            ratio_results_to_completed_lstyr4=100*(sum(cnt_results_submitted_lstyr4)/sum(cnt_completed_status_lstyr4)),
            
            cnt_study_registered_lstyr5=sum(ifelse(study_first_posted_year==var_last_year_5[1],1,0)),
            cnt_started_actual_lstyr5=sum(ifelse(start_year==var_last_year_5[1],flag_actual_started,0)),
            cnt_completed_status_lstyr5=sum(ifelse(completed_year==var_last_year_5[1],flag_completed_status,0)),
            cnt_results_submitted_lstyr5=sum(ifelse(results_first_posted_year==var_last_year_5[1],flag_results_posted,0)),
            ratio_results_to_completed_lstyr5=100*(sum(cnt_results_submitted_lstyr5)/sum(cnt_completed_status_lstyr5)),
            
            ratio_results_to_completed_lstyr_change=sum(ratio_results_to_completed_lstyr)-sum(ratio_results_to_completed_lstyr2),
            ratio_results_to_completed_lstyr2_change=sum(ratio_results_to_completed_lstyr2)-sum(ratio_results_to_completed_lstyr3),
            ratio_results_to_completed_lstyr3_change=sum(ratio_results_to_completed_lstyr3)-sum(ratio_results_to_completed_lstyr4),
            ratio_results_to_completed_lstyr4_change=sum(ratio_results_to_completed_lstyr4)-sum(ratio_results_to_completed_lstyr5),
            
            
            cnt_completed_status_lstyr_change=sum(cnt_completed_status_lstyr)-sum(cnt_completed_status_lstyr2),
            cnt_completed_status_lstyr2_change=sum(cnt_completed_status_lstyr2)-sum(cnt_completed_status_lstyr3),
            cnt_completed_status_lstyr3_change=sum(cnt_completed_status_lstyr3)-sum(cnt_completed_status_lstyr4),
            cnt_completed_status_lstyr4_change=sum(cnt_completed_status_lstyr4)-sum(cnt_completed_status_lstyr5),
            
            cnt_completed_status_lstyr_chgpc=100*(sum(cnt_completed_status_lstyr_change)/sum(cnt_completed_status_lstyr2)),
            cnt_completed_status_lstyr2_chgpc=100*(sum(cnt_completed_status_lstyr2_change)/sum(cnt_completed_status_lstyr3)),
            cnt_completed_status_lstyr3_chgpc=100*(sum(cnt_completed_status_lstyr3_change)/sum(cnt_completed_status_lstyr4)),
            cnt_completed_status_lstyr4_chgpc=100*(sum(cnt_completed_status_lstyr4_change)/sum(cnt_completed_status_lstyr5)),
            
            cnt_results_submitted_lstyr_change=sum(cnt_results_submitted_lstyr)-sum(cnt_results_submitted_lstyr2),
            cnt_results_submitted_lstyr2_change=sum(cnt_results_submitted_lstyr2)-sum(cnt_results_submitted_lstyr3),
            cnt_results_submitted_lstyr3_change=sum(cnt_results_submitted_lstyr3)-sum(cnt_results_submitted_lstyr4),
            cnt_results_submitted_lstyr4_change=sum(cnt_results_submitted_lstyr4)-sum(cnt_results_submitted_lstyr5),
            
            cnt_results_submitted_lstyr_chgpc=100*(sum(cnt_results_submitted_lstyr_change)/sum(cnt_results_submitted_lstyr2)),
            cnt_results_submitted_lstyr2_chgpc=100*(sum(cnt_results_submitted_lstyr2_change)/sum(cnt_results_submitted_lstyr3)),
            cnt_results_submitted_lstyr3_chgpc=100*(sum(cnt_results_submitted_lstyr3_change)/sum(cnt_results_submitted_lstyr4)),
            cnt_results_submitted_lstyr4_chgpc=100*(sum(cnt_results_submitted_lstyr4_change)/sum(cnt_results_submitted_lstyr5))
            
  )

#left join agg_sponsors to lead_sponsors to get remaining attributes - has 1-1 relationship
agg_studies<-left_join(x_lead_sponsors,agg_studies,by="lead_sponsor_id")

#write to txt file
write.table(agg_sponsors,"C:/ACCT/DATA/warehouse/agg_sponsors.txt", sep = "|", row.names = FALSE)
