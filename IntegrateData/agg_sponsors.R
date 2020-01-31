#import libraries
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)

#set paths for data files
in_path_agg_studies<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_studies.txt", sep="")

#reads the data files into dataframes
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, na.strings = "NA", sep = "|", nrows = -100)


#create agg at sponsor level

#create sponsor agg table with all attributes from lead sponsor
#use data.table or setDT for faster aggregations
agg_sponsors1<-setDT(agg_studies)[,list(
  cnt_studies_registered=sum(flag_study_first_posted, na.rm = TRUE),
  cnt_started_actual=sum(flag_actual_started, na.rm = TRUE),
  cnt_recruiting_status=sum(flag_recruiting_status, na.rm = TRUE),
  cnt_results_submitted=sum(flag_results_posted, na.rm = TRUE),
  cnt_completed_status=sum(flag_completed_status, na.rm = TRUE),
  ratio_results_to_completed=100*(sum(flag_results_posted, na.rm = TRUE)/sum(flag_completed_status, na.rm = TRUE)),
  cnt_has_dmc=sum(flag_has_dmc, na.rm = TRUE),
  cnt_interventional=sum(ifelse(study_type=="Interventional",1,0), na.rm = TRUE),
  cnt_observational=sum(ifelse(study_type=="Observational",1,0), na.rm = TRUE),
  cnt_phase3=sum(ifelse(phase=="Phase 3",1,0), na.rm = TRUE),
  cnt_phase4=sum(ifelse(phase=="Phase 4",1,0), na.rm = TRUE),
  cnt_actual_study_enrollment=sum(actual_study_enrollment, na.rm = TRUE),
  cnt_anticipated_study_enrollment=sum(anticipated_study_enrollment, na.rm = TRUE),
  cnt_study_registered_lstyr=sum(ifelse(study_first_posted_year==var_last_year[1],1,0), na.rm = TRUE),
  cnt_started_actual_lstyr=sum(ifelse(start_year==var_last_year[1],flag_actual_started,0L), na.rm = TRUE),
  cnt_completed_status_lstyr=sum(ifelse(completed_year==var_last_year[1],flag_completed_status,0L), na.rm = TRUE),
  cnt_results_submitted_lstyr=sum(ifelse(results_first_posted_year==var_last_year[1],1L,0L), na.rm = TRUE),
  
  cnt_study_registered_lstyr2=sum(ifelse(study_first_posted_year==var_last_year_2[1],1,0), na.rm = TRUE),
  cnt_started_actual_lstyr2=sum(ifelse(start_year==var_last_year_2[1],flag_actual_started,0L), na.rm = TRUE),
  cnt_completed_status_lstyr2=sum(ifelse(completed_year==var_last_year_2[1],flag_completed_status,0L), na.rm = TRUE),
  cnt_results_submitted_lstyr2=sum(ifelse(results_first_posted_year==var_last_year_2[1],flag_results_posted,0L), na.rm = TRUE),
  cnt_study_registered_lstyr3=sum(ifelse(study_first_posted_year==var_last_year_3[1],1,0), na.rm = TRUE),
  cnt_started_actual_lstyr3=sum(ifelse(start_year==var_last_year_3[1],flag_actual_started,0L), na.rm = TRUE),
  cnt_completed_status_lstyr3=sum(ifelse(completed_year==var_last_year_3[1],flag_completed_status,0L), na.rm = TRUE),
  cnt_results_submitted_lstyr3=sum(ifelse(results_first_posted_year==var_last_year_3[1],flag_results_posted,0L), na.rm = TRUE),
  cnt_study_registered_lstyr4=sum(ifelse(study_first_posted_year==var_last_year_4[1],1,0), na.rm = TRUE),
  cnt_started_actual_lstyr4=sum(ifelse(start_year==var_last_year_4[1],flag_actual_started,0L), na.rm = TRUE),
  cnt_completed_status_lstyr4=sum(ifelse(completed_year==var_last_year_4[1],flag_completed_status,0L), na.rm = TRUE),
  cnt_results_submitted_lstyr4=sum(ifelse(results_first_posted_year==var_last_year_4[1],flag_results_posted,0L), na.rm = TRUE),
  cnt_study_registered_lstyr5=sum(ifelse(study_first_posted_year==var_last_year_5[1],1,0), na.rm = TRUE),
  cnt_started_actual_lstyr5=sum(ifelse(start_year==var_last_year_5[1],flag_actual_started,0L), na.rm = TRUE),
  cnt_completed_status_lstyr5=sum(ifelse(completed_year==var_last_year_5[1],flag_completed_status,0L), na.rm = TRUE),
  cnt_results_submitted_lstyr5=sum(ifelse(results_first_posted_year==var_last_year_5[1],flag_results_posted,0L), na.rm = TRUE),
  
  cnt_conditions=sum(cnt_conditions, na.rm = TRUE),
  cnt_rare_condition_match=sum(cnt_match_rare_condition, na.rm = TRUE),
  cnt_rare_condition_studies=sum(flag_rare_condition, na.rm = TRUE)
  
  ), by='lead_sponsor_name']



#create sponsor agg table with all attributes from lead sponsor
agg_sponsors2<-agg_sponsors1 %>%
  group_by(lead_sponsor_name) %>%
  summarise(
            ratio_results_to_completed_lstyr=100*((cnt_results_submitted_lstyr)/(cnt_completed_status_lstyr)),
            
            ratio_results_to_completed_lstyr2=100*(sum(cnt_results_submitted_lstyr2, na.rm = TRUE)/sum(cnt_completed_status_lstyr2, na.rm = TRUE)),
            
            ratio_results_to_completed_lstyr3=100*(sum(cnt_results_submitted_lstyr3, na.rm = TRUE)/sum(cnt_completed_status_lstyr3, na.rm = TRUE)),
            
            ratio_results_to_completed_lstyr4=100*(sum(cnt_results_submitted_lstyr4, na.rm = TRUE)/sum(cnt_completed_status_lstyr4, na.rm = TRUE)),
            
            ratio_results_to_completed_lstyr5=100*(sum(cnt_results_submitted_lstyr5, na.rm = TRUE)/sum(cnt_completed_status_lstyr5, na.rm = TRUE)),
            
            ratio_results_to_completed_lstyr_change=sum(ratio_results_to_completed_lstyr, na.rm = TRUE)-sum(ratio_results_to_completed_lstyr2, na.rm = TRUE),
            ratio_results_to_completed_lstyr2_change=sum(ratio_results_to_completed_lstyr2, na.rm = TRUE)-sum(ratio_results_to_completed_lstyr3, na.rm = TRUE),
            ratio_results_to_completed_lstyr3_change=sum(ratio_results_to_completed_lstyr3, na.rm = TRUE)-sum(ratio_results_to_completed_lstyr4, na.rm = TRUE),
            ratio_results_to_completed_lstyr4_change=sum(ratio_results_to_completed_lstyr4, na.rm = TRUE)-sum(ratio_results_to_completed_lstyr5, na.rm = TRUE),
            
            
            cnt_completed_status_lstyr_change=sum(cnt_completed_status_lstyr, na.rm = TRUE)-sum(cnt_completed_status_lstyr2, na.rm = TRUE),
            cnt_completed_status_lstyr2_change=sum(cnt_completed_status_lstyr2, na.rm = TRUE)-sum(cnt_completed_status_lstyr3, na.rm = TRUE),
            cnt_completed_status_lstyr3_change=sum(cnt_completed_status_lstyr3, na.rm = TRUE)-sum(cnt_completed_status_lstyr4, na.rm = TRUE),
            cnt_completed_status_lstyr4_change=sum(cnt_completed_status_lstyr4, na.rm = TRUE)-sum(cnt_completed_status_lstyr5, na.rm = TRUE),
            
            cnt_completed_status_lstyr_chgpc=100*(sum(cnt_completed_status_lstyr_change, na.rm = TRUE)/sum(cnt_completed_status_lstyr2, na.rm = TRUE)),
            cnt_completed_status_lstyr2_chgpc=100*(sum(cnt_completed_status_lstyr2_change, na.rm = TRUE)/sum(cnt_completed_status_lstyr3, na.rm = TRUE)),
            cnt_completed_status_lstyr3_chgpc=100*(sum(cnt_completed_status_lstyr3_change, na.rm = TRUE)/sum(cnt_completed_status_lstyr4, na.rm = TRUE)),
            cnt_completed_status_lstyr4_chgpc=100*(sum(cnt_completed_status_lstyr4_change, na.rm = TRUE)/sum(cnt_completed_status_lstyr5, na.rm = TRUE)),
            
            cnt_results_submitted_lstyr_change=sum(cnt_results_submitted_lstyr, na.rm = TRUE)-sum(cnt_results_submitted_lstyr2, na.rm = TRUE),
            cnt_results_submitted_lstyr2_change=sum(cnt_results_submitted_lstyr2, na.rm = TRUE)-sum(cnt_results_submitted_lstyr3, na.rm = TRUE),
            cnt_results_submitted_lstyr3_change=sum(cnt_results_submitted_lstyr3, na.rm = TRUE)-sum(cnt_results_submitted_lstyr4, na.rm = TRUE),
            cnt_results_submitted_lstyr4_change=sum(cnt_results_submitted_lstyr4, na.rm = TRUE)-sum(cnt_results_submitted_lstyr5, na.rm = TRUE),
            
            cnt_results_submitted_lstyr_chgpc=100*(sum(cnt_results_submitted_lstyr_change, na.rm = TRUE)/sum(cnt_results_submitted_lstyr2, na.rm = TRUE)),
            cnt_results_submitted_lstyr2_chgpc=100*(sum(cnt_results_submitted_lstyr2_change, na.rm = TRUE)/sum(cnt_results_submitted_lstyr3, na.rm = TRUE)),
            cnt_results_submitted_lstyr3_chgpc=100*(sum(cnt_results_submitted_lstyr3_change, na.rm = TRUE)/sum(cnt_results_submitted_lstyr4, na.rm = TRUE)),
            cnt_results_submitted_lstyr4_chgpc=100*(sum(cnt_results_submitted_lstyr4_change, na.rm = TRUE)/sum(cnt_results_submitted_lstyr5, na.rm = TRUE))
            
  )

agg_sponsors<-left_join(agg_sponsors1,agg_sponsors2,by="lead_sponsor_name")

#write to txt file
write.table(agg_sponsors, paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_sponsors.txt", sep=""), sep = "|", row.names = FALSE)
