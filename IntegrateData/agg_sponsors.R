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


#create agg at sponsor level

#create sponsor agg table with all attributes from lead sponsor
#use data.table or setDT for faster aggregations
agg_sponsors1 <- setDT(agg_studies)[, list(
  agency_class=unique(agency_class),
  sponsor_type=unique(sponsor_type),
  cnt_studies_registered = length(unique(nct_id)),
  year_first_study_reg = min(study_first_posted_year, na.rm = TRUE),
  year_last_study_reg = max(study_first_posted_year, na.rm = TRUE),
  cnt_US_only_studies = sum(flag_USonly, na.rm = TRUE),
  cnt_nonUS_only_studies = sum(ifelse((flag_has_US_site == 0 & cnt_countries_nonUS >= 1), 1L, 0L), na.rm = TRUE),
  cnt_global_studies = sum(ifelse((flag_has_US_site == 1 & cnt_countries_nonUS >= 1), 1L, 0L), na.rm = TRUE),
  cnt_studies_1country = sum(ifelse((cnt_countries == 1), 1L, 0L), na.rm = TRUE),
  cnt_studies_2to5country = sum(ifelse((cnt_countries >= 2 & cnt_countries <=5), 1L, 0L), na.rm = TRUE),
  cnt_studies_6to10country = sum(ifelse((cnt_countries >= 6 & cnt_countries <=10), 1L, 0L), na.rm = TRUE),
  cnt_studies_11to30country = sum(ifelse((cnt_countries >= 11 & cnt_countries <=30), 1L, 0L), na.rm = TRUE),
  cnt_studies_31to50country = sum(ifelse((cnt_countries >= 31 & cnt_countries <=50), 1L, 0L), na.rm = TRUE),
  cnt_studies_50pluscountry = sum(ifelse((cnt_countries >= 51), 1L, 0L), na.rm = TRUE),
  cnt_completed_status = sum(flag_completed_status, na.rm = TRUE),
  cnt_suspended = sum(ifelse(overall_status == "Suspended", 1, 0), na.rm = TRUE),
  cnt_started_actual = sum(flag_actual_started, na.rm = TRUE),
  cnt_recruiting_status = sum(flag_recruiting_status, na.rm = TRUE),
  cnt_results_submitted = sum(flag_results_posted, na.rm = TRUE),
  ratio_results_to_completed = 100 * (round(
    (sum(flag_results_posted, na.rm = TRUE) / sum(flag_completed_status, na.rm = TRUE)),
  digits=2)),
  cnt_has_dmc = sum(flag_has_dmc, na.rm = TRUE),
  cnt_interventional = sum(ifelse(study_type == "Interventional", 1, 0), na.rm = TRUE),
  cnt_observational = sum(ifelse(study_type == "Observational", 1, 0), na.rm = TRUE),
  cnt_phase1 = sum(ifelse(phase == "Phase 1", 1, 0), na.rm = TRUE),
  cnt_phase2 = sum(ifelse(phase == "Phase 2", 1, 0), na.rm = TRUE),
  cnt_phase3 = sum(ifelse(phase == "Phase 3", 1, 0), na.rm = TRUE),
  cnt_phase4 = sum(ifelse(phase == "Phase 4", 1, 0), na.rm = TRUE),
  cnt_actual_study_enrollment = sum(actual_study_enrollment, na.rm = TRUE),
  cnt_anticipated_study_enrollment = sum(anticipated_study_enrollment, na.rm = TRUE),
  cnt_study_registered_curryr = sum(ifelse(
    study_first_posted_year == var_current_year[1], 1, 0
  ), na.rm = TRUE),
  cnt_started_actual_curryr = sum(
    ifelse(start_year == var_current_year[1], flag_actual_started, 0L),
    na.rm = TRUE
  ),
  cnt_study_registered_lstyr = sum(ifelse(
    study_first_posted_year == var_last_year[1], 1, 0
  ), na.rm = TRUE),
  cnt_started_actual_lstyr = sum(
    ifelse(start_year == var_last_year[1], flag_actual_started, 0L),
    na.rm = TRUE
  ),
  cnt_completed_status_lstyr = sum(
    ifelse(completed_year == var_last_year[1], flag_completed_status, 0L),
    na.rm = TRUE
  ),
  cnt_results_submitted_lstyr = sum(
    ifelse(results_first_posted_year == var_last_year[1], 1L, 0L),
    na.rm = TRUE
  ),
  cnt_conditions = sum(cnt_conditions, na.rm = TRUE),
  cnt_rare_condition_match = sum(cnt_match_rare_condition, na.rm = TRUE),
  cnt_rare_condition_studies = sum(if_else(cnt_match_rare_condition>0,1,0), na.rm = TRUE),
  avg_register_to_start_days_ph1 = round(mean(case_when(is.na(phase)==FALSE & phase == "Phase 1" & flag_actual_started==1 ~ register_to_start_days), na.rm = TRUE)),
  avg_register_to_start_days_ph2 = round(mean(case_when(is.na(phase)==FALSE & phase == "Phase 2" & flag_actual_started==1 ~ register_to_start_days), na.rm = TRUE)),
  avg_register_to_start_days_ph3 = round(mean(case_when(is.na(phase)==FALSE & phase == "Phase 3" & flag_actual_started==1 ~ register_to_start_days), na.rm = TRUE)),
  avg_register_to_start_days_ph4 = round(mean(case_when(is.na(phase)==FALSE & phase == "Phase 4" & flag_actual_started==1 ~ register_to_start_days), na.rm = TRUE)),
  avg_start_to_complete_days_ph1 = round(mean(case_when(is.na(phase)==FALSE & phase == "Phase 1" & flag_completed_status==1 ~ start_to_complete_days), na.rm = TRUE)),
  avg_start_to_complete_days_ph2 = round(mean(case_when(is.na(phase)==FALSE & phase == "Phase 2" & flag_completed_status==1 ~ start_to_complete_days), na.rm = TRUE)),
  avg_start_to_complete_days_ph3 = round(mean(case_when(is.na(phase)==FALSE & phase == "Phase 3" & flag_completed_status==1 ~ start_to_complete_days), na.rm = TRUE)),
  avg_start_to_complete_days_ph4 = round(mean(case_when(is.na(phase)==FALSE & phase == "Phase 4" & flag_completed_status==1 ~ start_to_complete_days), na.rm = TRUE))
  
), by = 'lead_sponsor_name']


agg_sponsors<-mutate(agg_sponsors1,
                     sponsor_size=case_when(cnt_studies_registered>=0 &cnt_studies_registered<=10 ~ "1_XXXS_0T10",
                                            cnt_studies_registered>=11 &cnt_studies_registered<=50 ~ "2_XXS_11T50",
                                            cnt_studies_registered>=51 &cnt_studies_registered<=100 ~ "3_XS_51T100",
                                            cnt_studies_registered>=101 &cnt_studies_registered<=200 ~ "4_S_101T200",
                                            cnt_studies_registered>=201 &cnt_studies_registered<=500 ~ "5_M_201T500",
                                            cnt_studies_registered>=501 &cnt_studies_registered<=1000 ~ "6_L_501T1K",
                                            cnt_studies_registered>=1001 &cnt_studies_registered<=2000 ~ "7_XL_1KT2K",
                                            cnt_studies_registered>=2001 ~ "8_XXL_over_2K")
                     )

#add collaborator per sponsors
#set paths for data files
in_path_x_sponsors<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_lead_sponsors.txt", sep="")

#reads the data files into dataframes
sponsors<-read.csv(in_path_x_sponsors, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

in_path_x_collaborators<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_collaborators.txt", sep="")

#reads the data files into dataframes
collaborators<-read.csv(in_path_x_collaborators, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)


sponsor_collaborators<- sqldf("select sponsors.lead_sponsor_name as 'lead_sponsor_name',
                                        count(distinct(collaborators.collaborator_name)) as cnt_collaborators,
                                        count(distinct(CASE when collaborators.agency_class='NIH' THEN collaborators.collaborator_name END)) as 'cnt_colab_NIH',
                                        count(distinct(CASE when collaborators.sponsor_type!='Industry' THEN collaborators.collaborator_name END)) as 'cnt_colab_nonind',
                                        count(distinct(CASE when collaborators.agency_class='U.S. Fed' THEN collaborators.collaborator_name END)) as 'cnt_colab_USFed',
                                        count(distinct(CASE when collaborators.agency_class='Industry' THEN collaborators.collaborator_name END)) as 'cnt_colab_Ind',
                                        count(distinct(CASE when collaborators.sponsor_type='Hospital' THEN collaborators.collaborator_name END)) as 'cnt_colab_Hosp',
                                        count(distinct(CASE when collaborators.sponsor_type='Academic' THEN collaborators.collaborator_name END)) as 'cnt_colab_Acad'
                                  from sponsors
                                  left join collaborators
                                  on sponsors.nct_id=collaborators.nct_id
                               group by sponsors.lead_sponsor_name")

#stich collaborators with agg studies
agg_sponsors<-left_join(agg_sponsors, sponsor_collaborators, by=c("lead_sponsor_name"))

agg_sponsors<-subset.data.frame(agg_sponsors,
                                subset = (cnt_studies_registered>1))

#write to txt file
write.table(
  agg_sponsors,
  paste(
    var_DIR_HOME,
    "Data/ACCT/DATA/warehouse/agg_sponsors.txt",
    sep = ""
  ),
  sep = "|",
  row.names = FALSE
)
