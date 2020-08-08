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
  cnt_US_only_studies = sum(flag_USonly, na.rm = TRUE),
  cnt_nonUS_only_studies = sum(ifelse((flag_has_US_site == 0 & cnt_countries_nonUS >= 1), 1L, 0L), na.rm = TRUE),
  cnt_global_studies = sum(ifelse((flag_has_US_site == 1 & cnt_countries_nonUS >= 1), 1L, 0L), na.rm = TRUE),
  cnt_studies_1country = sum(ifelse((cnt_countries == 1), 1L, 0L), na.rm = TRUE),
  cnt_studies_2to5country = sum(ifelse((cnt_countries >= 2 & cnt_countries <=5), 1L, 0L), na.rm = TRUE),
  cnt_studies_6to10country = sum(ifelse((cnt_countries >= 6 & cnt_countries <=10), 1L, 0L), na.rm = TRUE),
  cnt_studies_11to30country = sum(ifelse((cnt_countries >= 11 & cnt_countries <=30), 1L, 0L), na.rm = TRUE),
  cnt_studies_31to50country = sum(ifelse((cnt_countries >= 31 & cnt_countries <=50), 1L, 0L), na.rm = TRUE),
  cnt_studies_50pluscountry = sum(ifelse((cnt_countries >= 51), 1L, 0L), na.rm = TRUE),
  cnt_started_actual = sum(flag_actual_started, na.rm = TRUE),
  cnt_recruiting_status = sum(flag_recruiting_status, na.rm = TRUE),
  cnt_results_submitted = sum(flag_results_posted, na.rm = TRUE),
  cnt_completed_status = sum(flag_completed_status, na.rm = TRUE),
  ratio_results_to_completed = 100 * (
    sum(flag_results_posted, na.rm = TRUE) / sum(flag_completed_status, na.rm = TRUE)
  ),
  cnt_has_dmc = sum(flag_has_dmc, na.rm = TRUE),
  cnt_interventional = sum(ifelse(study_type == "Interventional", 1, 0), na.rm = TRUE),
  cnt_observational = sum(ifelse(study_type == "Observational", 1, 0), na.rm = TRUE),
  cnt_phase1 = sum(ifelse(phase == "Phase 1", 1, 0), na.rm = TRUE),
  cnt_phase2 = sum(ifelse(phase == "Phase 2", 1, 0), na.rm = TRUE),
  cnt_phase3 = sum(ifelse(phase == "Phase 3", 1, 0), na.rm = TRUE),
  cnt_phase4 = sum(ifelse(phase == "Phase 4", 1, 0), na.rm = TRUE),
  cnt_actual_study_enrollment = sum(actual_study_enrollment, na.rm = TRUE),
  cnt_anticipated_study_enrollment = sum(anticipated_study_enrollment, na.rm = TRUE),
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
  
  cnt_study_registered_lstyr2 = sum(
    ifelse(study_first_posted_year == var_last_year_2[1], 1, 0),
    na.rm = TRUE
  ),
  cnt_started_actual_lstyr2 = sum(
    ifelse(start_year == var_last_year_2[1], flag_actual_started, 0L),
    na.rm = TRUE
  ),
  cnt_completed_status_lstyr2 = sum(
    ifelse(completed_year == var_last_year_2[1], flag_completed_status, 0L),
    na.rm = TRUE
  ),
  cnt_results_submitted_lstyr2 = sum(
    ifelse(
      results_first_posted_year == var_last_year_2[1],
      flag_results_posted,
      0L
    ),
    na.rm = TRUE
  ),
  cnt_study_registered_lstyr3 = sum(
    ifelse(study_first_posted_year == var_last_year_3[1], 1, 0),
    na.rm = TRUE
  ),
  cnt_started_actual_lstyr3 = sum(
    ifelse(start_year == var_last_year_3[1], flag_actual_started, 0L),
    na.rm = TRUE
  ),
  cnt_completed_status_lstyr3 = sum(
    ifelse(completed_year == var_last_year_3[1], flag_completed_status, 0L),
    na.rm = TRUE
  ),
  cnt_results_submitted_lstyr3 = sum(
    ifelse(
      results_first_posted_year == var_last_year_3[1],
      flag_results_posted,
      0L
    ),
    na.rm = TRUE
  ),
  cnt_study_registered_lstyr4 = sum(
    ifelse(study_first_posted_year == var_last_year_4[1], 1, 0),
    na.rm = TRUE
  ),
  cnt_started_actual_lstyr4 = sum(
    ifelse(start_year == var_last_year_4[1], flag_actual_started, 0L),
    na.rm = TRUE
  ),
  cnt_completed_status_lstyr4 = sum(
    ifelse(completed_year == var_last_year_4[1], flag_completed_status, 0L),
    na.rm = TRUE
  ),
  cnt_results_submitted_lstyr4 = sum(
    ifelse(
      results_first_posted_year == var_last_year_4[1],
      flag_results_posted,
      0L
    ),
    na.rm = TRUE
  ),
  cnt_study_registered_lstyr5 = sum(
    ifelse(study_first_posted_year == var_last_year_5[1], 1, 0),
    na.rm = TRUE
  ),
  cnt_started_actual_lstyr5 = sum(
    ifelse(start_year == var_last_year_5[1], flag_actual_started, 0L),
    na.rm = TRUE
  ),
  cnt_completed_status_lstyr5 = sum(
    ifelse(completed_year == var_last_year_5[1], flag_completed_status, 0L),
    na.rm = TRUE
  ),
  cnt_results_submitted_lstyr5 = sum(
    ifelse(
      results_first_posted_year == var_last_year_5[1],
      flag_results_posted,
      0L
    ),
    na.rm = TRUE
  ),
  
  cnt_conditions = sum(cnt_conditions, na.rm = TRUE),
  cnt_rare_condition_match = sum(cnt_match_rare_condition, na.rm = TRUE),
  cnt_rare_condition_studies = sum(if_else(cnt_match_rare_condition>0,1,0), na.rm = TRUE)
  
), by = 'lead_sponsor_name']


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
                                        count(distinct(CASE when collaborators.agency_class='U.S. Fed' THEN collaborators.collaborator_name END)) as 'cnt_colab_USFed',
                                        count(distinct(CASE when collaborators.agency_class='Industry' THEN collaborators.collaborator_name END)) as 'cnt_colab_Ind',
                                        count(distinct(CASE when collaborators.sponsor_type='Hospital' THEN collaborators.collaborator_name END)) as 'cnt_colab_Hosp',
                                        count(distinct(CASE when collaborators.sponsor_type='Academic' THEN collaborators.collaborator_name END)) as 'cnt_colab_Acad'
                                  from sponsors
                                  left join collaborators
                                  on sponsors.nct_id=collaborators.nct_id
                               group by sponsors.lead_sponsor_name")

#stich collaborators with agg studies
agg_sponsors<-left_join(agg_sponsors1, sponsor_collaborators, by=c("lead_sponsor_name"))

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
