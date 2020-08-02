library(dplyr)
library(sqldf)

#ACCT_HOME to store path of home dir
assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)

#set paths for data files
path_outcome_measurements<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_outcome_measurements.txt", sep="")

#reads the data files into dataframes
outcome_measurements<-read.csv(path_outcome_measurements, header=TRUE, sep = "|",na.strings = "NA", nrows = -1000, stringsAsFactors=FALSE)

#set paths for data files
path_agg_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies.txt", sep="")

#reads the data files into dataframes
studies<-read.csv(path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -1000, stringsAsFactors=FALSE)

studies<-studies %>% 
  select("nct_id","phase","overall_status","official_title","condition_name","intervention_type","intervention_name","intervention_desc","actual_study_enrollment") %>%
  filter(overall_status=="Completed")

#join studies and outcomes
studies_outcomes<-left_join(studies,outcome_measurements, by="nct_id")

#write to txt file
#write.table(studies_outcomes, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies_outcome_measurements.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)

studies_outcomes<-sqldf("select distinct condition_name, title, units, param_type, avg(param_value) as mean_param_val, dispersion_type, avg(dispersion_value) as mean_disp_val
                         from studies_outcomes
                         where length(title)>0
                        group by condition_name, title, units, param_type, dispersion_type")

#write to txt file
write.table(studies_outcomes, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies_outcome_measurements.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)
