library(lubridate)
#all scripts must be executed in the order
#create global parameters

#ACCT_HOME to store path of home dir
assign("var_DIR_HOME", "C:/Users/ranamanohar/Documents/GitHub/OpenAnalytics/", envir = .GlobalEnv)

assign("var_DIR_ACCT_HOME", "C:/Users/ranamanohar/Documents/GitHub/OpenAnalytics/Data/ACCT/", envir = .GlobalEnv)

assign("var_DIR_ACCT_LOGS", "C:/Users/ranamanohar/Documents/GitHub/OpenAnalytics/Data/ACCT/DATA/logs/", envir = .GlobalEnv)

assign("var_DIR_MISC_HOME", "C:/Users/ranamanohar/Documents/GitHub/OpenAnalytics/Data/MISC/", envir = .GlobalEnv)


#create parameters for year period
assign("var_current_date", today(), envir = .GlobalEnv)
assign("var_current_year", year(today()), envir = .GlobalEnv)
assign("var_last_year", (var_current_year-1), envir = .GlobalEnv)
assign("var_last_year_2", (var_current_year-2), envir = .GlobalEnv)
assign("var_last_year_3", (var_current_year-3), envir = .GlobalEnv)
assign("var_last_year_4", (var_current_year-4), envir = .GlobalEnv)
assign("var_last_year_5", (var_current_year-5), envir = .GlobalEnv)
assign("var_last_year_10", (var_current_year-10), envir = .GlobalEnv)

#execute script to download ACCT zip file from web and extract
source(paste0(var_DIR_HOME,"ImportData/download_ACCT.R"))

#execute basic transformation scripts
source(paste0(var_DIR_HOME,"TransformData/x_studies.R"))
source(paste0(var_DIR_HOME,"TransformData/x_sponsors.R"))
source(paste0(var_DIR_HOME,"TransformData/x_conditions.R"))
source(paste0(var_DIR_HOME,"TransformData/x_facilities.R"))
source(paste0(var_DIR_HOME,"TransformData/x_interventions.R"))

#execute to create integrated aggregate tables in warehouse
source(paste0(var_DIR_HOME,"IntegrateData/agg_conditions_study.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_facilities.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_facilities_study.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_interventions_study.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_studies.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_sponsors.R"))
source(paste0(var_DIR_HOME,"IntegrateData/agg_year.R"))

#execute to create extract summary tables in extract folder
source(paste0(var_DIR_HOME,"Extracts/mv_year_Lst10Yr.R"))
source(paste0(var_DIR_HOME,"Extracts/mv_studies_recruiting.R"))


