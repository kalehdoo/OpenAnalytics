library(lubridate)

#create global parameters

#ACCT_HOME to store path of home dir

assign("var_DIR_ACCT_HOME", "C:/OpenClinicalAnalytics/ACCT/", envir = .GlobalEnv)

assign("var_DIR_ACCT_LOGS", "C:/OpenClinicalAnalytics/ACCT/DATA/logs/", envir = .GlobalEnv)

assign("var_DIR_MISC_HOME", "C:/OpenClinicalAnalytics/MISC/", envir = .GlobalEnv)

#create parameters for year period
assign("var_current_date", today(), envir = .GlobalEnv)
assign("var_current_year", year(today()), envir = .GlobalEnv)
assign("var_last_year", (var_current_year-1), envir = .GlobalEnv)
assign("var_last_year_2", (var_current_year-2), envir = .GlobalEnv)
assign("var_last_year_3", (var_current_year-3), envir = .GlobalEnv)
assign("var_last_year_4", (var_current_year-4), envir = .GlobalEnv)
assign("var_last_year_5", (var_current_year-5), envir = .GlobalEnv)




