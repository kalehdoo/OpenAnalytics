library(lubridate)
library(sqldf)

#the master should be executed only once


#HOME to store path of home dir
assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)
source(paste0(var_DIR_HOME,"TransformData/x_prep_geo_master.R"))

#remove data in the memory and quit the session without saving the session
rm(list = ls())
#q("no", 1, FALSE)