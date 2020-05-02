library(dplyr)

#ACCT_HOME to store path of home dir
assign("var_DIR_HOME", "C:/Users/ranamanohar/Documents/GitHub/OpenAnalytics/", envir = .GlobalEnv)

#set paths for data files
in_path_outcome_measurements<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/outcome_measurements.txt", sep="")

#reads the data files into dataframes
outcome_measurements<-read.csv(in_path_outcome_measurements, header=TRUE, sep = "|",na.strings = "NA", nrows = -1000, stringsAsFactors=FALSE)

outcome_measurements<-outcome_measurements %>% 
  select("nct_id","ctgov_group_code","classification","category","title","description","units","param_type","param_value","dispersion_type","dispersion_value","dispersion_lower_limit","dispersion_upper_limit")

#write to txt file
write.table(outcome_measurements, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_outcome_measurements.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)
