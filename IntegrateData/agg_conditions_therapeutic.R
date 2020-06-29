#create aggregate at recruiting condition level
#import libraries
library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

#HOME to store path of home dir
assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)

#set paths for data files
in_path_x_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep="")
in_path_therapeutic<-paste(var_DIR_HOME, "Data/MISC/Therapeutic_Indications_List.txt", sep="")

#reads the data files into dataframes
x_conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
therapeutic<-read.csv(in_path_therapeutic, header=TRUE, sep = "\t",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)

conditions<-sqldf("select distinct condition_name from x_conditions")

for (i in 1:nrow(x_conditions)) {
  for (j in 1:nrow(therapeutic)) 
  row_id = observation_log_control$row_id[i]
}