#Stores conditions at Study Level (1 record per study)
#import libraries
library(dplyr)
library(stringr)
library(data.table)

#set paths for data files
in_path_x_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep="")

#reads the data files into dataframes
x_conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)


#agg conditions at study level
agg_conditions_study<-data.table(x_conditions)[,list( 
  condition_name  = paste(unique(condition_name), collapse =","),
  cnt_conditions = length(unique(condition_name)),
  cnt_match_rare_condition = sum(flag_rare_disease, na.rm = TRUE)
), by='nct_id']


#write to txt file
write.table(agg_conditions_study, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_conditions_study.txt", sep=""), sep = "|", row.names = FALSE)
