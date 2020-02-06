#import libraries
library(dplyr)
library(data.table)

#set paths for data files
in_path_x_interventions<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/x_interventions.txt", sep="")

#reads the data files into dataframes
x_interventions<-read.csv(in_path_x_interventions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)


#agg interventions at study level
agg_study_interventions<-data.table(x_interventions)[,list( 
  intervention_type  = paste(unique(intervention_type), collapse =","),
  intervention_name  = paste(name, collapse =","),
  intervention_desc  = paste(description, collapse ="~"),
  cnt_interventions = length(name)
  ), by='nct_id']

#write to txt file
write.table(agg_study_interventions, paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_study_interventions.txt", sep=""), sep = "|", row.names = FALSE)
