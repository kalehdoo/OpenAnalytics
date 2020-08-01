#import libraries
library(dplyr)

#set paths for data files
in_path_intervention_other_names<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/intervention_other_names.txt", sep="")

#reads the data files into dataframes
intervention_other_names<-read.csv(in_path_intervention_other_names, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#write to txt file
write.table(intervention_other_names, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_intervention_other_names.txt", sep=""), sep = "|", row.names = FALSE)

rm(intervention_other_names)
