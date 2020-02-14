#import libraries
library(dplyr)

#set paths for data files
in_path_interventions<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/interventions.txt", sep="")

#reads the data files into dataframes
interventions<-read.csv(in_path_interventions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#write to txt file
write.table(interventions, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_interventions.txt", sep=""), sep = "|", row.names = FALSE)

rm(interventions)
