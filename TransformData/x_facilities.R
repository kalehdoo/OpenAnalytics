library(dplyr)

#set paths for data files
in_path_facilities<-paste(var_DIR_ACCT_HOME, "DATA/unzipSrcFiles/facilities.txt", sep="")

#reads the data files into dataframes
facilities<-read.csv(in_path_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

colnames(facilities)[colnames(facilities)=="id"]<-"facility_id"
colnames(facilities)[colnames(facilities)=="name"]<-"facility_name"

#write to txt file
write.table(facilities, paste(var_DIR_ACCT_HOME, "DATA/warehouse/x_facilities.txt", sep=""), sep = "|", row.names = FALSE)

#remove objects to cleanup
rm(facilities)

