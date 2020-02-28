#combines conditions with facilities - level is condition-study (one study will have one condition per row)
#import libraries
library(dplyr)
library(stringr)
library(data.table)

#rm(list=ls())

#set paths for data files
in_path_agg_studyconditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studycondition_study.txt", sep="")
in_path_agg_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt", sep="")

#reads the data files into dataframes
agg_studyconditions<-read.csv(in_path_agg_studyconditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
agg_facilities<-read.csv(in_path_agg_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)


#join studies to conditions
studycondition_facilities<-left_join(agg_studyconditions, agg_facilities, by="nct_id")

studycondition_facilities_rec<-subset(studycondition_facilities, subset=overall_status=="Recruiting")

#write to txt file
write.table(studycondition_facilities_rec, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studycondition_facilities_rec.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)

write.table(studycondition_facilities, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studycondition_facilities.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)
