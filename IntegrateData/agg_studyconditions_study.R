#combines conditions with studies - level is condition-study (one study will have one condition per row)
#import libraries
library(dplyr)
library(stringr)
library(data.table)
#library(tm)
#library("SnowballC")

#set paths for data files
in_path_x_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep="")
in_path_agg_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies.txt", sep="")

#reads the data files into dataframes
x_conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)

agg_studies<-subset(agg_studies, select=c("nct_id",
                                          "lead_sponsor_name",
                                          "overall_status",
                                          "agency_class",
                                          "sponsor_type"))

#join studies to conditions
condition_study<-left_join(x_conditions, agg_studies, by="nct_id")

#write to txt file
write.table(condition_study, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studycondition_study.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)



