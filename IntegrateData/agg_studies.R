#import libraries
library(dplyr)
library(stringr)
library(lubridate)

#set paths for data files
in_path_x_studies<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/x_studies.txt", sep="")
in_path_x_lead_sponsors<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/x_lead_sponsors.txt", sep="")

#reads the data files into dataframes
x_studies<-read.csv(in_path_x_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
x_lead_sponsors<-read.csv(in_path_x_lead_sponsors, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#left join lead_sponsors to studies - has 1-1 relationship
agg_studies<-left_join(x_studies,x_lead_sponsors,by="nct_id")

#write to txt file
write.table(agg_studies,paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_studies.txt", sep=""), sep = "|", row.names = FALSE)



