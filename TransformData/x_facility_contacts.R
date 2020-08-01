#import libraries
library(dplyr)

#set paths for data files
in_path_fac_contacts<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/facility_contacts.txt", sep="")

#reads the data files into dataframes
facility_contacts<-read.csv(in_path_fac_contacts, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#write to txt file
write.table(facility_contacts, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facility_contacts.txt", sep=""), sep = "|", row.names = FALSE)

#rm(facility_contacts)

facility_contacts_email<-unique(facility_contacts$email)

#write to txt file
write.table(facility_contacts_email, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/facility_contacts_email.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)
