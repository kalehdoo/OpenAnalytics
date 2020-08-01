#import libraries
library(dplyr)

#set paths for data files
in_path_central_contacts<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/central_contacts.txt", sep="")

#reads the data files into dataframes
central_contacts<-read.csv(in_path_central_contacts, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#write to txt file
write.table(central_contacts, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_central_contacts.txt", sep=""), sep = "|", row.names = FALSE)

#rm(facility_contacts)

central_contacts_email<-unique(central_contacts$email)

#write to txt file
write.table(central_contacts_email, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/central_contacts_email.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)
