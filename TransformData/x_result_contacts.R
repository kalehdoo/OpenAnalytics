library(dplyr)
library(sqldf)

#set paths for data files
in_path_result_contacts<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/result_contacts.txt", sep="")

#reads the data files into dataframes
result_contacts<-read.csv(in_path_result_contacts, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors=FALSE)


#write to txt file
write.table(result_contacts, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_result_contacts.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)


result_contacts_email<-unique(result_contacts$email)

#write to txt file
write.table(result_contacts_email, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/result_contacts_email.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)
