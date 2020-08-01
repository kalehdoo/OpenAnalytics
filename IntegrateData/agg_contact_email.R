#import libraries
library(dplyr)
library(sqldf)
library(data.table)
library(stringr)

#HOME to store path of home dir
assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)


#set paths for data files
in_path_result_contacts_email<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/result_contacts_email.txt", sep="")
in_path_facility_contacts_email<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/facility_contacts_email.txt", sep="")
in_path_central_contacts_email<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/central_contacts_email.txt", sep="")

result_contacts_email<-read.csv(in_path_result_contacts_email, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
facility_contacts_email<-read.csv(in_path_facility_contacts_email, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
central_contacts_email<-read.csv(in_path_central_contacts_email, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)

unique_email<-sqldf("select distinct x as email from result_contacts_email
                    UNION
                    select distinct x as email from facility_contacts_email
                    UNION
                    select distinct x as email from central_contacts_email")

unique_email_all<-unique(unique_email$email)

unique_email_all2<-as_tibble(unique_email_all)

colnames(unique_email_all2)[colnames(unique_email_all2)=="value"]<-"email"

unique_email_all3<-str_remove_all(unique_email_all2$email, "[',()]")

unique_email_all4<-as_tibble(unique_email_all3)
colnames(unique_email_all4)[colnames(unique_email_all4)=="value"]<-"email"

#write to txt file
write.table(unique_email_all4, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/unique_email_all.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)


#create seperate list of only recruting facilities contacts
in_path_facility_contacts<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facility_contacts.txt", sep="")
in_path_agg_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studyfacilities_rec.txt", sep="")
facility_contacts<-read.csv(in_path_facility_contacts, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
agg_facilities<-read.csv(in_path_agg_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)



#join recruiting facilities to recruiting studies
rec_facility_contacts<-left_join(agg_facilities, facility_contacts, by=c("nct_id","facility_id"))

unique_rec_facility_contacts<-unique(rec_facility_contacts$email)

unique_rec_facility_contacts<-as_tibble(unique_rec_facility_contacts)

colnames(unique_rec_facility_contacts)[colnames(unique_rec_facility_contacts)=="value"]<-"email"

write.table(unique_rec_facility_contacts,paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/rec_facility_contacts.txt", sep=""), sep = ",", row.names = FALSE)


#create a list of rest all contacts who are not recruiting
non_rec_contacts<-sqldf("select email from unique_email_all4
                        EXCEPT
                        select email from unique_rec_facility_contacts")

#write to txt file
write.table(non_rec_contacts, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/non_rec_contacts.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)

