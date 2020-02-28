library(dplyr)
library(sqldf)

#set paths for data files
in_path_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/facilities.txt", sep="")

#reads the data files into dataframes
facilities<-read.csv(in_path_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors=FALSE)

facilities$name<-str_remove_all(facilities$name, "[',|]")
colnames(facilities)[colnames(facilities)=="id"]<-"facility_id"
colnames(facilities)[colnames(facilities)=="name"]<-"facility_name"

#create new columns to classify site into academic and hospitals
facilities<-mutate(facilities,
                       "flag_site_recruiting"=if_else(status=="Recruiting",1,0),
                       "flag_site_US"=if_else(casefold(country)=="united states",1,0),
                       "site_type"=if_else(facility_name!="Industry" &
                                             grepl("university",casefold(facility_name)) | 
                                             grepl("univerzi",casefold(facility_name)) |
                                             grepl("institut",casefold(facility_name)) |
                                             grepl("school",casefold(facility_name)) |
                                             grepl("campus",casefold(facility_name)) |
                                             grepl("college",casefold(facility_name)) |
                                             grepl("education",casefold(facility_name)) |
                                             grepl("academ",casefold(facility_name)) |
                                             grepl("univers",casefold(facility_name)), "Academic",
                                           if_else(facility_name!="Industry" &
                                                     grepl("hospital",casefold(facility_name))|
                                                     grepl("clinic",casefold(facility_name))|
                                                     grepl("medical center",casefold(facility_name))|
                                                     grepl("health center",casefold(facility_name))|
                                                     grepl("center",casefold(facility_name))|
                                                     grepl("centre",casefold(facility_name))|
                                                     grepl("h?pital",casefold(facility_name))|
                                                     grepl("hopital",casefold(facility_name)), "Hospital" ,
                                                   if_else(facility_name=="Industry","Industry",
                                                           if_else(facility_name=="U.S. Fed","U.S. Fed",
                                                                   if_else(facility_name=="[Redacted]","Redacted",
                                                                           if_else(facility_name=="NIH","NIH",
                                                                                   if_else(facility_name=="Other","Other"
                                                                                           ,"NA")))))))                      
)

#write to txt file
write.table(facilities, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facilities.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)



