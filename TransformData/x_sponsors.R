library(dplyr)
library(lubridate)
library(stringr)

#set paths for data files
in_path_sponsors<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/sponsors.txt", sep="")

#reads the data files into dataframes
sponsors<-read.csv(in_path_sponsors, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#create new df for only lead sponsors
lead_sponsors<-subset.data.frame(sponsors, subset=lead_or_collaborator=="lead", select = c("nct_id",
                                                                                           "agency_class",
                                                                                           "name",
                                                                                           "id")
)

#create new columns to classify lead sponsor into academic and hospitals
lead_sponsors<-mutate(lead_sponsors,
                      "flag_sponsor_industry"=if_else(agency_class=="Industry",1,0),
                      "sponsor_type"=if_else(agency_class!="Industry" &
                                               grepl("university",casefold(name)) | 
                                               grepl("univerzi",casefold(name)) |
                                               grepl("institut",casefold(name)) |
                                               grepl("school",casefold(name)) |
                                               grepl("campus",casefold(name)) |
                                               grepl("college",casefold(name)) |
                                               grepl("education",casefold(name)) |
                                               grepl("academ",casefold(name)) |
                                               grepl("univers",casefold(name)), "Academic",
                                             if_else(agency_class!="Industry" &
                                                       grepl("hospital",casefold(name))|
                                                       grepl("clinic",casefold(name))|
                                                       grepl("medical center",casefold(name))|
                                                       grepl("health center",casefold(name))|
                                                       grepl("center",casefold(name))|
                                                       grepl("centre",casefold(name))|
                                                       grepl("h?pital",casefold(name))|
                                                       grepl("hopital",casefold(name)), "Hospital" ,
                                                     if_else(agency_class=="Industry","Industry",
                                                             if_else(agency_class=="U.S. Fed","U.S. Fed",
                                                                     if_else(name=="[Redacted]","Redacted",
                                                                             if_else(agency_class=="NIH","NIH",
                                                                                     if_else(agency_class=="Other","Other"
                                                                                     ,"NA"))))))),
                      "flag_sponsor_type_academic"=if_else(sponsor_type=="Academic",1,0),
                      "flag_sponsor_type_hospital"=if_else(sponsor_type=="Hospital",1,0)
                      
                      
)

#rename columns
colnames(lead_sponsors)[colnames(lead_sponsors)=="name"]<-"lead_sponsor_name"
colnames(lead_sponsors)[colnames(lead_sponsors)=="id"]<-"lead_sponsor_id"

#write to txt file
write.table(lead_sponsors, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_lead_sponsors.txt", sep=""), sep = "|", row.names = FALSE)


