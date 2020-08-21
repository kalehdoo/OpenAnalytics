#import libraries
library(dplyr)

#set paths for data files
in_path_x_central_contacts<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_central_contacts.txt", sep="")

#reads the data files into dataframes
central_contacts<-read.csv(in_path_x_central_contacts, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#set paths for data files
in_path_x_sponsors<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_lead_sponsors.txt", sep="")

#reads the data files into dataframes
sponsors<-read.csv(in_path_x_sponsors, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

sponsor_central_contacts<-left_join(sponsors, central_contacts, by="nct_id")

sponsor_central_contacts1<-subset.data.frame(sponsor_central_contacts,
                                            select=c("lead_sponsor_name",
                                                     "agency_class",
                                                     "name", "email"),
                                            subset = (agency_class=="Industry") &
                                              is.na(email)==FALSE
                                            )


#write to txt file
write.table(sponsor_central_contacts1, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_central_contacts_Industry.txt", sep=""), sep = "|", row.names = FALSE)
