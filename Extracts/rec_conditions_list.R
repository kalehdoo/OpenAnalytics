#import libraries
library(dplyr)

in_path_agg_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt", sep="")
agg_facilities<-read.csv(in_path_agg_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)


#facilities recruiting with required columns and filter condition
mv_facilities_recruiting<-subset.data.frame(agg_facilities, 
                                            status=="Recruiting",
                                            select=c(
                                              "nct_id",
                                              "facility_name",
                                              "facility_id",
                                              "status",
                                              "city",
                                              "state",
                                              "country",
                                              "region",
                                              "iso2",
                                              "iso3",
                                              "statecode",
                                              "zip",
                                              "latitude",
                                              "longitude"
                                            )
                                            
)

in_path_x_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep="")
x_conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)


x_conditions$nct_id<-as.character(x_conditions$nct_id)
#join recruiting facilities to recruiting studies
mv_conditions_recruiting<-left_join(mv_facilities_recruiting,x_conditions, by="nct_id")

rec_conditions_list<-subset.data.frame(mv_conditions_recruiting,
                                  select = (condition_name)
                                    )

rec_conditions_list_unique<-as_tibble(unique(tolower(rec_conditions_list$condition_name)))

colnames(rec_conditions_list_unique)[colnames(rec_conditions_list_unique)=="value"]<-"ConditionName"

#write to txt file
write.table(rec_conditions_list_unique,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/rec_conditions_list.txt", sep=""), sep = "|", row.names = FALSE)
