#import libraries
library(dplyr)

#set paths for data files
in_path_agg_studies<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_studies.txt", sep="")
in_path_agg_facilities<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_facilities.txt", sep="")

#reads the data files into dataframes
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
agg_facilities<-read.csv(in_path_agg_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#create new df with required columns and filter condition
mv_studies_recruiting1<-subset.data.frame(agg_studies, 
                                   subset=overall_status=="Recruiting" ,
                                   select=c(
                                     "nct_id",
                                     "start_date",
                                     "official_title",
                                     "phase",
                                     "enrollment",
                                     "has_dmc",
                                     "lead_sponsor_name",
                                     "condition_name",
                                     "flag_rare_condition"
                                   )
                                   
)

#create new df with required columns and filter condition
mv_facilities_recruiting<-subset.data.frame(agg_facilities, 
                                         #subset=status=="Recruiting",
                                         select=c(
                                           "nct_id",
                                           "facility_name",
                                           "facility_status",
                                           "city",
                                           "state",
                                           "country",
                                           "zip",
                                           "latitude",
                                           "longitude"
                                           )
                                         
)

mv_studies_recruiting1$nct_id<-as.character(mv_studies_recruiting1$nct_id)
mv_facilities_recruiting$nct_id<-as.character(mv_facilities_recruiting$nct_id)
mv_studies_recruiting<-left_join(mv_studies_recruiting1, mv_facilities_recruiting, by="nct_id")
mv_studies_recruiting$urlid<-paste0("https://clinicaltrials.gov/ct2/show/",mv_studies_recruiting$nct_id, sep="")

#write to txt file
write.table(mv_studies_recruiting,paste(var_DIR_ACCT_HOME, "DATA/extracts/mv_studies_recruiting.txt", sep=""), sep = "|", row.names = FALSE)
