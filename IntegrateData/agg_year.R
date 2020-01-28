#import libraries
library(dplyr)
library(stringr)
library(lubridate)

#set paths for data files
in_path_agg_studies<-"C:/ACCT/DATA/warehouse/agg_studies.txt"

#reads the data files into dataframes
agg_studies<-read.csv(in_path_agg_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#create yearly facts and then union them based on year
#Registered studies
year_f_registered<-agg_studies %>%
  group_by(study_first_posted_year) %>%
  summarise("cnt_registered"=n())

#rename the year column - use any method
#names(year_f_completed)[1]<-"common_year"
colnames(year_f_registered)[colnames(year_f_registered)=="study_first_posted_year"]<-"common_year"

#started studies
year_f_started<-agg_studies %>%
  filter(start_date_type=="Actual") %>%
  group_by(start_year) %>%
  summarise("cnt_started"=n())

#rename the year column - use any method
colnames(year_f_started)[colnames(year_f_started)=="start_year"]<-"common_year"

#completed studies
year_f_completed<-agg_studies %>%
  filter(completion_date_type=="Actual" & overall_status=="Completed") %>%
  group_by(completed_year) %>%
  summarise("cnt_completed"=n())

#rename the year column - use any method
colnames(year_f_completed)[colnames(year_f_completed)=="completed_year"]<-"common_year"

#results posted
year_f_results<-agg_studies %>%
  group_by(results_first_posted_year) %>%
  summarise("cnt_resultsPosted"=n())

colnames(year_f_results)[colnames(year_f_results)=="results_first_posted_year"]<-"common_year"

#combine to create final yearly fact
agg_year1<-full_join(year_f_registered, year_f_started, by="common_year")
agg_year2<-full_join(year_f_completed, year_f_results, by="common_year")

agg_year<-full_join(agg_year1, agg_year2, by="common_year")

#year_f_1$common_year<- as.factor(year_f_1$common_year)

#write to txt file
write.table(agg_year,"C:/ACCT/DATA/warehouse/agg_year.txt", sep = "|", row.names = FALSE)
