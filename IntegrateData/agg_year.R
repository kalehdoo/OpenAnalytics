#import libraries
library(dplyr)
library(stringr)
library(lubridate)

#set paths for data files
in_path_agg_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_studies.txt", sep="")

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

#write yearly data to txt file
write.table(agg_year,paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_year.txt", sep=""), sep = "|", row.names = FALSE)

#Create monthly data comparing current months with the last year
#Registered studies last year
registered_lst_yr<-agg_studies %>%
  filter(study_first_posted_year==var_last_year) %>%
  group_by(study_first_posted_month) %>%
  summarise("cnt_reg_lstYr"=length(unique(nct_id)))

#Registered studies current year
registered_curr_yr<-agg_studies %>%
  filter(study_first_posted_year==var_current_year) %>%
  group_by(study_first_posted_month) %>%
  summarise("cnt_reg_CurrYr"=length(unique(nct_id)))

#started studies last year
started_lst_yr<-agg_studies %>%
  filter(start_year==var_last_year & start_date_type=="Actual") %>%
  group_by(start_month) %>%
  summarise("cnt_started_lstYr"=length(unique(nct_id)))

#started studies current year
started_curr_yr<-agg_studies %>%
  filter(start_year==var_current_year & start_date_type=="Actual") %>%
  group_by(start_month) %>%
  summarise("cnt_started_CurrYr"=length(unique(nct_id)))

#Completed studies last year
completed_lst_yr<-agg_studies %>%
  filter(completed_year==var_last_year & completion_date_type=="Actual" & overall_status=="Completed") %>%
  group_by(completed_month) %>%
  summarise("cnt_completed_lstYr"=length(unique(nct_id)))

#Completed studies current year
completed_curr_yr<-agg_studies %>%
  filter(completed_year==var_current_year & completion_date_type=="Actual" & overall_status=="Completed") %>%
  group_by(completed_month) %>%
  summarise("cnt_completed_CurrYr"=length(unique(nct_id)))

#Posted Results last year
posted_lst_yr<-agg_studies %>%
  filter(results_first_posted_year==var_last_year & overall_status=="Completed") %>%
  group_by(results_first_posted_month) %>%
  summarise("cnt_posted_lstYr"=length(unique(nct_id)))

#Posted Results current year
posted_curr_yr<-agg_studies %>%
  filter(results_first_posted_year==var_current_year & overall_status=="Completed") %>%
  group_by(results_first_posted_month) %>%
  summarise("cnt_posted_CurrYr"=length(unique(nct_id)))

#combine curr and last year
colnames(registered_lst_yr)[colnames(registered_lst_yr)=="study_first_posted_month"]<-"common_month"
colnames(registered_curr_yr)[colnames(registered_curr_yr)=="study_first_posted_month"]<-"common_month"
colnames(started_lst_yr)[colnames(started_lst_yr)=="start_month"]<-"common_month"
colnames(started_curr_yr)[colnames(started_curr_yr)=="start_month"]<-"common_month"
colnames(completed_lst_yr)[colnames(completed_lst_yr)=="completed_month"]<-"common_month"
colnames(completed_curr_yr)[colnames(completed_curr_yr)=="completed_month"]<-"common_month"
colnames(posted_lst_yr)[colnames(posted_lst_yr)=="results_first_posted_month"]<-"common_month"
colnames(posted_curr_yr)[colnames(posted_curr_yr)=="results_first_posted_month"]<-"common_month"


#agg_yearmonth<-full_join(registered_lst_yr, registered_curr_yr,, by="study_first_posted_month")
agg_month<-registered_lst_yr %>%
  full_join(registered_curr_yr, by="common_month") %>%
  full_join(started_lst_yr, by="common_month") %>%
  full_join(started_curr_yr, by="common_month") %>%
  full_join(completed_lst_yr, by="common_month") %>%
  full_join(completed_curr_yr, by="common_month") %>%
  full_join(posted_lst_yr, by="common_month") %>%
  full_join(posted_curr_yr, by="common_month")

agg_month <- mutate(agg_month,
                    month_name = case_when(common_month == "1" ~ "Jan",
                                           common_month == "2" ~ "Feb",
                                           common_month == "3" ~ "Mar",
                                           common_month == "4" ~ "Apr",
                                           common_month == "5" ~ "May",
                                           common_month == "6" ~ "Jun",
                                           common_month == "7" ~ "Jul",
                                           common_month == "8" ~ "Aug",
                                           common_month == "9" ~ "Sep",
                                           common_month == "10" ~ "Oct",
                                           common_month == "11" ~ "Nov",
                                           common_month == "12" ~ "Dec"),
                    qtr_name = case_when(common_month == "1" ~ "Q-1",
                                           common_month == "2" ~ "Q-1",
                                           common_month == "3" ~ "Q-1",
                                           common_month == "4" ~ "Q-2",
                                           common_month == "5" ~ "Q-2",
                                           common_month == "6" ~ "Q-2",
                                           common_month == "7" ~ "Q-3",
                                           common_month == "8" ~ "Q-3",
                                           common_month == "9" ~ "Q-3",
                                           common_month == "10" ~ "Q-4",
                                           common_month == "11" ~ "Q-4",
                                           common_month == "12" ~ "Q-4"))

#write monthly data to txt file
write.table(agg_month,paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_month.txt", sep=""), sep = "|", row.names = FALSE)

