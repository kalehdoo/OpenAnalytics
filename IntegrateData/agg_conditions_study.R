#import libraries
library(dplyr)
library(stringr)
library(data.table)

#set paths for data files
in_path_x_conditions<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/x_conditions.txt", sep="")

#reads the data files into dataframes
x_conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
rare_disease_list<-read.csv(paste(var_DIR_MISC_HOME,"list_rare_disease.txt", sep=""))

#check if condition matches with any of the rare disease in the list
x_conditions<- mutate(x_conditions,
                           flag_rare_disease_match=if_else((casefold(condition_name) %in% casefold(rare_disease$Name))== TRUE,1,0)
)

#agg conditions at study level
agg_study_conditions<-data.table(x_conditions)[,list( 
  condition_name  = paste(condition_name, collapse =","),
  cnt_conditions = length(condition_name),
  cnt_match_rare_condition = sum(flag_rare_disease_match, na.rm = TRUE),
  flag_rare_condition = if_else(sum(flag_rare_disease_match, na.rm = TRUE)>=1,1,0)
), by='nct_id']


#agg conditions at condition level
agg_conditions<-data.table(x_conditions)[,list( 
  nct_id  = paste(nct_id, collapse =","),
  cnt_studies = length(nct_id),
  flag_rare_condition = if_else(sum(flag_rare_disease_match, na.rm = TRUE)>=1,1,0)
), by='condition_name']


#write to txt file
write.table(agg_study_conditions, paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_study_conditions.txt", sep=""), sep = "|", row.names = FALSE)
write.table(agg_conditions, paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_conditions.txt", sep=""), sep = "|", row.names = FALSE)
