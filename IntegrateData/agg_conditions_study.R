#import libraries
library(dplyr)
library(stringr)
library(data.table)
library(tm)
library("SnowballC")


#set paths for data files
in_path_x_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep="")
in_path_x_studies<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_studies.txt", sep="")


#reads the data files into dataframes
x_conditions<-read.csv(in_path_x_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
rare_disease_list<-read.csv(paste(var_DIR_HOME,"Data/MISC/list_rare_disease.txt", sep=""))
x_studies<-read.csv(in_path_x_studies, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#get only recruiting studies
x_studies_recruiting<-subset.data.frame(x_studies, subset = overall_status=="Recruiting",
                                        select=c("nct_id"))

#check if condition matches with any of the rare disease in the list
x_conditions<- mutate(x_conditions,
                      flag_rare_disease_match=if_else((casefold(condition_name) %in% casefold(rare_disease_list$Name))== TRUE,1,0),
                      flag_study_recruiting=if_else(casefold(nct_id) %in% casefold(x_studies_recruiting$nct_id),1,0)
)

#agg conditions at study level
agg_study_conditions<-data.table(x_conditions)[,list( 
  condition_name  = paste(condition_name, collapse =","),
  cnt_conditions = length(condition_name),
  cnt_match_rare_condition = sum(flag_rare_disease_match, na.rm = TRUE),
  flag_rare_condition = if_else(sum(flag_rare_disease_match, na.rm = TRUE)>=1,1,0)
), by='nct_id']


#write to txt file
write.table(agg_study_conditions, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_conditionsbyStudy.txt", sep=""), sep = "|", row.names = FALSE)

#aggregate conditions by condition
agg_Studiesbyconditions<-data.table(x_conditions)[,list( 
  cnt_registeredstudies = length(nct_id),
  cnt_recruitingstudies = sum(flag_study_recruiting, na.rm = TRUE),
  cnt_rareconditionstudies = sum(flag_study_recruiting, na.rm = TRUE)
), by='condition_name']

#create data for wordcloud (total studies registered by condition)
write.table(agg_Studiesbyconditions, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_Studiesbyconditions.txt", sep=""), sep = "|", row.names = FALSE)

x_conditions_recruiting<-subset.data.frame(x_conditions, subset=flag_study_recruiting>0)

x_conditions_recruiting_name<- as.character(x_conditions_recruiting$condition_name)

conditions<-Corpus(VectorSource(x_conditions_recruiting_name))
conditions <- tm_map(conditions, content_transformer(tolower))
#conditions <- tm_map(conditions, removeNumbers)
conditions <- tm_map(conditions, removeWords, stopwords("english"))
conditions <- tm_map(conditions, removePunctuation)
conditions <- tm_map(conditions, stripWhitespace)
conditions <- tm_map(conditions, stemDocument)
conditions <- tm_map(conditions, removeWords, c("disord","diseas","syndrom"))

#converting conditions corpus into dataframe
df_conditions <- data.frame(condition_name=sapply(conditions, identity), 
                        stringsAsFactors=F)
#remove nulls
df_conditions<-df_conditions %>%
  filter(condition_name!="")
  
df_conditions$dummy<-1

#aggregate conditions by condition_name
condition_wordcount<-data.table(df_conditions)[,list( 
  cnt = sum(dummy)
), by='condition_name']

agg_condition_wordcount<-condition_wordcount %>%
  filter(cnt>1)

write.table(agg_condition_wordcount, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_condition_wordcount.txt", sep=""), sep = "|", row.names = FALSE)

#create data for wordcloud (total studies registered by rare condition)

x_rareconditions_recruiting<-subset.data.frame(x_conditions, subset=flag_rare_disease_match>0)

x_rareconditions_recruiting_name<- as.character(x_rareconditions_recruiting$condition_name)

rareconditions<-Corpus(VectorSource(x_rareconditions_recruiting_name))
rareconditions <- tm_map(rareconditions, content_transformer(tolower))
#conditions <- tm_map(conditions, removeNumbers)
rareconditions <- tm_map(rareconditions, removeWords, stopwords("english"))
rareconditions <- tm_map(rareconditions, removePunctuation)
rareconditions <- tm_map(rareconditions, stripWhitespace)
rareconditions <- tm_map(rareconditions, stemDocument)
rareconditions <- tm_map(rareconditions, removeWords, c("disord","diseas","syndrom"))

#converting conditions corpus into dataframe
df_rareconditions <- data.frame(condition_name=sapply(rareconditions, identity), 
                            stringsAsFactors=F)
#remove nulls
df_rareconditions<-df_rareconditions %>%
  filter(condition_name!="")

df_rareconditions$dummy<-1

#aggregate conditions by condition_name
rarecondition_wordcount<-data.table(df_rareconditions)[,list( 
  cnt = sum(dummy)
), by='condition_name']

agg_rarecondition_wordcount<-rarecondition_wordcount %>%
  filter(cnt>1)

write.table(agg_rarecondition_wordcount, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_rarecondition_wordcount.txt", sep=""), sep = "|", row.names = FALSE)
