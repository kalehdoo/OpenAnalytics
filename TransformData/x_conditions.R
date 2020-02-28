library(dplyr)
library(lubridate)
library(stringr)
library(sqldf)

#set paths for data files
in_path_conditions<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/conditions.txt", sep="")
in_path_conditions_mesh<-paste(var_DIR_HOME, "Data/ACCT/DATA/unzipSrcFiles/browse_conditions.txt", sep="")
rare_disease_list<-read.csv(paste(var_DIR_HOME,"Data/MISC/list_rare_disease.txt", sep=""))


#reads the data files into dataframes
conditions<-read.csv(in_path_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
conditions_mesh<-read.csv(in_path_conditions_mesh, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)

colnames(conditions)[colnames(conditions)=="name"]<-"condition_name"
colnames(conditions_mesh)[colnames(conditions_mesh)=="mesh_term"]<-"condition_name"
conditions<-subset(conditions, select=c("nct_id","condition_name"))
conditions_mesh<-subset(conditions_mesh, select=c("nct_id","condition_name"))
x_conditions<-union_all(conditions,conditions_mesh)
x_conditions<-sqldf("select distinct nct_id,condition_name from x_conditions")

#check if condition matches with any of the rare disease in the list
x_conditions<- mutate(x_conditions,
                      flag_rare_disease=if_else((casefold(condition_name) %in% casefold(rare_disease_list$Name))== TRUE,1,0),
                      )

#write to txt file
write.table(x_conditions, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_conditions.txt", sep=""), sep = "|", row.names = FALSE)





