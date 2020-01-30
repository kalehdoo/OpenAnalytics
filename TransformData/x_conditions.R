library(dplyr)
library(lubridate)
library(stringr)
library(sqldf)

#set paths for data files
in_path_conditions<-paste(var_DIR_ACCT_HOME, "DATA/unzipSrcFiles/conditions.txt", sep="")
in_path_conditions_mesh<-paste(var_DIR_ACCT_HOME, "DATA/unzipSrcFiles/browse_conditions.txt", sep="")

#reads the data files into dataframes
conditions<-read.csv(in_path_conditions, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
conditions_mesh<-read.csv(in_path_conditions_mesh, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

x_conditions<- sqldf("select nct_id, name as condition_name  from conditions
                     UNION select nct_id, mesh_term as condition_name from conditions_mesh")


# x_condition_cnt<-sqldf("select count(distinct nct_id) from x_conditions")
# x_condition_cnt

#write to txt file
write.table(x_conditions, paste(var_DIR_ACCT_HOME, "DATA/warehouse/x_conditions.txt", sep=""), sep = "|", row.names = FALSE)





