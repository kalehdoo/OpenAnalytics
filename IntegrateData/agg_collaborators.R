#import libraries
library(dplyr)
library(stringr)
library(data.table)


#set paths for data files
in_path_x_collaborators<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_collaborators.txt", sep="")

#reads the data files into dataframes
collaborators<-read.csv(in_path_x_collaborators, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#agg facilities at study level
agg_collaborators_by_study<-data.table(collaborators)[,list( 
  cnt_colab_NIH = sum(ifelse(agency_class == "NIH", 1, 0), na.rm = TRUE),
  cnt_colab_Ind = sum(ifelse(agency_class == "Industry", 1, 0), na.rm = TRUE),
  cnt_colab_USFed = sum(ifelse(agency_class == "U.S. Fed", 1, 0), na.rm = TRUE),
  cnt_colab_Hosp = sum(ifelse(sponsor_type == "Hospital", 1, 0), na.rm = TRUE),
  cnt_colab_Acad = sum(ifelse(sponsor_type == "Academic", 1, 0), na.rm = TRUE),
  cnt_colab_Other = sum(ifelse(sponsor_type == "Other", 1, 0), na.rm = TRUE),
  cnt_colab_Total = sum(ifelse(is.na(sponsor_type) == FALSE, 1, 0), na.rm = TRUE)
), by='nct_id']

#write to txt file
write.table(agg_collaborators_by_study, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_collaborators_by_study.txt", sep=""), sep = "|", row.names = FALSE)
