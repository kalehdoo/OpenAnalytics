#import libraries
library(dplyr)

#set paths for data files
in_path_agg_year<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_year.txt", sep="")

#reads the data files into dataframes
agg_year<-read.csv(in_path_agg_year, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#create new df with required columns and filter condition
mv_year_Lst10Yr<-subset.data.frame(agg_year, 
                                 subset=common_year<=var_current_year & common_year >=var_last_year_10 
                                 
)

#write to txt file
write.table(mv_year_Lst10Yr,paste(var_DIR_HOME, "Data/ACCT/DATA/extracts/mv_year_Lst10Yr.txt", sep=""), sep = "|", row.names = FALSE)
