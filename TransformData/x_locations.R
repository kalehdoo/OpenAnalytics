library(dplyr)
library(sqldf)

#set paths for data files
in_locations1<-paste(var_DIR_HOME, "Data/MISC/worldcities.csv", sep="")

#reads the data files into dataframes
locations1<-read.csv(in_locations1, header=TRUE, sep = ",",na.strings = "NA", stringsAsFactors = FALSE, nrows = -100)

x_locations1<-sqldf("select distinct city, country, latitude, longitude from locations1
                    where length(city)>0")
rm(locations1)

#set paths for data files
in_locations2<-paste(var_DIR_HOME, "Data/MISC/World_Cities_Location_table.csv", sep="")

#reads the data files into dataframes
locations2<-read.csv(in_locations2, header=FALSE, sep = ";",na.strings = "NA", stringsAsFactors = FALSE ,nrows = -100)

colnames(locations2)<-c("id", "country", "city", "latitude", "longitude", "altitude")

x_locations2<-sqldf("select distinct city, country, latitude, longitude from locations2
                    where length(city)>0")
rm(locations2)

x_locations<-merge(x_locations1, x_locations2, all=TRUE)

x_locations<-sqldf("select distinct city, country, max(latitude) as latitude, max(longitude) as longitude 
                   from x_locations 
                   group by city,country")

#write to txt file
write.table(x_locations, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_locations.txt", sep=""), sep = "|", row.names = FALSE)

