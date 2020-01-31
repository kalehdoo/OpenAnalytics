#import libraries
library(dplyr)
library(stringr)
#library(data.table)
library(sqldf)

#set paths for data files
in_path_x_facilities<-paste(var_DIR_ACCT_HOME, "DATA/warehouse/x_facilities.txt", sep="")

#reads the data files into dataframes
x_facilities<-read.csv(in_path_x_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
geodata<-read.csv(paste(var_DIR_MISC_HOME,"worldcities.csv", sep=""))

geodata<- subset.data.frame(geodata, select=c("city_ascii","admin_name", "lat", "lng", "country", "iso2", "iso3")
                 )

colnames(geodata)[colnames(geodata)=="city_ascii"]<-"city"
colnames(geodata)[colnames(geodata)=="admin_name"]<-"state"

geodata1<-sqldf("select distinct lower(city) as city, lower(state) as state, lat as latitude, lng as longitude, lower(country) as country, iso2, iso3 
                from geodata 
                group by city, state, country")


x_facilities$city<-as.character(casefold(x_facilities$city))
x_facilities$state<-as.character(casefold(x_facilities$state))
x_facilities$country<-as.character(casefold(x_facilities$country))

agg_facilities<-left_join(x_facilities, geodata1, by =c("city", "state", "country"))


#validate 
#checkagg<-sqldf("select facility_id, count(*) from agg_facilities
#                group by facility_id
#                having count(*)>1")
#checkagg

#write to txt file
write.table(agg_facilities, paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_facilities.txt", sep=""), sep = "|", row.names = FALSE)

