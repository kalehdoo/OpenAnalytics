#import libraries
library(dplyr)
library(stringr)
library(data.table)
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

#create new columns to classify site into academic and hospitals
agg_facilities<-mutate(agg_facilities,
                       "flag_site_recruiting"=if_else(status=="Recruiting",1,0),
                       "flag_site_US"=if_else(casefold(country)=="united states",1,0),
                       "site_type"=if_else(facility_name!="Industry" &
                                             grepl("university",casefold(facility_name)) | 
                                             grepl("univerzi",casefold(facility_name)) |
                                             grepl("institut",casefold(facility_name)) |
                                             grepl("school",casefold(facility_name)) |
                                             grepl("campus",casefold(facility_name)) |
                                             grepl("college",casefold(facility_name)) |
                                             grepl("education",casefold(facility_name)) |
                                             grepl("academ",casefold(facility_name)) |
                                             grepl("univers",casefold(facility_name)), "Academic",
                                           if_else(facility_name!="Industry" &
                                                     grepl("hospital",casefold(facility_name))|
                                                     grepl("clinic",casefold(facility_name))|
                                                     grepl("medical center",casefold(facility_name))|
                                                     grepl("health center",casefold(facility_name))|
                                                     grepl("center",casefold(facility_name))|
                                                     grepl("centre",casefold(facility_name))|
                                                     grepl("h?pital",casefold(facility_name))|
                                                     grepl("hopital",casefold(facility_name)), "Hospital" ,
                                                   if_else(facility_name=="Industry","Industry",
                                                           if_else(facility_name=="U.S. Fed","U.S. Fed",
                                                                   if_else(facility_name=="[Redacted]","Redacted",
                                                                           if_else(facility_name=="NIH","NIH",
                                                                                   if_else(facility_name=="Other","Other"
                                                                                           ,"NA")))))))                      
)

#write to txt file
write.table(agg_facilities, paste(var_DIR_ACCT_HOME, "DATA/warehouse/agg_facilities.txt", sep=""), sep = "|", row.names = FALSE)

