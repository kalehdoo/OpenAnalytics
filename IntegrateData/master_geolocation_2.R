library(httr)
library(jsonlite)
library(sqldf)
library(dplyr)
library(data.table)
library(digest)


#prepare master data - Execute only one time
#read facility data
##create list of cities from facilities to get longitude and latitude from Bing API
#set paths for data files
in_agg_facilities<-paste0(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt")

#reads the data files into dataframes
agg_facilities<-read.csv(in_agg_facilities, header=TRUE, sep = "|", na.strings = "", stringsAsFactors = FALSE, nrows = -2)

city_latlong<-sqldf("select distinct city, iso2 
                    from agg_facilities
                    where length(city)>0
                    and status='Recruiting'
                    ")

#add md5hash key
city_latlong$hashkey <- sapply(paste0(city_latlong$city,city_latlong$state, city_latlong$iso2), digest, algo="md5")

#identify rows mith missing lat and long - verify if already exits in master
#set paths for master data files for incremental
in_master_geolocation<-paste0(var_DIR_HOME, "Data/MISC/master_geo_city.csv")
#reads the data files into dataframes
master_geolocation<-read.csv(in_master_geolocation, header=TRUE, sep = ",", na.strings = "", stringsAsFactors = FALSE, nrows = -2)

#add md5hash key
master_geolocation$hashkey <- sapply(paste0(master_geolocation$city, master_geolocation$iso2), digest, algo="md5")


#find rows not present in master
city_latlong_missing<-setDT(city_latlong)[!master_geolocation, on="hashkey"]
#remove hash key column so that it can be added at the end later
city_latlong_missing<-subset(city_latlong_missing, select=-c(hashkey))


#check if nrows in missing present


#add md5hash key
city_latlong_missing$hashkey <- sapply(paste0(city_latlong_missing$city, city_latlong_missing$iso2), digest, algo="md5")


#prepare write and append log for warehouse load
dest_master_geolocation<-paste(var_DIR_HOME,"Data/MISC/master_geolocation.txt", sep="")
#create file if does not exists with header
if(!file.exists(dest_master_geolocation)) {
  file.create(dest_master_geolocation)
  write(paste("city","state","iso2","latitude","longitude","hashkey", sep="|"),dest_master_geolocation, append = TRUE)
}

#write (append) to txt file 
write.table(city_latlong_missing, paste(var_DIR_HOME, "Data/MISC/master_geolocation.txt", sep=""), sep = "|", row.names = FALSE, append = TRUE, col.names=FALSE)



