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

city_latlong<-sqldf("select distinct city, state, iso2 
                    from agg_facilities
                    where length(city)>0
                    and status='Recruiting'
                    ")

#convert na to - so as required in API calls for Bing
city_latlong$state <- ifelse(is.na(city_latlong$state),"-",city_latlong$state)
#city_latlong$zip <- ifelse(is.na(city_latlong$zip),"-",city_latlong$zip)
city_latlong$iso2 <- ifelse(is.na(city_latlong$iso2),"-",city_latlong$iso2)
#city_latlong$country <- ifelse(is.na(city_latlong$country),"-",city_latlong$country)

#prepare dynamic API URL and add to the table
#API key obtained from Bing maps
api_key<-"Ao0MpNFIpYF8Nz79R4DgQkMnUDnUbSxqFZU-nco5fWPcsY0MFpd2t48mSi5Qw279"
city_latlong$api_url<-paste0("https://dev.virtualearth.net/REST/v1/Locations?CountryRegion=",city_latlong$iso2,"&adminDistrict=",city_latlong$state,"&locality=",city_latlong$city,"&postalCode=-&addressLine=-&key=",api_key)

#add md5hash key
city_latlong$hashkey <- sapply(paste0(city_latlong$city,city_latlong$state, city_latlong$iso2), digest, algo="md5")

#identify rows mith missing lat and long - verify if already exits in master
#set paths for master data files for incremental
in_master_geolocation<-paste0(var_DIR_HOME, "Data/MISC/master_geolocation.txt")
#reads the data files into dataframes
master_geolocation<-read.csv(in_master_geolocation, header=TRUE, sep = "|", na.strings = "", stringsAsFactors = FALSE, nrows = -2)

#find rows not present in master
city_latlong_missing<-setDT(city_latlong)[!master_geolocation, on="hashkey"]
#remove hash key column so that it can be added at the end later
city_latlong_missing<-subset(city_latlong_missing, select=-c(hashkey))


#check if nrows in missing present

if (nrow(city_latlong_missing)>0) {
  #loop to get the latitude and longitude
  for (i in 1:nrow(city_latlong_missing)) {
    #get API results
    get_results<-GET(city_latlong_missing[i, "api_url"], type="basic")
    get_results_text<-content(get_results,"text")
    get_results_json<- fromJSON(get_results_text, flatten = TRUE)
    get_results_final<-as.data.frame(get_results_json)
    lat_long<-get_results_final[["resourceSets.resources"]][[1]]["point.coordinates"]
    #read first cell
    lat_long_1<-lat_long[1,1]
    latitude<-lat_long_1[[1:1]][1]
    longitude<-lat_long_1[[1:1]][2]
    
    #add the lat and long to the city table
    city_latlong_missing$latitude[i]<-latitude
    city_latlong_missing$longitude[i]<-longitude
  }
}

#remove api_url column
city_latlong_missing<-subset(city_latlong_missing, select=-c(api_url))

#add md5hash key
city_latlong_missing$hashkey <- sapply(paste0(city_latlong_missing$city,city_latlong_missing$state, city_latlong_missing$iso2), digest, algo="md5")


#prepare write and append log for warehouse load
dest_master_geolocation<-paste(var_DIR_HOME,"Data/MISC/master_geolocation.txt", sep="")
#create file if does not exists with header
if(!file.exists(dest_master_geolocation)) {
  file.create(dest_master_geolocation)
  write(paste("city","state","iso2","latitude","longitude","hashkey", sep="|"),dest_master_geolocation, append = TRUE)
}

#write (append) to txt file 
write.table(city_latlong_missing, paste(var_DIR_HOME, "Data/MISC/master_geolocation.txt", sep=""), sep = "|", row.names = FALSE, append = TRUE, col.names=FALSE)



