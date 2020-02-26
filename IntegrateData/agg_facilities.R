#import libraries
library(dplyr)
library(stringr)
library(data.table)
library(sqldf)
library(httr)
library(jsonlite)

#set paths for data files
in_path_x_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facilities.txt", sep="")

#reads the data files into dataframes
x_facilities<-read.csv(in_path_x_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors=FALSE)
country_iso<-read.csv(paste0(var_DIR_HOME,"Data/MISC/master_country_iso.csv"), sep=",", na.strings = "", stringsAsFactors=FALSE)
state_codes<-read.csv(paste0(var_DIR_HOME,"Data/MISC/master_state_codes.csv"), sep=",", na.strings = "", stringsAsFactors=FALSE)
in_master_geolocation<-paste0(var_DIR_HOME, "Data/MISC/master_geolocation.txt")
master_geolocation<-read.csv(in_master_geolocation, header=TRUE, sep = "|", na.strings = "", stringsAsFactors = FALSE, nrows = -2)


x_facilities<-left_join(x_facilities, country_iso, by="country")
x_facilities<-left_join(x_facilities, state_codes, by=c("iso2","state"))


x_facilities_country_geo_zip1<-sqldf("select zip.facility_id,zip.nct_id,zip.status,zip.facility_name,zip.city,zip.state,zip.zip,zip.country,zip.flag_site_recruiting,zip.flag_site_US,zip.site_type,zip.iso2,zip.iso3,zip.statecode,zip.region,masterzip.latitude, masterzip.longitude 
                                     from x_facilities zip
                                     left join (select distinct iso2, zip, max(latitude) as latitude, max(longitude) as longitude
                                     from master_geolocation
                                     group by iso2, zip) masterzip
                                     on zip.iso2=masterzip.iso2 and zip.zip=masterzip.zip and zip.zip is not null")

x_facilities_country_geo_zip1_matched<-subset(x_facilities_country_geo_zip1, subset = (is.na(latitude)!=TRUE &is.na(longitude)!=TRUE))

x_facilities_country_geo_zip2<-subset(x_facilities_country_geo_zip1, subset = (is.na(latitude)==TRUE &is.na(longitude)==TRUE))

x_facilities_country_geo_zip2<-sqldf("select zip.facility_id,zip.nct_id,zip.status,zip.facility_name,zip.city,zip.state,zip.zip,zip.country,zip.flag_site_recruiting,zip.flag_site_US,zip.site_type,zip.iso2,zip.iso3,zip.statecode,zip.region,masterzip.latitude, masterzip.longitude 
                                     from x_facilities_country_geo_zip2 zip
                                     left join (select distinct iso2, state, city, max(latitude) as latitude, max(longitude) as longitude
                                     from master_geolocation
                                     group by iso2, state, city) masterzip
                                     on (zip.iso2=masterzip.iso2 and zip.state=masterzip.state and zip.city=masterzip.city)
                                     ")

x_facilities_country_geo_zip2_matched<-subset(x_facilities_country_geo_zip2, subset = (is.na(latitude)!=TRUE &is.na(longitude)!=TRUE))

x_facilities_country_geo_zip3<-subset(x_facilities_country_geo_zip2, subset = (is.na(latitude)==TRUE &is.na(longitude)==TRUE))

x_facilities_country_geo_zip3<-sqldf("select zip.facility_id,zip.nct_id,zip.status,zip.facility_name,zip.city,zip.state,zip.zip,zip.country,zip.flag_site_recruiting,zip.flag_site_US,zip.site_type,zip.iso2,zip.iso3,zip.statecode,zip.region ,masterzip.latitude, masterzip.longitude 
                                     from x_facilities_country_geo_zip3 zip
                                     left join (select distinct iso2, city, max(latitude) as latitude, max(longitude) as longitude
                                     from master_geolocation
                                     group by iso2, city) masterzip
                                     on (zip.iso2=masterzip.iso2 and zip.city=masterzip.city)
                                     ")

x_facilities_country_geo_zip3_matched<-subset(x_facilities_country_geo_zip3, subset = (is.na(latitude)!=TRUE &is.na(longitude)!=TRUE))

x_facilities_country_geo_zip4<-subset(x_facilities_country_geo_zip3, subset = (is.na(latitude)==TRUE &is.na(longitude)==TRUE))



x_facilities_country_geo_zip4<-sqldf("select zip.facility_id,zip.nct_id,zip.status,zip.facility_name,zip.city,zip.state,zip.zip,zip.country,zip.flag_site_recruiting,zip.flag_site_US,zip.site_type,zip.iso2,zip.iso3,zip.statecode,zip.region ,masterzip.latitude, masterzip.longitude 
                                     from x_facilities_country_geo_zip4 zip
                                     left join (select distinct city, max(latitude) as latitude, max(longitude) as longitude
                                     from master_geolocation
                                     group by city) masterzip
                                     on (zip.city=masterzip.city)
                                     ")

x_facilities_country_geo_zip4_matched<-subset(x_facilities_country_geo_zip4, subset = (is.na(latitude)!=TRUE &is.na(longitude)!=TRUE))

x_facilities_country_geo_zip_missing<-subset(x_facilities_country_geo_zip4, subset = (is.na(latitude)==TRUE &is.na(longitude)==TRUE))



#join the matched sets
x_facilities_final<-union_all(x_facilities_country_geo_zip1_matched,x_facilities_country_geo_zip2_matched)
x_facilities_final2<-union_all(x_facilities_final,x_facilities_country_geo_zip3_matched)
x_facilities_final3<-union_all(x_facilities_final2,x_facilities_country_geo_zip4_matched)
x_facilities_final4<-union_all(x_facilities_final3,x_facilities_country_geo_zip_missing)


#find missing lat long info for cities
city_latlong_missing<-sqldf("select distinct iso2, state, city
                            from x_facilities_country_geo_zip4
                            where (latitude is null and longitude is null) 
                            order by iso2
                            ")

city_latlong_missing$latitude<-0
city_latlong_missing$longitude<-0
city_latlong_missing$zip<-""

#API key obtained from Bing maps
api_key<-"Ao0MpNFIpYF8Nz79R4DgQkMnUDnUbSxqFZU-nco5fWPcsY0MFpd2t48mSi5Qw279"
city_latlong_missing$api_url<-paste0("https://dev.virtualearth.net/REST/v1/Locations?CountryRegion=",city_latlong_missing$iso2,"&adminDistrict=",city_latlong_missing$state,"&locality=",city_latlong_missing$city,"&postalCode=-&addressLine=-&key=",api_key)


#set paths for data files
path_geo_missing<-paste0(var_DIR_HOME, "Data/MISC/geo_missing.txt")
#create file if does not exists with header
if(!file.exists(path_geo_missing)) {
  file.create(path_geo_missing)
  write(paste("iso2","country","state","city","latitude","longitude","zip","api_url", sep="|"),path_geo_missing, append = FALSE)
}

#write missing geo code to txt file
write.table(city_latlong_missing, paste(var_DIR_HOME, "Data/MISC/geo_missing.txt", sep=""), sep = "|", row.names = FALSE)


#write to txt file
write.table(x_facilities_country_geo_zip3, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt", sep=""), sep = "|", row.names = FALSE)

