#import libraries
library(dplyr)
library(stringr)
library(data.table)
library(sqldf)

#set paths for data files
in_path_x_facilities<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_facilities.txt", sep="")

#reads the data files into dataframes
x_facilities<-read.csv(in_path_x_facilities, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#create list of countries
facility_country<-sqldf("select distinct country from x_facilities")

#read master country iso codes
country_iso<-read.csv(paste0(var_DIR_HOME,"Data/MISC/master_country_iso.csv"), sep=",", na.strings = "", stringsAsFactors=FALSE)

#join to get iso codes for facility countries
facility_country_iso1<-sqldf("select distinct facility_country.country, country_iso.iso2, country_iso.iso3, country_iso.region
                            from facility_country
                            inner join country_iso 
                            on (lower(facility_country.country)=lower(country_iso.name)                            )
                                                       ")

facility_country_iso2<-sqldf("select distinct facility_country.country, country_iso.iso2, country_iso.iso3, country_iso.region
                            from facility_country
                            inner join country_iso 
                            on (lower(facility_country.country)=lower(country_iso.name2)
                            and length(country_iso.name2)>0)
                            ")

#merge the two sets
facility_country_iso<-merge.data.frame(facility_country_iso1, facility_country_iso2, no.dups = TRUE, all=TRUE)

#select distinct list
facility_country_iso_final<-sqldf("select distinct country, iso2, iso3, region
                            from facility_country_iso
                            where length(country)>0
                            and (length(iso2)>0 or length(iso3)>0)
                            ")

#stich country iso to facilities
agg_facilities_countryiso<-left_join(x_facilities, facility_country_iso_final, by="country")

#set paths for master geo data
in_master_geolocation<-paste0(var_DIR_HOME, "Data/MISC/master_geo_city.csv")
#reads the data files into dataframes
master_geolocation<-read.csv(in_master_geolocation, header=TRUE, sep = ",", na.strings = "", stringsAsFactors = FALSE, nrows = -2)

master_geolocation1<-sqldf("select distinct city, iso2, max(latitude) as latitude, max(longitude) as longitude
                          from master_geolocation
                          group by city, iso2")

#stich city lat and long to facilities
agg_facilities_citylatlong<-left_join(agg_facilities_countryiso, master_geolocation1, by=c("city","iso2"))

agg_facilities<-agg_facilities_citylatlong

#write to txt file
write.table(agg_facilities, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt", sep=""), sep = "|", row.names = FALSE)

