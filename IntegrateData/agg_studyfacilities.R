#the code adds geo information to x_facilities

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
in_master_geolocation<-paste0(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_master_geolocation.txt")
master_geolocation<-read.csv(in_master_geolocation, header=TRUE, sep = "|", na.strings = "", stringsAsFactors = FALSE, nrows = -2, quote = "")

x_facilities$city<-iconv(x_facilities$city, from = 'UTF-8', to = 'ASCII//TRANSLIT')
x_facilities$state<-iconv(x_facilities$state, from = 'UTF-8', to = 'ASCII//TRANSLIT')

#rm(list = ls())

x_facilities<-left_join(x_facilities, country_iso, by="country")
x_facilities<-left_join(x_facilities, state_codes, by=c("iso2","state"))

####### step 1
master_geolocation_zip<-data.table(master_geolocation)[,list( 
  latitude = max(latitude, na.rm = TRUE),
  longitude = max(longitude, na.rm = TRUE)
), by=c('iso2','zip')]

x_facilities_country_geo_zip1<-left_join(x_facilities,master_geolocation_zip, by=c("iso2",'zip'))

x_facilities_country_geo_zip1_matched<-subset(x_facilities_country_geo_zip1, subset = (is.na(latitude)!=TRUE &is.na(longitude)!=TRUE))
#####################

x_facilities_country_geo_zip2<-subset(x_facilities_country_geo_zip1, subset = (is.na(latitude)==TRUE &is.na(longitude)==TRUE))


master_geolocation_state<-subset(master_geolocation, subset = (!is.na(state)==TRUE))
master_geolocation_state<-data.table(master_geolocation_state)[,list( 
  latitude = max(latitude, na.rm = TRUE),
  longitude = max(longitude, na.rm = TRUE)
), by=c('iso2','state','city')]


x_facilities_country_geo_zip2 <- subset(x_facilities_country_geo_zip2, select = -c(latitude, longitude))
x_facilities_country_geo_zip2<-left_join(x_facilities_country_geo_zip2,master_geolocation_state, by=c("iso2",'state',"city"))


x_facilities_country_geo_zip2_matched<-subset(x_facilities_country_geo_zip2, subset = (is.na(latitude)!=TRUE &is.na(longitude)!=TRUE))

############
x_facilities_country_geo_zip3<-subset(x_facilities_country_geo_zip2, subset = (is.na(latitude)==TRUE &is.na(longitude)==TRUE))


master_geolocation_city<-data.table(master_geolocation)[,list( 
  latitude = max(latitude, na.rm = TRUE),
  longitude = max(longitude, na.rm = TRUE)
), by=c('iso2','city')]


x_facilities_country_geo_zip3 <- subset(x_facilities_country_geo_zip3, select = -c(latitude, longitude))
x_facilities_country_geo_zip3<-left_join(x_facilities_country_geo_zip3,master_geolocation_city, by=c("iso2","city"))


x_facilities_country_geo_zip3_matched<-subset(x_facilities_country_geo_zip3, subset = (is.na(latitude)!=TRUE &is.na(longitude)!=TRUE))
#######################
#facility records that did not find any geo match
x_facilities_country_geo_zip_missing<-subset(x_facilities_country_geo_zip3, subset = (is.na(latitude)==TRUE &is.na(longitude)==TRUE))
####################


#join the matched sets
x_facilities_final<-union_all(x_facilities_country_geo_zip1_matched,x_facilities_country_geo_zip2_matched)
x_facilities_final<-union_all(x_facilities_final,x_facilities_country_geo_zip3_matched)
x_facilities_final<-union_all(x_facilities_final,x_facilities_country_geo_zip_missing)


#write to txt file
write.table(x_facilities_final, paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_facilities.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)


