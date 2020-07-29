#Run only once per project to create the master
#import libraries
library(dplyr)
library(stringr)
library(data.table)


in_master_geolocation<-paste0(var_DIR_HOME, "Data/MISC/master_geo_city.csv")
in_master_geolocation_US<-paste0(var_DIR_HOME, "Data/MISC/master_geo_city_US.csv")
#reads the data files into dataframes
master_geolocation<-read.csv(in_master_geolocation, header=TRUE, sep = ",", na.strings = c(""), stringsAsFactors = FALSE, nrows = -2)
master_geolocation_US<-read.csv(in_master_geolocation_US, header=TRUE, sep = ",", na.strings = "", stringsAsFactors = FALSE, nrows = -2)

master_geolocation$city<-str_remove_all(master_geolocation$city, "[',]")
master_geolocation$state<-str_remove_all(master_geolocation$state, "[',]")

master_geolocation_clean<-data.table(master_geolocation)[,list( 
  latitude = max(latitude, na.rm = TRUE),
  longitude = max(longitude, na.rm = TRUE)
), by=c('iso2','state','city','zip')]

master_geolocationUS_clean<-data.table(master_geolocation_US)[,list( 
  latitude = max(latitude, na.rm = TRUE),
  longitude = max(longitude, na.rm = TRUE)
), by=c('iso2','state','city','zip')]

master_geolocationUS_clean$zip<-as.character(master_geolocationUS_clean$zip)

#union two data sets
master_geolocation_final<-union_all(master_geolocation_clean,master_geolocationUS_clean)

#take out the unique set
master_geolocationUS_final2<-data.table(master_geolocation_final)[,list( 
  latitude = max(latitude, na.rm = TRUE),
  longitude = max(longitude, na.rm = TRUE)
), by=c('iso2','state','city','zip')]


##############################Get the data from second datafile only take country and state
in_geo2<-paste0(var_DIR_HOME, "Data/MISC/worldcitiespop.csv")
geolocation2<-read.csv(in_geo2, header=TRUE, sep = ",", na.strings = "", stringsAsFactors = FALSE, nrows = -2)
geolocation2$iso2<-casefold(geolocation2$Country, upper = TRUE)
geolocation2$city2<-iconv(geolocation2$AccentCity, from = 'UTF-8', to = 'ASCII//TRANSLIT')
geolocation2$city2<-str_remove_all(geolocation2$city2, "['`,]")
geolocation2$state<-""
geolocation2$zip<-""

geolocation2_final2<-data.table(geolocation2)[,list( 
  latitude = max(Latitude, na.rm = TRUE),
  longitude = max(Longitude, na.rm = TRUE)
), by=c('iso2','city2')]

colnames(geolocation2_final2)[colnames(geolocation2_final2)=="city2"]<-"city"
geolocation2_final2$state<-""
geolocation2_final2$zip<-""

geolocation2_final2<-subset(geolocation2_final2, select = c("iso2","state","city","zip","latitude","longitude"))


#union with master geolocation
master_geolocation_union<-union_all(master_geolocationUS_final2,geolocation2_final2)

master_union_final<-data.table(master_geolocation_union)[,list( 
  latitude = max(latitude, na.rm = TRUE),
  longitude = max(longitude, na.rm = TRUE)
), by=c('iso2','state','city','zip')]

master_union_final$latitude<-as.character(master_union_final$latitude)
master_union_final$longitude<-as.character(master_union_final$longitude)

write.table(master_union_final, paste0(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_master_geolocation.txt"), sep = "|", row.names = FALSE, quote = FALSE)


