#import libraries
library(dplyr)
library(stringr)
library(data.table)
library(sqldf)
library(httr)
library(jsonlite)

in_master_geolocation<-paste0(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_master_geolocation.txt")
master_geolocation<-read.csv(in_master_geolocation, header=TRUE, sep = "|", na.strings = "", stringsAsFactors = FALSE, nrows = -2, quote = "")

#set paths for data files
in_path_missing_geo<-paste(var_DIR_HOME, "Data/MISC/geo_missing.txt", sep="")

#reads the data files into dataframes
missing_geo<-read.csv(in_path_missing_geo, header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors=FALSE)

missing_geo_temp<-sqldf("select
                        miss.iso2,
                        miss.state,
                        miss.city,
                        master.latitude,
                        master.longitude
                        from missing_geo miss
                        left join master_geolocation master
                        on (miss.city=master.city and miss.state=master.state)
                        where master.longitude is not null
                        UNION
                        select
                        miss.iso2,
                        miss.state,
                        miss.city,
                        master.latitude,
                        master.longitude
                        from missing_geo miss
                        left join master_geolocation master
                        on (miss.city=master.city)
                        where master.longitude is not null
                        ")

geo_found<-sqldf("select distinct iso2, state, city, max(latitude) as latitude, max(longitude) as longitude
                 from missing_geo_temp
                 where iso2 is not null
                 group by iso2, state, city")

geo_found_2<-sqldf("select distinct iso2, state, city,'' as zip ,latitude, longitude
                 from geo_found
                 ")

#write missing geo code to txt file
write.table(geo_found_2, paste(var_DIR_HOME, "Data/MISC/geo_missing_found.txt", sep=""), sep = "|", row.names = FALSE, quote = FALSE)

