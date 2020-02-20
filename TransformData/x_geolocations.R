library(httr)
library(jsonlite)

#execute this load only one time



api_url<-"https://dev.virtualearth.net/REST/v1/Locations?CountryRegion=ZW&adminDistrict=-&locality=Beitbridge&postalCode=-&addressLine=-&key=Ao0MpNFIpYF8Nz79R4DgQkMnUDnUbSxqFZU-nco5fWPcsY0MFpd2t48mSi5Qw279"

get_results<-GET(api_url, type="basic")

get_results_text<-content(get_results,"text")

get_results_json<- fromJSON(get_results_text, flatten = TRUE)

get_results_final<-as.data.frame(get_results_json)

lat_long<-get_results_final[[5]][[1]][9]
lat_long_1<-lat_long[[1,1]]
latitude<-lat_long_1[1:1]
longitude<-lat_long_1[2:2]
lat_long
latitude
str(lat_long)

lat_long[[1]][[1]][1]
