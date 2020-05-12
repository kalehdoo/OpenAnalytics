library("rvest")
library(stringr)

#HOME to store path of home dir
assign("var_DIR_HOME", "C:/Users/ranamanohar/Documents/GitHub/OpenAnalytics/", envir = .GlobalEnv)


url <- "https://www.worldometers.info/world-population/population-by-country/"

world_population <- url %>%
  xml2::read_html() %>%
  html_nodes(xpath='//*[@id="example2"]') %>%
  html_table()

#str(world_population)

#world_population1<-world_population[[1]]

world_population1<-as.data.frame(world_population[[1]])

#extract year from 3rd column
pop_year_col<-colnames(world_population1)[3]
start_pos<-stringr::str_locate(pop_year_col,"\\(")[1]
pop_year<-stringr::str_sub(pop_year_col, start_pos+1,start_pos+4)

world_population1$yearNm<-pop_year
#rename column names by position to better names
colnames(world_population1)[1]<-"rowId"
colnames(world_population1)[2]<-"countryName"
colnames(world_population1)[3]<-"population"

#write to txt file
write.table(world_population1,paste(var_DIR_HOME, "Data/MISC/world_population.txt", sep=""), sep = "|", row.names = FALSE)

