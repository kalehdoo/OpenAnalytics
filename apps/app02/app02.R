#import libraries
library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(data.table)



#read data
#mv_studies_recruiting<-read.csv("data/mv_studies_recruiting.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = 50000, stringsAsFactors = FALSE)
mv_studies_recruiting<-readRDS("data/mv_studies_recruiting.rds")
mv_studies_recruiting<-subset.data.frame(mv_studies_recruiting, 
                                          subset = (is.na(nct_id)==FALSE & is.na(latitude)==FALSE),
                                          select=c("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude"))
mv_studies_recruiting_americas<- select(filter(mv_studies_recruiting, Region=="Americas"),c("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude"))
#mv_studies_recruiting_us<- select(filter(mv_studies_recruiting, Region=="Americas" & country=="United States"),c("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude"))
mv_studies_recruiting_europe<- select(filter(mv_studies_recruiting, Region=="Europe"),c("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude"))
mv_studies_recruiting_asia<- select(filter(mv_studies_recruiting, Region=="Asia"),c("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude"))
mv_studies_recruiting_oceania<- select(filter(mv_studies_recruiting, Region=="Oceania"),c("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude"))
mv_studies_recruiting_africa<- select(filter(mv_studies_recruiting, Region=="Africa"),c("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude"))

rm(mv_studies_recruiting)

ui <- navbarPage("Kalehdoo",
                 tabPanel("Europe",
                          fluidRow(
                            column(3, align="left", 
                                   textInput(inputId = "select_city_map_europe",
                                               label = "Search City"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_condition_map_europe",
                                                 label = "Search Condition"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_facility_map_europe",
                                             label = "Search Facility"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_sponsor_map_europe",
                                             label = "Search Sponsor"
                                   )
                            )
                          ),
                          fluidRow(
                            leafletOutput("plot_1014")
                          )
                          ),
                 tabPanel("Asia",
                          fluidRow(
                            column(3, align="left", 
                                   textInput(inputId = "select_city_map_asia",
                                             label = "Search City"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_condition_map_asia",
                                             label = "Search Condition"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_facility_map_asia",
                                             label = "Search Facility"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_sponsor_map_asia",
                                             label = "Search Sponsor"
                                   )
                            )
                          ),
                          fluidRow(
                            leafletOutput("plot_1015")
                          )
                 ),
                 tabPanel("Africa",
                          fluidRow(
                            column(3, align="left", 
                                   textInput(inputId = "select_city_map_africa",
                                             label = "Search City"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_condition_map_africa",
                                             label = "Search Condition"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_facility_map_africa",
                                             label = "Search Facility"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_sponsor_map_africa",
                                             label = "Search Sponsor"
                                   )
                            )
                          ),
                          fluidRow(
                            leafletOutput("plot_1016")
                          )
                 ),
                 tabPanel("Oceania",
                          fluidRow(
                            column(3, align="left", 
                                   textInput(inputId = "select_city_map_oceania",
                                             label = "Search City"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_condition_map_oceania",
                                             label = "Search Condition"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_facility_map_oceania",
                                             label = "Search Facility"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_sponsor_map_oceania",
                                             label = "Search Sponsor"
                                   )
                            )
                          ),
                          fluidRow(
                            leafletOutput("plot_1017")
                          )
                 )
                 
                 
  
)



server <- function(input, output) {
  
  #reactive dataset for maps with reduced columns
  mv_studies_recruiting_map_europe<-reactive({
    #createleaflet plot 1014 based on reactive set
    subset(mv_studies_recruiting_europe, 
           #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
           subset=(casefold(city) %like%  casefold(input$select_city_map_europe) & casefold(Condition) %like%  casefold(input$select_condition_map_europe) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map_europe)& casefold(Facility) %like%  casefold(input$select_facility_map_europe)))
  })
  
  #createleaflet plot 1014
  #function to display labels
  f_labels_1014 <- function(){sprintf("<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting_map_europe()$ID,mv_studies_recruiting_map_europe()$country,mv_studies_recruiting_map_europe()$state, mv_studies_recruiting_map_europe()$city, mv_studies_recruiting_map_europe()$Facility,mv_studies_recruiting_map_europe()$Sponsor,mv_studies_recruiting_map_europe()$Condition) %>% 
      lapply(htmltools::HTML)
  }
  output$plot_1014 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_map_europe()) %>%
      setView(lng=41.44326, lat=52.73169, zoom=2.7) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 popup = f_labels_1014(),
                 #label = f_labels_1014(),
                 clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for asia maps with reduced columns
  mv_studies_recruiting_map_asia<-reactive({
    #createleaflet plot 1015 based on reactive set
    subset(mv_studies_recruiting_asia, 
           #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
           subset=(casefold(city) %like%  casefold(input$select_city_map_asia) & casefold(Condition) %like%  casefold(input$select_condition_map_asia) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map_asia)& casefold(Facility) %like%  casefold(input$select_facility_map_asia)))
  })
  
  #createleaflet plot 1015
  #function to display labels
  f_labels_1015 <- function(){sprintf("<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting_map_asia()$ID,mv_studies_recruiting_map_asia()$country,mv_studies_recruiting_map_asia()$state, mv_studies_recruiting_map_asia()$city, mv_studies_recruiting_map_asia()$Facility,mv_studies_recruiting_map_asia()$Sponsor,mv_studies_recruiting_map_asia()$Condition) %>% 
      lapply(htmltools::HTML)
  }
  output$plot_1015 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_map_asia()) %>%
      setView(lng=77.22445, lat=28.63576, zoom=2.8) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 popup = f_labels_1015(),
                 #label = f_labels_1014(),
                 clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for africa maps with reduced columns
  mv_studies_recruiting_map_africa<-reactive({
    #createleaflet plot 1016 based on reactive set
    subset(mv_studies_recruiting_africa, 
           #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
           subset=(casefold(city) %like%  casefold(input$select_city_map_africa) & casefold(Condition) %like%  casefold(input$select_condition_map_africa) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map_africa)& casefold(Facility) %like%  casefold(input$select_facility_map_africa)))
  })
  
  #createleaflet plot 1016
  #function to display labels
  f_labels_1016 <- function(){sprintf("<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting_map_africa()$ID,mv_studies_recruiting_map_africa()$country,mv_studies_recruiting_map_africa()$state, mv_studies_recruiting_map_africa()$city, mv_studies_recruiting_map_africa()$Facility,mv_studies_recruiting_map_africa()$Sponsor,mv_studies_recruiting_map_africa()$Condition) %>% 
      lapply(htmltools::HTML)
  }
  output$plot_1016 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_map_africa()) %>%
      setView(lng=30.81667, lat=3.05, zoom=2.6) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 popup = f_labels_1016(),
                 #label = f_labels_1014(),
                 clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for Oceania maps with reduced columns
  mv_studies_recruiting_map_oceania<-reactive({
    #createleaflet plot 1017 based on reactive set
    subset(mv_studies_recruiting_oceania, 
           #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
           subset=(casefold(city) %like%  casefold(input$select_city_map_africa) & casefold(Condition) %like%  casefold(input$select_condition_map_africa) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map_africa)& casefold(Facility) %like%  casefold(input$select_facility_map_africa)))
  })
  
  #createleaflet plot 1016
  #function to display labels
  f_labels_1017 <- function(){sprintf("<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting_map_oceania()$ID,mv_studies_recruiting_map_oceania()$country,mv_studies_recruiting_map_oceania()$state, mv_studies_recruiting_map_oceania()$city, mv_studies_recruiting_map_oceania()$Facility,mv_studies_recruiting_map_oceania()$Sponsor,mv_studies_recruiting_map_oceania()$Condition) %>% 
      lapply(htmltools::HTML)
  }
  output$plot_1017 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_map_oceania()) %>%
      setView(lng=133.8836, lat=-23.69748, zoom=3.0) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 popup = f_labels_1017(),
                 #label = f_labels_1014(),
                 clusterOptions = markerClusterOptions())
  })
  
}

shinyApp(ui, server)