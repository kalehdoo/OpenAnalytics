#import libraries
library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(data.table)



#read data
mv_studies_recruiting<-read.csv("data/mv_studies_recruiting.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = 50000, stringsAsFactors = FALSE)
mv_studies_recruiting<-subset.data.frame(mv_studies_recruiting, 
                                         subset = (is.na(nct_id)==FALSE),
                                         select=c("ID","Condition","Title","DataMonitoring","RareDisease","city","state","country","Region","ZipCode","StudyPhase","Sponsor","Facility","nct_id","latitude","longitude"))


ui <- navbarPage("Kalehdoo",
                 tabPanel("Study Finder",
                          fluidRow(
                            column(3, align="center", 
                                   textInput(inputId = "select_city_map",
                                               label = "Search City"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_condition_map",
                                                 label = "Search Condition"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_facility_map",
                                             label = "Search Facility"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_sponsor_map",
                                             label = "Search Sponsor"
                                   )
                            )
                          ),
                          fluidRow(
                            leafletOutput("plot_1014")
                          )
                          )
                 
  
)



server <- function(input, output) {
  
  #reactive dataset for maps with reduced columns
  mv_studies_recruiting_map<-reactive({
    #createleaflet plot 1014 based on reactive set
    subset(mv_studies_recruiting, 
           select = c("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
           subset=(casefold(city) %like%  casefold(input$select_city_map) & casefold(Condition) %like%  casefold(input$select_condition_map) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map)& casefold(Facility) %like%  casefold(input$select_facility_map)))
  })
  
  #createleaflet plot 1014
  #function to display labels
  f_labels_1014 <- function(){sprintf("<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting_map()$ID,mv_studies_recruiting_map()$country,mv_studies_recruiting_map()$state, mv_studies_recruiting_map()$city, mv_studies_recruiting_map()$Facility,mv_studies_recruiting_map()$Sponsor,mv_studies_recruiting_map()$Condition) %>% 
      lapply(htmltools::HTML)
  }
  output$plot_1014 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_map()) %>%
      setView(lng=-15.037721, lat=14.451703, zoom=2.2) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 popup = f_labels_1014(),
                 #label = f_labels_1014(),
                 clusterOptions = markerClusterOptions())
  })
  
}

shinyApp(ui, server)