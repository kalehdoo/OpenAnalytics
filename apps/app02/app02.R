#import libraries
library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(data.table)
library(DT)

#read data
mv_studies_recruiting<-readRDS("data/mv_studies_recruiting.rds") %>%
  filter(is.na(nct_id)==FALSE) %>%
  select("ID","Condition","Title","DataMonitoring","RareDisease","city","state","country","ZipCode","StudyPhase","Sponsor","Facility","Region","latitude","longitude","nct_id")

mv_studies_recruiting_tab<-mv_studies_recruiting %>% 
       select("ID","Condition","Title","DataMonitoring","RareDisease","city","state","country","ZipCode","StudyPhase","Sponsor","Facility")
       

mv_studies_recruiting_world<-mv_studies_recruiting %>%
  filter(casefold(Condition) %like% "coronavirus" & is.na(nct_id)==FALSE & is.na(latitude)==FALSE) %>%
  select("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude")

mv_studies_recruiting_US<-mv_studies_recruiting %>%
  filter(country=="United States" & is.na(nct_id)==FALSE & is.na(latitude)==FALSE) %>%
  select("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude")

mv_studies_recruiting_americas<-mv_studies_recruiting %>%
  filter(country!="United States" & Region=="Americas" & is.na(nct_id)==FALSE & is.na(latitude)==FALSE) %>%
  select("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude")


mv_studies_recruiting_europe<-mv_studies_recruiting %>%
  filter(Region=="Europe" & is.na(nct_id)==FALSE & is.na(latitude)==FALSE) %>%
  select("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude")

mv_studies_recruiting_asia<-mv_studies_recruiting %>%
  filter(Region=="Asia" & is.na(nct_id)==FALSE & is.na(latitude)==FALSE) %>%
  select("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude")

mv_studies_recruiting_oceania<-mv_studies_recruiting %>%
  filter(Region=="Oceania" & is.na(nct_id)==FALSE & is.na(latitude)==FALSE) %>%
  select("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude")

mv_studies_recruiting_africa<-mv_studies_recruiting %>%
  filter(Region=="Africa" & is.na(nct_id)==FALSE & is.na(latitude)==FALSE) %>%
  select("ID","city","state","country","Region","Condition","Sponsor","Facility","nct_id","latitude","longitude")


ui <- navbarPage("Kalehdoo",
                 tabPanel("Global-Coronavirus",
                          fluidRow(
                            column(3, align="left", 
                                   textInput(inputId = "select_city_map_world",
                                             label = "Search City"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_condition_map_world",
                                             label = "Search Condition"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_facility_map_world",
                                             label = "Search Facility"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_sponsor_map_world",
                                             label = "Search Sponsor"
                                   )
                            )
                          ),
                          fluidRow(
                            leafletOutput("plot_1020")
                          )
                 ),
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
                 ),
                 tabPanel("USA",
                          fluidRow(
                            column(3, align="left", 
                                   textInput(inputId = "select_city_map_us",
                                             label = "Search City"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_condition_map_us",
                                             label = "Search Condition"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_facility_map_us",
                                             label = "Search Facility"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_sponsor_map_us",
                                             label = "Search Sponsor"
                                   )
                            )
                          ),
                          fluidRow(
                            leafletOutput("plot_1018")
                          )
                 ),
                 tabPanel("Rest of Americas",
                          fluidRow(
                            column(3, align="left", 
                                   textInput(inputId = "select_city_map_americas",
                                             label = "Search City"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_condition_map_americas",
                                             label = "Search Condition"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_facility_map_americas",
                                             label = "Search Facility"
                                   )
                            ),
                            column(3, align="left", 
                                   textInput(inputId = "select_sponsor_map_americas",
                                             label = "Search Sponsor"
                                   )
                            )
                          ),
                          fluidRow(
                            leafletOutput("plot_1019")
                          )
                 ),
                 tabPanel("Table Details",
                          fluidRow(
                            DT::dataTableOutput("dt_recruitment_1021")
                          )
                 )
                 
                 
  
)



server <- function(input, output) {
  
  #reactive dataset for world maps with reduced columns
  mv_studies_recruiting_map_world<-reactive({
    #createleaflet plot 1020 based on reactive set
    subset(mv_studies_recruiting_world, 
           subset=(casefold(city) %like%  casefold(input$select_city_map_world) & casefold(Condition) %like%  casefold(input$select_condition_map_world) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map_world)& casefold(Facility) %like%  casefold(input$select_facility_map_world)))
  })
  
  #createleaflet plot 1020
  #function to display labels
  f_labels_1020 <- function(){sprintf("<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting_map_world()$ID,mv_studies_recruiting_map_world()$country,mv_studies_recruiting_map_world()$state, mv_studies_recruiting_map_world()$city, mv_studies_recruiting_map_world()$Facility,mv_studies_recruiting_map_world()$Sponsor,mv_studies_recruiting_map_world()$Condition) %>% 
      lapply(htmltools::HTML)
  }
  output$plot_1020 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_map_world()) %>%
      setView(lng=-4.055685, lat=41.294856, zoom=1.5) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 popup = f_labels_1020(),
                 clusterOptions = markerClusterOptions())
  })
  
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
           subset=(casefold(city) %like%  casefold(input$select_city_map_oceania) & casefold(Condition) %like%  casefold(input$select_condition_map_oceania) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map_oceania)& casefold(Facility) %like%  casefold(input$select_facility_map_oceania)))
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
  
  #reactive dataset for US maps
  mv_studies_recruiting_map_us<-reactive({
    #createleaflet plot 1017 based on reactive set
    subset(mv_studies_recruiting_US, 
           #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
           subset=(casefold(city) %like%  casefold(input$select_city_map_us) & casefold(Condition) %like%  casefold(input$select_condition_map_us) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map_us)& casefold(Facility) %like%  casefold(input$select_facility_map_us)))
  })
  
  #createleaflet plot 1018
  #function to display labels
  f_labels_1018 <- function(){sprintf("<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting_map_us()$ID,mv_studies_recruiting_map_us()$country,mv_studies_recruiting_map_us()$state, mv_studies_recruiting_map_us()$city, mv_studies_recruiting_map_us()$Facility,mv_studies_recruiting_map_us()$Sponsor,mv_studies_recruiting_map_us()$Condition) %>% 
      lapply(htmltools::HTML)
  }
  output$plot_1018 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_map_us()) %>%
      setView(lng=-100.437012, lat=47.650589, zoom=2.7) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 popup = f_labels_1018(),
                 clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for US maps
  mv_studies_recruiting_map_americas<-reactive({
    #createleaflet plot 1017 based on reactive set
    subset(mv_studies_recruiting_americas, 
           #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
           subset=(casefold(city) %like%  casefold(input$select_city_map_americas) & casefold(Condition) %like%  casefold(input$select_condition_map_americas) & casefold(Sponsor) %like%  casefold(input$select_sponsor_map_americas)& casefold(Facility) %like%  casefold(input$select_facility_map_americas)))
  })
  
  #createleaflet plot 1019
  #function to display labels
  f_labels_1019 <- function(){sprintf("<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting_map_americas()$ID,mv_studies_recruiting_map_americas()$country,mv_studies_recruiting_map_americas()$state, mv_studies_recruiting_map_americas()$city, mv_studies_recruiting_map_americas()$Facility,mv_studies_recruiting_map_americas()$Sponsor,mv_studies_recruiting_map_americas()$Condition) %>% 
      lapply(htmltools::HTML)
  }
  output$plot_1019 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_map_americas()) %>%
      setView(lng=-90.522713, lat=14.628434, zoom=2.0) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 popup = f_labels_1019(),
                 clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for maps with reduced columns
  output$dt_recruitment_1021<- renderDataTable({
  DT::datatable(mv_studies_recruiting_tab, filter = 'top',
                escape=FALSE,rownames = FALSE,
                options = list(lengthChange = FALSE)
  )
  })
  
}

shinyApp(ui, server)