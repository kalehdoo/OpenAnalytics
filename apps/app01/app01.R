#import libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(markdown)
library(DT)
library("wordcloud")
library(tm)
library(RColorBrewer)
library(leaflet)
library(htmltools)



#read data
mv_year_Lst10Yr<-read.csv("data/mv_year_Lst10Yr.txt", header=TRUE, sep = "|",na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
mv_studies_recruiting<-read.csv("data/mv_studies_recruiting.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
mv_studies_recruiting<-subset.data.frame(mv_studies_recruiting, subset = (is.na(latitude)==FALSE))
mv_studies_recruiting_loc<-read.csv("data/mv_studies_recruiting_loc.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
mv_studies_recruiting_loc<-subset.data.frame(mv_studies_recruiting_loc, subset = (is.na(latitude)==FALSE))
mv_studies_recruiting_loc_US<-subset.data.frame(mv_studies_recruiting_loc, subset = (iso3=="USA" & length(latitude)>0))
agg_Studiesbyconditions<-read.csv("data/agg_Studiesbyconditions.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
agg_condition_wordcount<-read.csv("data/agg_condition_wordcount.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)
agg_rarecondition_wordcount<-read.csv("data/agg_rarecondition_wordcount.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = -100, stringsAsFactors = FALSE)


ui <- navbarPage("Open Clinical Analytics",
                 navbarMenu("Recruitment",
                            tabPanel("Recruitment Summary",
                                     mainPanel(width=12,
                                               tabsetPanel(type="tabs",
                                                           tabPanel("dummy",
                                                                    fluidRow(
                                                                      leafletOutput("plot_1012xxx")
                                                                    )
                                                           ),
                                                           tabPanel("Summary",
                                                             fluidRow(
                                                               column(6, align="left",
                                                                      plotlyOutput("plot_1008")                                                               )
                                                               
                                                             )
                                                           )
                                                           )
                                               
                                     )
                                     
                            ),         
                   tabPanel("Find Studies",
                              mainPanel(width = 12,
                                        tabsetPanel(type="tabs",
                                                    tabPanel("Map",
                                                             fluidRow(
                                                               leafletOutput("plot_1014")
                                                             )
                                                             ),
                                                    tabPanel("Table",
                                                             fluidRow(
                                                               checkboxGroupInput(inputId = "select_dt_1005", 
                                                                                  label = "Select your Region to start", 
                                                                                  choices = unique(mv_studies_recruiting$Region),
                                                                                  inline = TRUE
                                                               )
                                                             ),
                                                             #display recriting studies data table
                                                             fluidRow(
                                                               DT::dataTableOutput("dt_recruitment_1005")
                                                             )
                                                             )
                                                             
                                                    )
                                                    
                                        )
                              ),
                   tabPanel("Geography",
                            mainPanel(width=12,
                                      tabsetPanel(type="tabs",
                                                  tabPanel("USA Map",
                                                           fluidRow(
                                                             leafletOutput("plot_1012")
                                                           )
                                                           
                                                  ),
                                                  tabPanel("USA Scatter",
                                                           fluidRow(
                                                             plotlyOutput("plot_1006")
                                                           )
                                                    
                                                  ),
                                                  tabPanel("World Map",
                                                           fluidRow(
                                                             leafletOutput("plot_1013")
                                                           )
                                                           
                                                  ),
                                                  tabPanel("World",
                                                           fluidRow(
                                                             column(12, align="center",
                                                               selectInput("select_map_1007", "Select Region",
                                                                           c("world", "europe","asia","africa","north america","south america")
                                                             )
                                                             )
                                                           ),
                                                           fluidRow(
                                                             plotlyOutput("plot_1007")
                                                           )
                                                  )
                                        
                                      )
                              
                            )
                     
                   ),
                   tabPanel("Conditions",
                            mainPanel(width=12,
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Studies",
                                                    fluidRow(
                                                      plotOutput("plot_1009")
                                                    )
                                                  ),
                                                  tabPanel("Conditioncount",
                                                    fluidRow(
                                                      plotOutput("plot_1010")
                                                    )
                                                  ),
                                                  tabPanel("Rare Disease",
                                                           fluidRow(
                                                             plotOutput("plot_1011")
                                                           )
                                                  )
                                        
                                      )
                                      
                            )
                            
                   )
                 )
                 ,
                 tabPanel("Trends",
                          mainPanel(width = 12,
                              fluidRow(
                                tags$h4("Historical trend for last 10 years")
                              ),        
                              fluidRow(
                                box(plotlyOutput("plot_1001", height = 200), title = "Studies Registered"),
                                box(plotlyOutput("plot_1002", height = 200), title = "Studies Started")
                              ),
                              #create box for plot 1001
                              fluidRow(
                                box(plotlyOutput("plot_1003", height = 200), title = "Studies Completed"),
                                box(plotlyOutput("plot_1004", height = 200), title = "Results Posted")
                              )
                            )
                          
                   
                 ),
                 tabPanel("Summary",
                          sidebarLayout(
                            sidebarPanel(width = 2,
                              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                              menuItem("Widgets", tabName = "widgets", icon = icon("th")),
                              checkboxInput("checkbox", "Choice A", value = TRUE),
                              radioButtons("radio", h3("Radio buttons"),
                                           choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                          "Choice 3" = 3),selected = 1)
                            ),
                            mainPanel(
                              fluidRow(
                                box(plotlyOutput("plot_10051", height = 200), title = "Studies Registered"),
                                box(plotlyOutput("plot_10061", height = 200), title = "Studies Started")
                              ),
                              #create box for plot 1001
                              fluidRow(
                                box(plotlyOutput("plot_10071", height = 200), title = "Studies Completed"),
                                box(plotlyOutput("plot_10081", height = 200), title = "Results Posted")
                              )
                            )
                          )
                          
                 ),
                 navbarMenu("Sponsor",
                            tabPanel("Sponsor1"
                              
                            ),
                            tabPanel("Sponsor2"
                              
                            )
                   
                 )
                 )
  

server <- function(input, output) {
  
  #createleaflet plot 1014
  labels_1014 <- sprintf("<strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Conditions: %s</strong>",mv_studies_recruiting$country,mv_studies_recruiting$state, mv_studies_recruiting$city, mv_studies_recruiting$Facility,mv_studies_recruiting$Condition) %>% 
    lapply(htmltools::HTML)
  
  output$plot_1014 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting) %>%
      setView(lng=-3.727919, lat=40.463666, zoom=2) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 label = labels_1014,
                 clusterOptions = markerClusterOptions())
  })
  
  #createleaflet plot 1013
  labels_1013 <- sprintf("<strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Recruiting Studies: %s</strong>",mv_studies_recruiting_loc$state, mv_studies_recruiting_loc$city, mv_studies_recruiting_loc$cnt_studies) %>% 
    lapply(htmltools::HTML)
  
  output$plot_1013 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_loc) %>%
      setView(lng=-3.727919, lat=40.463666, zoom=2) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 label = labels_1013,
                 clusterOptions = markerClusterOptions())
  })
  
  #createleaflet only US plot 1012
  labels_1012 <- sprintf("<strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Recruiting Studies: %s</strong>",mv_studies_recruiting_loc_US$state, mv_studies_recruiting_loc_US$city, mv_studies_recruiting_loc_US$cnt_studies) %>% 
    lapply(htmltools::HTML)
  
  output$plot_1012 <- renderLeaflet({
    leaflet(data=mv_studies_recruiting_loc_US) %>%
      setView(lng=-97.922211, lat=39.381266, zoom=4.4) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~longitude, ~latitude,
                 label = labels_1012,
                 clusterOptions = markerClusterOptions())
  })

  
  #Create wordcloud plot 1011
  output$plot_1011 <- renderPlot({
    wordcloud(words=agg_rarecondition_wordcount$condition_name,
              freq = agg_rarecondition_wordcount$cnt,
              scale = c(3,0.5),
              min.freq = 1, max.words=500,
              random.order=FALSE,
              random.color = FALSE,
              fixed.asp = FALSE,
              rot.per=0,
              colors=brewer.pal(8, "Dark2"))
  })
  
  #Create wordcloud plot 1010
  output$plot_1010 <- renderPlot({
    wordcloud(words=agg_condition_wordcount$condition_name,
              freq = agg_condition_wordcount$cnt,
              scale = c(3,0.5),
              min.freq = 1, max.words=500,
              random.order=FALSE,
              random.color = FALSE,
              fixed.asp = FALSE,
              rot.per=0,
              colors=brewer.pal(8, "Dark2"))
  })
  
  #Create wordcloud plot 1009
  output$plot_1009 <- renderPlot({
    wordcloud(words=agg_Studiesbyconditions$condition_name,
              freq = agg_Studiesbyconditions$cnt_recruitingstudies,
              scale = c(3,0.5), 
              min.freq = 1, max.words=500,
              random.order=FALSE,
              random.color = FALSE,
              rot.per=0,
              fixed.asp = FALSE,
              #textStemming=TRUE,
              #excludeWords=c("Disease","Syndrome"),
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  #create plot 1008
  output$plot_1008 <- renderPlotly({
      mv_studies_recruiting %>%
      group_by(Region) %>%
      summarise(cnt=length(unique((ID)))) %>%
      plot_ly(values=~cnt, labels=~factor(Region), type='pie')%>%
      layout(title="Recruiting Studies by Region",
             legend=list(orientation="h"))
      
  })
  
  #create plot 1006
  output$plot_1006 <- renderPlotly(
    {
      plot_geo(mv_studies_recruiting_loc_US, lat = ~latitude, lon = ~longitude) %>%
        add_markers(
          text = ~paste(city, state, paste("Studies:", cnt_studies), sep = "<br />"),
          color = ~cnt_studies, symbol = I("circle"), size = ~cnt_studies*4, hoverinfo = "text"
        ) %>%
        hide_colorbar() %>%
        layout(
          title = 'Recruiting Studies in USA', 
            geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showland = TRUE,
            landcolor = toRGB("gray95"),
            subunitcolor = toRGB("gray85"),
            countrycolor = toRGB("gray85"),
            countrywidth = 0.5,
            subunitwidth = 1.0
          )
        )
    })
  
  #cteate plot 1007
  output$plot_1007 <- renderPlotly(
    {
      plot_geo(mv_studies_recruiting_loc, lat = ~latitude, lon = ~longitude) %>%
        add_markers(
          text = ~paste(city, iso3, paste("Studies:", cnt_studies), sep = "<br />"),
          color = ~cnt_studies, symbol = I("circle"), size = ~cnt_studies*4, hoverinfo = "text"
        ) %>%
        hide_colorbar() %>%
        layout(
          title = 'Recruiting Studies Across Globe', 
          geo = list(
            scope = input$select_map_1007,
            projection = list(type = 'natural earth'),
            showland = TRUE,
            landcolor="#DEDEDE",
            showocean=TRUE,
            oceancolor="#A0CFDF",
            showcountries=TRUE,
            subunitcolor = "#1A82C7",
            countrycolor = "#115380",
            countrywidth = 0.5,
            subunitwidth = 0.8
          )
        )
    })
  

  
  #datatable for recruitment
  output$dt_recruitment_1005<- renderDataTable(
    {
      mv_studies_recruiting_tab<-subset(mv_studies_recruiting, 
                                        select = c("ID","Condition","Title","DataMonitoring","RareDisease","city","state","country","ZipCode","StudyPhase","Sponsor","Facility"),
                                        subset=Region==input$select_dt_1005
                                        )
      
      DT::datatable(mv_studies_recruiting_tab, filter = 'top',
                    escape=FALSE,rownames = FALSE,
                    options = list(lengthChange = FALSE),callback=JS("
           //hide column filters for the first column
          $.each([0], function(i, v) {
                $('input.form-control').eq(v).hide()
              });")
                    )
    }
  )
   
  #cteate plot 1001
  output$plot_1001 <- renderPlotly(
    {plot_ly(mv_year_Lst10Yr, x=~common_year, y=~cnt_registered, type='bar', text=~cnt_registered, textposition = 'auto')
      })
  
  #cteate plot 1002
  output$plot_1002 <- renderPlotly(
    {plot_ly(mv_year_Lst10Yr, x=~common_year, y=~cnt_started, type='bar', text=~cnt_started, textposition = 'auto')
    })
  
  #cteate plot 1003
  output$plot_1003 <- renderPlotly(
    {plot_ly(mv_year_Lst10Yr, x=~common_year, y=~cnt_completed, type='bar', text=~cnt_completed, textposition = 'auto')
    })
  
  #cteate plot 1004
  output$plot_1004 <- renderPlotly(
    {plot_ly(mv_year_Lst10Yr, x=~common_year, y=~cnt_resultsPosted, type='bar', text=~cnt_resultsPosted, textposition = 'auto')
    })
}

shinyApp(ui, server)