#import libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(markdown)
library(DT)
library(RColorBrewer)
library(leaflet)
library(htmltools)
library(data.table)
library(wordcloud)
library(shinythemes)
library(rsample)
library(lubridate)
library(mongolite)

#read data
mv_year_Lst10Yr <-
  read.csv(
    "data/mv_year_Lst10Yr.txt",
    header = TRUE,
    sep = "|",
    na.strings = "NA",
    nrows = -100,
    stringsAsFactors = FALSE
  )

mv_month <-
  read.csv(
    "data/agg_month.txt",
    header = TRUE,
    sep = "|",
    na.strings = "NA",
    nrows = -100,
    stringsAsFactors = FALSE
  )

mv_studies_recruiting_loc <-
  read.csv(
    "data/mv_studies_recruiting_loc.txt",
    header = TRUE,
    sep = "|",
    na.strings = "NA",
    nrows = 5000,
    stringsAsFactors = FALSE
  )
mv_studies_recruiting_loc <-
  subset.data.frame(mv_studies_recruiting_loc, subset = (is.na(latitude) ==
                                                           FALSE))
mv_studies_recruiting_loc_US <-
  subset.data.frame(mv_studies_recruiting_loc, subset = (iso3 == "USA" &
                                                           length(latitude) > 0))
agg_Studiesbyconditions <-
  read.csv(
    "data/agg_conditions_recruiting.txt",
    header = TRUE,
    sep = "|",
    na.strings = "NA",
    nrows = 5000,
    stringsAsFactors = FALSE
  )

#create connection to cloud database
#conn_mv_studies_recruiting <- mongo(collection = "mv_studies_recruiting", 
#                      db="openanalytics",
#                      url = "mongodb+srv://kalehdoo_user:Aquano139182@kalehdoo-gx7df.mongodb.net/test"
#)

#import recruiting data
#mv_studies_recruiting<-read.csv("data/mv_studies_recruiting.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = 5000, stringsAsFactors = FALSE)
mv_studies_recruiting <- readRDS("data/mv_studies_recruiting.rds")

#mv_studies_recruiting<-conn_mv_studies_recruiting$find('{}')
#mv_studies_recruiting<-as_tibble(mv_studies_recruiting)

mv_studies_recruiting <-
  subset.data.frame(mv_studies_recruiting) %>%
  sample_frac(0.1, replace = FALSE)

#recruiting data without ID
mv_studies_recruiting_table <-
  subset.data.frame(
    mv_studies_recruiting,
    select = c(
      "nct_id",
      "Condition",
      "Title",
      "DataMonitoring",
      "RareDisease",
      "Region",
      "city",
      "state",
      "country",
      "ZipCode",
      "StudyPhase",
      "Sponsor",
      "Facility",
      "StudyType",
      "PostedYrMon",
      "StartYrMon",
      "RegToStartDays",
      "AgencyClass",
      "SponsorType",
      "CntConditions",
      "CntSites",
      "InterventionType"
    )
  )

#recruiting data at study level
mv_recruiting_studylevel <-
  data.table(mv_studies_recruiting)[, list(
    totalFacilities = unique(CntSites),
    cnt_cities = length(unique(city)),
    cnt_countries = length(unique(country)),
    cnt_recFacilities = length(unique(Facility)),
    Condition = unique(Condition),
    Title = unique(Title),
    DataMonitoring = unique(DataMonitoring),
    RareDisease = unique(RareDisease),
    StudyPhase = unique(StudyPhase),
    Sponsor = unique(Sponsor),
    StudyType = unique(StudyType),
    PostedYrMon = unique(PostedYrMon),
    StartYrMon = unique(StartYrMon),
    RegToStartDays = unique(RegToStartDays),
    AgencyClass = unique(AgencyClass),
    SponsorType = unique(SponsorType),
    CntConditions = unique(CntConditions),
    InterventionType = unique(InterventionType)
  ), by = 'nct_id']

#recruiting data at sponsor level
rec_sponsors <- mv_studies_recruiting %>%
  group_by(Sponsor) %>%
  summarise(
    AgencyClass = unique(AgencyClass),
    SponsorType = unique(SponsorType),
    cnt_studies = length(unique(nct_id)),
    cnt_dataMonitor = sum(ifelse(DataMonitoring == "Yes", 1, 0)),
    cnt_rareDisease = sum(ifelse(RareDisease == "Yes", 1, 0)),
    median_regToStartDays = median(RegToStartDays),
    cnt_cities = length(unique(city)),
    cnt_countries = length(unique(country)),
    cnt_conditions = sum(unique(CntConditions)),
    cnt_recFacilities = length(unique(Facility)),
    TotalFacilities = sum(unique(CntSites))
  )

#####################


ui <- navbarPage(
  title = "Kalehdoo",
  windowTitle = "Kalehdoo Analytics",
  theme = shinytheme("united"),
  collapsible = TRUE,
  #Landing Home page starts
  tabPanel(title = "Home",
           fluidRow((
             h4("Clinical Analytics Platform", style = "margin-top:0px;margin-left:5%; margin-right:5%")
           )),
           fluidRow(style = "margin-top:0px;margin-left:1%; margin-right:1%",
             p("Clinical Analytics provides insights into the clinical research industry. 
               The market and competitive intelligence gained from the insights can benefit Sponsors, CROs, Industry Analysts, Non-Profit, Government and Special Interest Organizations explore avenues for future growth"
             ),
             p("Sponsors",
               tags$ul(
                 tags$li("Design Clinical Study"),
                 tags$li("Reduce Redundant Studies"),
                 tags$li("Clinical Site Selection"),
                 tags$li("Design Study Endpoints"),
                 tags$li("Explore Collaboration Opportunities"),
                 tags$li("Competitive Intelligence"),
                 tags$li("Clinical Trends"),
                 tags$li("Clinical Industry Landscape")
               )
             ),
             p("Physicians & Patients",
               tags$ul(
                 tags$li("Participation Opportunities"),
                 tags$li("Recruiting Studies")
               )
             ),
             p("Market & Industry Analysts",
               tags$ul(
                 tags$li("Current Trends"),
                 tags$li("Market Intelligence")
               )
             ),
             p("Special Interest Organizations",
               tags$ul(
                 tags$li("Global Clinical Activity"),
                 tags$li("Demographics")
               )
             )
           ),
           fluidRow((
             p(
               "For any queries, feel free to visit  "
               ,
               tags$a(href = "https://oakbloc.com", "Oakbloc Technologies", target =
                        "_blank"),
               style = "margin-top:0px;margin-left:1%; margin-right:1%"
             )
           ))),
  #Recruitment Dashboard starts here
  navbarMenu(
    "Recruitment",
    tabPanel("Summary",
             mainPanel(
               width = 12,
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Summary",
                   fluidRow(
                     style = "height:50px;background-color: orange; padding: 5px; border-style: solid; border-width: 1px;",
                     column(
                       12,
                       shinydashboard::valueBoxOutput("box_studies", width = 2),
                       shinydashboard::valueBoxOutput("box_sponsors", width = 2),
                       shinydashboard::valueBoxOutput("box_countries", width = 2),
                       shinydashboard::valueBoxOutput("box_cities", width = 2),
                       shinydashboard::valueBoxOutput("box_facilities", width = 2)
                     )
                   ),
                   
                   fluidRow(
                     style = "padding: 5px; border-style: solid; border-width: 1px;",
                     column(
                       6,
                       align = "left",
                       style = "border-right: 1px solid",
                       plotlyOutput("plot_1008")
                     ),
                     column(
                       6,
                       align = "left",
                       style = "border-left: 1px solid;",
                       plotlyOutput("plot_1008_2")
                     )
                   ),
                   fluidRow(
                     style = "padding: 5px; border-style: solid; border-width: 1px;",
                     column(
                       6,
                       align = "left",
                       style = "border-right: 1px solid",
                       plotlyOutput("plot_1008_3")
                     ),
                     column(
                       6,
                       align = "left",
                       style = "border-left: 1px solid;",
                       plotlyOutput("plot_1008_4")
                     )
                   )
                 ),
                 tabPanel(
                   "Summary 2",
                   fluidRow(
                     style = "padding: 5px; border-style: solid; border-width: 1px;",
                     column(
                       6,
                       align = "left",
                       style = "border-right: 1px solid",
                       plotlyOutput("plot_1008_5")
                     ),
                     column(
                       6,
                       align = "left",
                       style = "border-left: 1px solid;",
                       plotlyOutput("plot_1008_6")
                     )
                   ),
                   fluidRow(
                     style = "padding: 5px; border-style: solid; border-width: 1px;",
                     column(
                       6,
                       align = "left",
                       style = "border-right: 1px solid",
                       plotlyOutput("plot_1008_31")
                     ),
                     column(
                       6,
                       align = "left",
                       style = "border-left: 1px solid;",
                       plotlyOutput("plot_1008_41")
                     )
                   )
                 ),
                 #Summary tab 2 ends here
                 #summary tab ends here
                 tabPanel("Data",
                          fluidRow(DT::dataTableOutput("dt_recruitment")))
               )
             )),
    tabPanel("Study Finder",
             mainPanel(
               width = 12,
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "World Map",
                   fluidRow(
                     column(
                       3,
                       align = "left",
                       textInput(inputId = "select_city_map",
                                 label = "Search City")
                     ),
                     column(
                       3,
                       align = "left",
                       textInput(inputId = "select_condition_map",
                                 label = "Search Condition")
                     ),
                     column(
                       3,
                       align = "left",
                       textInput(inputId = "select_facility_map",
                                 label = "Search Facility")
                     ),
                     column(
                       3,
                       align = "left",
                       textInput(inputId = "select_sponsor_map",
                                 label = "Search Sponsor")
                     )
                   ),
                   fluidRow(leafletOutput("plot_1014"))
                 ),
                 tabPanel(
                   "Table Details",
                   fluidRow(
                     column(
                       3,
                       align = "left",
                       textInput(inputId = "select_city_tab",
                                 label = "Search City")
                     ),
                     column(
                       3,
                       align = "left",
                       textInput(inputId = "select_condition_tab",
                                 label = "Search Condition")
                     ),
                     column(
                       3,
                       align = "left",
                       textInput(inputId = "select_facility_tab",
                                 label = "Search Facility")
                     ),
                     column(
                       3,
                       align = "left",
                       textInput(inputId = "select_sponsor_tab",
                                 label = "Search Sponsor")
                     )
                   ),
                   #display recriting studies data table
                   fluidRow(DT::dataTableOutput("dt_recruitment_1005"))
                 )
                 
               )
               
             )),
    tabPanel("Geography",
             mainPanel(
               width = 12,
               tabsetPanel(
                 type = "tabs",
                 tabPanel("USA States",
                          fluidRow(plotlyOutput("plot_1015"))),
                 tabPanel("USA Cities",
                          fluidRow(leafletOutput("plot_1012"))),
                 tabPanel("USA Scatter",
                          fluidRow(plotlyOutput("plot_1006"))),
                 tabPanel("World Cities",
                          fluidRow(leafletOutput("plot_1013"))),
                 tabPanel("World Scatter",
                          fluidRow(column(
                            12, align = "center",
                            selectInput(
                              "select_map_1007",
                              "Select Region",
                              c(
                                "world",
                                "europe",
                                "asia",
                                "africa",
                                "north america",
                                "south america"
                              )
                            )
                          )),
                          fluidRow(plotlyOutput("plot_1007"))),
                 tabPanel("World Countries",
                          fluidRow(plotlyOutput("plot_1016")))
                 
               )
               
             )),
    tabPanel("Sponsor",
             mainPanel(
               width = 12,
               tabsetPanel(
                 type = "tabs",
                 tabPanel(
                   "Summary",
                   fluidRow(
                     style = "height:50px;background-color: orange; padding: 5px; border-style: solid; border-width: 1px;",
                     column(
                       12,
                       shinydashboard::valueBoxOutput("box_studies1", width = 2),
                       shinydashboard::valueBoxOutput("box_sponsors1", width = 2),
                       shinydashboard::valueBoxOutput("box_countries1", width = 2),
                       shinydashboard::valueBoxOutput("box_cities1", width = 2),
                       shinydashboard::valueBoxOutput("box_facilities1", width = 2)
                     )
                   ),
                   
                   fluidRow(
                     style = "padding: 5px; border-style: solid; border-width: 1px;",
                     column(
                       6,
                       align = "left",
                       style = "border-right: 1px solid",
                       plotlyOutput("plot_1030_1")
                     ),
                     column(
                       6,
                       align = "left",
                       style = "border-left: 1px solid;",
                       plotlyOutput("plot_1030_3")
                     )
                   ),
                   fluidRow(
                     style = "padding: 5px; border-style: solid; border-width: 1px;",
                     column(
                       6,
                       align = "left",
                       style = "border-right: 1px solid",
                       plotlyOutput("plot_1030_4")
                     ),
                     column(
                       6,
                       align = "left",
                       style = "border-left: 1px solid;",
                       plotlyOutput("plot_1030_2")
                     )
                   )
                   
                 ),
                 tabPanel("Sponsor Data",
                          fluidRow(
                            DT::dataTableOutput("dt_sponsor_recruitment")
                          ))
               )
             )),
    tabPanel("Conditions",
             mainPanel(
               width = 12,
               tabsetPanel(type = "tabs",
                           tabPanel("Studies",
                                    fluidRow(
                                      plotOutput("plot_1009")
                                    )))
               
             ))
  )
  ,
  navbarMenu(
    "Trends",
    tabPanel(
      "Study",
      tabsetPanel(
        type = "tabs",
        tabPanel("Yearly",
                 mainPanel(
                   width = 12,
                   fluidRow(tags$h4("Previous vs Current Year Trends")),
                   fluidRow(
                     box(plotlyOutput("plot_1001", height = 200), title = "Studies Registered"),
                     box(plotlyOutput("plot_1002", height = 200), title = "Studies Started")
                   ),
                   #create box for plot 1001
                   fluidRow(
                     box(plotlyOutput("plot_1003", height = 200), title = "Studies Completed"),
                     box(plotlyOutput("plot_1004", height = 200), title = "Results Posted")
                   )
                 )),
        tabPanel("Monthly",
                 mainPanel(
                   width = 12,
                   fluidRow(tags$h4("Historical trend for last 10 years")),
                   fluidRow(
                     box(plotlyOutput("plot_month_1001", height = 200), title = "Studies Registered"),
                     box(plotlyOutput("plot_month_1002", height = 200), title = "Studies Started")
                   ),
                   #create box for plot 1001
                   fluidRow(
                     box(plotlyOutput("plot_month_1003", height = 200), title = "Studies Completed"),
                     box(plotlyOutput("plot_month_1004", height = 200), title = "Results Posted")
                   )
                 ))
      ))
    )
  
  #ending main bracket
)

############################################################################################################
#Server function begins
###########################################################################################################

server <- function(input, output) {
  
####################################################
#SERVER Begins
#############################################################################################  
 
  #display recruitment data
  output$dt_recruitment <-
    renderDataTable(DT::datatable(mv_studies_recruiting_table, filter = 'top'))
  
  #reactive dataset for maps with reduced columns
  mv_studies_recruiting_map <- reactive({
    #createleaflet plot 1014 based on reactive set
    subset(
      mv_studies_recruiting,
      select = c(
        "ID",
        "city",
        "state",
        "country",
        "Region",
        "Condition",
        "Sponsor",
        "Facility",
        "nct_id",
        "latitude",
        "longitude"
      ),
      subset = (
        casefold(city) %like%  casefold(input$select_city_map) &
          casefold(Condition) %like%  casefold(input$select_condition_map) &
          casefold(Sponsor) %like%  casefold(input$select_sponsor_map) &
          casefold(Facility) %like%  casefold(input$select_facility_map)
      )
    )
  })
  
  #reactive dataset for table with selected columns
  mv_studies_recruiting_tab <- reactive({
    subset(
      mv_studies_recruiting,
      select = c(
        "nct_id",
        "Condition",
        "Title",
        "DataMonitoring",
        "RareDisease",
        "city",
        "state",
        "country",
        "ZipCode",
        "StudyPhase",
        "Sponsor",
        "Facility"
      ),
      subset = (
        casefold(city) %like%  casefold(input$select_city_tab) &
          casefold(Condition) %like%  casefold(input$select_condition_tab) &
          casefold(Sponsor) %like%  casefold(input$select_sponsor_tab) &
          casefold(Facility) %like%  casefold(input$select_facility_tab)
      )
    )
  })
  
  #createleaflet plot 1014
  #function to display labels
  f_labels_1014 <-
    function() {
      sprintf(
        "<br><strong>Study ID: %s</strong></br><strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
        mv_studies_recruiting_map()$ID,
        mv_studies_recruiting_map()$country,
        mv_studies_recruiting_map()$state,
        mv_studies_recruiting_map()$city,
        mv_studies_recruiting_map()$Facility,
        mv_studies_recruiting_map()$Sponsor,
        mv_studies_recruiting_map()$Condition
      ) %>%
        lapply(htmltools::HTML)
    }
  output$plot_1014 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_map()) %>%
      setView(lng = -15.037721,
              lat = 14.451703,
              zoom = 2.2) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~ longitude,
                 ~ latitude,
                 popup = f_labels_1014(),
                 #label = f_labels_1014(),
                 clusterOptions = markerClusterOptions())
  })
  
  #create choropleth plot 1016 world map
  mv_studies_recruiting_loc_agg <- mv_studies_recruiting_loc %>%
    group_by(iso3) %>%
    summarise(total_studies = sum(cnt_studies))
  
  output$plot_1016 <- renderPlotly({
    plot_geo(mv_studies_recruiting_loc_agg) %>%
      add_trace(
        z =  ~ total_studies,
        locations =  ~ iso3,
        color =  ~ total_studies,
        colors = brewer.pal(7, "Reds")
      ) %>%
      hide_colorbar() %>%
      layout(
        title = 'Recruiting Studies in World',
        geo = list(
          scope = input$select_map_1007,
          projection = list(type = 'natural earth'),
          showland = TRUE,
          landcolor = "#DEDEDE",
          showocean = TRUE,
          oceancolor = "#A0CFDF",
          showcountries = TRUE,
          subunitcolor = "#1A82C7",
          countrycolor = "#115380",
          countrywidth = 0.5,
          subunitwidth = 0.8
        )
      )
  })
  
  mv_studies_recruiting_loc_US_agg <-
    mv_studies_recruiting_loc_US %>%
    group_by(statecode) %>%
    summarise(total_studies = sum(cnt_studies))
  
  #create choropleth plot 1015 US states map
  output$plot_1015 <- renderPlotly({
    plot_geo(mv_studies_recruiting_loc_US_agg, locationmode = "USA-states") %>%
      add_trace(
        z =  ~ total_studies,
        locations =  ~ statecode,
        color =  ~ total_studies,
        colors = brewer.pal(7, "Oranges")
      ) %>%
      hide_colorbar() %>%
      layout(
        title = 'Recruiting Studies in USA',
        geo = list(
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
  
  #createleaflet plot 1013 world map
  labels_1013 <-
    sprintf(
      "<strong>Country: %s</strong><br/><strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Recruiting Studies: %s</strong>",
      mv_studies_recruiting_loc$iso3,
      mv_studies_recruiting_loc$state,
      mv_studies_recruiting_loc$city,
      mv_studies_recruiting_loc$cnt_studies
    ) %>%
    lapply(htmltools::HTML)
  
  output$plot_1013 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_loc) %>%
      setView(lng = -3.727919,
              lat = 40.463666,
              zoom = 2) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~ longitude,
                 ~ latitude,
                 label = labels_1013,
                 clusterOptions = markerClusterOptions())
  })
  
  #createleaflet only US plot 1012
  labels_1012 <-
    sprintf(
      "<strong>State: %s</strong><br/><strong>City: %s</strong><br/><strong>Recruiting Studies: %s</strong>",
      mv_studies_recruiting_loc_US$state,
      mv_studies_recruiting_loc_US$city,
      mv_studies_recruiting_loc_US$cnt_studies
    ) %>%
    lapply(htmltools::HTML)
  
  output$plot_1012 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_loc_US) %>%
      setView(lng = -97.922211,
              lat = 39.381266,
              zoom = 4.4) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(~ longitude,
                 ~ latitude,
                 label = labels_1012,
                 clusterOptions = markerClusterOptions())
  })
  
  
  #Create wordcloud plot 1009
  output$plot_1009 <- renderPlot({
    wordcloud(
      words = agg_Studiesbyconditions$condition_name,
      freq = agg_Studiesbyconditions$cnt_recruitingstudies,
      scale = c(3, 0.5),
      min.freq = 1,
      max.words = 500,
      random.order = FALSE,
      random.color = FALSE,
      rot.per = 0,
      fixed.asp = FALSE,
      #textStemming=TRUE,
      #excludeWords=c("Disease","Syndrome"),
      colors = brewer.pal(8, "Dark2")
    )
  })
  
  
  #create plot 1008
  output$plot_1008 <- renderPlotly({
    mv_studies_recruiting_table %>%
      group_by(Region) %>%
      summarise(cnt = length(unique((nct_id)))) %>%
      plot_ly(
        values =  ~ cnt,
        labels =  ~ factor(Region),
        type = 'pie'
      ) %>%
      layout(title = "Recruiting Studies by Region",
             legend = list(orientation = "h"))
    
  })
  
  #create plot 1008_2
  top_rec_sponsors <-
    data.frame(mv_studies_recruiting_table, stringsAsFactors = FALSE) %>%
    filter(is.na(Sponsor) == FALSE) %>%
    group_by(Sponsor) %>%
    summarise(cnt_st = length(unique((nct_id)))) %>%
    top_n(10, cnt_st) %>%
    arrange(desc(cnt_st))
  
  top_rec_sponsors$Sponsor <-
    factor(top_rec_sponsors$Sponsor,
           levels = unique(top_rec_sponsors$Sponsor)[order(top_rec_sponsors$cnt_st, decreasing = FALSE)])
  top_rec_sponsors$text_display <-
    paste0(top_rec_sponsors$Sponsor, ' ', top_rec_sponsors$cnt_st)
  
  output$plot_1008_2 <- renderPlotly({
    top_rec_sponsors %>%
      plot_ly(
        y =  ~ Sponsor,
        x =  ~ cnt_st,
        type = 'bar',
        text =  ~ text_display,
        textposition = 'inside',
        orientation = 'h'
      ) %>%
      layout(
        title = "Top Recruiting Sponsors",
        xaxis = list(title = '# Recruiting Studies'),
        yaxis = list(title = 'Sponsor Name', showticklabels = FALSE)
      )
  })
  
  #create plot 1008_3
  top_rec_countries <-
    data.frame(mv_studies_recruiting_table, stringsAsFactors = FALSE) %>%
    filter(is.na(country) == FALSE) %>%
    group_by(country) %>%
    summarise(cnt_st = length(unique((nct_id)))) %>%
    top_n(10, cnt_st) %>%
    arrange(desc(cnt_st))
  #sorting conditions by y values
  top_rec_countries$country <-
    factor(top_rec_countries$country,
           levels = unique(top_rec_countries$country)[order(top_rec_countries$cnt_st, decreasing = FALSE)])
  top_rec_countries$text_display <-
    paste0(top_rec_countries$country, ' ', top_rec_countries$cnt_st)
  
  output$plot_1008_3 <- renderPlotly({
    top_rec_countries %>%
      plot_ly(
        y =  ~ country,
        x =  ~ cnt_st,
        type = 'bar',
        text =  ~ text_display,
        textposition = 'auto',
        orientation = 'h'
      ) %>%
      layout(
        title = "Top Recruiting Countries",
        xaxis = list(title = '# Recruiting Studies'),
        yaxis = list(title = 'Country Name', showticklabels = FALSE)
      )
  })
  
  #create plot 1008_4
  output$plot_1008_4 <- renderPlotly({
    mv_studies_recruiting_table %>%
      group_by(StudyPhase) %>%
      summarise(cnt_st = length(unique((nct_id)))) %>%
      plot_ly(
        values =  ~ cnt_st,
        labels =  ~ (StudyPhase),
        type = 'pie'
      ) %>%
      layout(title = "Recruiting Studies by Phase",
             legend = list(orientation = "h"))
    
  })
  
  #Create recruitment infobox
  summ_rec <- data.frame(
    cnt_studies = length(unique(mv_studies_recruiting$nct_id)),
    cnt_sponsors = length(unique(mv_studies_recruiting$Sponsor)),
    cnt_countries = length(unique(mv_studies_recruiting$country)),
    cnt_cities = length(unique(mv_studies_recruiting$city)),
    cnt_facilities = length(unique(mv_studies_recruiting$Facility))
  )
  
  output$box_studies <- shinydashboard::renderValueBox({
    infoBox("Studies: ",
            summ_rec$cnt_studies,
            icon = icon("credit-card"))
  })
  
  output$box_sponsors <- shinydashboard::renderValueBox({
    infoBox("Sponsors: ",
            summ_rec$cnt_sponsors,
            icon = icon("credit-card"))
  })
  
  output$box_countries <- shinydashboard::renderValueBox({
    infoBox("Countries: ",
            summ_rec$cnt_countries,
            icon = icon("credit-card"))
  })
  
  output$box_cities <- shinydashboard::renderValueBox({
    infoBox("Cities: ",
            summ_rec$cnt_cities,
            icon = icon("credit-card"))
  })
  
  output$box_facilities <- shinydashboard::renderValueBox({
    infoBox("Facilities: ",
            summ_rec$cnt_facilities,
            icon = icon("credit-card"))
  })
  
  #Recruitment - Summary2
  #create plot 1008_5
  output$plot_1008_5 <- renderPlotly({
    plot_ly(
      data = mv_recruiting_studylevel,
      x = ~ RegToStartDays,
      type = "histogram",
      histfunc = "count",
      histnorm = ""
    )
  })
  
  #create plot 1008_6
  output$plot_1008_6 <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = mv_recruiting_studylevel,
        x =  ~ RegToStartDays,
        y =  ~ totalFacilities,
        type = "scatter",
        mode = "markers",
        color = ~ StudyType,
        text = ~ paste(
          paste("Study ID:", nct_id),
          paste("Sponsor:", Sponsor),
          paste("Num of Sites:", totalFacilities),
          paste("Lead Time:", RegToStartDays),
          sep = "<br />"
        ),
        hoverinfo = 'text'
      ) %>%
      layout(
        xaxis = list(title = "Days (Reg to Study Start)", showgrid = TRUE),
        yaxis = list(title = "Number of Sites", showgrid = TRUE)
      )
  })
  
  
  
  #create plot 1030 - for Recruiting-sponsor
  #create plot 1030_1
  output$plot_1030_1 <- renderPlotly({
    rec_sponsors %>%
      group_by(AgencyClass) %>%
      summarise(cnt_sponsors = length(unique((Sponsor)))) %>%
      plot_ly(
        values =  ~ cnt_sponsors,
        labels =  ~ (AgencyClass),
        type = 'pie'
      ) %>%
      layout(title = "Sponsors by Class",
             legend = list(orientation = "h"))
  })
  
  #create plot 1030_2
  output$plot_1030_2 <- renderPlotly({
    rec_sponsors %>%
      group_by(SponsorType) %>%
      summarise(cnt_sponsors = length(unique((Sponsor)))) %>%
      plot_ly(
        values =  ~ cnt_sponsors,
        labels =  ~ (SponsorType),
        type = 'pie'
      ) %>%
      layout(title = "Sponsors by Class",
             legend = list(orientation = "h"))
  })
  
  #create plot 1030_3
  output$plot_1030_3 <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = rec_sponsors,
        x =  ~ cnt_countries,
        y =  ~ cnt_recFacilities,
        type = "scatter",
        mode = "markers",
        color = ~ AgencyClass,
        hoverinfo = 'text',
        text = ~ paste(
          paste("Sponsor:", Sponsor),
          paste("Countries:", cnt_countries),
          paste("Facilities:", cnt_recFacilities),
          sep = "<br />"
        )
      ) %>%
      layout(
        xaxis = list(title = "Countries Recruiting", showgrid = TRUE),
        yaxis = list(title = "Facilities Recruiting", showgrid = TRUE)
      )
  })
  
  #create plot 1030_4
  output$plot_1030_4 <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = rec_sponsors,
        y =  ~ cnt_rareDisease,
        x = ~ SponsorType,
        type = "bar"
      ) %>%
      layout(
        xaxis = list(title = "Type of Sponsor", showgrid = TRUE),
        yaxis = list(title = "Studies with Rare Condition", showgrid = TRUE)
      )
  })
  
  #display sponsor recruiting summary data
  output$dt_sponsor_recruitment <-
    renderDataTable(DT::datatable(rec_sponsors, filter = 'top'))
  
  #create plot 1006
  output$plot_1006 <- renderPlotly({
    plot_geo(
      mv_studies_recruiting_loc_US,
      lat = ~ latitude,
      lon = ~ longitude
    ) %>%
      add_markers(
        text = ~ paste(
          paste("City Name:", city),
          paste("State Name:", state),
          paste("Num of Studies:", cnt_studies),
          sep = "<br />"
        ),
        colors = "Set2",
        color = ~ cnt_studies,
        symbol = I("circle"),
        size = ~ cnt_studies * 100,
        hoverinfo = "text"
      ) %>%
      hide_colorbar() %>%
      layout(
        title = 'Recruiting Studies in USA',
        geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showland = TRUE,
          landcolor = "#F9F4F8",
          subunitcolor = "#6C5E68",
          subunitwidth = 1
        )
      )
  })
  
  #cteate plot 1007
  output$plot_1007 <- renderPlotly({
    plot_geo(mv_studies_recruiting_loc,
             lat = ~ latitude,
             lon = ~ longitude) %>%
      add_markers(
        text = ~ paste(city, iso3, paste("Studies:", cnt_studies), sep = "<br />"),
        color = ~ cnt_studies,
        symbol = I("circle"),
        size = ~ cnt_studies * 4,
        hoverinfo = "text"
      ) %>%
      hide_colorbar() %>%
      layout(
        title = 'Recruiting Studies Across Globe',
        geo = list(
          scope = input$select_map_1007,
          projection = list(type = 'natural earth'),
          showland = TRUE,
          landcolor = "#DEDEDE",
          showocean = TRUE,
          oceancolor = "#A0CFDF",
          showcountries = TRUE,
          subunitcolor = "#1A82C7",
          countrycolor = "#115380",
          countrywidth = 0.5,
          subunitwidth = 0.8
        )
      )
  })
  
  
  
  #datatable for recruitment find studies
  output$dt_recruitment_1005 <- renderDataTable({
    DT::datatable(
      mv_studies_recruiting_tab(),
      escape = FALSE,
      rownames = FALSE,
      options = list(lengthChange = FALSE)
    )
  })
  
  #cteate plot 1001
  output$plot_1001 <- renderPlotly({
    plot_ly(
      mv_year_Lst10Yr,
      x =  ~ common_year,
      y =  ~ cnt_registered,
      type = 'bar',
      text =  ~ cnt_registered,
      textposition = 'auto'
    )
  })
  
  #cteate plot 1002
  output$plot_1002 <- renderPlotly({
    plot_ly(
      mv_year_Lst10Yr,
      x =  ~ common_year,
      y =  ~ cnt_started,
      type = 'bar',
      text =  ~ cnt_started,
      textposition = 'auto'
    )
  })
  
  #cteate plot 1003
  output$plot_1003 <- renderPlotly({
    plot_ly(
      mv_year_Lst10Yr,
      x =  ~ common_year,
      y =  ~ cnt_completed,
      type = 'bar',
      text =  ~ cnt_completed,
      textposition = 'auto'
    )
  })
  
  #cteate plot 1004
  output$plot_1004 <- renderPlotly({
    plot_ly(
      mv_year_Lst10Yr,
      x =  ~ common_year,
      y =  ~ cnt_resultsPosted,
      type = 'bar',
      text =  ~ cnt_resultsPosted,
      textposition = 'auto'
    )
  })
  
  mv_month$month_name <-
    factor(mv_month$month_name,
           levels = unique(mv_month$month_name)[order(mv_month$common_month, decreasing = FALSE)])
  
  #cteate plot month_1001
  output$plot_month_1001 <- renderPlotly({
    mv_month %>%
      plot_ly(
      x =  ~ month_name,
      y =  ~ cnt_reg_lstYr,
      type = 'bar',
      text =  ~ cnt_reg_lstYr,
      textposition = 'auto',
      name="Previous",
      showlegend = FALSE) %>%
      add_trace(y =  ~ cnt_reg_CurrYr, text =  ~ cnt_reg_CurrYr, name="Current") %>%
      layout(xaxis=list(title="")) %>%
      layout(yaxis=list(title=""))
  })
  
  #cteate plot month_1002
  output$plot_month_1002 <- renderPlotly({
    mv_month %>%
    plot_ly(
      x =  ~ month_name,
      y =  ~ cnt_started_lstYr,
      type = 'bar',
      text =  ~ cnt_started_lstYr,
      textposition = 'auto',
      name="Previous",
      showlegend = FALSE) %>%
      add_trace(y =  ~ cnt_started_CurrYr, text =  ~ cnt_started_CurrYr, name="Current") %>%
      layout(xaxis=list(title="")) %>%
      layout(yaxis=list(title=""))
  })
  
  #cteate plot month_1003
  output$plot_month_1003 <- renderPlotly({
    mv_month %>%
      plot_ly(
        x =  ~ month_name,
        y =  ~ cnt_completed_lstYr,
        type = 'bar',
        text =  ~ cnt_completed_lstYr,
        textposition = 'auto',
        name="Previous",
        showlegend = FALSE) %>%
      add_trace(y =  ~ cnt_completed_CurrYr, text =  ~ cnt_completed_CurrYr, name="Current") %>%
      layout(xaxis=list(title="")) %>%
      layout(yaxis=list(title=""))
  })
  
  #cteate plot month_1004
  output$plot_month_1004 <- renderPlotly({
    mv_month %>%
      plot_ly(
        x =  ~ month_name,
        y =  ~ cnt_posted_lstYr,
        type = 'bar',
        text =  ~ cnt_posted_lstYr,
        textposition = 'auto',
        name="Previous",
        showlegend = FALSE) %>%
      add_trace(y =  ~ cnt_posted_CurrYr, text =  ~ cnt_posted_CurrYr, name="Current") %>%
      layout(xaxis=list(title="")) %>%
      layout(yaxis=list(title=""))
  })
  
  ####################server ends###########################
  
}

shinyApp(ui, server)