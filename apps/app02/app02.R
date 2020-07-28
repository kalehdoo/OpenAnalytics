#import libraries
library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(data.table)
library(DT)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)

#read log file
log_app02 <- read.csv("data/log_app02.txt", header = FALSE)
log_app02_time <- substr(log_app02, 1, 10)
log_app02_time <- format.Date(log_app02_time, "%d-%b-%Y")


#read conditions file
rec_conditions <-
  read.csv(
    "data/rec_conditions_list.txt",
    header = TRUE,
    sep = "|",
    na.strings = "NA",
    nrows = -100,
    stringsAsFactors = FALSE
  )

rec_conditions_list<-sort(rec_conditions$ConditionName, decreasing=FALSE)

#read data
mv_studies_recruiting <-
  readRDS("data/mv_studies_recruiting_mini.rds") %>%
  filter(is.na(nct_id) == FALSE)


var_studyphase_name <- unique(mv_studies_recruiting$StudyPhase)

var_studyphase_name
#mv_studies_recruiting <- subset.data.frame(mv_studies_recruiting) %>%
#  sample_frac(0.2, replace = FALSE)

mv_studies_recruiting_tab <- mv_studies_recruiting %>%
  select(
    "ID",
    "Condition",
    "StudyPhase",
    "city",
    "state",
    "country",
    "ZipCode",
    "Region",
    "Sponsor",
    "Facility"
  )



mv_studies_recruiting_world <- mv_studies_recruiting %>%
  filter(is.na(nct_id) == FALSE & is.na(latitude) == FALSE)


mv_studies_recruiting_US <- mv_studies_recruiting %>%
  filter(country == "United States" &
           is.na(nct_id) == FALSE & is.na(latitude) == FALSE)


mv_studies_recruiting_americas <- mv_studies_recruiting %>%
  filter(
    country != "United States" &
      Region == "Americas" &
      is.na(nct_id) == FALSE & is.na(latitude) == FALSE
  )



mv_studies_recruiting_europe <- mv_studies_recruiting %>%
  filter(Region == "Europe" &
           is.na(nct_id) == FALSE & is.na(latitude) == FALSE)


mv_studies_recruiting_asia <- mv_studies_recruiting %>%
  filter(Region == "Asia" &
           is.na(nct_id) == FALSE & is.na(latitude) == FALSE)


mv_studies_recruiting_oceania <- mv_studies_recruiting %>%
  filter(Region == "Oceania" &
           is.na(nct_id) == FALSE & is.na(latitude) == FALSE)


mv_studies_recruiting_africa <- mv_studies_recruiting %>%
  filter(Region == "Africa" &
           is.na(nct_id) == FALSE & is.na(latitude) == FALSE)

# for spinners 2-3 match the background color of spinner
options(spinner.color.background = "#772953")
options(spinner.size = 1.5)

ui <-
  fluidPage(
    #below includehtml is added for google analytics
    tags$head(includeHTML((
      "googleanalyticsapp02.html"
    ))),
    
    navbarPage(
      #title = div(img(src="https://www.oakbloc.com/images/Oakbloc_transparent.png")),
      title = div(tags$a(href="https://www.oakbloc.com", target="_blank", tags$img(src = "https://www.oakbloc.com/images/Oakbloc_transparent.svg", width="100px", height="35px",style = "padding-top: 0px; padding-bottom: 5px; width: '100%';"))),
      windowTitle = "Kalehdoo Analytics",
      #theme = "bootstrap.css",
      theme = shinytheme("united"),
      collapsible	= TRUE,
      fluid = TRUE,
      inverse = TRUE,
      footer = h4(
        tags$a(href = "https://www.oakbloc.com/", "Oakbloc Technologies", target =
                 "_blank"),
        style = "margin-top:2%;margin-bottom:2%;",
        align = "center",
        "2019-2020",
      ),
      
      tabPanel(
        title = "Globe",
        fluidRow((
          h2(
            "Kalehdoo Clinical Site Finder",
            #class = "display-4",
            style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
            align = "center"
          )
        )),
        fluidRow(
          h4(
            "Modify search criteria and Hit Display Results button",
            style = "color: #F37312;",
            align = "center",
          )
        ),
        fluidRow(
          column(
            2,
            align = "center",
            pickerInput(
              inputId = "select_studyphase_name_world",
              label = NULL,
              choices = var_studyphase_name,
              options = list(`actions-box` = TRUE, style = "btn-info"
              ),
              multiple = TRUE,
              selected=c(var_studyphase_name)
            )
          ),
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_condition_map_world",
              label = NULL,
              value="covid-19",
              placeholder = "Condition Name"
            )
          ),
          
          
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_city_map_world",
              label = NULL,
              placeholder = "City Name"
            )
          ),    
          
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_sponsor_map_world",
              label = NULL,
              placeholder = "Sponsor Name"
            )
          ),
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_facility_map_world",
              label = NULL,
              placeholder = "Facility Name"
            )
          )
        ),
        fluidRow(tags$div(
          class = "text-center",
          actionButton(
            "update_global",
            "Display Results",
            class = "btn btn-primary",
            style = "margin-bottom: 0.5%;"
          )
        )),
        fluidRow(withSpinner(leafletOutput("plot_1020"), type = 3)),
        fluidRow(
          style = "margin-top:3%;margin-left:1%; margin-right:1%",
          h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
             paste0("Data Refreshed On: ",log_app02_time)),
          h4(
            "The Clinical Study Site Finder App is an interactive application that helps in finding clinical studies that are currently recruiting patients globally.
                                The patients can search based on the condition name, sponsor, site, or location, and the results will appear on a map.",
            style = "text-align:justify;"
          ),
          h4(
            "Once a study is located, the details of the study and contact details of the site can be viewed by navigating to clinicaltrials.gov by simply clicking on the hyperlink.
                                ",
            style = "text-align:justify;"
          ),
          h4(
            "The application is built using open-source OpenStreet Maps, R and Shiny.",
            style = "text-align:justify;"
          )
        ),
        fluidRow(
          h5(
            "Disclaimer: The source data for the application is obtained from clinicaltrials.gov ACCT-CTTI website ",
            tags$a(href = "https://aact.ctti-clinicaltrials.org/download", "ACCT-CTTI.", target =
                     "_blank"),
            style = "margin-top:2%;margin-left:1%; margin-right:1%; text-align:justify;"
          ),
          h5(
            "The detail level data for individual clinical trials is available on ",
            tags$a(href = "https://clinicaltrials.gov/", "ClinicalTrials.gov", target =
                     "_blank"),
            "The users are advised to verify the details on clinicaltrials.gov and consult with their physicians for any medical and legal advise. This study finder app should be used as an interactive assistant in finding the relevant clinical trials. Oakbloc Technologies do not accept any responsibility or liability for any direct, indirect, or consequential loss or damage resulting from any such irregularity, inaccuracy, or use of the information.",
            style = "margin-top:0.1%;margin-left:1%; margin-right:1%; margin-bottom:2%; text-align:justify;"
          )
        )
        
      ),
      tabPanel(
        "Europe",
        fluidRow((
          h2(
            "Kalehdoo Clinical Site Finder",
            #class = "display-4",
            style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
            align = "center"
          )
        )),
        fluidRow(
          h4(
            "Modify search criteria and Hit Display Results button",
            style = "color: #F37312;",
            align = "center",
          )
        ),
        fluidRow(
          column(
            2,
            align = "center",
            pickerInput(
              inputId = "select_studyphase_name_europe",
              label = NULL,
              choices = var_studyphase_name,
              options = list(`actions-box` = TRUE, style = "btn-info"
              ),
              multiple = TRUE,
              selected=c(var_studyphase_name)
            )
          ),		  
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_condition_map_europe",
              label = NULL,
              placeholder = "Condition/Disease Name",
              value="covid-19"
            )
          ),		  
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_city_map_europe",
              label = NULL,
              placeholder = "City Name"
            )
          ),
          
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_sponsor_map_europe",
              label = NULL,
              placeholder = "Sponsor Name"
            )
          ),
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_facility_map_europe",
              label = NULL,
              placeholder = "Facility Name"
            )
          )
        ),
        fluidRow(tags$div(
          class = "text-center",
          actionButton(
            "update_europe",
            "Display Results",
            class = "btn btn-primary",
            style = "margin-bottom: 0.5%;"
          )
        )),
        fluidRow(withSpinner(leafletOutput("plot_1014"), type = 3)),
        fluidRow(
          style = "margin-top:3%;margin-left:1%; margin-right:1%",
          h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
             paste0("Data Refreshed On: ",log_app02_time))
        )
      ),
      tabPanel(
        "Asia",
        fluidRow((
          h2(
            "Kalehdoo Clinical Site Finder",
            #class = "display-4",
            style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
            align = "center"
          )
        )),
        fluidRow(
          h4(
            "Modify search criteria and Hit Display Results button",
            style = "color: #F37312;",
            align = "center",
          )
        ),
        fluidRow(
          column(
            2,
            align = "center",
            pickerInput(
              inputId = "select_studyphase_name_asia",
              label = NULL,
              choices = var_studyphase_name,
              options = list(`actions-box` = TRUE, style = "btn-info"
              ),
              multiple = TRUE,
              selected=c(var_studyphase_name)
            )
          ),		  
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_condition_map_asia",
              label = NULL,
              placeholder = "Condition/Disease Name",
              value="covid-19"
            )
          ),
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_city_map_asia",
              label = NULL,
              placeholder = "City Name"
            )
          ),          
          
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_sponsor_map_asia",
              label = NULL,
              placeholder = "Sponsor Name"
            )
          ),
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_facility_map_asia",
              label = NULL,
              placeholder = "Facility Name"
            )
          )
        ),
        fluidRow(tags$div(
          class = "text-center",
          actionButton(
            "update_asia",
            "Display Results",
            class = "btn btn-primary",
            style = "margin-bottom: 0.5%;"
          )
        )),
        fluidRow(withSpinner(leafletOutput("plot_1015"), type = 3)),
        fluidRow(
          style = "margin-top:3%;margin-left:1%; margin-right:1%",
          h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
             paste0("Data Refreshed On: ",log_app02_time))
        )
      ),
      tabPanel(
        "Africa",
        fluidRow((
          h2(
            "Kalehdoo Clinical Site Finder",
            #class = "display-4",
            style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
            align = "center"
          )
        )),
        fluidRow(
          h4(
            "Modify search criteria and Hit Display Results button",
            style = "color: #F37312;",
            align = "center",
          )
        ),
        fluidRow(
          column(
            2,
            align = "center",
            pickerInput(
              inputId = "select_studyphase_name_africa",
              label = NULL,
              choices = var_studyphase_name,
              options = list(`actions-box` = TRUE, style = "btn-info"
              ),
              multiple = TRUE,
              selected=c(var_studyphase_name)
            )
          ),		  
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_condition_map_africa",
              label = NULL,
              placeholder = "Condition/Disease Name",
              value="covid-19"
            )
          ),
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_city_map_africa",
              label = NULL,
              placeholder = "City Name"
            )
          ),          
          
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_sponsor_map_africa",
              label = NULL,
              placeholder = "Sponsor Name"
            )
          ),
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_facility_map_africa",
              label = NULL,
              placeholder = "Facility Name"
            )
          )
        ),
        fluidRow(tags$div(
          class = "text-center",
          actionButton(
            "update_africa",
            "Display Results",
            class = "btn btn-primary",
            style = "margin-bottom: 0.5%;"
          )
        )),
        fluidRow(withSpinner(leafletOutput("plot_1016"), type = 3)),
        fluidRow(
          style = "margin-top:3%;margin-left:1%; margin-right:1%",
          h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
             paste0("Data Refreshed On: ",log_app02_time))
        )
      ),
      tabPanel(
        "Oceania",
        fluidRow((
          h2(
            "Kalehdoo Clinical Site Finder",
            #class = "display-4",
            style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
            align = "center"
          )
        )),
        fluidRow(
          h4(
            "Modify search criteria and Hit Display Results button",
            style = "color: #F37312;",
            align = "center",
          )
        ),
        fluidRow(
          column(
            2,
            align = "center",
            pickerInput(
              inputId = "select_studyphase_name_oceania",
              label = NULL,
              choices = var_studyphase_name,
              options = list(`actions-box` = TRUE, style = "btn-info"
              ),
              multiple = TRUE,
              selected=c(var_studyphase_name)
            )
          ),		  
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_condition_map_oceania",
              label = NULL,
              placeholder = "Condition/Disease Name",
              value="covid-19"
            )
          ),
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_city_map_oceania",
              label = NULL,
              placeholder = "City Name"
            )
          ),         
          
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_sponsor_map_oceania",
              label = NULL,
              placeholder = "Sponsor Name"
            )
          ),
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_facility_map_oceania",
              label = NULL,
              placeholder = "Facility Name"
            )
          )
        ),
        fluidRow(tags$div(
          class = "text-center",
          actionButton(
            "update_oceania",
            "Display Results",
            class = "btn btn-primary",
            style = "margin-bottom: 0.5%;"
          )
        )),
        fluidRow(withSpinner(leafletOutput("plot_1017"), type = 3)),
        fluidRow(
          style = "margin-top:3%;margin-left:1%; margin-right:1%",
          h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
             paste0("Data Refreshed On: ",log_app02_time))
        )
      ),
      tabPanel(
        "USA",
        fluidRow((
          h2(
            "Kalehdoo Clinical Site Finder",
            #class = "display-4",
            style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
            align = "center"
          )
        )),
        fluidRow(
          h4(
            "Modify search criteria and Hit Display Results button",
            style = "color: #F37312;",
            align = "center",
          )
        ),
        fluidRow(
          column(
            2,
            align = "center",
            pickerInput(
              inputId = "select_studyphase_name_us",
              label = NULL,
              choices = var_studyphase_name,
              options = list(`actions-box` = TRUE, style = "btn-info"
              ),
              multiple = TRUE,
              selected=c(var_studyphase_name)
            )
          ),		  
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_condition_map_us",
              label = NULL,
              placeholder = "Condition/Disease Name",
              value="covid-19"
            )
          ),
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_city_map_us",
              label = NULL,
              placeholder = "City Name"
            )
          ),          
          
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_sponsor_map_us",
              label = NULL,
              placeholder = "Sponsor Name"
            )
          ),
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_facility_map_us",
              label = NULL,
              placeholder = "Facility Name"
            )
          )
        ),
        fluidRow(tags$div(
          class = "text-center",
          actionButton(
            "update_us",
            "Display Results",
            class = "btn btn-primary",
            style = "margin-bottom: 0.5%;"
          )
        )),
        fluidRow(withSpinner(leafletOutput("plot_1018"), type = 3)),
        fluidRow(
          style = "margin-top:3%;margin-left:1%; margin-right:1%",
          h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
             paste0("Data Refreshed On: ",log_app02_time))
        )
      ),
      tabPanel(
        "Rest of Americas",
        fluidRow((
          h2(
            "Kalehdoo Clinical Site Finder",
            #class = "display-4",
            style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
            align = "center"
          )
        )),
        fluidRow(
          h4(
            "Modify search criteria and Hit Display Results button",
            style = "color: #F37312;",
            align = "center",
          )
        ),
        fluidRow(
          column(
            2,
            align = "center",
            pickerInput(
              inputId = "select_studyphase_name_americas",
              label = NULL,
              choices = var_studyphase_name,
              options = list(`actions-box` = TRUE, style = "btn-info"
              ),
              multiple = TRUE,
              selected=c(var_studyphase_name)
            )
          ),		  
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_condition_map_americas",
              label = NULL,
              placeholder = "Condition/Disease Name",
              value="covid-19"
            )
          ),
          column(
            2,
            align = "center",
            textInput(
              inputId = "select_city_map_americas",
              label = NULL,
              placeholder = "City Name"
            )
          ),          
          
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_sponsor_map_americas",
              label = NULL,
              placeholder = "Sponsor Name"
            )
          ),
          column(
            3,
            align = "center",
            textInput(
              inputId = "select_facility_map_americas",
              label = NULL,
              placeholder = "Facility Name"
            )
          )
        ),
        fluidRow(tags$div(
          class = "text-center",
          actionButton(
            "update_americas",
            "Display Results",
            class = "btn btn-primary",
            style = "margin-bottom: 0.5%;"
          )
        )),
        fluidRow(withSpinner(leafletOutput("plot_1019"), type = 3)),
        fluidRow(
          style = "margin-top:3%;margin-left:1%; margin-right:1%",
          h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
             paste0("Data Refreshed On: ",log_app02_time))
        )
      ),
      tabPanel("Table Details",
               fluidRow((
                 h2(
                   "Kalehdoo Clinical Site Finder",
                   #class = "display-4",
                   style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
                   align = "center"
                 )
               )),
               fluidRow(
                 withSpinner(DT::dataTableOutput("dt_recruitment_1021"), type = 3)
               ),
               fluidRow(
                 style = "margin-top:3%;margin-left:1%; margin-right:1%",
                 h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
                    paste0("Data Refreshed On: ",log_app02_time))
               )
      )
    )
  )





server <- function(input, output, session) {
  
  #updateSelectizeInput(session = session, inputId = 'select_condition_map_world', choices = rec_conditions$ConditionName, selected = "covid-19", server = TRUE)
  
  #reactive dataset for world maps with reduced columns
  mv_studies_recruiting_map_world <-
    eventReactive(input$update_global, {
      
      #createleaflet plot 1020 based on reactive set
      subset(
        mv_studies_recruiting_world,
        subset = (
          StudyPhase %in%  c(input$select_studyphase_name_world) &
            casefold(city) %like%  casefold(input$select_city_map_world) &
            casefold(Condition) %like%   casefold(input$select_condition_map_world) &
            casefold(Facility) %like%  casefold(input$select_facility_map_world) &
            casefold(Sponsor) %like%  casefold(input$select_sponsor_map_world)
        )
      )
    })
  
  #createleaflet plot 1020
  #function to display labels
  f_labels_1020 <-
    function() {
      sprintf(
        "<br><strong>Study ID: %s</strong></br><strong>Phase: %s</strong><br/><strong>Country: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
        mv_studies_recruiting_map_world()$ID,
        mv_studies_recruiting_map_world()$StudyPhase,
        mv_studies_recruiting_map_world()$country,
        mv_studies_recruiting_map_world()$city,
        mv_studies_recruiting_map_world()$Facility,
        mv_studies_recruiting_map_world()$Sponsor,
        mv_studies_recruiting_map_world()$Condition
      ) %>%
        lapply(htmltools::HTML)
    }
  
  
  output$plot_1020 <- renderLeaflet({
    global_chart <- (
      leaflet(data = mv_studies_recruiting_map_world()) %>%
        setView(
          lng = -4.055685,
          lat = 41.294856,
          zoom = 1.5
        ) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(
          ~ longitude,
          ~ latitude,
          popup = f_labels_1020(),
          clusterOptions = markerClusterOptions()
        )
    )
  })
  
  #reactive dataset for maps with reduced columns
  mv_studies_recruiting_map_europe <-
    eventReactive(input$update_europe, {
      #createleaflet plot 1014 based on reactive set
      subset(
        mv_studies_recruiting_europe,
        #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
        subset = (
          
          StudyPhase %in%  c(input$select_studyphase_name_europe) &
            casefold(city) %like%  casefold(input$select_city_map_europe) &
            casefold(Condition) %like%  casefold(input$select_condition_map_europe) &
            casefold(Facility) %like%  casefold(input$select_facility_map_europe) &
            casefold(Sponsor) %like%  casefold(input$select_sponsor_map_europe)
        )
      )
    })
  
  #createleaflet plot 1014
  #function to display labels
  f_labels_1014 <-
    function() {
      sprintf(
        "<br><strong>Study ID: %s</strong></br><strong>Phase: %s</strong><br/><strong>Country: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
        mv_studies_recruiting_map_europe()$ID,
        mv_studies_recruiting_map_europe()$StudyPhase,
        mv_studies_recruiting_map_europe()$country,        
        mv_studies_recruiting_map_europe()$city,
        mv_studies_recruiting_map_europe()$Facility,
        mv_studies_recruiting_map_europe()$Sponsor,
        mv_studies_recruiting_map_europe()$Condition
      ) %>%
        lapply(htmltools::HTML)
    }
  output$plot_1014 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_map_europe()) %>%
      setView(lng = 41.44326,
              lat = 52.73169,
              zoom = 2.7) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers( ~ longitude,
                  ~ latitude,
                  popup = f_labels_1014(),
                  #label = f_labels_1014(),
                  clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for asia maps with reduced columns
  mv_studies_recruiting_map_asia <-
    eventReactive(input$update_asia, {
      #createleaflet plot 1015 based on reactive set
      subset(
        mv_studies_recruiting_asia,
        #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
        subset = (
          StudyPhase %in%  c(input$select_studyphase_name_asia) &
            casefold(city) %like%  casefold(input$select_city_map_asia) &
            casefold(Condition) %like%  casefold(input$select_condition_map_asia) &
            casefold(Facility) %like%  casefold(input$select_facility_map_asia) &
            casefold(Sponsor) %like%  casefold(input$select_sponsor_map_asia)
        )
      )
    })
  
  #createleaflet plot 1015
  #function to display labels
  f_labels_1015 <-
    function() {
      sprintf(
        "<br><strong>Study ID: %s</strong></br><strong>Phase: %s</strong><br/><strong>Country: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
        mv_studies_recruiting_map_asia()$ID,
        mv_studies_recruiting_map_asia()$StudyPhase,
        mv_studies_recruiting_map_asia()$country,        
        mv_studies_recruiting_map_asia()$city,
        mv_studies_recruiting_map_asia()$Facility,
        mv_studies_recruiting_map_asia()$Sponsor,
        mv_studies_recruiting_map_asia()$Condition
      ) %>%
        lapply(htmltools::HTML)
    }
  output$plot_1015 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_map_asia()) %>%
      setView(lng = 77.22445,
              lat = 28.63576,
              zoom = 2.8) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers( ~ longitude,
                  ~ latitude,
                  popup = f_labels_1015(),
                  #label = f_labels_1014(),
                  clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for africa maps with reduced columns
  mv_studies_recruiting_map_africa <-
    eventReactive(input$update_africa, {
      #createleaflet plot 1016 based on reactive set
      subset(
        mv_studies_recruiting_africa,
        #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
        subset = (
          StudyPhase %in%  c(input$select_studyphase_name_africa) &
            casefold(city) %like%  casefold(input$select_city_map_africa) &
            casefold(Condition) %like%  casefold(input$select_condition_map_africa) &
            casefold(Facility) %like%  casefold(input$select_facility_map_africa) &
            casefold(Sponsor) %like%  casefold(input$select_sponsor_map_africa)
        )
      )
    })
  
  #createleaflet plot 1016
  #function to display labels
  f_labels_1016 <-
    function() {
      sprintf(
        "<br><strong>Study ID: %s</strong></br><strong>Phase: %s</strong><br/><strong>Country: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
        mv_studies_recruiting_map_africa()$ID,
        mv_studies_recruiting_map_africa()$StudyPhase,
        mv_studies_recruiting_map_africa()$country,        
        mv_studies_recruiting_map_africa()$city,
        mv_studies_recruiting_map_africa()$Facility,
        mv_studies_recruiting_map_africa()$Sponsor,
        mv_studies_recruiting_map_africa()$Condition
      ) %>%
        lapply(htmltools::HTML)
    }
  output$plot_1016 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_map_africa()) %>%
      setView(lng = 30.81667,
              lat = 3.05,
              zoom = 2.6) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers( ~ longitude,
                  ~ latitude,
                  popup = f_labels_1016(),
                  #label = f_labels_1014(),
                  clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for Oceania maps with reduced columns
  mv_studies_recruiting_map_oceania <-
    eventReactive(input$update_oceania, {
      #createleaflet plot 1017 based on reactive set
      subset(
        mv_studies_recruiting_oceania,
        #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
        subset = (
          StudyPhase %in%  c(input$select_studyphase_name_oceania) &
            casefold(city) %like%  casefold(input$select_city_map_oceania) &
            casefold(Condition) %like%  casefold(input$select_condition_map_oceania) &
            casefold(Facility) %like%  casefold(input$select_facility_map_oceania) &
            casefold(Sponsor) %like%  casefold(input$select_sponsor_map_oceania)
        )
      )
    })
  
  #createleaflet plot 1016
  #function to display labels
  f_labels_1017 <-
    function() {
      sprintf(
        "<br><strong>Study ID: %s</strong></br><strong>Phase: %s</strong><br/><strong>Country: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
        mv_studies_recruiting_map_oceania()$ID,
        mv_studies_recruiting_map_oceania()$StudyPhase,
        mv_studies_recruiting_map_oceania()$country,        
        mv_studies_recruiting_map_oceania()$city,
        mv_studies_recruiting_map_oceania()$Facility,
        mv_studies_recruiting_map_oceania()$Sponsor,
        mv_studies_recruiting_map_oceania()$Condition
      ) %>%
        lapply(htmltools::HTML)
    }
  output$plot_1017 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_map_oceania()) %>%
      setView(lng = 133.8836,
              lat = -23.69748,
              zoom = 3.0) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers( ~ longitude,
                  ~ latitude,
                  popup = f_labels_1017(),
                  #label = f_labels_1014(),
                  clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for US maps
  mv_studies_recruiting_map_us <- eventReactive(input$update_us, {
    #createleaflet plot 1017 based on reactive set
    subset(
      mv_studies_recruiting_US,
      #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
      subset = (
        StudyPhase %in%  c(input$select_studyphase_name_us) &
          casefold(city) %like%  casefold(input$select_city_map_us) &
          casefold(Condition) %like%  casefold(input$select_condition_map_us) &
          casefold(Facility) %like%  casefold(input$select_facility_map_us) &
          casefold(Sponsor) %like%  casefold(input$select_sponsor_map_us)
      )
    )
  })
  
  #createleaflet plot 1018
  #function to display labels
  f_labels_1018 <-
    function() {
      sprintf(
        "<br><strong>Study ID: %s</strong></br><strong>Phase: %s</strong><br/><strong>Country: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
        mv_studies_recruiting_map_us()$ID,
        mv_studies_recruiting_map_us()$StudyPhase,
        mv_studies_recruiting_map_us()$country,       
        mv_studies_recruiting_map_us()$city,
        mv_studies_recruiting_map_us()$Facility,
        mv_studies_recruiting_map_us()$Sponsor,
        mv_studies_recruiting_map_us()$Condition
      ) %>%
        lapply(htmltools::HTML)
    }
  output$plot_1018 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_map_us()) %>%
      setView(lng = -100.437012,
              lat = 47.650589,
              zoom = 2.7) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers( ~ longitude,
                  ~ latitude,
                  popup = f_labels_1018(),
                  clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for US maps
  mv_studies_recruiting_map_americas <-
    eventReactive(input$update_americas, {
      #createleaflet plot 1017 based on reactive set
      subset(
        mv_studies_recruiting_americas,
        #select = c("ID","city","state","country","Condition","Sponsor","Facility","nct_id","latitude","longitude"),
        subset = (
          StudyPhase %in%  c(input$select_studyphase_name_americas) &
            casefold(city) %like%  casefold(input$select_city_map_americas) &
            casefold(Condition) %like%  casefold(input$select_condition_map_americas) &
            casefold(Facility) %like%  casefold(input$select_facility_map_americas) &
            casefold(Sponsor) %like%  casefold(input$select_sponsor_map_americas)
        )
      )
    })
  
  #createleaflet plot 1019
  #function to display labels
  f_labels_1019 <-
    function() {
      sprintf(
        "<br><strong>Study ID: %s</strong></br><strong>Phase: %s</strong><br/><strong>Country: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
        mv_studies_recruiting_map_americas()$ID,
        mv_studies_recruiting_map_americas()$StudyPhase,
        mv_studies_recruiting_map_americas()$country,        
        mv_studies_recruiting_map_americas()$city,
        mv_studies_recruiting_map_americas()$Facility,
        mv_studies_recruiting_map_americas()$Sponsor,
        mv_studies_recruiting_map_americas()$Condition
      ) %>%
        lapply(htmltools::HTML)
    }
  output$plot_1019 <- renderLeaflet({
    leaflet(data = mv_studies_recruiting_map_americas()) %>%
      setView(lng = -90.522713,
              lat = 14.628434,
              zoom = 2.0) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers( ~ longitude,
                  ~ latitude,
                  popup = f_labels_1019(),
                  clusterOptions = markerClusterOptions())
  })
  
  #reactive dataset for maps with reduced columns
  output$dt_recruitment_1021 <- renderDataTable({
    DT::datatable(
      mv_studies_recruiting_tab,
      filter = 'top',
      escape = FALSE,
      rownames = FALSE,
      options = list(lengthChange = FALSE)
    )
  })
  
  
  
}

shinyApp(ui, server)

