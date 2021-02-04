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
library(aws.s3)



#read log file
log_app02 <- read.csv("data/log_app02.txt", header = FALSE)
log_app02_time <- substr(log_app02, 1, 10)
log_app02_time <- format.Date(log_app02_time, "%d-%b-%Y")




#read data from amazon aws s3 bucket
#mv_studies_recruiting <- 
#  aws.s3::s3read_using(read.csv, object = "s3://oakblocsite/mv_studies_recruiting_mini.txt",
#                       header = TRUE,
#                       sep = "|",
#                       na.strings = "NA",
#                       nrows = -100,
#                       stringsAsFactors = FALSE)

mv_studies_recruiting <-
    fread(
        "data/mv_studies_recruiting_mini.txt",
       header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

site_advert_existing <- 
  aws.s3::s3read_using(read.csv, object = "s3://oakblocsite/site_advert_existing.txt",
                       header = TRUE,
                       sep = "|",
                       na.strings = "NA",
                       nrows = -100,
                       stringsAsFactors = FALSE)

#site_advert_existing <-
#  read.csv(
#    "data/site_advert_existing.txt",
#    header = TRUE,
#    sep = "|",
#    na.strings = "NA",
#    nrows = -100,
#    stringsAsFactors = FALSE
#  )
#site_advert_existing<-read.xlsx("data/site_advert_existing.xlsx", sheetIndex = 1)
mv_studies_recruiting<-left_join(mv_studies_recruiting, site_advert_existing, by=c("nct_id","Sponsor","Facility","country","state","city"))

site_advert_missing <- 
  aws.s3::s3read_using(read.csv, object = "s3://oakblocsite/site_advert_missing.txt",
                       header = TRUE,
                       sep = "|",
                       na.strings = "NA",
                       nrows = -100,
                       stringsAsFactors = FALSE)

mv_studies_recruiting<-rbind(mv_studies_recruiting, site_advert_missing)



var_studyphase_name <- unique(mv_studies_recruiting$StudyPhase)


#mv_studies_recruiting <- subset.data.frame(mv_studies_recruiting) %>%
#  sample_frac(0.2, replace = FALSE)

mv_studies_recruiting_tab <- mv_studies_recruiting %>%
    select(
        "ID",
        "Facility",
        "Sponsor",
        "Condition",
        "StudyPhase",
        "city",
        "state",
        "country",
        "ZipCode",
        "Region"                
    )



mv_studies_recruiting_world <- mv_studies_recruiting %>%
    filter(is.na(nct_id) == FALSE & is.na(latitude) == FALSE)




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
                fluidRow(style = "margin-top:0%; margin-bottom:1%;",                         
                         column(4, align="center",
                                h2("Kalehdoo Site Finder",
                                   #class = "display-4",
                                   style = "color: #1175B8; margin-top:0px;margin-left:2%; margin-right:2%",
                                   align = "center"
                                ),
                                h4( "Modify search & Hit Display Results button",
                                    style = "color: #F37312;",
                                    align = "center"),
                                dropdownButton(
                                                 tags$h5("Change the condition or disease name and hit the Orange Display Results buttons. The results would appear below. Click on the colored cirles to narrow down until you find the site location in blue. Click this Blue icon to view the Clinical Trial details and Facility contact details."),
                                                 circle = TRUE, 
                                                 status = "info",
                                                 icon = icon("info"), 
                                                 size = "sm",
                                                 width = "500px",
                                                 tooltip = tooltipOptions(title = "Help!")
                                             )
                         ),
                         column(4, align="center",
                                div(style="display: inline-block;",tags$a(href="https://kalehdoo.shinyapps.io/app02/", target="_blank", img(src="https://oakblocsite.s3.amazonaws.com/AddContactDetailsAd.jpg", height=150, width=300, style="padding: 2px;"))),
                                #div(style="display: inline-block;",tags$a(href="https://www.oakbloc.com", target="_blank", img(src="https://www.oakbloc.com/images/smartdevice.png", height=150, width=300, style="padding: 2px;")))
                         ),
                         column(4, align="center",
                                div(style="display: inline-block;",tags$a(href="https://www.oakbloc.com", target="_blank", img(src="https://oakblocsite.s3.amazonaws.com/SpaceAdv.jpg", height=150, width=300, style="padding: 2px;")))
                                #div(style="display: inline-block;",tags$a(href="http://www.nano-retina.com/retinal-degenerative-diseases/", target="_blank", img(src="https://www.oakbloc.com/images/app02/adv_111.jpg", height=150, width=200, style="padding: 1px;")))
                         )
                ),
                fluidRow(
                    column(
                        3,
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
                        3,
                        align = "center",
                        textInput(
                            inputId = "select_condition_map_world",
                            label = NULL,
                            value="diabetes",
                            placeholder = "Condition Name"
                        )
                    ),                    
                    
                    #code to create selectable and searchable LOV
                    #column(
                     #   2,
                      #  align = "center",
                      #  pickerInput(
                       #     inputId = "select_condition_map_world",
                        #    choices = rec_conditions_list1,
                         #   selected = c("covid-19"),
                          #  multiple = TRUE,
                           # options = list(`live-search` = TRUE,style = "btn-info",
                            #               `actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"
                            #),
                            #choicesOpt = list(
                             #   content = rec_conditions_list2
                            #)
                        #)
                   # ),
                    
                    
                    column(
                        3,
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
                    (tags$div(
                    class = "text-center",
                    actionButton(
                        "update_global",
                        "Display Results",
                        class = "btn btn-primary",
                        style = "margin-bottom: 0.5%;"
                    )
                )))
                ),
                tags$style(type = "text/css", "#plot_1020 {height: calc(100vh - 80px) !important;}"),
                fluidRow(withSpinner(leafletOutput("plot_1020"), type = 3)),
                #fluidRow(style = "margin-top:2%; margin-bottom:1%;",
                #         column(12, align="center",
                #                div(style="display: inline-block;",tags$a(href="http://www.nano-retina.com/retinal-degenerative-diseases/", target="_blank", img(src="https://www.oakbloc.com/images/app02/adv_111.jpg", height=150, width=150, style="padding: 2px;"))),
                #                div(style="display: inline-block;",tags$a(href="https://www.oakbloc.com", target="_blank", img(src="https://www.oakbloc.com/images/smartdevice.png", height=150, width=150, style="padding: 2px;"))),
                #                div(style="display: inline-block;",tags$a(href="https://www.oakbloc.com", target="_blank", img(src="https://www.oakbloc.com/images/smartdevice.png", height=150, width=150, style="padding: 2px;"))),
                #                div(style="display: inline-block;",tags$a(href="http://www.nano-retina.com/retinal-degenerative-diseases/", target="_blank", img(src="https://www.oakbloc.com/images/app02/adv_111.jpg", height=150, width=150, style="padding: 2px;"))),
                #                div(style="display: inline-block;",tags$a(href="https://www.oakbloc.com", target="_blank", img(src="https://www.oakbloc.com/images/smartdevice.png", height=150, width=150, style="padding: 2px;")))
                #         )
                #),
                fluidRow(style = "margin-top:2%;margin-left:1%; margin-right:1%",
                         h4(class="text-center",style = "color: #F37312; margin-top:2px; margin-left:2px; margin-right:2px;",
                            paste0("Data Refreshed On: ",log_app02_time))
                ),
                fluidRow(
                    style = "margin-top:1%;margin-left:1%; margin-right:1%",
                    h4(
                        "Kalehdoo Site Finder App is an interactive application that helps patients in finding clinical studies that are currently recruiting patients globally.",
                        style = "text-align:justify;"
                    )
                ),
                fluidRow(                    
                    h5(
                        "Disclaimer: The Clinical Trials data is available on ",
                        tags$a(href = "https://clinicaltrials.gov/", "ClinicalTrials.gov", target =
                                   "_blank"),
                        "Patients are advised to verify the details on clinicaltrials.gov and consult with their physician for any medical and legal advise before participating in any clinical trial. The purpose of site finder app is to help patients in finding the relevant clinical trials easily. Oakbloc Technologies do not accept any responsibility or liability for any direct, indirect, or consequential loss or damage resulting from any such irregularity, inaccuracy, or use of the information.",
                        style = "margin-top:0.1%;margin-left:1%; margin-right:1%; margin-bottom:2%; text-align:justify;"
                    )
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
    #updatePickerInput(session = session, inputId = 'select_condition_map_world',selected = "covid-19")
    #reactive dataset for world maps with reduced columns
    mv_studies_recruiting_map_world <-
        eventReactive(input$update_global, {            
            #createleaflet plot 1020 based on reactive set
            subset(
                mv_studies_recruiting_world,
                subset = (
                    StudyPhase %in%  c(input$select_studyphase_name_world) &
                        casefold(city) %like%  casefold(input$select_city_map_world) &
                        casefold(Condition) %like%   c(input$select_condition_map_world)                        
                )
            )
        })
    
    #createleaflet plot 1020
    #function to display labels
    f_labels_1020 <-
        function() {
            sprintf(
                "<strong>%s</strong><br/><strong>Facility: %s</strong><br/><strong>Email: %s</strong><br/><strong>Phone: %s</strong><br/><strong>Website: %s</strong><br/><strong>Location: %s</strong></br><strong>Sponsor: %s</strong><br/><strong>Trial Phase: %s</strong><br/><strong>Condition: %s</strong><br/><strong>View More Details: %s</strong>",
                #"<br><strong>ClinicalTrials.gov%s</strong></br><strong>Img%s</strong></br><strong>Phase: %s</strong><br/><strong>Country: %s</strong><br/><strong>City: %s</strong><br/><strong>Facility Name: %s</strong><br/><strong>Sponsor: %s</strong><br/><strong>Conditions: %s</strong>",
                mv_studies_recruiting_map_world()$img_url,
                mv_studies_recruiting_map_world()$Facility,
                mv_studies_recruiting_map_world()$contact_email,
                mv_studies_recruiting_map_world()$contact_phone,                
                mv_studies_recruiting_map_world()$contact_url,
                paste0(mv_studies_recruiting_map_world()$city,', ',mv_studies_recruiting_map_world()$country),                
                mv_studies_recruiting_map_world()$Sponsor,
                mv_studies_recruiting_map_world()$StudyPhase,                
                mv_studies_recruiting_map_world()$Condition,
                mv_studies_recruiting_map_world()$ID                
            ) %>%
                lapply(htmltools::HTML)
        }
    
    
    output$plot_1020 <- renderLeaflet({
        global_chart <- (
            leaflet(data = mv_studies_recruiting_map_world()) %>%
                setView(
                    lng = -4.055685,
                    lat = 41.294856,
                    zoom = 2.0
                ) %>%
                addProviderTiles(providers$CartoDB.Positron,
                                 options = providerTileOptions(noWrap = FALSE)) %>%
                addMarkers(
                    ~ longitude,
                    ~ latitude,
                    popup = f_labels_1020(),
                    clusterOptions = markerClusterOptions()
                )
        )
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
