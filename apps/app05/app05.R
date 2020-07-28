#import libraries
library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(data.table)
library(DT)
library(shinythemes)
#library(firebase)
library(shinyWidgets)
library(shinycssloaders)

read_bloodbank <- read.csv("data/blood-banks.csv", header = TRUE)

colnames(read_bloodbank)[colnames(read_bloodbank)=="Sr.No"]<-"SrNo"
colnames(read_bloodbank)[colnames(read_bloodbank)=="Blood.Bank.Name"]<-"BloodBankName"
colnames(read_bloodbank)[colnames(read_bloodbank)=="Contact.No"]<-"ContactNo"
colnames(read_bloodbank)[colnames(read_bloodbank)=="Nodal.Officer"]<-"NodalOfficer"
colnames(read_bloodbank)[colnames(read_bloodbank)=="Contact.Nodal.Officer"]<-"ContactNodOff"
colnames(read_bloodbank)[colnames(read_bloodbank)=="Mobile.Nodal.Officer"]<-"MobileNodOff"
colnames(read_bloodbank)[colnames(read_bloodbank)=="Email.Nodal.Officer"]<-"EmailNodOff"
colnames(read_bloodbank)[colnames(read_bloodbank)=="Blood.Component.Available"]<-"BloodComponentAv"

bloodbank <- read_bloodbank %>%
    select(
        "SrNo",
        "BloodBankName",
        "State",
        "District",
        "City",
        "Address",
        "Pincode",
        "ContactNo",
        "Mobile",
        "Helpline",
        "Fax",
        "Email",
        "Website",
        "NodalOfficer",
        "ContactNodOff",
        "MobileNodOff",
        "EmailNodOff",
        "Category",
        "BloodComponentAv",
        "Apheresis",
        "Latitude",
        "Longitude"
    )

bloodbank_tab <- read_bloodbank %>%
    select(
        "SrNo",
        "BloodBankName",
        "State",
        "District",
        "City",
        "Address",
        "Pincode",
        "ContactNo",
        "Mobile",
        "Helpline",
        "Fax",
        "Email",
        "Website",
        "NodalOfficer",
        "ContactNodOff",
        "MobileNodOff",
        "EmailNodOff",
        "Category",
        "BloodComponentAv",
        "Apheresis"
    )

# for spinners 2-3 match the background color of spinner
options(spinner.color.background = "#772953")
options(spinner.size = 1.5)


ui <-
    fluidPage(
        navbarPage(
            title = "Kalehdoo",
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
                fluidRow(
                    #uiOutput("img"),
                    h1(
                        "Blood Bank Finder",
                        class = "display-4",
                        style = "color: #F37312; margin-top:0px;margin-left:2%; margin-right:2%",
                        align = "center"
                    )
                ),
                fluidRow(
                    column(
                        2,
                        align = "center",
                        textInput(
                            inputId = "select_State_map",
                            label = NULL,
                            placeholder = "State"
                        )
                    ),
                    column(
                        2,
                        align = "center",
                        textInput(
                            inputId = "select_District_map",
                            label = NULL,
                            placeholder = "District"
                        )
                    ),
                    column(
                        2,
                        align = "center",
                        textInput(
                            inputId = "select_City_map",
                            label = NULL,
                            placeholder = "City Name"
                        )
                    ),
                    column(
                        2,
                        align = "center",
                        searchInput(
                            inputId = "select_City_map",
                            label = NULL,
                            placeholder = "City Name"
                        )
                    ),
                    column(
                        3,
                        align = "center",
                        textInput(
                            inputId = "select_Pincode_map",
                            label = NULL,
                            placeholder = "Pincode"
                        )
                    ),
                    column(
                        3,
                        align = "center",
                        textInput(
                            inputId = "select_BloodBank_map",
                            label = NULL,
                            placeholder = "Blood Bank Name"
                        )
                    )
                ),
                fluidRow(
                    withSpinner(leafletOutput("plot_1020"), type=3)
                ),
                fluidRow(
                    style = "margin-top:3%;margin-left:1%; margin-right:1%",
                    h4(
                        "The Clinical Study Finder App is an interactive application that helps in finding Blood Banks in India.",
                        style = "text-align:justify;"
                    ),
                    h4(
                        "The application is built using open-source Openstreet Maps, R and Shiny.",
                        style = "text-align:justify;"
                    )
                ),
                fluidRow(
                    h5(
                        "Disclaimer: The source data for the application is obtained from data.gov.in ",
                        tags$a(href = "https://data.gov.in", "data.gov.in.", target =
                                   "_blank"),
                        style = "margin-top:2%;margin-left:1%; margin-right:1%; text-align:justify;"
                    ),
                    h5("Disclaimer: The users are advised to verify the details on data.gov.in and consult with their physicians for any medical and legal advise. This Blood Bank finder app should be used as an interactive assistant in finding Blood Banks nearby. Oakbloc Technologies do not accept any responsibility or liability for any direct, indirect, or consequential loss or damage resulting from any such irregularity, inaccuracy, or use of the information.",
                        style = "margin-top:0.1%;margin-left:1%; margin-right:1%; margin-bottom:2%; text-align:justify;"
                    )
                )
                
            ),
            
            tabPanel("Table Details",
                     fluidRow(
                         withSpinner(DT::dataTableOutput("dt_bloodbank_tab"), type = 3)
                     ))
        )
    )





server <- function(input, output, session) {
    #ImgPath<-"https://www.oakbloc.com/images/mobileVan.png"
    #URLPath<-"https://oakbloc.com"
    #output$img <- renderUI({
     #   tags$a(href=URLPath, target="_blank", tags$img(src = ImgPath, width="200", height="100"))
    #})
    
    #reactive dataset for world maps with reduced columns
    bloodbank_map <-
        reactive({
            #createleaflet plot 1020 based on reactive set
            subset(
                bloodbank,
                subset = (
                        casefold(State) %like%  casefold(input$select_State_map) &
                        casefold(District) %like%  casefold(input$select_District_map) &
                        casefold(City) %like%  casefold(input$select_City_map) &
                        Pincode %like% input$select_Pincode_map &
                        casefold(BloodBankName) %like%  casefold(input$select_BloodBank_map)
                )
            )
        })
    
    #createleaflet plot 1020
    #function to display labels
    f_labels_1020 <-
        function() {
            sprintf(
                "<br><strong>Bank Name: %s</strong></br><strong>Address: %s</strong><br/><strong>City: %s</strong><br/><strong>Pincode: %s</strong><br/><strong>Contact No: %s</strong><br/><strong>Mobile: %s</strong><br/><strong>Helpline: %s</strong><br/><strong>Email: %s</strong>",
                bloodbank_map()$BloodBankName,
                bloodbank_map()$Address,
                bloodbank_map()$City,
                bloodbank_map()$Pincode,
                bloodbank_map()$ContactNo,
                bloodbank_map()$Mobile,
                bloodbank_map()$Helpline,
                bloodbank_map()$Email
            ) %>%
                lapply(htmltools::HTML)
        }
    output$plot_1020 <- renderLeaflet({
        global_chart <- (
            leaflet(data = bloodbank_map()) %>%
                setView(
                    lng = 79.88115,
                    lat = 23.14988,
                    zoom = 5.0
                ) %>%
                addProviderTiles(providers$CartoDB.Positron,
                                 options = providerTileOptions(noWrap = TRUE)) %>%
                addMarkers(
                    ~ Longitude,
                    ~ Latitude,
                    popup = f_labels_1020(),
                    clusterOptions = markerClusterOptions()
                )
        )
    })
    
    
    #reactive dataset for maps with reduced columns
    output$dt_bloodbank_tab <- renderDataTable({
        DT::datatable(
            bloodbank_tab,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    
    
}


shinyApp(ui, server)
