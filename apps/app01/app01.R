#import libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(markdown)
library(DT)


#read data
mv_year_Lst10Yr<-read.csv("data/mv_year_Lst10Yr.txt", header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
mv_studies_recruiting_s<-read.csv("data/mv_studies_recruiting_s.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = 100)
mv_studies_recruiting_loc<-read.csv("data/mv_studies_recruiting_loc.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = -100)
mv_studies_recruiting_loc_US<-subset.data.frame(mv_studies_recruiting_loc, subset = country=="united states")

ui <- navbarPage("Open Clinical Analytics",
                 navbarMenu("Recruitment",
                   tabPanel("Find Studies",
                              mainPanel(width = 12,
                                        fluidRow(
                                          tags$h5("Find recruiting Clinical Studies. Search for anything on the search box on right top or search in individual columns.
                                              Click the hyperlink on ID to navigate to clinicaltrials.gov to view study and contact details.")
                                          
                                        ),
                                        #display recriting studies data table
                                        fluidRow(
                                          DT::dataTableOutput("dt_recruitment_1005")
                                        )
                              )
                            
                            
                   ),
                   tabPanel("Recruitment By Location",
                            mainPanel(width=12,
                                      tabsetPanel(type="tabs",
                                                  tabPanel("USA",
                                                           fluidRow(
                                                             plotlyOutput("plot_1006")
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
                   tabPanel("Recruitment World",
                            mainPanel(width=12,
                                      
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
  
  #cteate plot 1006
  output$plot_1006 <- renderPlotly(
    {
      plot_geo(mv_studies_recruiting_loc_US, lat = ~latitude, lon = ~longitude) %>%
        add_markers(
          text = ~paste(city, state, paste("Studies:", cnt_studies), sep = "<br />"),
          color = ~cnt_studies, symbol = I("circle"), size = I(10), hoverinfo = "text"
        ) %>%
        hide_colorbar() %>%
        layout(
          title = 'Recruiting Studies in USA (Hover for details)', 
            geo = list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showland = TRUE,
            landcolor = toRGB("gray95"),
            subunitcolor = toRGB("gray85"),
            countrycolor = toRGB("gray85"),
            countrywidth = 0.8,
            subunitwidth = 0.8
          )
        )
    })
  
  #cteate plot 1007
  output$plot_1007 <- renderPlotly(
    {
      plot_geo(mv_studies_recruiting_loc, lat = ~latitude, lon = ~longitude) %>%
        add_markers(
          text = ~paste(city, country, paste("Studies:", cnt_studies), sep = "<br />"),
          color = ~cnt_studies, symbol = I("circle"), size = I(8), hoverinfo = "text"
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
            subunitwidth = 0.5
          )
        )
    })
  
  
  #datatable for recruitment
  output$dt_recruitment_1005<- renderDataTable(
    {
      DT::datatable(mv_studies_recruiting_s, filter = 'top', escape=FALSE,rownames = FALSE,
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