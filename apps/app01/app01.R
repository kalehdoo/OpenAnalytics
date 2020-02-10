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
mv_studies_recruiting_s<-read.csv("data/mv_studies_recruiting_s.txt", header=TRUE, sep = "|", na.strings = "NA", nrows = -100)

ui <- navbarPage("Open Clinical Analytics",
                 navbarMenu("Recruitment",
                   tabPanel("Find Studies",
                            sidebarLayout(
                              sidebarPanel(width = 2,
                                           checkboxGroupInput("cnames_recruiting", "Select Columns:",
                                                              names(mv_studies_recruiting_s), selected = names(mv_studies_recruiting_s))
                              ),
                              mainPanel(width = 10,
                                        fluidRow(
                                          tags$h5("Find Clinical Studies. Search for anything on the search box on right top or search in individual columns.
                                              Click the hyperlink on ID to navigate to clinicaltrials.gov to view study and contact details.")
                                          
                                        ),        
                                        #display recriting studies data table
                                        fluidRow(
                                          DT::dataTableOutput("dt_recruitment_5001")
                                        )
                              ))
                            
                            
                   ),
                   tabPanel("Tab2"
                     
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
                                box(plotlyOutput("plot_1005", height = 200), title = "Studies Registered"),
                                box(plotlyOutput("plot_1006", height = 200), title = "Studies Started")
                              ),
                              #create box for plot 1001
                              fluidRow(
                                box(plotlyOutput("plot_1007", height = 200), title = "Studies Completed"),
                                box(plotlyOutput("plot_1008", height = 200), title = "Results Posted")
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
  #datatable for recruitment
  output$dt_recruitment_5001<- renderDataTable(
    {
      DT::datatable(mv_studies_recruiting_s[, input$cnames_recruiting, drop=FALSE], filter = 'top', rownames = FALSE,
                   escape=FALSE )
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