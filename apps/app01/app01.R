#import libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- dashboardPage(
  #controls dashboard header
  dashboardHeader(title="Open Analytics"),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    checkboxInput("checkbox", "Choice A", value = TRUE),
    radioButtons("radio", h3("Radio buttons"),
                 choices = list("Choice 1" = 1, "Choice 2" = 2,
                                "Choice 3" = 3),selected = 1)
    
  ),
  #defines the body
  dashboardBody(
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
)

server <- function(input, output) {
  #read data
  in_mv_year_Lst10Yr<-paste(var_DIR_ACCT_HOME, "DATA/extracts/mv_year_Lst10Yr.txt", sep="")
  mv_year_Lst10Yr<-read.csv(in_mv_year_Lst10Yr, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)
  
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