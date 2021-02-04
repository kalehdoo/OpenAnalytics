#import libraries
library(shiny)
library(shinydashboard)
library(shinythemes)

#####################

ui <-
    fluidPage(
        #below includehtml is added for google analytics
        tags$head(includeHTML((
            "googleanalyticsapp01.html"
        ))), 
        navbarPage(
            title = div(
                tags$a(
                    href = "https://www.oakbloc.com",
                    target = "_blank",
                    tags$img(
                        src = "https://www.oakbloc.com/images/Oakbloc_transparent.svg",
                        width = "100px",
                        height = "35px",
                        style = "padding-top: 0px; padding-bottom: 5px; width: '100%';"
                    )
                )
            ),
            windowTitle = "Kalehdoo Analytics",
            #title=tags$head(tags$link(rel="icon",type="image/png", src="https://oakblocsite.s3.amazonaws.com/SiteFinder.png")),
            #theme = "united.min.css",
            title = tags$head(tags$link(rel="shortcut icon", 
                                        href="https://www.noaa.gov/sites/all/themes/custom/noaa/favicon.ico", 
                                        type="image/vnd.microsoft.icon"))),
            theme = shinytheme("united"),
            collapsible	= TRUE,
            fluid = TRUE,
            inverse = TRUE,
            footer = fluidRow(
                #tags$br(),
                h4(
                    tags$a(href = "https://www.oakbloc.com/", "Oakbloc Technologies", target =
                               "_blank"),
                    style = "margin-bottom:2%;",
                    align = "center",
                    "2019-2020",
                )
            ),
            tabPanel("Home",
                     fluidRow(style = "margin-top:2%; margin-bottom:1%;",
                              column(12, align="center",
                                     div(style="display: inline-block;",tags$a(href="https://kalehdoo.shinyapps.io/app02", target="_blank", img(src="https://oakblocsite.s3.amazonaws.com/SiteFinder.png", height=400, width=400, style="padding: 12px;"))),
                                     div(style="display: inline-block;",tags$a(href="https://kalehdoo.shinyapps.io/sponsor", target="_blank", img(src="https://oakblocsite.s3.amazonaws.com/SponsorAnalytics.png", height=400, width=400, style="padding: 12px;"))),
                                     div(style="display: inline-block;",tags$a(href="https://kalehdoo.shinyapps.io/clinical", target="_blank", img(src="https://oakblocsite.s3.amazonaws.com/ClinicalAnalytics.png", height=400, width=400, style="padding: 12px;")))
                                     
                              )
                     ))
            
            
            
            
        )
    )

################################################################
#SERVER BEGINS
################################################################

server <- function(input, output) {
}

shinyApp(ui, server)