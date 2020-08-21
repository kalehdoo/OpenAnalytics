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
library(shinycssloaders)
library(shinyWidgets)
library(collapsibleTree)


#read study Measurements by condition
measurements <-
    read.csv(
        "data/agg_studies_outcome_measurements.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

#read sponsor agg data
agg_sponsors <-
    read.csv(
        "data/agg_sponsors.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )


agg_sponsors_by_time <-
    read.csv(
        "data/agg_sponsors_by_time.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

agg_sponsorstype_by_time <-
    read.csv(
        "data/agg_sponsorstype_by_time.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

#read sponsor network data
r_sponsor_collaborator <-
    read.csv(
        "data/r_sponsor_collaborator.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )



r_sponsor_site <-
    read.csv(
        "data/r_sponsor_site.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

r_sponsor_conditions <-
    read.csv(
        "data/r_sponsor_conditions.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

r_sponsor_interventions <-
    read.csv(
        "data/r_sponsor_interventions.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

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

#import recruiting data
mv_studies_recruiting <-
    readRDS("data/mv_studies_recruiting.rds") %>%
    filter(is.na(nct_id) == FALSE)


var_studyphase_name <- unique(mv_studies_recruiting$StudyPhase)

#mv_studies_recruiting <-
#    subset.data.frame(mv_studies_recruiting) %>%
#    sample_frac(0.1, replace = FALSE)

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



#recruiting data at sponsor level
rec_sponsors <-
    read.csv(
        "data/mv_agg_rec_sponsors.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )


#####################
# for spinners 2-3 match the background color of spinner
options(spinner.color.background = "#772953")
options(spinner.size = 1.5)

ui <- navbarPage(
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
    #theme = "united.min.css",
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
    #Landing Home page starts
    
    tabPanel(
        title = "Home",
        fluidRow((
            h3("Kalehdoo Clinical Analytics Platform (Under Development)", style = "margin-top:0px;margin-left:5%; margin-right:5%")
        )),
        fluidRow(
            style = "margin-top:0px;margin-left:1%; margin-right:1%",
            p(
                "Clinical Analytics provides insights into the clinical research industry.
               The market and competitive intelligence gained from the insights can benefit Sponsors, CROs, Industry Analysts, Non-Profit, Government and Special Interest Organizations explore avenues for future growth."
            ),
            p(
                tags$h4("Sponsors:"),
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
            p(tags$h4("Physicians & Patients:"),
              tags$ul(
                  tags$li("Participation Opportunities"),
                  tags$li("Recruiting Studies")
              )),
            p(
                tags$h4("Market & Industry Analysts:"),
                tags$ul(tags$li("Current Trends"),
                        tags$li("Market Intelligence"))
            ),
            p(
                tags$h4("Special Interest Organizations:"),
                tags$ul(tags$li("Global Clinical Activity"),
                        tags$li("Demographics"))
            )
        ),
        fluidRow((
            p(
                "Visit us at  "
                ,
                tags$a(href = "https://oakbloc.com", "Oakbloc Technologies", target =
                           "_blank"),
                style = "margin-top:0px;margin-left:1%; margin-right:1%"
            )
        )),
        fluidRow(
            h5(
                "Disclaimer: The source data for the application is obtained from ",
                tags$a(href = "https://clinicaltrials.gov/", "ClinicalTrials.gov", target =
                           "_blank"),
                "Oakbloc Technologies tries to keep the data as accurate as possible. However, there are chances of mistakes and inaccuracies which is solely unintentional. Oakbloc Technologies do not accept any responsibility or liability for any direct, indirect, or consequential loss or damage resulting from any such irregularity, inaccuracy, or use of the information by anyone. Please use at your own risk.",
                style = "margin-top:0.1%;margin-left:1%; margin-right:1%; margin-bottom:2%; text-align:justify;"
            )
        ),
        fluidRow((
            p(
                "Follow us on Twitter for regular updates  "
                ,
                tags$a(href = "https://twitter.com/OakBlocTech", "Twitter", target =
                           "_blank"),
                style = "margin-top:0px;margin-left:1%; margin-right:1%"
            )
        ))
    ),
    #sponsor Starts Here
    navbarMenu(
        "Sponsor",
        tabPanel("Summary",
                 mainPanel(
                     width = 12,
                     tabsetPanel(
                         type = "tabs",
                         tabPanel(
                             "Summary",
                             fluidRow(
                                 style = "margin-top:2px;",
                                 column(
                                     1,
                                     align = "left",
                                     #tags$h2("Dropdown Button"),
                                     #br(),
                                     dropdownButton(
                                         tags$h5("Only Sponsors with more than one clinical study registered are included in this analysis."),
                                         #br(),
                                         tags$h5("See the Data Dictionary tab for detailed definitions of the metrics."),
                                         circle = TRUE, 
                                         status = "info",
                                         icon = icon("info"), 
                                         size = "xs",
                                         width = "500px",
                                         tooltip = tooltipOptions(title = "Help!")
                                     )
                                 )
                             ),
                             fluidRow(
                                 column(align = "center", style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("sponsorbox_cnt_sponsors", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("sponsorbox_cnt_sponsors_Ind", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("sponsorbox_cnt_sponsors_Hosp", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("sponsorbox_cnt_sponsors_Acad", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("sponsorbox_cnt_sponsors_Rec", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("sponsorbox_cnt_sponsors_Global", width = 2)
                                 )
                             ),
                             
                             fluidRow(
                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid",
                                     withSpinner(plotlyOutput("plot_sp_sum_1111"), type = 3)
                                 ),
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid;",
                                     withSpinner(plotlyOutput("plot_sp_sum_1112"), type = 3)
                                 )
                             ),
                             fluidRow(
                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid",
                                     withSpinner(plotlyOutput("plot_sp_sum_1113"), type = 3)
                                 ),
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid;",
                                     withSpinner(plotlyOutput("plot_sp_sum_1114"), type = 3)
                                 )
                             )
                         ),
                         tabPanel(
                             "Industry Share",
                             fluidRow(
                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid",
                                     withSpinner(plotlyOutput("plot_sp_sum_1121"), type = 3)
                                 ),
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid;",
                                     withSpinner(plotlyOutput("plot_sp_sum_1122"), type = 3)
                                 )
                             ),
                             fluidRow(
                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid",
                                     withSpinner(plotlyOutput("plot_sp_sum_1123"), type = 3)
                                 ),
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid;",
                                     withSpinner(plotlyOutput("plot_sp_sum_1124"), type = 3)
                                 )
                             )
                         ),
                         tabPanel(
                             "Industry Trend",
                             fluidRow(
                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid",
                                     withSpinner(plotlyOutput("plot_sp_sum_1131"), type = 3)
                                 ),
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid;",
                                     withSpinner(plotlyOutput("plot_sp_sum_1132"), type = 3)
                                 )
                             ),
                             fluidRow(
                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid",
                                     withSpinner(plotlyOutput("plot_sp_sum_1133"), type = 3)
                                 ),
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid;",
                                     withSpinner(plotlyOutput("plot_sp_sum_1134"), type = 3)
                                 )
                             )
                         ),
                         #summary tab ends here
                         tabPanel("Sponsor Data",
                                  fluidRow(DT::dataTableOutput("dt_agg_sponsors_summ"))
                                  ),
                         tabPanel("Data Dictionary",
                                  fluidRow(
                                      style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                      tags$h5("Only Sponsors with more than one clinical study registered are included in this analysis."),
                                      h5("List of column names and their descriptions:"),
                                      tags$div(tags$ol(
                                        tags$li(tags$span("lead_sponsor_name: The full name of the lead sponsor who sponsored the clinical trial.")),
                                        tags$li(tags$span("agency_class: The agency class of a sponsor indicating if the sponsor belongs to Industry or others.")),
                                        tags$li(tags$span("sponsor_type: The field is not available in original data and is derived from agency class. The others in agency class is further expanded to categorise sponsors into Hospital, Academic or Others based on the name of the sponsor. It may not be 100% correct.")),
                                        tags$li(tags$span("cnt_studies_registered: The total number of studies registered by a sponsor in clinicaltrials.gov.")),
                                        tags$li(tags$span("year_first_study_reg: The year when the sponsor registered its first study.")),
                                        tags$li(tags$span("year_last_study_reg: The year when the sponsor registered its most recent study.")),
                                        tags$li(tags$span("cnt_US_only_studies: The number of studies the were conducted inside the United States only.")),
                                        tags$li(tags$span("cnt_nonUS_only_studies: The number of studies the were conducted completely outside the United States.")),
                                        tags$li(tags$span("cnt_global_studies: The number of studies the were conducted both in the United States as well as atleast one other country.")),
                                        tags$li(tags$span("cnt_studies_1country: The number of studies that involved exactly one country.")),
                                        tags$li(tags$span("cnt_studies_2to5country: The number of studies that involved greater than or equal to 2 countries and less than or equal to 5 countries.")),
                                        tags$li(tags$span("cnt_studies_6to10country: The number of studies that involved greater than or equal to 6 countries and less than or equal to 10 countries.")),
                                        tags$li(tags$span("cnt_studies_11to30country: The number of studies that involved greater than or equal to 11 countries and less than or equal to 30 countries.")),
                                        tags$li(tags$span("cnt_studies_31to50country: The number of studies that involved greater than or equal to 31 countries and less than or equal to 50 countries.")),
                                        tags$li(tags$span("cnt_studies_50pluscountry: The number of studies that involved greater than to 50 countries.")),
                                        tags$li(tags$span("cnt_completed_status: The number of studies marked as completed by the sponsor.")),
                                        tags$li(tags$span("cnt_suspended: The number of studies marked as completed by the sponsor.")),
                                        tags$li(tags$span("cnt_started_actual: The number of studies that were actually started in the past.")),
                                        tags$li(tags$span("cnt_recruiting_status: The number of studies marked as Racruiting by the sponsor.")),
                                        tags$li(tags$span("cnt_results_submitted: The number of studies with study results posted or submitted by the sponsor.")),
                                        tags$li(tags$span("ratio_results_to_completed: The percentage ratio of number of studies with results posted and number of completed studies.")),
                                        tags$li(tags$span("cnt_has_dmc: The number of studies having DMC(Data Monitoring Committee).")),
                                        tags$li(tags$span("cnt_interventional: The number of Interventional studies.")),
                                        tags$li(tags$span("cnt_observational: The number of Observational studies.")),
                                        tags$li(tags$span("cnt_phase1: The number of studies having Phase 1 study phase.")),
                                        tags$li(tags$span("cnt_phase2: The number of studies having Phase 2 study phase.")),
                                        tags$li(tags$span("cnt_phase3: The number of studies having Phase 3 study phase.")),
                                        tags$li(tags$span("cnt_phase4: The number of studies having Phase 4 study phase.")),
                                        tags$li(tags$span("cnt_actual_study_enrollment: The total number of study participants actually enrolled by the sponsor.")),
                                        tags$li(tags$span("cnt_anticipated_study_enrollment: The total number of study participants the sponsor anticipated to enroll.")),
                                        tags$li(tags$span("cnt_study_registered_curryr: The total number of studies registered by the sponsor in the current year.")),
                                        tags$li(tags$span("cnt_started_actual_curryr: The total number of studies started by the sponsor in the current year.")),
                                        tags$li(tags$span("cnt_study_registered_lstyr: The total number of studies registered by the sponsor in the previous year.")),
                                        tags$li(tags$span("cnt_started_actual_lstyr: The total number of studies started by the sponsor in the previous year.")),
                                        tags$li(tags$span("cnt_completed_status_lstyr: The total number of studies completed by the sponsor in the previous year.")),
                                        tags$li(tags$span("cnt_results_submitted_lstyr: The total number of studies with results posted or submitted by the sponsor in the previous year.")),
                                        tags$li(tags$span("cnt_conditions: The total number of study conditions for all the studies registered by the sponsor.")),
                                        tags$li(tags$span("cnt_rare_condition_match: The total number of rare-disease conditions for all the studies registered by the sponsor.")),
                                        tags$li(tags$span("cnt_rare_condition_studies: The total number of studies that involved at least one rare-disease condition.")),
                                        tags$li(tags$span("avg_register_to_start_days_ph1: The average number of days from study registration to study initiation for Phase 1 studies.")),
                                        tags$li(tags$span("avg_register_to_start_days_ph2: The average number of days from study registration to study initiation for Phase 2 studies.")),
                                        tags$li(tags$span("avg_register_to_start_days_ph3: The average number of days from study registration to study initiation for Phase 3 studies.")),
                                        tags$li(tags$span("avg_register_to_start_days_ph4: The average number of days from study registration to study initiation for Phase 4 studies.")),
                                        tags$li(tags$span("avg_start_to_complete_days_ph1: The average number of days from study initiation to study completion for Phase 1 studies.")),
                                        tags$li(tags$span("avg_start_to_complete_days_ph2: The average number of days from study initiation to study completion for Phase 2 studies.")),
                                        tags$li(tags$span("avg_start_to_complete_days_ph3: The average number of days from study initiation to study completion for Phase 3 studies.")),
                                        tags$li(tags$span("avg_start_to_complete_days_ph4: The average number of days from study initiation to study completion for Phase 4 studies.")),
                                        tags$li(tags$span("sponsor_size: The size of the sponsor based on the number of studies registered. For example in 2_XXS_11T50, 2 is the rank 1 being the smallest, XXS is Extra Extra Small, 11T50 means the sponor has registered studies between 11 and 50.")),
                                        tags$li(tags$span("cnt_collaborators: The total number of collaborators a sponsor has worked with.")),
                                        tags$li(tags$span("cnt_colab_NIH: The total number of NIH collaborators a sponsor has worked with.")),
                                        tags$li(tags$span("cnt_colab_nonind: The total number of Non-Industry collaborators a sponsor has worked with.")),
                                        tags$li(tags$span("cnt_colab_USFed: The total number of US Fed collaborators a sponsor has worked with.")),
                                        tags$li(tags$span("cnt_colab_Ind: The total number of Industry collaborators a sponsor has worked with.")),
                                        tags$li(tags$span("cnt_colab_Hosp: The total number of Hospital collaborators a sponsor has worked with.")),
                                        tags$li(tags$span("cnt_colab_Acad: The total number of Academic collaborators a sponsor has worked with."))
                                        ))
                                  )
                         )
                     )
                 )),
        tabPanel("Network",
                 mainPanel(
                     width = 12,
                     tabsetPanel(
                         type = "tabs",
                         tabPanel("Explore Data",
                                  fluidRow(
                                      withSpinner(DT::dataTableOutput("dt_sponsor_collaborator"), type = 3)
                                  )),
                         tabPanel(
                             "Network Tree",
                             fluidRow(
                                 style = "margin-top:2px;",
                                 column(
                                     5,
                                     align = "center",
                                     textInput(
                                         inputId = "select_sponsor_coll",
                                         label = NULL,
                                         value = "pfizer",
                                         placeholder = "Sponsor Name"
                                     )
                                 ),
                                 column(
                                     5,
                                     align = "center",
                                     textInput(
                                         inputId = "select_sponsor_coll_colname",
                                         label = NULL,
                                         value = "",
                                         placeholder = "Collaborator Name"
                                     )
                                 ),
                                 column(2,
                                        align = "left",
                                        tags$div(
                                            class = "text-center",
                                            actionButton(
                                                "update_sponsor_collaborator_1",
                                                "Display Results",
                                                class = "btn btn-primary",
                                                style = "margin-bottom: 0.5%;"
                                            )
                                        ))
                             ),
                             fluidRow(withSpinner(
                                 collapsibleTreeOutput("sponsor_coll_tree"), type = 3
                             ))
                         )
                     )
                 )),
        tabPanel("Performance",
                 mainPanel(
                     width = 12,
                     tabsetPanel(type = "tabs",
                                 tabPanel(
                                     "Compare",
                                     fluidRow(
                                         style = "margin-top:2px;",
                                         column(
                                             1,
                                             align = "left",
                                             #tags$h2("Dropdown Button"),
                                             #br(),
                                             dropdownButton(
                                                 tags$h5("Enter the sponsor names and hit Compare button to compare the sponsors side by side. Sponsor names are case insensitive."),
                                                 circle = TRUE, 
                                                 status = "info",
                                                 icon = icon("info"), 
                                                 size = "xs",
                                                 width = "500px",
                                                 tooltip = tooltipOptions(title = "Help!")
                                             )
                                         ),
                                         column(
                                             5,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_sponsor_name_1",
                                                 label = NULL,
                                                 value = "pfizer",
                                                 placeholder = "Sponsor Name 1"
                                             )
                                         ),
                                         column(1,
                                                align = "left",
                                                tags$div(
                                                    class = "text-center",
                                                    actionButton(
                                                        "update_agg_sponsor_1",
                                                        "Compare",
                                                        class = "btn btn-primary",
                                                        style = "margin-bottom: 0.5%;"
                                                    )
                                                )),
                                         column(
                                             5,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_sponsor_name_2",
                                                 label = NULL,
                                                 value = "Novartis Pharmaceuticals",
                                                 placeholder = "Sponsor Name 2"
                                             )
                                         )
                                     ),
                                     fluidRow(
                                         withSpinner(DT::dataTableOutput("dt_agg_sponsor_compare"), type = 3)
                                     )
                                 ),
                                 tabPanel(
                                     "Initiation",
                                     fluidRow(
                                         style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid",
                                             withSpinner(plotlyOutput("plot_sp_per_1321"), type = 3)
                                         ),
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid;",
                                             withSpinner(plotlyOutput("plot_sp_per_1322"), type = 3)
                                         )
                                     ),
                                     fluidRow(
                                         style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid",
                                             withSpinner(plotlyOutput("plot_sp_per_1323"), type = 3)
                                         ),
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid;",
                                             withSpinner(plotlyOutput("plot_sp_per_1324"), type = 3)
                                         )
                                     )
                                 ),
                                 tabPanel(
                                     "Completion",
                                     fluidRow(
                                         style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid",
                                             withSpinner(plotlyOutput("plot_sp_per_1311"), type = 3)
                                         ),
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid;",
                                             withSpinner(plotlyOutput("plot_sp_per_1312"), type = 3)
                                         )
                                     ),
                                     fluidRow(
                                         style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid",
                                             withSpinner(plotlyOutput("plot_sp_per_1313"), type = 3)
                                         ),
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid;",
                                             withSpinner(plotlyOutput("plot_sp_per_1314"), type = 3)
                                         )
                                     )
                                 ),
                                 tabPanel(
                                     "KPIs",
                                     fluidRow(
                                         style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid",
                                             withSpinner(plotlyOutput("plot_sp_per_1331"), type = 3)
                                         ),
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid;",
                                             withSpinner(plotlyOutput("plot_sp_per_1332"), type = 3)
                                         )
                                     ),
                                     fluidRow(
                                         style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid",
                                             withSpinner(plotlyOutput("plot_sp_per_1333"), type = 3)
                                         ),
                                         column(
                                             6,
                                             align = "left",
                                             style = "border: 0.3px solid;",
                                             withSpinner(plotlyOutput("plot_sp_per_1334"), type = 3)
                                         )
                                     )
                                 ),
                                 tabPanel("Metrics Table",
                                          fluidRow(
                                              withSpinner(DT::dataTableOutput("dt_agg_sponsors"), type = 3)
                                          ))
                                 )
                 )),
        tabPanel("Sites",
                 mainPanel(
                     width = 12,
                     tabsetPanel(type = "tabs",
                                 tabPanel("Explore Data",
                                          fluidRow(
                                              withSpinner(DT::dataTableOutput("dt_sponsor_site"), type = 3)
                                          )),
                                 tabPanel(
                                     "Network Tree",
                                     fluidRow(
                                         style = "margin-top:2px;",
                                         column(
                                             3,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_sponsor_site",
                                                 label = NULL,
                                                 value = "pfizer",
                                                 placeholder = "Sponsor Name"
                                             )
                                         ),
                                         column(
                                             2,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_sponsor_site_country",
                                                 label = NULL,
                                                 value = "United States",
                                                 placeholder = "Country"
                                             )
                                         ),
                                         column(
                                             2,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_sponsor_site_city",
                                                 label = NULL,
                                                 value = "",
                                                 placeholder = "City"
                                             )
                                         ),
                                         column(
                                             3,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_sponsor_site_facility",
                                                 label = NULL,
                                                 value = "",
                                                 placeholder = "Site Name"
                                             )
                                         ),
                                         column(2,
                                                align = "left",
                                                tags$div(
                                                    class = "text-center",
                                                    actionButton(
                                                        "update_sponsor_site_1",
                                                        "Display Results",
                                                        class = "btn btn-primary",
                                                        style = "margin-bottom: 0.5%;"
                                                    )
                                                ))
                                     ),
                                     fluidRow(withSpinner(
                                         collapsibleTreeOutput("sponsor_site_tree"), type = 3
                                     ))
                                 )
                     )
                 )),
        tabPanel("Conditions",
                 mainPanel(
                     width = 12,
                     tabsetPanel(type = "tabs",
                                 tabPanel("Explore Data",
                                          fluidRow(
                                              withSpinner(DT::dataTableOutput("dt_sponsor_conditions"), type = 3)
                                          )),
                                 tabPanel(
                                     "Network Tree",
                                     fluidRow(
                                         style = "margin-top:2px;",
                                         column(
                                             5,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_sponsor_cond",
                                                 label = NULL,
                                                 value = "pfizer",
                                                 placeholder = "Sponsor Name"
                                             )
                                         ),
                                         column(
                                             5,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_sponsor_condname",
                                                 label = NULL,
                                                 value = "diabetes",
                                                 placeholder = "Condition Name"
                                             )
                                         ),
                                         column(2,
                                                align = "left",
                                                tags$div(
                                                    class = "text-center",
                                                    actionButton(
                                                        "update_sponsor_cond_1",
                                                        "Display Results",
                                                        class = "btn btn-primary",
                                                        style = "margin-bottom: 0.5%;"
                                                    )
                                                ))
                                     ),
                                     fluidRow(withSpinner(
                                         collapsibleTreeOutput("sponsor_cond_tree"), type = 3
                                     ))
                                 )
                     )
                 )),
        tabPanel("Interventions",
                 mainPanel(
                     width = 12,
                     tabsetPanel(type = "tabs",
                                 tabPanel("Explore Data",
                                          fluidRow(
                                              withSpinner(DT::dataTableOutput("dt_sponsor_interventions"), type = 3)
                                          )))
                 ))
    ),
    navbarMenu("Collaborator",
               tabPanel(
                   "Network",
                   mainPanel(width = 12,
                             tabsetPanel(
                                 type = "tabs",
                                 tabPanel("Data Table",
                                          fluidRow(
                                              withSpinner(DT::dataTableOutput("dt_collaborator_sponsor"), type = 3)
                                          )),
                                 tabPanel(
                                     "Network",
                                     fluidRow(
                                         style = "margin-top:2px;",
                                         column(
                                             3,
                                             align = "center",
                                             textInput(
                                                 inputId = "select_coll_sponsor",
                                                 label = NULL,
                                                 value = "quintiles",
                                                 placeholder = "Collaborator Name"
                                             )
                                         ),
                                         column(9,
                                                align = "left",
                                                tags$div(
                                                    class = "text-center",
                                                    actionButton(
                                                        "update_collaborator_sponsor_1",
                                                        "Display Results",
                                                        class = "btn btn-primary",
                                                        style = "margin-bottom: 0.5%;"
                                                    )
                                                ))
                                     ),
                                     fluidRow(withSpinner(
                                         collapsibleTreeOutput("coll_sponsor_tree"), type = 3
                                     ))
                                 )
                             ))
               )),
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
                                 column(align = "center", style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        3,
                                        shinydashboard::valueBoxOutput("box_studies", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        3,
                                        shinydashboard::valueBoxOutput("box_sponsors", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("box_countries", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("box_cities", width = 2)
                                 ),
                                 column(align = "center",style = "background-color: #46D2CE; padding: 2px; border-style: solid; border-width: 0.3px;",
                                        2,
                                        shinydashboard::valueBoxOutput("box_facilities", width = 2)
                                 )
                             ),
                             
                             fluidRow(
                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid",
                                     plotlyOutput("plot_1008")
                                 ),
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid;",
                                     plotlyOutput("plot_1008_2")
                                 )
                             ),
                             fluidRow(
                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid",
                                     plotlyOutput("plot_1008_3")
                                 ),
                                 column(
                                     6,
                                     align = "left",
                                     style = "border: 0.3px solid;",
                                     plotlyOutput("plot_1008_4")
                                 )
                             )
                         ),
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
                             title = "Globe",
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
                                         options = list(`actions-box` = TRUE, style = "btn-info"),
                                         multiple = TRUE,
                                         selected = c(var_studyphase_name)
                                     )
                                 ),
                                 column(
                                     2,
                                     align = "center",
                                     textInput(
                                         inputId = "select_condition_map_world",
                                         label = NULL,
                                         value = "covid-19",
                                         placeholder = "Disease Name"
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
                             fluidRow(withSpinner(leafletOutput("plot_1020"), type = 3))
                             
                         ),
                         tabPanel("Table Details",
                                  #display recriting studies data table
                                  withSpinner(
                                      DT::dataTableOutput("dt_recruitment_1005"), type = 3
                                  ))
                         
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
                 ))
    )
    ,
    navbarMenu(
        "Study",
        tabPanel("Trends",
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
                     tabPanel(
                         "Monthly",
                         mainPanel(
                             width = 12,
                             fluidRow(tags$h4("Monthly Comparison (Last Year Month Vs Current Year Month)")),
                             fluidRow(
                                 box(plotlyOutput("plot_month_1001", height = 200), title = "Studies Registered"),
                                 box(plotlyOutput("plot_month_1002", height = 200), title = "Studies Started")
                             ),
                             #create box for plot 1001
                             fluidRow(
                                 box(plotlyOutput("plot_month_1003", height = 200), title = "Studies Completed"),
                                 box(plotlyOutput("plot_month_1004", height = 200), title = "Results Posted")
                             )
                         )
                     )
                 )),
        tabPanel("Design",
                 mainPanel(
                     width = 12,
                     tabsetPanel(type = "tabs",
                                 tabPanel("Measurements",
                                          fluidRow(
                                              withSpinner(DT::dataTableOutput("dt_measurements"), type = 3)
                                          )))
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
    
    #reactive dataset for sponsor comparison
    agg_sponsor_compare <-
        eventReactive(input$update_agg_sponsor_1, {
            agg_sponsor_1<-subset(agg_sponsors,
                subset = (
                    casefold(lead_sponsor_name) %like% casefold(c(input$select_sponsor_name_1))
                |
                    casefold(lead_sponsor_name) %like% casefold(c(input$select_sponsor_name_2))
                )
            )
            
                        
        })
    
    
    
    output$dt_agg_sponsor_compare <- renderDataTable({
        DT::datatable(
            agg_sponsor_compare(),
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE,
                           dom='t')
        )
    })
    
   ######################################################################
    #Create infobox for sponsor dashboard
    summ_sponsor <- data.frame(        
        cnt_sponsors = length(unique(agg_sponsors$lead_sponsor_name)),
        cnt_sponsors_Ind = sum(ifelse(agg_sponsors$agency_class == "Industry", 1, 0), na.rm = TRUE),
        cnt_sponsors_Hosp = sum(ifelse(agg_sponsors$sponsor_type == "Hospital", 1, 0), na.rm = TRUE),
        cnt_sponsors_Acad = sum(ifelse(agg_sponsors$sponsor_type == "Academic", 1, 0), na.rm = TRUE),
        cnt_sponsors_Rec = sum(ifelse(agg_sponsors$cnt_recruiting_status >= 1, 1, 0), na.rm = TRUE),
        cnt_sponsors_Global = sum(ifelse(agg_sponsors$cnt_global_studies >= 1, 1, 0), na.rm = TRUE)
    )
    
    output$sponsorbox_cnt_sponsors <- shinydashboard::renderValueBox({
        valueBox("Sponsors",
                 summ_sponsor$cnt_sponsors
        )
    })
    
    output$sponsorbox_cnt_sponsors_Ind <- shinydashboard::renderValueBox({
        valueBox("Industry",
                 summ_sponsor$cnt_sponsors_Ind
                 #icon = icon("credit-card")
        )
    })
    
    output$sponsorbox_cnt_sponsors_Hosp <- shinydashboard::renderValueBox({
        valueBox("Hospital",
                 summ_sponsor$cnt_sponsors_Hosp
        )
    })
    
    output$sponsorbox_cnt_sponsors_Acad <- shinydashboard::renderValueBox({
        valueBox("Academic",
                 summ_sponsor$cnt_sponsors_Acad)
    })
    
    output$sponsorbox_cnt_sponsors_Rec <- shinydashboard::renderValueBox({
        valueBox("Recruiting",
                 summ_sponsor$cnt_sponsors_Rec)
    })
    
    output$sponsorbox_cnt_sponsors_Global <- shinydashboard::renderValueBox({
        valueBox("Global",
                 summ_sponsor$cnt_sponsors_Global)
    })
    
    #top sponsors - summary
    top_sponsors <-
        data.frame(agg_sponsors, stringsAsFactors = FALSE) %>%
        filter(is.na(lead_sponsor_name) == FALSE) %>%
        group_by(lead_sponsor_name) %>%
        summarise(cnt_st = sum(cnt_studies_registered)) %>%
        top_n(10, cnt_st) %>%
        arrange(desc(cnt_st))
    
    top_sponsors$lead_sponsor_name <-
        factor(top_sponsors$lead_sponsor_name,
               levels = unique(top_sponsors$lead_sponsor_name)[order(top_sponsors$cnt_st, decreasing = FALSE)])
    
    
    
    output$plot_sp_sum_1111 <- renderPlotly({
        top_sponsors %>%
            plot_ly(
                y =  ~ lead_sponsor_name,
                x =  ~ cnt_st,
                type = 'bar',
                text = ~ paste(
                    paste("Sponsor:", lead_sponsor_name),
                    paste("Total Studies:", cnt_st),
                    sep = "<br />"
                ),
                hoverinfo = 'text',
                textposition = 'inside',
                orientation = 'h'
            ) %>%
            layout(
                title = "Top Sponsors by Studies Registered",
                xaxis = list(title = '# Studies Registered'),
                yaxis = list(title = 'Sponsor Name', showticklabels = FALSE)
            )
    })
    
    #top sponsors last year - summary
    top_sponsors_lstyr <-
        data.frame(agg_sponsors, stringsAsFactors = FALSE) %>%
        filter(is.na(lead_sponsor_name) == FALSE) %>%
        group_by(lead_sponsor_name) %>%
        summarise(cnt_st = sum(cnt_study_registered_lstyr)) %>%
        top_n(10, cnt_st) %>%
        arrange(desc(cnt_st))
    
    top_sponsors_lstyr$lead_sponsor_name <-
        factor(top_sponsors_lstyr$lead_sponsor_name,
               levels = unique(top_sponsors_lstyr$lead_sponsor_name)[order(top_sponsors_lstyr$cnt_st, decreasing = FALSE)])
    
    output$plot_sp_sum_1112 <- renderPlotly({
        top_sponsors_lstyr %>%
            plot_ly(
                y =  ~ lead_sponsor_name,
                x =  ~ cnt_st,
                type = 'bar',
                text = ~ paste(
                    paste("Sponsor:", lead_sponsor_name),
                    paste("Studies Last Year:", cnt_st),
                    sep = "<br />"
                ),
                hoverinfo = 'text',
                textposition = 'inside',
                orientation = 'h'
            ) %>%
            layout(
                title = "Top Sponsors by Studies Registered Last Year",
                xaxis = list(title = '# Studies Registered'),
                yaxis = list(title = 'Sponsor Name', showticklabels = FALSE)
            )
    })
    
    #Top global sponsors - Summary
    top_sponsors_global <-
        data.frame(agg_sponsors, stringsAsFactors = FALSE) %>%
        filter(is.na(lead_sponsor_name) == FALSE) %>%
        group_by(lead_sponsor_name) %>%
        summarise(cnt_st = sum(cnt_global_studies)) %>%
        top_n(10, cnt_st) %>%
        arrange(desc(cnt_st))
    
    top_sponsors_global$lead_sponsor_name <-
        factor(top_sponsors_global$lead_sponsor_name,
               levels = unique(top_sponsors_global$lead_sponsor_name)[order(top_sponsors_global$cnt_st, decreasing = FALSE)])
    
    output$plot_sp_sum_1113 <- renderPlotly({
        top_sponsors_global %>%
            plot_ly(
                y =  ~ lead_sponsor_name,
                x =  ~ cnt_st,
                type = 'bar',
                text = ~ paste(
                    paste("Sponsor:", lead_sponsor_name),
                    paste("Global Studies:", cnt_st),
                    sep = "<br />"
                ),
                hoverinfo = 'text',
                textposition = 'inside',
                orientation = 'h'
            ) %>%
            layout(
                title = "Top Global Sponsors by Studies",
                xaxis = list(title = '# Global Studies Registered'),
                yaxis = list(title = 'Sponsor Name', showticklabels = FALSE)
            )
    })
    
    #Num of Sponsors by Type - Summary
    output$plot_sp_sum_1114 <- renderPlotly({
        agg_sponsors %>%
            group_by(sponsor_type) %>%
            summarise(cnt_sponsors = length(unique((lead_sponsor_name)))) %>%
            plot_ly(
                values =  ~ cnt_sponsors,
                labels =  ~ (sponsor_type),
                type = 'pie'
            ) %>%
            layout(title = "Num of Sponsors by Type",
                   legend = list(orientation = "h"))
        
    })
    
    output$dt_agg_sponsors_summ <- renderDataTable({
        DT::datatable(
            agg_sponsors,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    
    #################################################
    #Registered trend by Type - Sponsor Trends
    agg_sponsors_by_time_registered <-
        data.frame(agg_sponsors_by_time, stringsAsFactors = FALSE) %>%
        group_by(common_year, sponsor_type) %>%
        summarise(cnt_st = sum(cnt_studies_registered, na.rm=TRUE)
        )
    
    agg_sponsors_by_time_registered<-mutate(agg_sponsors_by_time_registered,
                                            cnt_st2=sum(cnt_st),
                                            pct_st=round((cnt_st*100/sum(cnt_st, na.rm = TRUE)), digits = 2)
    )
    
    #create chart
    output$plot_sp_sum_1121 <- renderPlotly({
        plot_ly(agg_sponsors_by_time_registered,
                x = ~common_year, 
                y = ~pct_st, 
                type = 'bar',
                color = ~sponsor_type,
                text = ~ paste(
                    paste("Sponsor Type:", sponsor_type),
                    paste("Year Total:", cnt_st2),
                    paste("Studies:", cnt_st),
                    paste("% Share:", pct_st),
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = '% Study Share'),barmode = "stack",
                   xaxis = list(title = "Study Registration Year", showgrid = TRUE),
                   title = 'Study Registration Share - 15 Years')
    })
    
    #Started trend by Type - Sponsor Trends
    agg_sponsors_by_time_started <-
        data.frame(agg_sponsors_by_time, stringsAsFactors = FALSE) %>%
        group_by(common_year, sponsor_type) %>%
        summarise(cnt_st = sum(cnt_studies_started, na.rm=TRUE)
        )
    
    agg_sponsors_by_time_started<-mutate(agg_sponsors_by_time_started,
                                         cnt_st2=sum(cnt_st),
                                         pct_st=round((cnt_st*100/sum(cnt_st, na.rm = TRUE)), digits = 2)
    )
    
    #create chart
    output$plot_sp_sum_1122 <- renderPlotly({
        plot_ly(agg_sponsors_by_time_started,
                x = ~common_year, 
                y = ~pct_st, 
                type = 'bar',
                color = ~sponsor_type,
                text = ~ paste(
                    paste("Sponsor Type:", sponsor_type),
                    paste("Year Total:", cnt_st2),
                    paste("Studies:", cnt_st),
                    paste("% Share:", pct_st),
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = '% Study Share'),barmode = "stack",
                   xaxis = list(title = "Study Started Year", showgrid = TRUE),
                   title = 'Study Started Share - 15 Years')
    })
    
    #completed trend by Type - Sponsor Trends
    agg_sponsors_by_time_completed <-
        data.frame(agg_sponsors_by_time, stringsAsFactors = FALSE) %>%
        group_by(common_year, sponsor_type) %>%
        summarise(cnt_st = sum(cnt_studies_completed, na.rm=TRUE)
        )
    
    agg_sponsors_by_time_completed<-mutate(agg_sponsors_by_time_completed,
                                           cnt_st2=sum(cnt_st),
                                           pct_st=round((cnt_st*100/sum(cnt_st, na.rm = TRUE)), digits = 2)
    )
    
    #create chart
    output$plot_sp_sum_1123 <- renderPlotly({
        plot_ly(agg_sponsors_by_time_completed,
                x = ~common_year, 
                y = ~pct_st, 
                type = 'bar',
                color = ~sponsor_type,
                text = ~ paste(
                    paste("Sponsor Type:", sponsor_type),
                    paste("Year Total:", cnt_st2),
                    paste("Studies:", cnt_st),
                    paste("% Share:", pct_st),
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = '% Study Share'),barmode = "stack",
                   xaxis = list(title = "Study Completed Year", showgrid = TRUE),
                   title = 'Study Completed Share - 15 Years')
    })
    
    #results_posted trend by Type - Sponsor Trends
    agg_sponsors_by_time_results_posted <-
        data.frame(agg_sponsors_by_time, stringsAsFactors = FALSE) %>%
        group_by(common_year, sponsor_type) %>%
        summarise(cnt_st = sum(cnt_studies_results_posted, na.rm=TRUE)
        )
    
    agg_sponsors_by_time_results_posted<-mutate(agg_sponsors_by_time_results_posted,
                                                cnt_st2=sum(cnt_st),
                                                pct_st=round((cnt_st*100/sum(cnt_st, na.rm = TRUE)), digits = 2)
    )
    
    #create chart
    output$plot_sp_sum_1124 <- renderPlotly({
        plot_ly(agg_sponsors_by_time_results_posted,
                x = ~common_year, 
                y = ~pct_st, 
                type = 'bar',
                color = ~sponsor_type,
                text = ~ paste(
                    paste("Sponsor Type:", sponsor_type),
                    paste("Year Total:", cnt_st2),
                    paste("Studies:", cnt_st),
                    paste("% Share:", pct_st),
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = '% Study Share'),barmode = "stack",
                   xaxis = list(title = "Results Posted Year", showgrid = TRUE),
                   title = 'Results Posted Share - 15 Years')
    })
    
    
    #################################################
    #Registered trend2 by Type - Sponsor Trends
    agg_sponsorstype_by_time1 <-
        data.frame(agg_sponsorstype_by_time, stringsAsFactors = FALSE) %>%
        group_by(study_first_posted_year, sponsor_type) %>%
        summarise(cnt_sponsors = sum(cnt_sponsors, na.rm=TRUE),
                  cnt_studies_with_collab = sum(cnt_studies_with_collab, na.rm=TRUE),
                  cnt_studies_USonly = sum(cnt_studies_USonly, na.rm=TRUE),
                  cnt_studies_with_nonUS_country = sum(cnt_studies_with_nonUS_country, na.rm=TRUE)
        )
    
    agg_sponsorstype_by_time1<-mutate(agg_sponsorstype_by_time1,
                                      cnt_sponsors_total=sum(cnt_sponsors),
                                      pct_sponsors=round((cnt_sponsors*100/sum(cnt_sponsors, na.rm = TRUE)), digits = 2),
                                      cnt_studies_with_collab_total=sum(cnt_studies_with_collab),
                                      pct_studies_with_collab=round((cnt_studies_with_collab*100/sum(cnt_studies_with_collab, na.rm = TRUE)), digits = 2),
                                      cnt_studies_USonly_total=sum(cnt_studies_USonly),
                                      pct_studies_USonly=round((cnt_studies_USonly*100/sum(cnt_studies_USonly, na.rm = TRUE)), digits = 2),
                                      cnt_studies_with_nonUS_country_total=sum(cnt_studies_with_nonUS_country),
                                      pct_studies_with_nonUS_country=round((cnt_studies_with_nonUS_country*100/sum(cnt_studies_with_nonUS_country, na.rm = TRUE)), digits = 2)
    )
    
    #create chart
    output$plot_sp_sum_1131 <- renderPlotly({
        plot_ly(agg_sponsorstype_by_time1,
                x = ~study_first_posted_year, 
                y = ~cnt_sponsors, 
                type = 'bar',
                color = ~sponsor_type,
                text = ~ paste(
                    paste("Sponsor Type:", sponsor_type),
                    paste("Year Total:", cnt_sponsors_total),
                    paste("Sponsors:", cnt_sponsors),
                    paste("% Share:", pct_sponsors),
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = ' No of Sponsors'),barmode = "stack",
                   xaxis = list(title = "Study Registration Year", showgrid = TRUE),
                   title = 'Sponsors Trend - 15 Years')
    })
    
    #create chart
    output$plot_sp_sum_1132 <- renderPlotly({
        plot_ly(agg_sponsorstype_by_time1,
                x = ~study_first_posted_year, 
                y = ~cnt_studies_with_collab, 
                type = 'bar',
                color = ~sponsor_type,
                text = ~ paste(
                    paste("Sponsor Type:", sponsor_type),
                    paste("Year Total:", cnt_studies_with_collab_total),
                    paste("Collaboration Studies:", cnt_studies_with_collab),
                    paste("% Share:", pct_studies_with_collab),
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = ' No of Studies with Collaborator'),barmode = "stack",
                   xaxis = list(title = "Study Registration Year", showgrid = TRUE),
                   title = 'Collaborated Studies Trend - 15 Years')
    })
    
    #create chart
    output$plot_sp_sum_1133 <- renderPlotly({
        plot_ly(agg_sponsorstype_by_time1,
                x = ~study_first_posted_year, 
                y = ~cnt_studies_USonly, 
                type = 'bar',
                color = ~sponsor_type,
                text = ~ paste(
                    paste("Sponsor Type:", sponsor_type),
                    paste("Year Total:", cnt_studies_USonly_total),
                    paste("US Only Studies:", cnt_studies_USonly),
                    paste("% Share:", pct_studies_USonly),
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = ' No of US Only Studies'),barmode = "stack",
                   xaxis = list(title = "Study Registration Year", showgrid = TRUE),
                   title = 'US Only Studies Trend - 15 Years')
    })
    
    #create chart
    output$plot_sp_sum_1134 <- renderPlotly({
        plot_ly(agg_sponsorstype_by_time1,
                x = ~study_first_posted_year, 
                y = ~cnt_studies_with_nonUS_country, 
                type = 'bar',
                color = ~sponsor_type,
                text = ~ paste(
                    paste("Sponsor Type:", sponsor_type),
                    paste("Year Total:", cnt_studies_with_nonUS_country_total),
                    paste("Studies with non-US Country:", cnt_studies_with_nonUS_country),
                    paste("% Share:", pct_studies_with_nonUS_country),
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = ' No of Studies with non-US Country'),barmode = "stack",
                   xaxis = list(title = "Study Registration Year", showgrid = TRUE),
                   title = 'Studies with non-US Country Trend - 15 Years')
    })
    
    ####################################################################
    #Sponsor - Performance
    #sponsor - performance - Summary
    #Completion Duration
    output$plot_sp_per_1311 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ avg_start_to_complete_days_ph1,
                x =  ~ sponsor_type,
                color = ~sponsor_type,
                type = 'box') %>%
            layout(
                title = "Avg Start to Complete Days - Phase 1",
                xaxis = list(title = 'Sponsor Type', showticklabels = FALSE),
                yaxis = list(title = 'Average Days'),
                showlegend = TRUE,
                legend = list(x = 0.9, y = 0.9)
            )
    })
    
    output$plot_sp_per_1312 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ avg_start_to_complete_days_ph2,
                x =  ~ sponsor_type,
                color = ~sponsor_type,
                type = 'box') %>%
            layout(
                title = "Avg Start to Complete Days - Phase 2",
                xaxis = list(title = 'Sponsor Type', showticklabels = FALSE),
                yaxis = list(title = 'Average Days'),
                showlegend = TRUE,
                legend = list(x = 0.9, y = 0.9)
            )
    })
    
    output$plot_sp_per_1313 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ avg_start_to_complete_days_ph3,
                x =  ~ sponsor_type,
                color = ~sponsor_type,
                type = 'box') %>%
            layout(
                title = "Avg Start to Complete Days - Phase 3",
                xaxis = list(title = 'Sponsor Type', showticklabels = FALSE),
                yaxis = list(title = 'Average Days'),
                showlegend = TRUE,
                legend = list(x = 0.9, y = 0.9)
            )
    })
    
    output$plot_sp_per_1314 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ avg_start_to_complete_days_ph4,
                x =  ~ sponsor_type,
                color = ~sponsor_type,
                type = 'box') %>%
            layout(
                title = "Avg Start to Complete Days - Phase 4",
                xaxis = list(title = 'Sponsor Type', showticklabels = FALSE),
                yaxis = list(title = 'Average Days'),
                showlegend = TRUE,
                legend = list(x = 0.9, y = 0.9)
            )
    })
    
    #Initiation Duration
    output$plot_sp_per_1321 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ avg_register_to_start_days_ph1,
                x =  ~ sponsor_type,
                color = ~sponsor_type,
                type = 'box') %>%
            layout(
                title = "Avg Reg to Initiation Days - Phase 1",
                xaxis = list(title = 'Sponsor Type', showticklabels = FALSE),
                yaxis = list(title = 'Average Days'),
                showlegend = TRUE,
                legend = list(x = 0.9, y = 0.9)
            )
    })
    
    output$plot_sp_per_1322 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ avg_register_to_start_days_ph2,
                x =  ~ sponsor_type,
                color = ~sponsor_type,
                type = 'box') %>%
            layout(
                title = "Avg Reg to Initiation Days - Phase 2",
                xaxis = list(title = 'Sponsor Type', showticklabels = FALSE),
                yaxis = list(title = 'Average Days'),
                showlegend = TRUE,
                legend = list(x = 0.9, y = 0.9)
            )
    })
    
    output$plot_sp_per_1323 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ avg_register_to_start_days_ph3,
                x =  ~ sponsor_type,
                color = ~sponsor_type,
                type = 'box') %>%
            layout(
                title = "Avg Reg to Initiation Days - Phase 3",
                xaxis = list(title = 'Sponsor Type', showticklabels = FALSE),
                yaxis = list(title = 'Average Days'),
                showlegend = TRUE,
                legend = list(x = 0.9, y = 0.9)
            )
    })
    
    output$plot_sp_per_1324 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ avg_register_to_start_days_ph4,
                x =  ~ sponsor_type,
                color = ~sponsor_type,
                type = 'box') %>%
            layout(
                title = "Avg Reg to Initiation Days - Phase 4",
                xaxis = list(title = 'Sponsor Type', showticklabels = FALSE),
                yaxis = list(title = 'Average Days'),
                showlegend = TRUE,
                legend = list(x = 0.9, y = 0.9)
            )
    })
    
    #Sponsor KPIs
    output$plot_sp_per_1331 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ cnt_observational,
                x =  ~ cnt_interventional,
                color = ~ sponsor_type,
                size = ~ cnt_has_dmc,
                type = 'scatter',
                mode = 'markers',
                marker = list(opacity = 0.6, sizemode = 'diameter'),
                text = ~ paste(
                    lead_sponsor_name,
                    paste("Type:", sponsor_type),
                    paste("Interventional:", cnt_interventional),
                    paste("Observational:", cnt_observational),
                    paste("DMC:", cnt_has_dmc),
                    sep = "<br />"
                ),
                hoverinfo = 'text'
                ) %>%
            layout(
                title = "Interventional Vs Observational",
                xaxis = list(title = 'Interventional Studies'),
                yaxis = list(title = 'Observational Studies'),
                showlegend = TRUE
            )
    })
    
    output$plot_sp_per_1332 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ cnt_started_actual,
                x =  ~ cnt_studies_registered,
                color = ~ sponsor_type,
                size = ~ cnt_results_submitted,
                type = 'scatter',
                mode = 'markers',
                marker = list(opacity = 0.6, sizemode = 'diameter'),
                text = ~ paste(
                    lead_sponsor_name,
                    paste("Type:", sponsor_type),
                    paste("Registered:", cnt_studies_registered),
                    paste("Initiated:", cnt_started_actual),
                    paste("Results Posted:", cnt_results_submitted),
                    sep = "<br />"
                ),
                hoverinfo = 'text'
            ) %>%
            layout(
                title = "Registered Vs Initiated",
                xaxis = list(title = 'Registered Studies'),
                yaxis = list(title = 'Initiated Studies'),
                showlegend = TRUE
            )
    })
    
    output$plot_sp_per_1333 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ cnt_nonUS_only_studies,
                x =  ~ cnt_US_only_studies,
                color = ~ sponsor_type,
                size = ~ cnt_global_studies,
                type = 'scatter',
                mode = 'markers',
                marker = list(opacity = 0.5, sizemode = 'diameter'),
                text = ~ paste(
                    lead_sponsor_name,
                    paste("Type:", sponsor_type),
                    paste("US Only:", cnt_US_only_studies),
                    paste("non-US Only:", cnt_nonUS_only_studies),
                    paste("Global:", cnt_global_studies),
                    sep = "<br />"
                ),
                hoverinfo = 'text'
            ) %>%
            layout(
                title = "US Only Vs non-US Only Studies",
                xaxis = list(title = 'US Only Studies'),
                yaxis = list(title = 'non-US Only Studies'),
                showlegend = TRUE
            )
    })
    
    output$plot_sp_per_1334 <- renderPlotly({
        agg_sponsors %>%
            plot_ly(
                y =  ~ cnt_colab_nonind,
                x =  ~ cnt_colab_Ind,
                color = ~ sponsor_type,
                type = 'scatter',
                mode = 'markers',
                marker = list(opacity = 0.8, size=10),
                text = ~ paste(
                    lead_sponsor_name,
                    paste("Type:", sponsor_type),
                    paste("Industry:", cnt_colab_Ind),
                    paste("non-Industry:", cnt_colab_nonind),
                    sep = "<br />"
                ),
                hoverinfo = 'text'
            ) %>%
            layout(
                title = "Industry Vs Non-Industry Collaboration",
                xaxis = list(title = 'Industry Collaborators'),
                yaxis = list(title = 'Non-Industry Collaborators'),
                showlegend = TRUE
            )
    })
    
    
    ####################################################################
    #reactive dataset for sponsor-collaborator
    sponsor_coll_tree_data <-
        eventReactive(input$update_sponsor_collaborator_1, {
            #createleaflet plot 1020 based on reactive set
            subset(
                r_sponsor_collaborator,
                select = c(
                    "Sponsor",
                    "CollaboratorType",
                    "studyPhase",
                    "Collaborator",
                    "cnt_studies"
                ),
                subset = (casefold(Sponsor) %like% casefold(c(
                    input$select_sponsor_coll
                )) & casefold(Collaborator) %like% casefold(c(
                    input$select_sponsor_coll_colname
                ))
                )
            )
        })
    
    output$sponsor_coll_tree <- renderCollapsibleTree({
        sponsor_coll_tree_data() %>%
            group_by(
                sponsor_coll_tree_data()$Sponsor,
                sponsor_coll_tree_data()$CollaboratorType,
                sponsor_coll_tree_data()$studyPhase,
                sponsor_coll_tree_data()$Collaborator,
                sponsor_coll_tree_data()$cnt_studies
            ) %>%
            summarise("Number of Collaborators" = n()) %>%
            collapsibleTreeSummary(
                hierarchy = c(
                    "sponsor_coll_tree_data()$Sponsor",
                    "sponsor_coll_tree_data()$CollaboratorType",
                    "sponsor_coll_tree_data()$studyPhase",
                    "sponsor_coll_tree_data()$Collaborator",
                    "sponsor_coll_tree_data()$cnt_studies"
                ),
                root = "Sponsor",
                width = 800,
                attribute = c("Number of Collaborators"),
                zoomable = TRUE
            )
        
    })
    
    #reactive dataset for sponsor-collaborator
    coll_sponsor_tree_data <-
        eventReactive(input$update_collaborator_sponsor_1, {
            #createleaflet plot 1020 based on reactive set
            subset(
                r_sponsor_collaborator,
                select = c(
                    "Sponsor",
                    "sponsorType",
                    "studyPhase",
                    "Collaborator",
                    "cnt_studies"
                ),
                subset = (casefold(Collaborator) %like% casefold(c(
                    input$select_coll_sponsor
                )))
            )
        })
    
    output$coll_sponsor_tree <- renderCollapsibleTree({
        coll_sponsor_tree_data() %>%
            group_by(
                coll_sponsor_tree_data()$Collaborator,
                coll_sponsor_tree_data()$sponsorType,
                coll_sponsor_tree_data()$studyPhase,
                coll_sponsor_tree_data()$Sponsor,
                coll_sponsor_tree_data()$cnt_studies
            ) %>%
            summarise("Number of Sponsors" = n()) %>%
            collapsibleTreeSummary(
                hierarchy = c(
                    "coll_sponsor_tree_data()$Collaborator",
                    "coll_sponsor_tree_data()$sponsorType",
                    "coll_sponsor_tree_data()$studyPhase",
                    "coll_sponsor_tree_data()$Sponsor",
                    "coll_sponsor_tree_data()$cnt_studies"
                ),
                root = "Collaborator",
                width = 800,
                attribute = c("Number of Sponsors"),
                zoomable = TRUE
            )
        
    })
    
    #reactive dataset for Collaborator network
    output$dt_collaborator_sponsor <- renderDataTable({
        r_collaborator_sponsor <- subset(
            r_sponsor_collaborator,
            select = c(
                "Collaborator",
                "CollaboratorType",
                "Sponsor",
                "sponsorType",
                "studyPhase",
                "cnt_studies"
            )
        )
        
        DT::datatable(
            r_collaborator_sponsor,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    
    #reactive dataset for measurements by conditions
    output$dt_measurements <- renderDataTable({
        DT::datatable(
            measurements,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    
    #reactive dataset for sponsor network
    output$dt_sponsor_collaborator <- renderDataTable({
        DT::datatable(
            r_sponsor_collaborator,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    #######################################################
    #reactive dataset for sponsor-site
    sponsor_site_tree_data <-
        eventReactive(input$update_sponsor_site_1, {
            #createleaflet plot 1020 based on reactive set
            subset(
                r_sponsor_site,
                select = c(
                    "Sponsor",
                    "StudyYear",
                    "Country",
                    "State",
                    "City",
                    "FacilityType",
                    "studyPhase",
                    "Facility",
                    "cnt_studies"
                ),
                subset = (casefold(Sponsor) %like% casefold(c(
                    input$select_sponsor_site
                )) &
                    casefold(Country) %like% casefold(c(
                        input$select_sponsor_site_country
                    )) &
                    casefold(City) %like% casefold(c(
                        input$select_sponsor_site_city
                    )) &
                    casefold(Facility) %like% casefold(c(
                        input$select_sponsor_site_facility
                    ))
                
                )
            )
        })
    
    output$sponsor_site_tree <- renderCollapsibleTree({
        sponsor_site_tree_data() %>%
            group_by(
                sponsor_site_tree_data()$Sponsor,
                sponsor_site_tree_data()$studyPhase,
                sponsor_site_tree_data()$StudyYear,
                sponsor_site_tree_data()$FacilityType,
                sponsor_site_tree_data()$Country,
                sponsor_site_tree_data()$State,
                sponsor_site_tree_data()$City,
                sponsor_site_tree_data()$Facility,
                sponsor_site_tree_data()$cnt_studies
            ) %>%
            summarise("Number of Sites" = n()) %>%
            collapsibleTreeSummary(
                hierarchy = c(
                    "sponsor_site_tree_data()$Sponsor",
                    "sponsor_site_tree_data()$studyPhase",
                    "sponsor_site_tree_data()$StudyYear",
                    "sponsor_site_tree_data()$FacilityType",
                    "sponsor_site_tree_data()$Country",
                    "sponsor_site_tree_data()$State",
                    "sponsor_site_tree_data()$City",
                    "sponsor_site_tree_data()$Facility",
                    "sponsor_site_tree_data()$cnt_studies"
                ),
                root = "Sponsor",
                width = 800,
                attribute = c("Number of Sites"),
                zoomable = TRUE
            )
    })
    
    output$dt_sponsor_site <- renderDataTable({
        DT::datatable(
            r_sponsor_site,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    
    #########################################################
    
    #######################################################
    #reactive dataset for sponsor-condition
    sponsor_cond_tree_data <-
        eventReactive(input$update_sponsor_cond_1, {
            #createleaflet plot 1020 based on reactive set
            subset(
                r_sponsor_conditions,
                select = c(
                    "Sponsor",
                    "studyPhase",
                    "StudyYear",
                    "StartWord",
                    "Condition",
                    "cnt_studies"
                ),
                subset = (casefold(Sponsor) %like% casefold(c(
                    input$select_sponsor_cond
                )) &
                    casefold(Condition) %like% casefold(c(
                        input$select_sponsor_condname
                    ))
                )
            )
        })
    
    output$sponsor_cond_tree <- renderCollapsibleTree({
        sponsor_cond_tree_data() %>%
            group_by(
                sponsor_cond_tree_data()$Sponsor,
                sponsor_cond_tree_data()$studyPhase,
                sponsor_cond_tree_data()$StudyYear,
                sponsor_cond_tree_data()$StartWord,
                sponsor_cond_tree_data()$Condition,
                sponsor_cond_tree_data()$cnt_studies
            ) %>%
            summarise("Number of Conditions" = n()) %>%
            collapsibleTreeSummary(
                hierarchy = c(
                    "sponsor_cond_tree_data()$Sponsor",
                    "sponsor_cond_tree_data()$studyPhase",
                    "sponsor_cond_tree_data()$StudyYear",
                    "sponsor_cond_tree_data()$StartWord",
                    "sponsor_cond_tree_data()$Condition",
                    "sponsor_cond_tree_data()$cnt_studies"
                ),
                root = "Sponsor",
                width = 800,
                height = 1000,
                attribute = c("Number of Conditions"),
                zoomable = TRUE
            )
    })
    
    output$dt_sponsor_conditions <- renderDataTable({
        DT::datatable(
            r_sponsor_conditions,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    
    #########################################################
    
    
    output$dt_sponsor_interventions <- renderDataTable({
        DT::datatable(
            r_sponsor_interventions,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    
    #reactive dataset for sponsor agg
    output$dt_agg_sponsors <- renderDataTable({
        DT::datatable(
            agg_sponsors,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE)
        )
    })
    
    #display recruitment data
    output$dt_recruitment <-
        renderDataTable(DT::datatable(mv_studies_recruiting_table, filter = 'top'))
    
    #reactive dataset for maps with reduced columns
    mv_studies_recruiting_map_world <-
        eventReactive(input$update_global, {
            #createleaflet plot 1020 based on reactive set
            subset(
                mv_studies_recruiting,
                subset = (
                    StudyPhase %in%  c(input$select_studyphase_name_world) &
                        casefold(city) %like%  casefold(input$select_city_map_world) &
                        casefold(Condition) %like%   casefold(input$select_condition_map_world) &
                        casefold(Facility) %like%  casefold(input$select_facility_map_world) &
                        casefold(Sponsor) %like%  casefold(input$select_sponsor_map_world)
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
        leaflet(data = mv_studies_recruiting_map_world()) %>%
            setView(lng = -15.037721,
                    lat = 14.451703,
                    zoom = 2.2) %>%
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers( ~ longitude,
                        ~ latitude,
                        popup = f_labels_1020(),
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
            addMarkers( ~ longitude,
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
            addMarkers( ~ longitude,
                        ~ latitude,
                        label = labels_1012,
                        clusterOptions = markerClusterOptions())
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
        valueBox("Studies",
                 summ_rec$cnt_studies
        )
    })
    
    output$box_sponsors <- shinydashboard::renderValueBox({
        valueBox("Sponsors",
                 summ_rec$cnt_sponsors
                 #icon = icon("credit-card")
        )
    })
    
    output$box_countries <- shinydashboard::renderValueBox({
        valueBox("Countries",
                 summ_rec$cnt_countries
        )
    })
    
    output$box_cities <- shinydashboard::renderValueBox({
        valueBox("Cities",
                 summ_rec$cnt_cities)
    })
    
    output$box_facilities <- shinydashboard::renderValueBox({
        valueBox("Facilities",
                 summ_rec$cnt_facilities)
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
        plot_geo(
            mv_studies_recruiting_loc,
            lat = ~ latitude,
            lon = ~ longitude
        ) %>%
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
    #reactive dataset for maps with reduced columns
    output$dt_recruitment_1005 <- renderDataTable({
        DT::datatable(
            mv_studies_recruiting_table,
            filter = 'top',
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
                name = "Previous",
                showlegend = FALSE
            ) %>%
            add_trace(
                y =  ~ cnt_reg_CurrYr,
                text =  ~ cnt_reg_CurrYr,
                name = "Current"
            ) %>%
            layout(xaxis = list(title = "")) %>%
            layout(yaxis = list(title = ""))
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
                name = "Previous",
                showlegend = FALSE
            ) %>%
            add_trace(
                y =  ~ cnt_started_CurrYr,
                text =  ~ cnt_started_CurrYr,
                name = "Current"
            ) %>%
            layout(xaxis = list(title = "")) %>%
            layout(yaxis = list(title = ""))
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
                name = "Previous",
                showlegend = FALSE
            ) %>%
            add_trace(
                y =  ~ cnt_completed_CurrYr,
                text =  ~ cnt_completed_CurrYr,
                name = "Current"
            ) %>%
            layout(xaxis = list(title = "")) %>%
            layout(yaxis = list(title = ""))
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
                name = "Previous",
                showlegend = FALSE
            ) %>%
            add_trace(
                y =  ~ cnt_posted_CurrYr,
                text =  ~ cnt_posted_CurrYr,
                name = "Current"
            ) %>%
            layout(xaxis = list(title = "")) %>%
            layout(yaxis = list(title = ""))
    })
    
    ####################server ends###########################
    
}

shinyApp(ui, server)