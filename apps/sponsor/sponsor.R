#import libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(RColorBrewer)
library(leaflet)
library(htmltools)
library(data.table)
library(shinythemes)
library(lubridate)
library(shinycssloaders)
library(shinyWidgets)
library(collapsibleTree)

#read log file
log_app02 <- read.csv("data/log_app02.txt", header = FALSE)
log_app02_time <- substr(log_app02, 1, 10)
log_app02_time <- format.Date(log_app02_time, "%d-%b-%Y")

var_current_year<- year(today())


#read sponsor agg data
agg_sponsors <-
    fread(
        "data/agg_sponsors.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )


agg_sponsors_by_time <-
    fread(
        "data/agg_sponsors_by_time.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

agg_sponsorstype_by_time <-
    fread(
        "data/agg_sponsorstype_by_time.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

#read sponsor network data
r_sponsor_collaborator <-
    fread(
        "data/r_sponsor_collaborator.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )



r_sponsor_site <-
    fread(
        "data/r_sponsor_site.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

r_sponsor_conditions <-
    fread(
        "data/r_sponsor_conditions.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

r_sponsor_interventions <-
    fread(
        "data/r_sponsor_interventions.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

#Study data
agg_studies <-
    fread(
        "data/agg_studies_mini.txt",
        header = TRUE,
        sep = "|",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )

var_studyphase_name <- unique(agg_studies$phase)
var_study_type <- unique(agg_studies$study_type)
var_intervention_type <- unique(agg_studies$intervention_type)
var_sponsor_size <- sort(unique(agg_sponsors$sponsor_size), decreasing=TRUE)

#####################
# for spinners 2-3 match the background color of spinner
options(spinner.color.background = "#772953")
options(spinner.size = 1.5)

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
            #theme = "united.min.css",
            theme = shinytheme("united"),
            collapsible	= TRUE,
            fluid = TRUE,
            inverse = TRUE,
            footer = fluidRow(
                h5(class="text-center",style = "color: #772953; margin-top:2px; margin-left:2px; margin-right:2px;",
                   paste0("Data Refreshed On: ",log_app02_time)),
                #tags$br(),
                h4(
                    tags$a(href = "https://www.oakbloc.com/", "Oakbloc Technologies", target =
                               "_blank"),
                    style = "margin-bottom:2%;",
                    align = "center",
                    "2019-2020",
                )
            ),
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
                                         style = "padding:2px;",
                                         column(
                                             1,
                                             align = "left",
                                             #tags$h2("Dropdown Button"),
                                             #br(),
                                             dropdownButton(
                                                 tags$h5("Only Sponsors with 2 or more clinical studies registered are included in this analysis."),
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
                                              tags$h5("Only Sponsors with 2 or more clinical studies registered are included in this analysis."),
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
                                                  tags$li(tags$span("sponsor_size: The size of the sponsor based on the number of studies registered. For example in 2_XXS_11T50, 2 is the rank 1 being the smallest, XXS is Extra Extra Small, 11T50 means the sponsor has registered total studies between 11 and 50.")),
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
                         )),   # Summary tab in sponsors end here
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
                                             1,
                                             align = "left",
                                             #tags$h2("Dropdown Button"),
                                             #br(),
                                             dropdownButton(
                                                 tags$h5("Enter the name of Sponsor and hit Display Results Button."),
                                                 #br(),
                                                 tags$h5("Network Tree: Sponsor - Sponsor Type - Study Phase - Collaborator Name - Number of Studies."),
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
                                                 inputId = "select_sponsor_coll",
                                                 label = NULL,
                                                 value = "pfizer",
                                                 placeholder = "Sponsor Name"
                                             )
                                         ),
                                         column(
                                             4,
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
                                             "Summary",
                                             fluidRow(
                                                 style = "padding:2px;",
                                                 column(
                                                     3,
                                                     align = "center",
                                                     textInput(
                                                         inputId = "select_sponsor_per_summ1",
                                                         label = NULL,
                                                         value = "pfizer",
                                                         placeholder = "Sponsor Name"
                                                     )
                                                 ),  
                                                 
                                                 column(
                                                     3,
                                                     align = "center",
                                                     pickerInput(
                                                         inputId = "select_phase_spon_summ_1",
                                                         label = NULL,
                                                         choices = var_studyphase_name,
                                                         options = list(`actions-box` = TRUE, style = "btn-info"),
                                                         multiple = TRUE,
                                                         selected = c(var_studyphase_name)
                                                     )
                                                 ),
                                                 column(
                                                     4,
                                                     align = "center",
                                                     pickerInput(
                                                         inputId = "select_st_type_spon_summ_1",
                                                         label = NULL,
                                                         choices = var_study_type,
                                                         options = list(`actions-box` = TRUE, style = "btn-info"),                                     
                                                         multiple = TRUE,
                                                         selected = c(var_study_type)
                                                     )
                                                 ),                                
                                                 column(2,
                                                        align = "left",
                                                        tags$div(
                                                            class = "text-center",
                                                            actionButton(
                                                                "update_per_summ_1",
                                                                "Display Results",
                                                                class = "btn btn-primary",
                                                                style = "margin-bottom: 0.5%;"
                                                            )
                                                        ))
                                             ),
                                             fluidRow(
                                                 style = "padding: 5px; border-style: solid; border-width: 0.3px;",
                                                 column(
                                                     5,
                                                     align = "left",
                                                     style = "border: 0.3px solid",
                                                     withSpinner(plotlyOutput("plot_sp_per_summ_1311"), type = 3)
                                                 ),
                                                 column(
                                                     7,
                                                     align = "left",
                                                     style = "border: 0.3px solid;",
                                                     withSpinner(DT::dataTableOutput("dt_agg_sponsor_per_summ"), type = 3)
                                                 )
                                             )
                                         ),
                                         tabPanel(
                                             "Details",
                                             fluidRow(
                                                 withSpinner(DT::dataTableOutput("dt_agg_studies_per_summ"), type = 3)
                                             )
                                         ),
                                         tabPanel(
                                             "Compare",
                                             fluidRow(
                                                 style = "padding:2px;",
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
                                             fluidRow(style = "padding:2px;",
                                                      column(
                                                          1,
                                                          align = "left",
                                                          #tags$h2("Dropdown Button"),
                                                          #br(),
                                                          dropdownButton(
                                                              tags$h5("Select the sponsor size. The size of the sponsor based on the number of studies registered. For example in 2_XXS_11T50, 2 is the rank 1 being the smallest, XXS is Extra Extra Small, 11T50 means the sponor has registered studies between 11 and 50."),
                                                              circle = TRUE, 
                                                              status = "info",
                                                              icon = icon("info"), 
                                                              size = "xs",
                                                              width = "500px",
                                                              tooltip = tooltipOptions(title = "Help!")
                                                          )
                                                      ),                                         
                                                      column(
                                                          11,
                                                          align = "center",
                                                          pickerInput(
                                                              inputId = "select_sponsor_size_initiation",
                                                              label = NULL,
                                                              choices = var_sponsor_size,
                                                              options = list(style = "btn-info"),
                                                              multiple = FALSE,
                                                              selected = "7_XL_1KT2K"
                                                          )
                                                      )
                                             ),
                                             
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
                                             fluidRow(style = "padding:2px;",
                                                      column(
                                                          1,
                                                          align = "left",
                                                          #tags$h2("Dropdown Button"),
                                                          #br(),
                                                          dropdownButton(
                                                              tags$h5("Select the sponsor size. The size of the sponsor based on the number of studies registered. For example in 2_XXS_11T50, 2 is the rank 1 being the smallest, XXS is Extra Extra Small, 11T50 means the sponsor has registered total studies between 11 and 50."),
                                                              circle = TRUE, 
                                                              status = "info",
                                                              icon = icon("info"), 
                                                              size = "xs",
                                                              width = "500px",
                                                              tooltip = tooltipOptions(title = "Help!")
                                                          )
                                                      ),                                         
                                                      column(
                                                          11,
                                                          align = "center",
                                                          pickerInput(
                                                              inputId = "select_sponsor_size_completion",
                                                              label = NULL,
                                                              choices = var_sponsor_size,
                                                              options = list(style = "btn-info"),
                                                              multiple = FALSE,
                                                              selected = "7_XL_1KT2K"
                                                          )
                                                      )
                                             ),
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
                                                 column(
                                                     1,
                                                     align = "left",
                                                     #tags$h2("Dropdown Button"),
                                                     #br(),
                                                     dropdownButton(
                                                         tags$h5("Select the sponsor size. The size of the sponsor based on the number of studies registered. For example in 2_XXS_11T50, 2 is the rank 1 being the smallest, XXS is Extra Extra Small, 11T50 means the sponsor has registered total studies between 11 and 50."),
                                                         circle = TRUE, 
                                                         status = "info",
                                                         icon = icon("info"), 
                                                         size = "xs",
                                                         width = "500px",
                                                         tooltip = tooltipOptions(title = "Help!")
                                                     )
                                                 ),                                       
                                                 column(
                                                     11,
                                                     align = "center",
                                                     pickerInput(
                                                         inputId = "select_sponsor_size_kpi",
                                                         label = NULL,
                                                         choices = var_sponsor_size,
                                                         options = list(style = "btn-info"),
                                                         multiple = FALSE,
                                                         selected = "7_XL_1KT2K"
                                                     )
                                                 )
                                             ),
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
                tabPanel(
                    "Collaborators",
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
                ),
                tabPanel("Interventions",
                         mainPanel(
                             width = 12,
                             tabsetPanel(type = "tabs",
                                         tabPanel("Explore Data",
                                                  fluidRow(
                                                      withSpinner(DT::dataTableOutput("dt_sponsor_interventions"), type = 3)
                                                  )))
                         ))
            )
        )
    )

################################################################
#SERVER BEGINS
################################################################

server <- function(input, output) {
    
    
    ################################################################
    #SUMMARY(SUMMARY)
    ################################################################
    
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
                type = 'pie',
                hole=0.3,
                textinfo='label+percent',
                marker= list(colors = brewer.pal(8,"Set2"))
            ) %>%
            layout(title = "Num of Sponsors by Type",
                   legend = list(orientation = "h"))
        
    })
    
    ################################################################
    #SUMMARY(SUMMARY) ENDS
    ################################################################
    
    ################################################################
    #SUMMARY(INDUSTRY SHARE)
    ################################################################
    
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
    
    ################################################################
    #SUMMARY(INDUSTRY SHARE) ENDS
    ################################################################
    
    ################################################################
    #SUMMARY(SPONSOR DATA)
    ################################################################
    
    output$dt_agg_sponsors_summ <- renderDataTable({
        DT::datatable(
            agg_sponsors,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE, searchHighlight = TRUE)
        )
    })
    
    ################################################################
    #SUMMARY(SPONSOR DATA) ENDS
    ################################################################
    
    ################################################################
    #SUMMARY(INDUSTRY TREND)
    ################################################################
    
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
    
    ################################################################
    #SUMMARY(INDUSTRY TREND) ENDS
    ################################################################
    
    ################################################################
    #PERFORMANCE(SUMMARY)
    ################################################################
    agg_studies_spon_per_summ <-
        eventReactive(input$update_per_summ_1, {
            subset(agg_studies,
                   subset = (
                       casefold(lead_sponsor_name) %like% casefold(c(input$select_sponsor_per_summ1))
                       &
                           casefold(phase) %in% casefold(c(input$select_phase_spon_summ_1))
                       &
                           casefold(study_type) %in% casefold(c(input$select_st_type_spon_summ_1))                
                   )
            )
            
            
        })
    
    output$plot_sp_per_summ_1311 <- renderPlotly({
        agg_studies_spon_per_summ() %>%        
            group_by(study_first_posted_year) %>%
            summarize(cnt_studies=length(unique(nct_id))) %>%
            plot_ly(
                y =  ~ cnt_studies,
                x =  ~ study_first_posted_year,
                type = 'bar',
                text = ~ paste(                    
                    paste("Year:", study_first_posted_year),                    
                    paste("Studies Registered:", cnt_studies),                    
                    sep = "<br />"
                ),
                hoverinfo = 'text') %>%
            layout(yaxis = list(title = 'No of Studies Registered'),
                   xaxis = list(title = "Registration Year", showgrid = TRUE),
                   title = 'Sponsors Trend - Registration')
    })
    
    output$dt_agg_sponsor_per_summ <- renderDataTable({
        agg_studies_spon_per_summ_dt<- agg_studies_spon_per_summ() %>%
            group_by(condition_name, study_first_posted_year) %>%
            summarize(cnt_studies=length(unique(nct_id)))
        
        DT::datatable(
            agg_studies_spon_per_summ_dt,
            escape = FALSE,
            filter = 'top',
            rownames = FALSE,            
            options = list(lengthChange = FALSE,
                           pageLength = 5,
                           searchHighlight = TRUE,
                           order = list(list(1, 'desc'), 
                                        list(2, 'desc'))
            ))
    })
    
    ################################################################
    #PERFORMANCE(SUMMARY) ENDS
    ################################################################
    
    ################################################################
    #PERFORMANCE(DETAIL)
    ################################################################
    
    output$dt_agg_studies_per_summ <- renderDataTable({
        DT::datatable(
            agg_studies,
            escape = FALSE,
            filter = 'top',
            rownames = FALSE,            
            options = list(lengthChange = FALSE,
                           pageLength = 10,
                           searchHighlight = TRUE,
                           order = list(list(8, 'desc'), 
                                        list(10, 'desc'))
            ))
    })
    
    ################################################################
    #PERFORMANCE(DETAIL) ENDS
    ################################################################
    
    ################################################################
    #PERFORMANCE(COMPARE)
    ################################################################
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
                           searchHighlight = TRUE,
                           dom='t')
        )
    })
    
    ################################################################
    #PERFORMANCE(COMPARE) ENDS
    ################################################################
    
    
    ################################################################
    #PERFORMANCE(COMPLETION)
    ################################################################
    
    agg_sponsors_completion <- reactive({
        subset.data.frame(agg_sponsors, subset=(sponsor_size == input$select_sponsor_size_completion)
        )
    })
    
    #Completion Duration
    output$plot_sp_per_1311 <- renderPlotly({
        agg_sponsors_completion() %>%
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
        agg_sponsors_completion() %>%
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
        agg_sponsors_completion() %>%
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
        agg_sponsors_completion() %>%
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
    
    ################################################################
    #PERFORMANCE(COMPLETION) ENDS
    ################################################################
    
    ################################################################
    #PERFORMANCE(INITIATION)
    ################################################################
    
    agg_sponsors_initiation <- reactive({
        subset.data.frame(agg_sponsors, subset=(sponsor_size == input$select_sponsor_size_initiation)
        )
    })
    
    #Initiation Duration
    output$plot_sp_per_1321 <- renderPlotly({
        agg_sponsors_initiation() %>%
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
        agg_sponsors_initiation() %>%
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
        agg_sponsors_initiation() %>%
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
        agg_sponsors_initiation() %>%
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
    
    ################################################################
    #PERFORMANCE(INITITATION) ENDS
    ################################################################
    
    ################################################################
    #PERFORMANCE(KPI)
    ################################################################
    
    agg_sponsors_kpi <- reactive({
        subset.data.frame(agg_sponsors, subset=(sponsor_size == input$select_sponsor_size_kpi)
        )
    })
    
    output$plot_sp_per_1331 <- renderPlotly({
        agg_sponsors_kpi() %>%
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
        agg_sponsors_kpi() %>%
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
        agg_sponsors_kpi() %>%
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
        agg_sponsors_kpi() %>%
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
    
    ################################################################
    #PERFORMANCE(KPI) ENDS
    ################################################################
    
    ################################################################
    #PERFORMANCE(METRICS TABLE)
    ################################################################
    
    output$dt_agg_sponsors <- renderDataTable({
        DT::datatable(
            agg_sponsors,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE, searchHighlight = TRUE)
        )
    })
    
    ################################################################
    #PERFORMANCE(METRICS TABLE) ENDS
    ################################################################
    
    
    ################################################################
    #NETWORK(NETWORK TREE)
    ################################################################
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
    ################################################################
    #NETWORK(NETWORK TREE) ENDS
    ################################################################
    
    ################################################################
    #NETWORK(EXPLORE DATA)
    ################################################################
    output$dt_sponsor_collaborator <- renderDataTable({
        DT::datatable(
            r_sponsor_collaborator,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE, searchHighlight = TRUE)
        )
    })
    ################################################################
    #NETWORK(EXPLORE DATA) ENDS
    ################################################################
    
    ################################################################
    #COLLABORATORS(NETWORK)
    ################################################################
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
    ################################################################
    #COLLABORATORS(NETWORK) ENDS
    ################################################################
    
    ################################################################
    #COLLABORATORS(DATA TABLE)
    ################################################################
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
            options = list(lengthChange = FALSE, searchHighlight = TRUE)
        )
    })
    ################################################################
    #COLLABORATORS(DATA TABLE) ENDS
    ################################################################
    
    
    ################################################################
    #SITES(NETWORK TREE)
    ################################################################
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
    ################################################################
    #SITES(NETWORK TREE) ENDS
    ################################################################
    
    ################################################################
    #SITES(EXPLORE DATA)
    ################################################################
    output$dt_sponsor_site <- renderDataTable({
        DT::datatable(
            r_sponsor_site,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE, searchHighlight = TRUE)
        )
    })
    ################################################################
    #SITES(EXPLORE DATA) ENDS
    ################################################################
    
    ################################################################
    #CONDITIONS(NETWORK TREE)
    ################################################################
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
    ################################################################
    #CONDITIONS(NETWORK TREE) ENDS
    ################################################################
    
    ################################################################
    #CONDITIONS(EXPLORE DATA)
    ################################################################
    output$dt_sponsor_conditions <- renderDataTable({
        DT::datatable(
            r_sponsor_conditions,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE, searchHighlight = TRUE)
        )
    })
    ################################################################
    #CONDITIONS(EXPLORE DATA) ENDS
    ################################################################
    
    ################################################################
    #INTERVENTIONS(EXPLORE DATA)
    ################################################################
    output$dt_sponsor_interventions <- renderDataTable({
        DT::datatable(
            r_sponsor_interventions,
            filter = 'top',
            escape = FALSE,
            rownames = FALSE,
            options = list(lengthChange = FALSE, searchHighlight = TRUE)
        )
    })
    ################################################################
    #INTERVENTIONS(EXPLORE DATA) ENDS
    ################################################################
}

shinyApp(ui, server)