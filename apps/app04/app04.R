#import libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)
library(DT)
#library(RColorBrewer)
library(data.table)
library(shinythemes)
library(rsample)
library(lubridate)
library(mongolite)
library(Hmisc)



#create connection to cloud database
conn_obs_log <- mongo(collection = "observation_log", 
                         db="openanalytics",
                         url = "mongodb+srv://kalehdoo_user:Aquano139182@kalehdoo-gx7df.mongodb.net/test"
)


#####################

#var_obs_col_name<-colnames(patient_observation_log)
#list of ANOVA Groups
var_obs_col_name <- c("gender", "ethnicity")
var_obs_col_name2 <- c("gender", "ethnicity")
var_obs_metric_col_name <- c("before_value", "after_value", "diff")
var_random_name <- c("Treatment", "Control")
var_measurement_name <- conn_obs_log$distinct("measurement_name")
var_patient_id <- conn_obs_log$distinct("patient_id")
#var_patient_id_2 <- "P_1002"


ui <- navbarPage(
    title = "Kalehdoo",
    windowTitle = "Kalehdoo Analytics",
    theme = shinytheme("united"),
    collapsible = TRUE,
    #Landing Home page starts
    tabPanel(title = "Home",
             fluidRow(style = "margin-top:0px;margin-left:2%; margin-right:2%",
                      h4("Patient Analysis and Monitoring", style = "margin-top:0px;margin-left:5%; margin-right:5%")
             ),
             fluidRow(style = "margin-top:0px;margin-left:2%; margin-right:2%",
                      p(
                          "Making Clinical Intelligence accessible to all patients and organizations such as Pharmaceuticals,
                     CROs, public interest groups, independent consultants, and non-profits contributing to improve clinical research
                     and life sciences for the larger benefit to the entire community.",
                      )
             ),
             fluidRow(style = "margin-top:0px;margin-left:2%; margin-right:2%",
                      p(
                          "Patient Analysis and Monitoring enables the patients and physicians to analyse the results of a virtual study experiment on a real time.
                          The data used in this application is a sample test data generated programatically using the Study Experiment Simulator App.
                          This application connects to real-time data on MongoDB cloud server to mimic real-time analysis.
                          The analysis tab allows data scientists and Biostatisticians to analyse the results.
                          The monitoring tab is for physicians and study investigators to monitor the patients health.",
                      )
             ),
             fluidRow(style = "margin-top:0px;margin-left:2%; margin-right:2%",
                 p(
                     "Visit the Study Experiment Simulator App to design a virtual study.",
                     tags$a(href = "https://kalehdoo.shinyapps.io/app03", "Study Experiment Simulator", target =
                                "_blank"),
                     style = "margin-top:0px;margin-left:1%; margin-right:1%"
                 )
             ),
             fluidRow(style = "margin-top:0px;margin-left:2%; margin-right:2%",
                 p(
                     "Follow on twitter for regular updates",
                     tags$a(href = "https://twitter.com/kalehdoo", "Kalehdoo", target =
                                "_blank"),
                     style = "margin-top:0px;margin-left:1%; margin-right:1%"
                 )
             )
             ),
    
    #Patient dashboard
    navbarMenu(
        "Patient",
        tabPanel(
            "Analysis",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Paired",
                    column(
                        3,
                        align = "left",
                        style = "border-width: 1px solid",
                        selectInput(
                            inputId = "in_var_measurement_name",
                            label = "Measurement Name",
                            choices = var_measurement_name
                        ),
                        selectInput(
                            inputId = "in_var_random_name",
                            label = "Group Name",
                            choices = var_random_name
                        ),
                        numericInput(
                            inputId = "select_var_confidence",
                            label = "Enter Confidence Level(0.8-0.99)",
                            value = 0.95,
                            min = 0.8,
                            max = 0.99
                        ),
                        sliderInput(
                            inputId = "in_var_binsize",
                            label = "Histogram Bins",
                            min = 1,
                            max = 50,
                            value = 10
                        )
                    ),
                    column(
                        9,
                        align = "left",
                        style = "border-width: 1px solid",
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            box(plotlyOutput("plot_outcome_1001", height = 200), title = "Box Plot"),
                            box(plotOutput("plot_outcome_1002", height = 200), title = "Q-Q Plot")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            box(plotlyOutput("plot_outcome_1003", height = 200), title = "Histogram"),
                            p(
                                "p-values greater than the significance level imply that the distribution of the data is not significantly different from the normal distribution."
                            ),
                            verbatimTextOutput("ShapiroTest_set1")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "Paired T-Test: Is there any significant difference between means of two pairs(before and after treatment) of one sample?
                p-values less than the significance level imply that the means of the pairs are significantly different."
                            ),
                            verbatimTextOutput("T_Test_Paired")
                        )
                    )
                ),
                tabPanel(
                    "ANOVA",
                    column(
                        3,
                        align = "left",
                        style = "border-width: 1px solid",
                        selectInput(
                            inputId = "in_var_measurement_name3",
                            label = "Measurement Name",
                            choices = var_measurement_name
                        ),
                        selectInput(
                            inputId = "in_var_random_name3",
                            label = "Group Name",
                            choices = var_random_name,
                            selected = "Treatment"
                        ),
                        selectInput(
                            inputId = "in_var_obs_col_name",
                            label = "ANOVA Group",
                            choices = var_obs_col_name,
                            selected = "gender"
                        ),
                        selectInput(
                            inputId = "in_var_obs_col_name24",
                            label = "ANOVA Group 2",
                            choices = var_obs_col_name2,
                            selected = "ethnicity"
                        ),
                        selectInput(
                            inputId = "in_var_obs_metric_col_name3",
                            label = "Metric",
                            choices = var_obs_metric_col_name,
                            selected = "diff"
                        ),
                        numericInput(
                            inputId = "select_var_confidence3",
                            label = "Enter Confidence Level",
                            value = 0.95,
                            min = 0.8,
                            max = 0.99
                        )
                    ),
                    column(
                        9,
                        align = "left",
                        style = "border-width: 1px solid",
                        fluidRow(style = "padding: 5px; border-style: solid; border-width: 1px;",
                                 plotlyOutput("boxplot_outcome_1003")),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p("Summary Statistics"),
                            verbatimTextOutput("summary3")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "ANOVA: Is there any significant difference between means of two samples?
                p-values less than the significance level imply that the means of the two samples are significantly different."
                            ),
                            verbatimTextOutput("ANOVA1")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "TuKey Post Hoc test for pairwise comparison: which group pair has a different mean?
                p-values less than the significance level imply that the means of the two samples are significantly different."
                            ),
                            verbatimTextOutput("ANOVA_PostHoc3")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "Two Way ANOVA: H0: The means are equal for both variables.
                p-values less than the significance level imply that the means of the two samples are significantly different."
                            ),
                            verbatimTextOutput("TwoWayANOVA")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "2-Way ANOVA TuKey Post Hoc test for pairwise comparison
                p-values less than the significance level imply that the means of the two samples are significantly different."
                            ),
                            verbatimTextOutput("TwoWayANOVA_PostHoc3")
                        )
                    )
                ),
                tabPanel(
                    "UnPaired",
                    column(
                        3,
                        align = "left",
                        style = "border-width: 1px solid",
                        selectInput(
                            inputId = "in_var_measurement_name2",
                            label = "Measurement Name",
                            choices = var_measurement_name
                        ),
                        selectInput(
                            inputId = "in_var_obs_metric_col_name23",
                            label = "Metric",
                            choices = var_obs_metric_col_name
                        ),
                        numericInput(
                            inputId = "select_var_confidence2",
                            label = "Enter Confidence Level",
                            value = 0.95,
                            min = 0.8,
                            max = 0.99
                        )
                    ),
                    column(
                        9,
                        align = "left",
                        style = "border-width: 1px solid",
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            box(plotlyOutput("plot_outcome_1001_2", height = 200), title = "Box Plot"),
                            box(plotlyOutput("plot_outcome_1003_2", height = 200), title = "Before Vs After Treatment")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "F-Test: Do the two populations have the same variances?
                p-values greater than the significance level imply that there is no significant difference between the variances of the two sets of data.
                Therefore, we can use the classic t-test which assumes equality of the two variances."
                            ),
                            verbatimTextOutput("FTest_set2")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "Unpaired T-Test: Is there any significant difference between means of two samples?
                p-values less than the significance level imply that the means of the two samples are significantly different."
                            ),
                            verbatimTextOutput("T_Test_UnPaired_2")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "ANOVA: Is there any significant difference between means of two samples?
                p-values less than the significance level imply that the means of the two samples are significantly different."
                            ),
                            verbatimTextOutput("ANOVA2")
                        ),
                        fluidRow(
                            style = "padding: 5px; border-style: solid; border-width: 1px;",
                            p(
                                "TuKey Post Hoc test for pairwise comparison: which group pair has a different mean?
                p-values less than the significance level imply that the means of the two samples are significantly different."
                            ),
                            verbatimTextOutput("ANOVA_PostHoc2")
                        )
                    )
                ),
                tabPanel("Detail Summary",
                         verbatimTextOutput("observation_log_mon_summ")
                ),
                tabPanel("Detail DataView",
                         DT::dataTableOutput("dt_observation_log_mon")
                ),
                tabPanel("Agg Summary",
                         verbatimTextOutput("tabsummaryagg")
                ),
                tabPanel("Agg DataView",
                         DT::dataTableOutput("dt_observation_set4_agg")
                )
            )
        ),
        #Monitoring Tab starts##############################
        tabPanel("Monitoring",
                 tabsetPanel(type = "tabs",
                             tabPanel("Patient",
                                      column(
                                          3,
                                          align = "left",
                                          style = "border-width: 1px solid",
                                          selectInput(
                                              inputId = "in_var_patient_id",
                                              label = "Patient ID",
                                              choices = var_patient_id
                                          ),
                                          selectInput(
                                              inputId = "in_var_measurement_name4",
                                              label = "Measurement Name",
                                              choices = var_measurement_name
                                          )
                                      ),
                                      column(
                                          9,
                                          align = "left",
                                          style = "border-width: 1px solid",
                                          fluidRow(
                                              style = "padding: 5px; border-style: solid; border-width: 1px;",
                                              DT::dataTableOutput("dt_patient_summary")
                                          ),
                                          fluidRow(
                                              style = "padding: 5px; border-style: solid; border-width: 1px;",
                                              plotlyOutput("inv_plot_1")
                                          )
                                      )
                             ),
                             tabPanel("Physician",
                                      column(
                                          3,
                                          align = "left",
                                          style = "border-width: 1px solid",
                                          selectInput(
                                              inputId = "in_var_measurement_name5",
                                              label = "Measurement Name",
                                              choices = var_measurement_name
                                          )
                                      ),
                                      column(
                                          9,
                                          align = "left",
                                          style = "border-width: 1px solid",
                                          fluidRow(
                                              style = "padding: 5px; border-style: solid; border-width: 1px;",
                                              plotlyOutput("inv_plot_2")
                                          )
                                          
                                      )
                             ),
                             tabPanel("RealTime",
                                      column(
                                        12,
                                        align = "left",
                                        style = "border-width: 1px solid",
                                        fluidRow(
                                          style = "padding: 5px; border-style: solid; border-width: 1px;",
                                          plotlyOutput("inv_plot_4")
                                        )
                                        
                                      )
                             )
                 )
        )
    )
    
    #ending main bracket
)


############################################################################################################
#Server function begins
###########################################################################################################

server <- function(input, output, session) {
    #Begin experiment design
    
    
    ######################################patient data ends
    
    
    #observation data
    observation_log <- reactive({
        
        read_observation_log<-conn_obs_log$find('{}')
        read_observation_log<-as_tibble(read_observation_log)
    })
    
    #seperate observation log for real time refresh every 30 sec
    observation_log_RT <- reactive({
      invalidateLater(30000, session)
      read_observation_log<-conn_obs_log$find('{}')
      read_observation_log<-as_tibble(read_observation_log)
    })
    
    #update select inputs
    observe({
        updateSelectInput(
            session,
            'in_var_measurement_name',
            choices = unique(observation_log()$measurement_name)
        )
        updateSelectInput(
            session,
            'in_var_measurement_name2',
            choices = unique(observation_log()$measurement_name)
        )
        updateSelectInput(
            session,
            'in_var_measurement_name3',
            choices = unique(observation_log()$measurement_name)
        )
        updateSelectInput(
            session,
            'in_var_measurement_name4',
            choices = unique(observation_log()$measurement_name)
        )
        updateSelectInput(
            session,
            'in_var_measurement_name5',
            choices = unique(observation_log()$measurement_name)
        )
    })
    
    ####################################################
    #Full aggregate data set without any filters
    observation_set4_agg <- reactive({
        observation_set4_agg_tmp<-subset.data.frame(observation_log()
        )
        observation_set4_agg_tmp<- observation_set4_agg_tmp %>%
            group_by(patient_id,measurement_name) %>%
            summarise(
                "random" = unique(random),
                "gender" = unique(gender),
                "ethnicity" = unique(ethnicity),
                "before_value" = max(if_else(
                    obsSeq == as.numeric(min(obsSeq)), actual_value, 0
                )),
                "after_value" = max(if_else(
                    obsSeq == as.numeric(max(obsSeq)), round(actual_value,digits = 3), 0
                )),
                "diff" = round((after_value - before_value),digits = 3),
                "diff_pc" = round((((after_value - before_value)/before_value)*100),digits = 2),
                "previous_value" = max(if_else(
                    obsSeq == (as.numeric(max(obsSeq)-1)), round(actual_value,digits = 3), 0
                )),
                "change" = round((after_value - previous_value),digits = 3),
                "change_pc" = round(((after_value - previous_value)/previous_value)*100,digits=2),
                "increase_good"=max(increase_good),
                "ind_diff"=unique(if_else((diff>0 & increase_good==1) | (diff<0 & increase_good==0),"Good","Bad")),
                "ind_change"=unique(if_else((change>0 & increase_good==1) | (diff<0 & increase_good==0),"Good","Bad")),
                
            )
        })
    ##################################################
    
    #reactive data set for PAIRED
    observation_set1_agg <- reactive({
        subset.data.frame(observation_set4_agg(), subset=(measurement_name == input$in_var_measurement_name & random == input$in_var_random_name)
        )
    })
    
    #box plot to visualize the normal distribution
    output$plot_outcome_1001 <- renderPlotly({
        plot_ly() %>%
            add_trace(
                y =  ~ before_value,
                type = "box",
                data = observation_set1_agg(),
                name = "Before",
                boxpoints = "all",
                jitter = 0.3,
                pointpos = -1.8,
                showlegend = FALSE
            ) %>%
            add_trace(
                y =  ~ after_value,
                type = "box",
                data = observation_set1_agg(),
                name = "After",
                boxpoints = "all",
                jitter = 0.3,
                pointpos = -1.8,
                showlegend = FALSE
            )
    })
    
    #histogram set 1
    output$plot_outcome_1003 <- renderPlotly({
        plot_ly(
            data = observation_set1_agg(),
            x =  ~ diff,
            type = "histogram",
            nbinsx = input$in_var_binsize
        )
    })
    
    output$ShapiroTest_set1 <- renderPrint({
        with(observation_set1_agg(), shapiro.test(diff))
    })
    
    #QQ plot to visualize the normal distribution
    output$plot_outcome_1002 <- renderPlot({
        qqnorm(observation_set1_agg()$diff)
        qqline(observation_set1_agg()$diff)
    })
    
    #T-Test (Paired: After-Before for same sample)
    output$T_Test_Paired <- renderPrint({
        t.test(
            observation_set1_agg()$after_value,
            observation_set1_agg()$before_value,
            mu = 0,
            alternative = "two.sided",
            paired = TRUE,
            conf.level = input$select_var_confidence
        )
    })
    
    ##############################################################
    #ANOVA - reactive data set 3 for ANOVA
    observation_set3_agg <- reactive({
        observation_set3_agg_tmp<-subset.data.frame(observation_set4_agg(), (measurement_name == input$in_var_measurement_name3 & random == input$in_var_random_name3)
        )
    })
    
    
    #box plot to visualize the normal distribution
    output$boxplot_outcome_1003 <- renderPlotly({
        plot_ly(
            data = observation_set3_agg(),
            y = as.formula(paste(
                '~', input$in_var_obs_metric_col_name3
            )),
            color = as.formula(paste('~', input$in_var_obs_col_name)),
            type = "box",
            showlegend = FALSE,
            boxpoints = "all"
        )
    })
    
    #summary statistics
    output$summary3 <- renderPrint({
        observation_set3_agg() %>%
            group_by(!!rlang::sym(input$in_var_obs_col_name)) %>%
            summarise(
                record_count = n(),
                mean = mean((
                    !!rlang::sym(input$in_var_obs_metric_col_name3)
                ), na.rm = TRUE),
                standardDeviation = sd((
                    !!rlang::sym(input$in_var_obs_metric_col_name3)
                ), na.rm = TRUE)
            )
    })
    
    #ANOVA 1
    output$ANOVA1 <- renderPrint({
        #model_anova<-lm(as.formula(paste('diff ~', input$in_var_obs_col_name)), data = observation_set3_agg())
        model_anova <-
            aov(as.formula(
                paste(
                    input$in_var_obs_metric_col_name3,
                    '~',
                    input$in_var_obs_col_name
                )
            ),
            data = observation_set3_agg())
        anova(model_anova)
        #summary(model_anova)
    })
    
    #ANOVA Post-hoc test - Which of the different groups have different means
    output$ANOVA_PostHoc3 <- renderPrint({
        model_anova <-
            aov(as.formula(
                paste(
                    input$in_var_obs_metric_col_name3,
                    '~',
                    input$in_var_obs_col_name
                )
            ),
            data = observation_set3_agg())
        TukeyHSD(model_anova, conf.level = input$select_var_confidence3)
    })
    
    #Two-Way ANOVA
    output$TwoWayANOVA <- renderPrint({
        model_twowayanova <-
            aov(as.formula(
                paste(
                    input$in_var_obs_metric_col_name3,
                    '~',
                    input$in_var_obs_col_name24,
                    '+',
                    input$in_var_obs_col_name
                )
            ),
            data = observation_set3_agg())
        anova(model_twowayanova)
    })
    
    #Two-Way ANOVA Post-hoc test
    output$TwoWayANOVA_PostHoc3 <- renderPrint({
        model_twowayanova <-
            aov(as.formula(
                paste(
                    input$in_var_obs_metric_col_name3,
                    '~',
                    input$in_var_obs_col_name24,
                    '+',
                    input$in_var_obs_col_name
                )
            ),
            data = observation_set3_agg())
        TukeyHSD(model_twowayanova,
                 conf.level = input$select_var_confidence3)
    })
    
    #######################################################################
    #UNPAIRED - reactive data set 2 for a measurement
    observation_set2_agg <- reactive({
        
        subset.data.frame(observation_set4_agg(),
                          subset=(measurement_name == input$in_var_measurement_name2)
        )
        
    })
    
    #box plot to visualize the normal distribution
    output$plot_outcome_1001_2 <- renderPlotly({
        plot_ly() %>%
            add_trace(
                y =  as.formula(paste(
                    '~', input$in_var_obs_metric_col_name23
                )),
                color =  ~ random,
                type = "box",
                data = observation_set2_agg(),
                boxpoints = "all",
                jitter = 0.3,
                pointpos = -1.8,
                showlegend = FALSE
            )
    })
    
    #Chart to compare increase in actual value
    output$plot_outcome_1003_2 <- renderPlotly({
        plot_ly(data = observation_set2_agg()) %>%
            add_trace(
                x =  ~ before_value,
                y =  ~ after_value,
                type = "scatter",
                color =  ~ random
            )
    })
    
    #F-Test: Do the two populations have the same variances?
    output$FTest_set2 <- renderPrint({
        var.test(
            as.formula(
                paste(input$in_var_obs_metric_col_name23, '~ random')
            ),
            data = observation_set2_agg(),
            alternative = "two.sided",
            conf.level = input$select_var_confidence2
        )
    })
    
    #T-Test (UnPaired: two samples (Control and Treatment))
    output$T_Test_UnPaired_2 <- renderPrint({
        t.test(
            as.formula(
                paste(input$in_var_obs_metric_col_name23, '~ random')
            ),
            data = observation_set2_agg(),
            mu = 0,
            alternative = "two.sided",
            paired = FALSE,
            conf.level = input$select_var_confidence2
        )
    })
    
    #ANOVA 2
    output$ANOVA2 <- renderPrint({
        #model_anova<-lm(as.formula(paste('diff ~', input$in_var_obs_col_name)), data = observation_set3_agg())
        model_anova2 <-
            aov(as.formula(
                paste(input$in_var_obs_metric_col_name23, '~ random')
            ),
            data = observation_set2_agg())
        anova(model_anova2)
        #summary(model_anova)
    })
    
    #ANOVA Post-hoc test - Which of the different groups have different means
    output$ANOVA_PostHoc2 <- renderPrint({
        model_anova2 <-
            aov(as.formula(
                paste(input$in_var_obs_metric_col_name23, '~ random')
            ),
            data = observation_set2_agg())
        TukeyHSD(model_anova2, conf.level = input$select_var_confidence2)
    })
    
    ###########################################################################################

    ##################################################
    #display observation log data
    output$dt_observation_set4_agg <-
        renderDataTable(DT::datatable(observation_set4_agg(), filter = "top"
                                      ))
    
    #display table summary
    output$tabsummaryagg <- renderPrint({
        Hmisc::describe(observation_set4_agg())
    })
    
    #display monitoring
    output$dt_observation_log_mon <-
      renderDataTable(DT::datatable(observation_log(), filter = "top"
      ))
    
    #display table summary
    output$observation_log_mon_summ <- renderPrint({
      Hmisc::describe(observation_log())
    })
    
    #Summary - patient
    patient_summary <- reactive({
      patient_summary<-subset.data.frame(observation_set4_agg(), subset=(patient_id == input$in_var_patient_id),
                                         select = c("measurement_name", "before_value", "after_value","diff", "diff_pc","previous_value","change","change_pc","ind_diff", "ind_change"))
      
      patient_summary %>%
        rename("Start"="before_value", "last"="after_value","previous"="previous_value")
    })
    #display monitoring patient agg summ with hide 2 columns
    output$dt_patient_summary <-
      renderDataTable(DT::datatable(patient_summary(), rownames=FALSE,
                                    extensions = 'FixedColumns',
                                    options=list(
                                      columnDefs = list(list(targets = c(8,9), visible = FALSE)),
                                      dom = 't',
                                      scrollX = TRUE,
                                      fixedColumns = TRUE
                                    )
      ) %>%
        formatStyle("diff","ind_diff",
                    backgroundColor = styleEqual(c("Good","Bad"), c('green','orange'))) %>%
        formatStyle("change","ind_change",
                    backgroundColor = styleEqual(c("Good","Bad"), c('green','orange')))
      )
    
    ###################################################
    #Monitoring
    observe({
        updateSelectInput(
            session,
            'in_var_patient_id',
            choices = unique(observation_log()$patient_id)
        )
        updateSelectInput(
            session,
            'in_var_patient_id_2',
            choices = unique(observation_log()$patient_id)
        )
    })
    
    #Patient Observation Chart
    output$inv_plot_1 <- renderPlotly({
        
        observation_log() %>%
            filter(measurement_name == input$in_var_measurement_name4
                   & patient_id == input$in_var_patient_id) %>%
            plot_ly() %>%
            add_trace(x=~obsSeq, y=~actual_value, 
                      type="bar", name = "Actual",
                      text = ~ paste(
                          paste("Patient ID:", patient_id),
                          paste("Observation:", obsSeq),
                          paste("Date:", obs_date),
                          paste("Value:", paste(round(actual_value, digits = 2),measurement_unit,sep = " ")),
                          sep = "<br />"
                      ),
                      hoverinfo = 'text'
            ) %>%
            add_trace(x=~obsSeq, y=~upper_limit, 
                      type="scatter",mode="lines", name = "Upper Limit") %>%
            add_trace(x=~obsSeq, y=~standard_threshold, 
                      type="scatter",mode="lines", name = "Standard",line=list(color="green")) %>%
            add_trace(x=~obsSeq, y=~lower_limit, 
                      type="scatter",mode="lines", name = "Lower Limit")
        
    })
    
    #Physician Observation Chart
    output$inv_plot_2 <- renderPlotly({
        observation_log() %>%
            filter(measurement_name == input$in_var_measurement_name5) %>%
            plot_ly() %>%
            add_trace(x=~obsSeq, y=~actual_value, 
                      type="scatter",mode = "markers", color=~random,
                      text = ~ paste(
                          paste("Patient ID:", patient_id),
                          paste("Gender:", gender),
                          paste("Ethnicity:", ethnicity),
                          paste("Observation:", obsSeq),
                          paste("Value:", paste(actual_value,measurement_unit,sep = " ")),
                          sep = "<br />"
                      ),
                      hoverinfo = 'text'
            ) %>%
            add_trace(x=~obsSeq, y=~upper_limit, 
                      type="scatter",mode="lines", name = "Upper",line=list(color="red")) %>%
            add_trace(x=~obsSeq, y=~standard_threshold, 
                      type="scatter",mode="lines", name = "Standard",line=list(color="green")) %>%
            add_trace(x=~obsSeq, y=~lower_limit, 
                      type="scatter",mode="lines", name = "Lower",line=list(color="orange"))
    })
    
   
    #Full aggregate data for realTime
    observation_set5_agg_RT <- reactive({
      observation_set5_agg_tmp<-subset.data.frame(observation_log_RT()
      )
      observation_set5_agg_tmp<- observation_set5_agg_tmp %>%
        group_by(patient_id,measurement_name) %>%
        summarise(
          "random" = unique(random),
          "gender" = unique(gender),
          "ethnicity" = unique(ethnicity),
          "before_value" = max(if_else(
            obsSeq == as.numeric(min(obsSeq)), actual_value, 0
          )),
          "after_value" = max(if_else(
            obsSeq == as.numeric(max(obsSeq)), round(actual_value,digits = 3), 0
          )),
          "diff" = round((after_value - before_value),digits = 3),
          "diff_pc" = round((((after_value - before_value)/before_value)*100),digits = 2),
          "previous_value" = max(if_else(
            obsSeq == (as.numeric(max(obsSeq)-1)), round(actual_value,digits = 3), 0
          )),
          "change" = round((after_value - previous_value),digits = 3),
          "change_pc" = round(((after_value - previous_value)/previous_value)*100,digits=2),
          "increase_good"=max(increase_good),
          "ind_diff"=unique(if_else((diff>0 & increase_good==1) | (diff<0 & increase_good==0),"Good","Bad")),
          "ind_change"=unique(if_else((change>0 & increase_good==1) | (diff<0 & increase_good==0),"Good","Bad")),
          
        )
    })
    
    output$inv_plot_4 <- renderPlotly({
      
      plot_4<- ggplot(observation_set5_agg_RT(), aes(x=patient_id, y=change_pc)) + geom_point()+
        facet_grid(rows = vars(measurement_name),scales = "free",space = "free",shrink = TRUE)
      ggplotly(plot_4)
        
      
    })
    
    ###############################################################
    #SERVER ENDS
    ###############################################################
    
    
}

shinyApp(ui, server)