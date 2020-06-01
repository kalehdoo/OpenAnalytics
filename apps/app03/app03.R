#import libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(RColorBrewer)
library(data.table)
library(shinythemes)
library(rsample)
library(lubridate)
library(skimr)


##Design Experiment
#Read measurements
measurements <-
    read.csv(
        "data/measurement.csv",
        header = TRUE,
        sep = ",",
        na.strings = "NA",
        nrows = -100,
        stringsAsFactors = FALSE
    )
#####################

#var_obs_col_name<-colnames(patient_observation_log)
#list of ANOVA Groups
var_obs_col_name <- c("gender", "ethnicity")
var_obs_col_name2 <- c("gender", "ethnicity")
var_obs_metric_col_name <- c("before_value", "after_value", "diff")
var_random_name <- c("Treatment", "Control")
var_measurement_name <- unique(measurements$measurement_name)
var_patient_id <- "P_1001"
var_patient_id_2 <- "P_1002"


ui <- navbarPage(
    title = "Kalehdoo",
    windowTitle = "Kalehdoo Analytics",
    theme = shinytheme("united"),
    collapsible = TRUE,
    #Landing Home page starts
    tabPanel(title = "Home",
             fluidRow(style = "margin-top:0px;margin-left:2%; margin-right:2%",
                 h4("Study Experiment Simulator (In Development)", style = "margin-top:0px;margin-left:5%; margin-right:5%")
             ),
             fluidRow(style = "margin-top:0px;margin-left:2%; margin-right:2%",
                 p(
                     "Making Clinical Intelligence accessible to all patients and organizations such as Pharmaceuticals,
                     CROs, public interest groups, independent consultants, and non-profits contributing to improve clinical research
                     and life sciences for the larger benefit to the entire community.",
                 )
             )),
    
    #Experiment dashboard
    navbarMenu(
        "Experiment",
        tabPanel(
            "Design",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Patient",
                    column(
                        3,
                        align = "left",
                        style = "border-width: 1px solid",
                        numericInput(
                            inputId = "in_var_pat_size",
                            label = "Patients (30-2000)",
                            value = 30,
                            min = 30,
                            max = 1000
                        ),
                        sliderInput(
                            inputId = "in_var_age_min",
                            label = "Min Age",
                            value = 30,
                            min = 1,
                            max = 100
                        ),
                        sliderInput(
                            inputId = "in_var_age_max",
                            label = "Max Age",
                            value = 60,
                            min = 1,
                            max = 100
                        ),
                        numericInput(
                            inputId = "in_var_random_ratio",
                            label = "Randomization Ratio",
                            value = 0.6,
                            min = 0.20,
                            max = 0.99
                        ),
                        numericInput(
                            inputId = "in_var_seed",
                            label = "Seed",
                            value = 123
                        )
                    ),
                    column(9,
                           fluidRow(DT::dataTableOutput("dt_patient")))
                ),
                tabPanel("Measurement",
                         fluidRow(DTOutput('x1'))
                         ),
                tabPanel(
                    "Summary",
                    fluidRow(verbatimTextOutput("tabsummary"))
                ),
                tabPanel(
                    "dataview",
                    fluidRow(DT::dataTableOutput("dt_observation_log"))
                )
            )
        ),
        tabPanel(
            "Results",
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
                tabPanel("Summary",
                         verbatimTextOutput("tabsummaryagg")
                ),
                tabPanel("Data",
                         DT::dataTableOutput("dt_observation_set4_agg")
                         )
            )
        ),
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
                        tabPanel("Summary",
                                 verbatimTextOutput("observation_log_mon_summ")
                        ),
                        tabPanel("DataView",
                                 DT::dataTableOutput("dt_observation_log_mon")
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
    #Create editable data table for Measurements
    ###################################
    x_measure = reactiveValues(df_measurement = NULL)
    
    observe({
        df_measurement <- measurements
        x_measure$df_measurement <- df_measurement
    })
    
    output$x1 = renderDT(x_measure$df_measurement,
                         selection = 'none',
                         editable = TRUE)
    
    proxy = dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
        info = input$x1_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        
        x_measure$df_measurement[i, j] <-
            isolate(DT::coerceValue(v, x_measure$df_measurement[i, j]))
    })
    
    ###############################
    #create patient table
    #Create dataframe for patients
    df_patient <- reactive({
        patient <- data.frame(
            row_id = seq(
                from = 1000 + 1,
                to = 1000 + input$in_var_pat_size,
                by = 1
            ),
            patient_id = paste0(
                "P_",
                seq(
                    from = 1000 + 1,
                    to = 1000 + input$in_var_pat_size,
                    by = 1
                )
            ),
            gender = sample(c("M", "F"), input$in_var_pat_size, replace =
                                TRUE),
            ethnicity = sample(
                c(
                    "Asian",
                    "Latino",
                    "Native American",
                    "African",
                    "White"
                ),
                input$in_var_pat_size,
                replace = TRUE
            ),
            age = sample(
                input$in_var_age_min:input$in_var_age_max,
                input$in_var_pat_size,
                replace = TRUE
            ),
            stringsAsFactors = FALSE
        )
    })
    
    df_patient_sample <- reactive({
        set.seed(input$in_var_seed)
        index <-
            initial_split(df_patient(),
                          prop = input$in_var_random_ratio,
                          strata = "gender")
        patient_treatment <- training(index) %>%
            mutate(random = "Treatment")
        patient_control <- testing(index) %>%
            mutate(random = "Control")
        df_patient <- union_all(patient_treatment, patient_control)
    })
    
    
    output$dt_patient <-
        renderDataTable(DT::datatable(df_patient_sample(), filter = "top"))
    ######################################patient data ends
    
    #measurement_observations
    observation_log <- reactive({
        df_measurement1 <- as.data.frame(x_measure$df_measurement)
        col_names <-
            c(
                "measure_id",
                "measurement_name",
                "total_readings",
                "obsSeq",
                "obs_date",
                "obs_id",
                "measurement_unit",
                "lower_limit",
                "upper_limit",
                "variance_normal",
                "standard_threshold",
                "increase_good"
            )
        df_observations = read.table(text = "", col.names = col_names)
        
        for (i in 1:nrow(df_measurement1)) {
            begin <- 1
            total_readings <- df_measurement1$total_readings[i]
            measure_id <- df_measurement1$measure_id[i]
            measurement_name <- df_measurement1$measurement_name[i]
            measurement_unit <- df_measurement1$measurement_unit[i]
            lower_limit <- df_measurement1$lower_limit[i]
            upper_limit <- df_measurement1$upper_limit[i]
            variance_normal <- df_measurement1$variance_normal[i]
            obs_frequency <- df_measurement1$obs_frequency[i]
            standard_threshold <-
                df_measurement1$standard_threshold[i]
            increase_good <- df_measurement1$increase_good[i]
            while (begin <= total_readings) {
                obs_id = paste(measure_id, "_", begin, sep = "")
                obs_date = if_else(
                    obs_frequency == "Daily",
                    Sys.time() + days(begin - 1),
                    if_else(
                        obs_frequency == "Weekly" && begin == 1,
                        Sys.time(),
                        if_else(
                            obs_frequency == "Weekly" && begin >= 1,
                            Sys.time() + days((begin - 1) * 7),
                            if_else(
                                obs_frequency == "Daily-2" && begin == 1,
                                Sys.time(),
                                if_else(
                                    obs_frequency == "Daily-2" &&
                                        begin >= 1,
                                    Sys.time() + hours((begin - 1) * 12),
                                    if_else(
                                        obs_frequency == "Daily-3" && begin == 1,
                                        Sys.time(),
                                        if_else(
                                            obs_frequency == "Daily-3" &&
                                                begin >= 1,
                                            Sys.time() + hours((begin - 1) * 8),
                                            Sys.time()
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
                #cat(paste(measure_id,measurement_name,total_readings,begin,obs_date,obs_id,measurement_unit,lower_limit,upper_limit,variance_normal,standard_threshold,increase_good, sep="|"),fill =TRUE,file=out_path, append = TRUE)
                row_1 <-
                    paste(
                        measure_id,
                        measurement_name,
                        total_readings,
                        begin,
                        obs_date,
                        obs_id,
                        measurement_unit,
                        lower_limit,
                        upper_limit,
                        variance_normal,
                        standard_threshold,
                        increase_good,
                        sep = "|"
                    )
                new_row <-
                    as.list(strsplit(row_1, split = '|', fixed = TRUE))[[1]]
                
                row_df <- as.data.frame(t(new_row))
                names(row_df) <-
                    c(
                        "measure_id",
                        "measurement_name",
                        "total_readings",
                        "obsSeq",
                        "obs_date",
                        "obs_id",
                        "measurement_unit",
                        "lower_limit",
                        "upper_limit",
                        "variance_normal",
                        "standard_threshold",
                        "increase_good"
                    )
                df_observations <-
                    rbind.data.frame(df_observations, row_df)
                begin <- begin + 1
                
            }
        }
        df_observations <- df_observations
        df_observations <-
            merge.data.frame(df_patient_sample(),
                             df_observations,
                             by = NULL,
                             sort = TRUE)
        df_observations <- arrange(df_observations, (patient_id)) %>%
            mutate(row_id = seq(
                from = 1,
                to = nrow(df_observations),
                by = 1
            ))
        
        df_observations<- data.frame(df_observations)
        
        observation_log_control<-subset.data.frame(df_observations, df_observations$random == "Control")
        observation_log_treatment<-subset.data.frame(df_observations, df_observations$random == "Treatment")
        
        observation_log_control$lower_limit <-
            as.numeric(levels(observation_log_control$lower_limit))[observation_log_control$lower_limit]
        observation_log_control$upper_limit <-
            as.numeric(levels(observation_log_control$upper_limit))[observation_log_control$upper_limit]
        
        col_names <- c("row_id", "actual_value")
        df_observations_control = read.table(text = "", col.names = col_names)
        
        for (i in 1:nrow(observation_log_control)) {
            row_id = observation_log_control$row_id[i]
            lower_limit <- observation_log_control$lower_limit[i]
            upper_limit <- observation_log_control$upper_limit[i]
            actual_value = round(sample(lower_limit:upper_limit, 1, replace = TRUE),
                                 digits = 2)
            
            row_1 <- paste(row_id, actual_value, sep = "|")
            new_row <-
                as.list(strsplit(row_1, split = '|', fixed = TRUE))[[1]]
            row_df <- as.data.frame(t(new_row))
            names(row_df) <- c("row_id", "actual_value")
            
            df_observations_control <-
                rbind.data.frame(df_observations_control, row_df)
        }
        
        observation_log_treatment$lower_limit <-
            as.numeric(levels(observation_log_treatment$lower_limit))[observation_log_treatment$lower_limit]
        observation_log_treatment$upper_limit <-
            as.numeric(levels(observation_log_treatment$upper_limit))[observation_log_treatment$upper_limit]
        observation_log_treatment$variance_normal <-
            as.numeric(levels(observation_log_treatment$variance_normal))[observation_log_treatment$variance_normal]
        observation_log_treatment$obsSeq <-
            as.integer(levels(observation_log_treatment$obsSeq))[observation_log_treatment$obsSeq]
        
        col_names <- c("row_id", "actual_value")
        df_observations_treatment = read.table(text = "", col.names = col_names)
        
        for (i in 1:nrow(observation_log_treatment)) {
            row_id = observation_log_treatment$row_id[i]
            lower_limit <- observation_log_treatment$lower_limit[i]
            upper_limit <- observation_log_treatment$upper_limit[i]
            variance_normal <-
                observation_log_treatment$variance_normal[i]
            obsSeq <- observation_log_treatment$obsSeq[i]
            actual_value = round((
                (variance_normal * log(obsSeq)) +
                    sample(lower_limit:upper_limit, 1, replace = TRUE)
            ), digits = 2)
            
            row_1_tr <- paste(row_id, actual_value, sep = "|")
            new_row_tr <-
                as.list(strsplit(row_1_tr, split = '|', fixed = TRUE))[[1]]
            row_df_tr <- as.data.frame(t(new_row_tr))
            names(row_df_tr) <- c("row_id", "actual_value")
            df_observations_treatment <-
                rbind.data.frame(df_observations_treatment, row_df_tr)
        }
        
        df_observations_treatment <- df_observations_treatment
        
        #Union control and treatment group observations patient_logs
        patient_obs <-
            union_all(df_observations_treatment, df_observations_control)
        #stich patient log to observation log
        df_observations$row_id <- as.numeric(df_observations$row_id)
        patient_obs$row_id <- as.numeric(patient_obs$row_id)
        patient_observation_log <-
            left_join(df_observations, patient_obs, by = "row_id")
        patient_observation_log$actual_value <-
            as.numeric(patient_observation_log$actual_value)
        patient_observation_log$obsSeq <-
            as.integer(patient_observation_log$obsSeq)
        
        patient_observation_log <- data.frame(patient_observation_log)
    })
    
    #update select inputs
    observe({
        updateSelectInput(
            session,
            'in_var_measurement_name',
            choices = unique(x_measure$df_measurement$measurement_name)
        )
        updateSelectInput(
            session,
            'in_var_measurement_name2',
            choices = unique(x_measure$df_measurement$measurement_name)
        )
        updateSelectInput(
            session,
            'in_var_measurement_name3',
            choices = unique(x_measure$df_measurement$measurement_name)
        )
        updateSelectInput(
            session,
            'in_var_measurement_name4',
            choices = unique(x_measure$df_measurement$measurement_name)
        )
    })
    
    #display table summary
    output$tabsummary <- renderPrint({
        skim(observation_log())
    })
    
    #display table data
    output$dt_observation_log <-
        renderDataTable(DT::datatable(observation_log(), filter = "top"))
    
    ####################################################
    
    #reactive data set for PAIRED
    observation_set1_agg <- reactive({
        observation_set1_agg_tmp<-subset.data.frame(observation_log(), subset=(observation_log()$measurement_name == input$in_var_measurement_name & observation_log()$random == input$in_var_random_name)
        )
        observation_set1_agg_tmp<- observation_set1_agg_tmp %>%
            group_by(patient_id) %>%
            summarise(
                "random" = unique(random),
                "gender" = unique(gender),
                "ethnicity" = unique(ethnicity),
                "before_value" = max(if_else(
                    obsSeq == as.numeric(min(obsSeq)), actual_value, 0
                )),
                "after_value" = max(if_else(
                    obsSeq == as.numeric(max(obsSeq)), actual_value, 0
                )),
                "diff" = after_value - before_value
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
        observation_set3_agg_tmp<-subset.data.frame(observation_log(), (observation_log()$measurement_name == input$in_var_measurement_name3 & observation_log()$random == input$in_var_random_name3)
        )
        observation_set3_agg_tmp<- observation_set3_agg_tmp %>%
            group_by(patient_id) %>%
            summarise(
                "random" = unique(random),
                "gender" = unique(gender),
                "ethnicity" = unique(ethnicity),
                "before_value" = max(if_else(
                    obsSeq == as.numeric(min(obsSeq)), actual_value, 0
                )),
                "after_value" = max(if_else(
                    obsSeq == as.numeric(max(obsSeq)), actual_value, 0
                )),
                "diff" = after_value - before_value
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
        observation_set2_agg_tmp<-subset.data.frame(observation_log(), (observation_log()$measurement_name == input$in_var_measurement_name2)
        )
        observation_set2_agg_tmp<- observation_set2_agg_tmp %>%
            group_by(patient_id) %>%
            summarise(
                "random" = unique(random),
                "gender" = unique(gender),
                "ethnicity" = unique(ethnicity),
                "before_value" = max(if_else(
                    obsSeq == as.numeric(min(obsSeq)), actual_value, 0
                )),
                "after_value" = max(if_else(
                    obsSeq == as.numeric(max(obsSeq)), actual_value, 0
                )),
                "diff" = after_value - before_value
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
    #Full data set
    observation_set4_agg <- reactive({
        observation_set4_agg_tmp<-subset.data.frame(observation_log()
        )
        observation_set4_agg_tmp<- observation_set4_agg_tmp %>%
            group_by(patient_id) %>%
            summarise(
                "random" = unique(random),
                "gender" = unique(gender),
                "ethnicity" = unique(ethnicity),
                "before_value" = max(if_else(
                    obsSeq == as.numeric(min(obsSeq)), actual_value, 0
                )),
                "after_value" = max(if_else(
                    obsSeq == as.numeric(max(obsSeq)), actual_value, 0
                )),
                "diff" = after_value - before_value
            )
    })
    ##################################################
    #display observation log data
    output$dt_observation_set4_agg <-
        renderDataTable(DT::datatable(observation_set4_agg(), filter = "top"))
    
    #display table summary
    output$tabsummaryagg <- renderPrint({
        skim(observation_set4_agg())
    })
    
    ###################################################
    #Monitoring
    observe({
        updateSelectInput(
            session,
            'in_var_patient_id',
            choices = unique(df_patient_sample()$patient_id)
        )
        updateSelectInput(
            session,
            'in_var_patient_id_2',
            choices = unique(df_patient_sample()$patient_id)
        )
    })
    #reactive data set
    observation_log_mon <- reactive({
        observation_log()
    })
    #display monitoring
    output$dt_observation_log_mon <-
        renderDataTable(DT::datatable(observation_log_mon(), filter = "top"))
    
    #display table summary
    output$observation_log_mon_summ <- renderPrint({
        skim(observation_log_mon())
    })
    
    #Patient Observation Chart
    output$inv_plot_1 <- renderPlotly({
        observation_log_mon() %>%
            filter(measurement_name == input$in_var_measurement_name4
                & patient_id == input$in_var_patient_id) %>%
            plot_ly() %>%
            add_trace(x=~obsSeq, y=~actual_value, 
                      type="bar", name = "Actual"
                    ) %>%
            add_trace(x=~obsSeq, y=~upper_limit, 
                      type="scatter",mode="lines", name = "Upper Limit") %>%
            add_trace(x=~obsSeq, y=~standard_threshold, 
                      type="scatter",mode="lines", name = "Standard",line=list(color="green")) %>%
            add_trace(x=~obsSeq, y=~lower_limit, 
                      type="scatter",mode="lines", name = "Lower Limit")
            
    })
    
    #Summary - patient
    patient_summary <- reactive({
        observation_log_mon() %>%
            filter(patient_id == input$in_var_patient_id) %>%
            group_by(patient_id, measurement_name, total_readings) %>%
            summarise(
                observations=length(unique(obsSeq))
            )
    })
    
    #display monitoring
    output$dt_patient_summary <-
        renderDataTable(DT::datatable(patient_summary(), rownames=FALSE,
                                      options = list(filter="none")
                                      ))
    
    #############################################################################################
    
    
}

shinyApp(ui, server)