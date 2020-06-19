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
library(Hmisc)


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
                 h4("Study Experiment Simulator", style = "margin-top:0px;margin-left:5%; margin-right:5%")
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
                          "Design a virtual clinical study experiment and analyze the results. Choose parameters of a study such as a study population size, randomization, study outcomes, and measurement frequency. Visualize the results and run various statistical tests (T-Tests and ANOVA) to measure the study results.",
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
                        sliderInput(
                            inputId = "in_var_pat_size",
                            label = "Patients (30-100)",
                            value = 30,
                            min = 30,
                            max = 100
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
                        sliderInput(
                            inputId = "in_var_random_ratio",
                            label = "Ratio (Treatment:Control)",
                            value = 0.6,
                            min = 0.3,
                            max = 0.7
                        ),
                        numericInput(
                            inputId = "in_var_seed",
                            label = "Seed",
                            value = 123
                        )
                    ),
                    column(9,
                           fluidRow(style = "margin:1%",
                               h5("Select patient size, age group and randamization ratio. Thereafter, move on to the measurement tab.")
                           ),
                           fluidRow(DT::dataTableOutput("dt_patient"))
                           )
                ),
                tabPanel("Measurement",
                         fluidRow(style = "margin:1%",
                                  h5("The table below displays 5 measurements that you can edit. You can edit any measurement by double click on the cell. You may not want to edit measurement ID"),
                                  p("obs_frequency - Values are case sensitive and can be changed to any of these (Daily, Weekly, Daily-2, Daily-3). Daily-3 would mean that 3 observations per day for a patient.
                                    lower_limit and upper_limits are the threshold values that a control group patient would observe.
                                    Variance_normal means the variance that you expect to see after the treatment. standard_threshold means the reading that would be considered normal for any patient."),
                                  p("Once done with changes, you can navigate to other tabs to view the summary statistics and view the analysis. Once you move away from measurement tab, it may take some time to view the results based on the number of observations it will generate. Once loaded, the rest of tabs should be responsive unless you change the design parameters again.")
                         ),
                         fluidRow(DTOutput('x1'))
                         ),
                tabPanel(
                    "Detail Summary",
                    fluidRow(verbatimTextOutput("tabsummary"))
                ),
                tabPanel(
                    "Detail dataview",
                    fluidRow(DT::dataTableOutput("dt_observation_log"))
                ),
                tabPanel("Agg Summary",
                         verbatimTextOutput("tabsummaryagg")
                ),
                tabPanel(" Agg DataView",
                         DT::dataTableOutput("dt_observation_set4_agg")
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
                        sliderInput(
                            inputId = "select_var_confidence",
                            label = "Enter Confidence Level",
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
        
        #observation_log_control$lower_limit <-
         #   as.numeric(levels(observation_log_control$lower_limit))[observation_log_control$lower_limit]
        #observation_log_control$upper_limit <-
         #   as.numeric(levels(observation_log_control$upper_limit))[observation_log_control$upper_limit]
        
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
        
        df_observations_control <- data.frame(df_observations_control)
        
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
                (as.numeric(variance_normal) * log(as.numeric(obsSeq)))
                +
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
        
        df_observations_treatment <- data.frame(df_observations_treatment)
        
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
        updateSelectInput(
            session,
            'in_var_measurement_name5',
            choices = unique(x_measure$df_measurement$measurement_name)
        )
    })
    
    
    ####################################################
    
    #reactive data set for PAIRED
    observation_set1_agg <- reactive({
        observation_set1_agg_tmp<-subset.data.frame(observation_set4_agg(), subset=(measurement_name == input$in_var_measurement_name & random == input$in_var_random_name)
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
        observation_set2_agg_tmp<-subset.data.frame(observation_set4_agg(), (measurement_name == input$in_var_measurement_name2)
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
    #Full aggregate data set
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
                "diff_pc" = round((((after_value - before_value)/after_value)*100),digits = 3),
                "previous_value" = max(if_else(
                    obsSeq == (as.numeric(max(obsSeq)-1)), round(actual_value,digits = 3), 0
                )),
                "change" = round((after_value - previous_value),digits = 3),
                "change_pc" = round(((after_value - previous_value)/after_value)*100,digits=3),
                "increase_good"=max(increase_good),
                "ind_diff"=unique(if_else((diff>0 & increase_good==1) | (diff<0 & increase_good==0),"Good","Bad")),
                "ind_change"=unique(if_else((change>0 & increase_good==1) | (diff<0 & increase_good==0),"Good","Bad")),
                
            )
    })
    
    
    ###################################################
    #Patient Monitoring
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
    
    #Patient Observation Chart
    output$inv_plot_1 <- renderPlotly({
        observation_log() %>%
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
    
    ################################
    #######datatables and summary############
    ##################################################
    #display table data
    output$dt_observation_log <-
        renderDataTable(DT::datatable(observation_log(), filter = "top"))
    
    #display table summary
    output$tabsummary <- renderPrint({
        Hmisc::describe(observation_log())
    })
    
    
    #display observation log data
    output$dt_observation_set4_agg <-
        renderDataTable(DT::datatable(observation_set4_agg(), filter = "top"))
    
    #display table summary
    output$tabsummaryagg <- renderPrint({
        Hmisc::describe(observation_set4_agg())
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
    
    ###############################################################
    #SERVER ENDS
    ###############################################################
    
    
}

shinyApp(ui, server)