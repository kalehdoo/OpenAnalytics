# Open Clinical Analytics Platform (In Development)
Link to the shiny application hosted at shinyapps.io(free account):
Note: Since the dashboards are data intensive, it may take few seconds for the app to load, so have patience.
https://kalehdoo.shinyapps.io/app01/

## Goal: 
The goal of the project is to make Clinical Intelligence better for all patients and organizations such as Pharmaceutical sponsors, CROs, public interest groups, and non-profits contributing to improve clinical research and life sciences. 
The platform is being built using open-source technologies like R and Shiny. 

## System Overview:
The clinical trials data is obtained from CTTI website: https://aact.ctti-clinicaltrials.org/pipe_files
The Open Source R programming code downloads the pipe delimited files into the local system and then transforms making it more apt for analytics. The data transformations and analytics are built using R programming.

## How to use the code
The code can be downloaded from GitHub and hosted in your premise on an operating system of your choice that supports R.
You can choose any directory of your choice. Once the folder OpenAnalytics is available on your server machine, navigate to Openanalytics/Global directory and look for files wf_ACCT_Download.R and wf_WH_ACCT.R. Open the files and update the code where it sets the variable to reflect the directory path on your server machine. This code shouls be at the top of the page just below the library. For example, below is the existing code:
assign("var_DIR_HOME", "C:/Users/manohar/Documents/GitHub/OpenAnalytics/", envir = .GlobalEnv)
If you have copied the code to D:/somedirectory/OpenAnalytics, you would update the code as below:
assign("var_DIR_HOME", "D:/somedirectory/OpenAnalytics/", envir = .GlobalEnv)

Next, you would need to execute the R programs to download, transform the data.
Step 1: Execute wf_ACCT_Download.R - This file downloads the latest data from CTTI website.
Step 2: Execute wf_WH_ACCT.R - This file executes files to transform and Integrate the data to load the Data Warehouse.

The shiny R apps are available in OpenAnalytics/apps folder which is an R code to produce the shiny dashboards. The main app is app01.
app02 is the study finder app.
app03qlik is the qlikview application.

## Issues
For any issues, feel free to connect.






