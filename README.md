# Open Clinical Analytics Platform by Kalehdoo (In Development)
Link to the shiny application hosted at shinyapps.io(free account):
Note: Since the dashboards are data intensive, it may take few seconds for the app to load, so have patience.
https://kalehdoo.shinyapps.io/app01/
If you are passionate about contributing to this project, send me a note and we can discuss further.

## Goal: 
The goal of the project is to make Clinical Intelligence accessible to all patients and organizations such as Pharmaceutical sponsors, CROs, public interest groups, and non-profits contributing to improve clinical research and life sciences. 
The platform is being built using open-source technologies like R and Shiny. 
We hope the analytics would help the entire community.

## System Overview:
The clinical trials data is obtained from CTTI website: https://aact.ctti-clinicaltrials.org/pipe_files
The Open Source R programming code downloads the pipe delimited files into the local system and then transforms making it more apt for analytics. The data transformations and analytics are built using R programming.

## How to use the code
The code can be downloaded and copied from GitHub and hosted in your premise on an operating system of your choice that supports R.
You can choose any directory of your choice. The list of directories required can change as development is progressed so always refer to latest param.R file in Global folder.
Once the project is downladed, you would need to execute the R programs to download, transform the data.
The apps are available in apps folder which is an R code to produce the shiny dashboards.






