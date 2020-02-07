# Open Clinical Analytics Platform by Kalehdoo

Goal: The purpose of Open Clinical Analytics Platform (OCAP) is to make clinical research analytics available to the patients and organisations that work to make patient lives better. 

## System Overview:
The clinical trials data is obtained from CTTI website: https://aact.ctti-clinicaltrials.org/pipe_files
The Open Source R programming code downloads the pipe delimited files into the local system and then transforms making it more apt for analytics.

## How to use the code
The code can be copied and hosted in your premise on an operating system of your choice tha supports R.
You would need to create the file directories required to download and store the data files as mentioned in param.R file under Global folder.
The directories required are (for example, I am on windows and have a C directory):
C:/OpenClinicalAnalytics/ACCT/
C:/OpenClinicalAnalytics/ACCT/DATA/logs/
C:/OpenClinicalAnalytics/MISC/
You can choose any directory of your choice. The list of directories required can change as development is progressed so always refer to latest param.R file in Global folder.
Once the directories are created, you would need to update the param.R file under Global folder.





