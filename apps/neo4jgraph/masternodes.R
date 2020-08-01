#import libraries
library(dplyr)
library(sqldf)
library(stringr)

#HOME to store path of home dir
assign("var_DIR_HOME", "C:/msrana/projects/github/OpenAnalytics/", envir = .GlobalEnv)

#set paths for data files
in_path_x_sponsors<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_lead_sponsors.txt", sep="")

#reads the data files into dataframes
sponsors<-read.csv(in_path_x_sponsors, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#set paths for data files
in_path_agg_sponsors<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/agg_sponsors.txt", sep="")

#reads the data files into dataframes
agg_sponsors<-read.csv(in_path_sponsors, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

#set paths for data files
in_path_x_collaborators<-paste(var_DIR_HOME, "Data/ACCT/DATA/warehouse/x_collaborators.txt", sep="")

#reads the data files into dataframes
collaborators<-read.csv(in_path_x_collaborators, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

##############################
#CREATE MASTER NODES
##############################

#select distinct sponsor type to make it a seperate node
m_sponsorType<-sqldf("select distinct sponsor_type from sponsors")

#select distinct sponsors to make it a seperate node
m_sponsor<-sqldf("select distinct lead_sponsor_name, count(distinct(nct_id)) as cnt_studies_sponsered 
                  from sponsors
                  group by lead_sponsor_name")
#clean the column by replacing commas and special
m_sponsor <- m_sponsor %>% mutate(lead_sponsor_name = gsub("[',]", "", lead_sponsor_name))

#select distinct collaborators to make it a seperate node
m_collaborator<-sqldf("select distinct collaborator_id, collaborator_name from collaborators")


##############################
#CREATE RELATIONSHIP NODES
##############################

r_sponsor_collaborator<- sqldf("select sponsors.lead_sponsor_name, collaborators.collaborator_name, count(distinct(sponsors.nct_id)) as cnt_studies
                                  from sponsors
                                  left join collaborators
                                  on sponsors.nct_id=collaborators.nct_id
                               group by sponsors.lead_sponsor_name, collaborators.collaborator_name")


##############################
#Generate Scripts
##############################


#add script column to sponsor master - use header col name as CREATE
m_sponsor$script<-paste0(paste0(paste0(paste0("(:Sponsor {sponsor_name: '",m_sponsor$lead_sponsor_name), "',sponsered: "),m_sponsor$cnt_studies_sponsered),"}),")
#remove the ending comma from the last row
m_sponsor1<-head(m_sponsor, nrow(m_sponsor)-1)
m_sponsor2<-tail(m_sponsor, 1)
m_sponsor2 <- m_sponsor2 %>% mutate(script = gsub("[,]", "", script))
m_sponsor<-rbind(m_sponsor1, m_sponsor2)

#extract script from sponsor master
s_sponsor<-subset.data.frame(m_sponsor, select = c(script))

#rename the header so that it acts an CREATE syntax
colnames(s_sponsor)[colnames(s_sponsor)=="script"]<-"CREATE"

#write to script for sponsor master node txt file
write.table(s_sponsor, paste(var_DIR_HOME, "apps/neo4jgraph/s_sponsor.txt", sep=""), sep = "\n", row.names = FALSE, quote = FALSE)

