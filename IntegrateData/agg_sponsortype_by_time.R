#import libraries
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)
library(sqldf)

#set paths for data files
in_path_agg_studies <-
  paste(var_DIR_HOME,
        "Data/ACCT/DATA/warehouse/agg_studies.txt",
        sep = "")

#reads the data files into dataframes
agg_studies <-
  read.csv(
    in_path_agg_studies,
    header = TRUE,
    na.strings = "NA",
    sep = "|",
    nrows = -100
  )

agg_sponsorstype_by_time <- setDT(agg_studies)[, list(
  cnt_studies_registered = length(unique(nct_id)),
  cnt_studies_with_collab = sum(if_else((cnt_colab_Total)>=1 ,1L,0L), na.rm = TRUE),
  cnt_studies_interventional = sum(if_else(study_type== "Interventional" ,1L,0L), na.rm = TRUE),
  cnt_studies_observational = sum(if_else(study_type== "Observational" ,1L,0L), na.rm = TRUE),
  cnt_studies_with_nonUS_country = sum(if_else((cnt_countries_nonUS)>=1 ,1L,0L), na.rm = TRUE),
  cnt_studies_USonly = sum(if_else((flag_USonly)>=1 ,1L,0L), na.rm = TRUE),
  cnt_sponsors = length(unique(lead_sponsor_name))
), by = c('sponsor_type','study_first_posted_year')]

agg_sponsorstype_by_time<-subset.data.frame(agg_sponsorstype_by_time,
                                           subset = (is.na(study_first_posted_year)==FALSE
                                                     & study_first_posted_year>=var_last_year_15
                                                     & study_first_posted_year<=var_current_year))

#write to txt file
write.table(
  agg_sponsorstype_by_time,
  paste(
    var_DIR_HOME,
    "Data/ACCT/DATA/warehouse/agg_sponsorstype_by_time.txt",
    sep = ""
  ),
  sep = "|",
  row.names = FALSE
)
