#Creates a dataframe of all reading assessments, except the PPVT
#Easiest for PPVT to be separated b/c it's only done once a year, not twice

library(dplyr)

lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")

#Specify entry and post testing dates as date data types
data$entrytestdate <- as.Date(as.character(data$entrytestdate),"%Y-%m-%d")
data$posttest_date <- as.Date(as.character(data$posttest_date),"%Y-%m-%d")

#Create data table
reading_assessments <- data %>%
  select(-childsex, -childsex.factor, -childdob) %>% 
  left_join(lookup_table, by = "record_id") %>% 
  group_by(record_id) %>% 
  filter(grepl("t1|t2|t3|t4|t5|p1|p2|p3|p4|p5",redcap_event_name)) %>% 
  mutate(entry_post_years = ifelse(!is.na(entryageyears),entryageyears,
                                   ifelse(!is.na(posttest_ageyears),posttest_ageyears,NA)),
         entry_post_months = ifelse(!is.na(entryagemonths),entryagemonths,
                                   ifelse(!is.na(posttest_agemonths),posttest_agemonths,NA)),
         entry_post_days = ifelse(!is.na(entryagedays),entryagedays,
                                   ifelse(!is.na(posttest_agedays),posttest_agedays,NA)),
         read_age = round(as.numeric(((entry_post_years*365.25)+(entry_post_months*30.42)+entry_post_days)/365.25),2),
         readtest_date = as.Date(as.numeric(ifelse(!is.na(entrytestdate),as.Date(entrytestdate, format = "%Y-%m-%d"),
                                                   ifelse(!is.na(posttest_date),as.Date(posttest_date, format = "%Y-%m-%d"),NA))), origin = "1970-01-01"),
         dob_read_gap = abs(difftime(childdob,readtest_date, units="days"))) %>% 
  select(record_id,
         read_age,
         entryassessmentcompleted:entry_assessment_information_complete,
         posttest_completed:posttest_information_complete,
         towre_completed:towre2_complete,
         wj_completed:wjiii_complete,
         ctopp_completed:ctopp2_complete,
         gort_completed:gort5_complete,
         lmm_completed:lexinome_motivational_measure_complete,
         tvip_completed:tvip_complete,
         entry_post_years, entry_post_months, entry_post_days,
         read_age, readtest_date, dob_read_gap)

#Write out data table
write.table(reading_assessments, file=paste("data_categories/reading_assessments/reading_assessments_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
