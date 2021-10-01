#Creates a dataframe of the reading assessments closest to their stork

library(dplyr)

#Load in data
stork <- read.delim(dir("data_categories/stork/",
                           full.names=T, pattern="^stork_20"),header=TRUE, sep="\t")
reading_assessments <- read.delim(dir("data_categories/reading_assessments/",
                                      full.names=T, pattern="^reading_assessments_20"),header=TRUE, sep="\t")

#Create data table
readassess_CloseToStork <- stork %>%
  left_join(reading_assessments, by="record_id")  %>% 
  mutate(stork_read_gap_abs = round(abs(difftime(stork_task_date_v2,readtest_date, units="days")),2),
         stork_read_gap = round(difftime(stork_task_date_v2,readtest_date, units="days")),2) %>%     
  group_by(record_id) %>% 
  slice(which.min(stork_read_gap_abs)) %>%
  select(-stork_task_comp_v2, -stork_notest_reason, -stork_task_date_v2, -stork_testername_v2,
         -stork_task_lefttime_v2, -stork_task_righttime_v2, -stork_task_firstfoot_v2,
         -stork_task_handwrite_v2, -stork_task_footkick_v2, -stork_task_2_complete, -dob_stork_gap) %>% 
  ungroup()

#Write out data table
write.table(readassess_CloseToStork, file=paste("data_categories/reading_assessments/additional_readassess/readassess_CloseToStork_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
