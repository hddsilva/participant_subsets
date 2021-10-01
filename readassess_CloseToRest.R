#Creates a dataframe of the reading assessments closest to the participant's most recent usable resting state

library(dplyr)

#Load data
most_recent_rest <- read.delim(dir("data_categories/mri_data/additional_mri_data/most_recent/",
                                      full.names=T, pattern="^most_recent_rest_20"),header=TRUE, sep="\t")
mri_data <- read.delim(dir("data_categories/mri_data/",
                                      full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")
reading_assessments <- read.delim(dir("data_categories/reading_assessments/",
                                      full.names=T, pattern="^reading_assessments_20"),header=TRUE, sep="\t")

#Create data table
readassess_CloseToRest <- most_recent_rest %>%
  left_join(mri_data, by = c("record_id","mrrc_id")) %>% 
  left_join(reading_assessments, by="record_id") %>% 
  mutate(rest_read_gap_abs = abs(difftime(mri_date,readtest_date, units="days")),
         rest_read_gap = difftime(mri_date,readtest_date, units="days")) %>%   
  group_by(record_id) %>% 
  slice(which.min(rest_read_gap_abs)) %>%
  select(-mrrc_id,-mri_date,-age_mri) %>% 
  ungroup()

#Write out data
write.table(readassess_CloseToRest, file=paste("data_categories/reading_assessments/additional_readassess/readassess_CloseToRest_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
