#Creates a dataframe of the reading assessments closest to the participant's most recent usable fastloc

library(dplyr)

#Load in data
most_recent_fastloc <- read.delim(dir("data_categories/mri_data/additional_mri_data/most_recent/",
                                      full.names=T, pattern="^most_recent_fastloc_20"),header=TRUE, sep="\t")
mri_data <- read.delim(dir("data_categories/mri_data/",
                                      full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")
reading_assessments <- read.delim(dir("data_categories/reading_assessments/",
                                      full.names=T, pattern="^reading_assessments_20"),header=TRUE, sep="\t")

#Create data table
readassess_CloseToRecentFastloc <- most_recent_fastloc %>%
  left_join(mri_data, by = c("record_id","mrrc_id")) %>% 
  left_join(reading_assessments, by="record_id") %>% 
  mutate(fastloc_read_gap_abs = abs(difftime(mri_date,readtest_date, units="days")),
        fastloc_read_gap = difftime(mri_date,readtest_date, units="days")) %>%   
  group_by(record_id) %>% 
  slice(which.min(fastloc_read_gap_abs)) %>%
  select(-mrrc_id,-mri_date,-age_mri) %>% 
  ungroup()

#Write out data table
write.table(readassess_CloseToRecentFastloc, file=paste("data_categories/reading_assessments/additional_readassess/readassess_CloseToRecentFastloc_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
