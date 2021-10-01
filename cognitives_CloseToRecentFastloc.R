#Creates a dataframe of the cognitive assessment closest to the participant's most recent usable fastloc

library(dplyr)

#Load in data
most_recent_fastloc <- read.delim(dir("data_categories/mri_data/additional_mri_data/most_recent/",
                               full.names=T, pattern="^most_recent_fastloc_20"),header=TRUE, sep="\t")
mri_data <- read.delim(dir("data_categories/mri_data/",
                               full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")
cognitives <- read.delim(dir("data_categories/cognitives/",
                               full.names=T, pattern="^cognitives_20"),header=TRUE, sep="\t")

#Create data frame
cognitives_CloseToFastloc <- most_recent_fastloc %>%
  left_join(mri_data, by = c("record_id","mrrc_id")) %>% 
  left_join(cognitives, by="record_id") %>% 
  mutate(fastloc_cognitives_gap_abs = abs(difftime(mri_date,cogtest_date, units="days")),
         fastloc_cognitives_gap = difftime(mri_date,cogtest_date, units="days")) %>%   
  group_by(record_id) %>% 
  slice(which.min(fastloc_cognitives_gap_abs)) %>%
  select(-mrrc_id,-mri_date,-age_mri) %>% 
  ungroup()

#Write out data frame
write.table(cognitives_CloseToFastloc, file=paste("data_categories/cognitives/additional_cognitives/cognitives_CloseToFastloc_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
