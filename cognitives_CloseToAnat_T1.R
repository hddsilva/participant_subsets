#Creates a dataframe of the cognitive assessment closest to the participant's most recent usable fastloc

library(dplyr)

#Load in data
T1_anats <- read.delim(dir("data_categories/mri_data/additional_mri_data/long_mri/",
                           full.names=T, pattern="^T1_anats_20"),header=TRUE, sep="\t")
cognitives <- read.delim(dir("data_categories/cognitives/",
                               full.names=T, pattern="^cognitives_20"),header=TRUE, sep="\t")

#Create data table
cognitives_CloseToAnat_T1 <- T1_anats %>%
  left_join(cognitives, by="record_id")  %>% 
  mutate(anat_cognitives_gap_abs = abs(difftime(T1_date,cogtest_date, units="days")),
         anat_cognitives_gap = difftime(T1_date,cogtest_date, units="days")) %>%     
  group_by(record_id) %>% 
  slice(which.min(anat_cognitives_gap_abs)) %>% 
  select(-T1_date, -T1_age) %>% 
  ungroup()

#Write out data frame
write.table(cognitives_CloseToAnat_T1, file=paste("data_categories/cognitives/additional_cognitives/cognitives_CloseToAnat_T1_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
