#Creates a dataframe of the reading assessments closest to any usable fastloc

library(dplyr)

#Load in data
T1_anats <- read.delim(dir("data_categories/mri_data/additional_mri_data/long_mri/",
                                      full.names=T, pattern="^T1_anats_20"),header=TRUE, sep="\t")
reading_assessments <- read.delim(dir("data_categories/reading_assessments/",
                                      full.names=T, pattern="^reading_assessments_20"),header=TRUE, sep="\t")

#Create data table
readassess_CloseToAnat_T1 <- T1_anats %>%
  left_join(reading_assessments, by="record_id") %>% 
  mutate(anat_read_gap_abs = round(abs(difftime(T1_date,readtest_date, units="days"))),
         anat_read_gap = round(difftime(T1_date,readtest_date, units="days"))) %>%     
  group_by(record_id) %>% 
  slice(which.min(anat_read_gap_abs)) %>% 
  select(-T1_date, -T1_age) %>% 
  ungroup()

#Write out data table
write.table(readassess_CloseToAnat_T1, file=paste("data_categories/reading_assessments/additional_readassess/readassess_CloseToAnat_T1_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
