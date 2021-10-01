#Creates a dataframe of the ppvt assessments closest to the participant's most recent usable fastloc

library(dplyr)

#Load in data
long_mri <- read.delim(dir("data_categories/mri_data/additional_mri_data/long_mri/",
                           full.names=T, pattern="^long_mri_20"),header=TRUE, sep="\t") 
mri_data <- read.delim(dir("data_categories/mri_data/",
                               full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")
ppvt <- read.delim(dir("data_categories/ppvt/",
                               full.names=T, pattern="^ppvt_20"),header=TRUE, sep="\t")

#Create data table
ppvt_CloseToFastloc_T1 <- long_mri %>%
  left_join(mri_data, by = "mrrc_id")  %>% 
  left_join(ppvt, by="record_id")  %>% 
  filter(mri_tp == "T1") %>% 
  mutate(fastloc_ppvt_gap_abs = abs(difftime(mri_date,ppvtdate, units="days")),
         fastloc_ppvt_gap = difftime(mri_date,ppvtdate, units="days")) %>%     
  group_by(record_id) %>% 
  slice(which.min(fastloc_ppvt_gap_abs)) %>%
  select(-mri_tp, -mrrc_id, -mri_date, -age_mri) %>%
  ungroup()

#Write out data table
write.table(ppvt_CloseToFastloc_T1, file=paste("data_categories/ppvt/additional_ppvt/ppvt_CloseToFastloc_T1_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
