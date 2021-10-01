#Finds the PPVT closest to the participant's most recent usable resting state

library(dplyr)

#Load in data
most_recent_rest <- read.delim(dir("data_categories/mri_data/additional_mri_data/most_recent/",
                               full.names=T, pattern="^most_recent_rest_20"),header=TRUE, sep="\t")
mri_data <- read.delim(dir("data_categories/mri_data/",
                               full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")
ppvt <- read.delim(dir("data_categories/ppvt/",
                               full.names=T, pattern="^ppvt_20"),header=TRUE, sep="\t")

#Create data table
ppvt_CloseToRest <- most_recent_rest %>%
  left_join(mri_data, by = c("record_id","mrrc_id")) %>% 
  left_join(ppvt, by="record_id") %>% 
  mutate(rest_ppvt_gap_abs = abs(difftime(mri_date,ppvtdate, units="days")),
         rest_ppvt_gap = difftime(mri_date,ppvtdate, units="days")) %>%    
  group_by(record_id) %>% 
  slice(which.min(rest_ppvt_gap_abs)) %>%
  select(-mrrc_id,-mri_date,-age_mri) %>%
  ungroup()

#Write out data table
write.table(ppvt_CloseToRest, file=paste("data_categories/ppvt/additional_ppvt/ppvt_CloseToRest_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
