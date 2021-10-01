#Finds the MRRC_ID of each participant's most recent usable fastloc. Must run lookup_table.R and mri_data.R first.

library(dplyr)

#Load sheet with usable data columns (must remove first line)
QC_ByTask <- read.csv("data_categories/mri_data/QC_ByTask.csv", header = TRUE) 
#Load in rest of data
lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")
mri_data <- read.delim(dir("data_categories/mri_data/",
                               full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")

#Remove maybe's from fastlocs in QC_ByTask
QC_ByTask$fastloc_id <- sub("MAYBE ", "", QC_ByTask$fastloc_id)

#Create data table
most_recent_fastloc <- mri_data %>% 
  left_join(lookup_table, by = "record_id") %>% 
  left_join(QC_ByTask, by = c("Subj","mrrc_id" = "fastloc_id")) %>% 
  filter(mrrc_id %in% QC_ByTask$fastloc_id) %>% 
  group_by(record_id) %>% 
  slice(which.max(mri_date))  %>% 
  select(record_id,mrrc_id) %>%
  ungroup()

#Write out data table
write.table(most_recent_fastloc, file=paste("data_categories/mri_data/additional_mri_data/most_recent/most_recent_fastloc_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
  
