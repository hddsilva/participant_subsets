#Finds the MRRC_ID of each participant's earliest usable anatomical. Must run lookup_table.R and mri_data.R first.

library(dplyr)

#Load sheet with usable data columns (must remove first line)
QC_ByTask <- read.csv("data_categories/mri_data/QC_ByTask.csv", header = TRUE) 
#Load in rest of data
lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")
mri_data <- read.delim(dir("data_categories/mri_data/",
                               full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")

#Remove maybe's from anats in QC_ByTask
QC_ByTask$anat_id <- sub("MAYBE ", "", QC_ByTask$anat_id)

#Create data table
earliest_anat <- mri_data %>% 
  left_join(lookup_table, by = "record_id") %>% 
  left_join(QC_ByTask, by = c("Subj","mrrc_id" = "anat_id")) %>%  
  filter(!is.na(mrrc_id) & mrrc_id != "",
         mrrc_id %in% QC_ByTask$anat_id) %>% 
  group_by(record_id) %>% 
  slice(which.min(mri_date))  %>% 
  select(record_id,mrrc_id) %>%
  ungroup()

#Next, find the subgroup of 20, spread representively across age?
#See MAGeT instructions for ideas

#Write out data table
write.table(earliest_anat, file=paste("data_categories/mri_data/additional_mri_data/earliest/earliest_anat_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)

#Write out mrrc_id list
EarliestAnat_mrrcIDs <- select(earliest_anat, mrrc_id) %>% arrange(mrrc_id)
write.table(EarliestAnat_mrrcIDs, file=paste("data_categories/mri_data/additional_mri_data/earliest/EarliestAnat_mrrcIDs_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE,
            col.names = FALSE, quote = FALSE)
  
