#Finds the MRRC_ID of each participant's most recent usable resting state that has been processed
#with Anaticor. 

library(dplyr)

#Load sheet with usable data columns (must remove first line)
QC_ByTask <- read.csv("data_categories/mri_data/QC_ByTask.csv", header = TRUE) 
#Load in rest of data
lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")
mri_data <- read.delim(dir("data_categories/mri_data/",
                           full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")
rest_wAnaticor <- read.delim(dir("data_categories/mri_data/",
                           full.names=T, pattern="^rest_wAnaticor_20"),header=TRUE, sep="\t")

#Create data table
most_recent_rest_wAnaticor <- rest_wAnaticor %>%
  left_join(mri_data, by = "mrrc_id")  %>%      
  left_join(lookup_table, by = "record_id") %>%   
  left_join(QC_ByTask, by = c(c("mrrc_id" = "rest_id"),"Subj")) %>% 
  group_by(record_id) %>% 
  slice(which.max(mri_date)) %>% 
  select(record_id,mrrc_id) %>% 
  ungroup()

#Write out data table
write.table(most_recent_rest_wAnaticor, file=paste("data_categories/mri_data/additional_mri_data/most_recent/most_recent_rest_wAnaticor_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
