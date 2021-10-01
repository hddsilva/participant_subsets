#Creates a dataframe of MRI data pulled from RedCap

library(dplyr)

lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")

#Define mri_date as date type
data$mri_date <- as.Date(as.character(data$mri_date),"%F")
data$childdob <- as.Date(as.character(data$childdob),"%F")

#Create data table
mri_data <- data %>% 
  select(record_id, mrrc_id, mri_date, redcap_event_name) %>%  
  left_join(lookup_table, by = "record_id") %>% 
  filter(grepl("mri",redcap_event_name),
         !is.na(mri_date))   %>% 
  mutate(age_mri = abs(difftime(childdob, mri_date, units="days"))) %>% 
  select(record_id, mrrc_id, mri_date, age_mri)
  

#Write out data table
write.table(mri_data, file=paste("data_categories/mri_data/mri_data_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
