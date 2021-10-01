#Creates a list of all record_ids who have any MRI data

library(dplyr)

#Load data
mri_data <- read.delim(dir("data_categories/mri_data/",
                           full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")

#Create dataframe
all_mri_subjs <- mri_data %>% 
  select(record_id) %>% 
  distinct(record_id)

#Write out data table
write.table(all_mri_subjs, file=paste("data_categories/mri_data/additional_mri_data/all_mri_subjs_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
