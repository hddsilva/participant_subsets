#Used to find the resting state closest to the fastloc being used for Mellissa's project

library(dplyr)

#Load data
#IFG_Mean is there so I can use the same list of kids used for Mellissa's analysis
IFG_Mean <- read.delim(dir("projects/DysCortexVariability/ROI_variability_mean/",
                           full.names=T, pattern="^IFG_Mean"),header=FALSE, sep=" ")
mri_data <- read.delim(dir("data_categories/mri_data/",
                           full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")
Resting_Driver_Summary <- read.delim(dir("data_categories/mri_data/additional_mri_data/driver_summaries",
                                         full.names=T, pattern="^Resting_Driver_Summary_20"),header=TRUE, sep="\t")
lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")
QC_ByTask <- read.csv("data_categories/mri_data/QC_ByTask.csv", header = TRUE)

#Remove maybe's from fastlocs in QC_ByTask
QC_ByTask$fastloc_id <- sub("MAYBE ", "", QC_ByTask$fastloc_id)
QC_ByTask$rest_id <- sub("MAYBE ", "", QC_ByTask$rest_id)
QC_ByTask <- QC_ByTask %>% filter(!is.na(Subj)) %>% select(-starts_with("X"))

#List dates of fastlocs and resting states so they can be compared later
fastloc_date <- mri_data %>% filter(mrrc_id %in% QC_ByTask$fastloc_id) %>% mutate(fastloc_date=as.Date(mri_date)) %>% select(mrrc_id, fastloc_date)
rest_date <- mri_data %>% filter(mrrc_id %in% QC_ByTask$rest_id) %>% mutate(rest_date=as.Date(mri_date)) %>% select(mrrc_id, rest_date)

#Find the earliest resting state. In the dataframe below, if there is no usable resting state
#at the same timepoint as the fastloc, it will take the earliest resting state.
df <- mri_data %>% 
  left_join(lookup_table, by = "record_id") %>% 
  left_join(QC_ByTask, by = c("Subj","mrrc_id" = "rest_id")) %>% 
  filter(mrrc_id %in% QC_ByTask$rest_id)
T1_rest <-  df %>% 
  group_by(record_id) %>% 
  slice(which.min(mri_date))  %>% 
  select(record_id,mrrc_id,mri_date,age_mri) %>%
  ungroup() %>% 
  rename(T1_rest = mrrc_id, T1_date_rest = mri_date, T1_age_rest = age_mri)

#Create the dataframe
Rest_CloseToFastloc <- QC_ByTask %>% 
  left_join(lookup_table, by = "Subj")  %>% 
  left_join(fastloc_date, by = c("fastloc_id" = "mrrc_id")) %>%
  left_join(rest_date, by = c("rest_id" = "mrrc_id")) %>% 
  left_join(T1_rest, by = "record_id")  %>%
  filter(fastloc_id %in% IFG_Mean$V1) %>%  
  mutate(equiv_rest = case_when(!is.na(rest_id) ~ rest_id,
                                is.na(rest_id) ~ T1_rest)) %>% 
  select(record_id, Subj, equiv_rest)
  

#Write out data table
write.table(Rest_CloseToFastloc, file=paste("data_categories/mri_data/additional_mri_data/match_modalities/Rest_CloseToFastloc_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)

#Write out mrrc id list
EquivRest_mrrcIDs <- select(Rest_CloseToFastloc, equiv_rest) %>% filter(!is.na(equiv_rest)) %>%  arrange(equiv_rest)
write.table(EquivRest_mrrcIDs, file=paste("projects/DysCortexVariability/EquivRest_mrrcIDs_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE,
                         col.names = FALSE, quote = FALSE)
