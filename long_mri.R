#Used to generate the list of IDs with usable T1 and T2 data
#Currently only written for resting states!!
#Hailey on 11/12/19

library(dplyr)
library(tidyr)

#Load sheet with usable data columns (must remove first line)
QC_ByTask <- read.csv("data_categories/mri_data/QC_ByTask.csv", header = TRUE, stringsAsFactors = FALSE) 
QC_ByTask <- select(QC_ByTask,Subj:dti_id) 
QC_ByTask[QC_ByTask=="NA"] <- NA
numSubjs <- sum(!is.na(QC_ByTask$Subj))
QC_ByTask <- QC_ByTask[1:numSubjs,]

#Load in rest of data
lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")
mri_data <- read.delim(dir("data_categories/mri_data/",
                           full.names=T, pattern="^mri_data_20"),header=TRUE, sep="\t")

#Remove maybe's from fastlocs in QC_ByTask
QC_ByTask$anat_id <- sub("MAYBE ", "", QC_ByTask$anat_id)

#Combine data frames
df <- mri_data %>% 
  left_join(lookup_table, by = "record_id")  %>%  
  left_join(QC_ByTask, by = c("Subj","mrrc_id" = "anat_id")) %>% 
  filter(mrrc_id %in% QC_ByTask$anat_id)

#Find first scan
T1 <-  df %>% 
  group_by(record_id) %>% 
  slice(which.min(mri_date))  %>% 
  select(record_id,mrrc_id,mri_date,age_mri) %>%
  ungroup() %>% 
  rename(T1 = mrrc_id, T1_date = mri_date, T1_age = age_mri)

#Find second scan & add with T1 info
T1_T2 <- T1 %>% 
  left_join(df, by = "record_id") %>% 
  mutate(T1T2_lag = difftime(mri_date, T1_date, units="days"))  %>% 
  filter(T1T2_lag != 0) %>% 
  group_by(record_id) %>% 
  slice(which.min(T1T2_lag))  %>% 
  select(record_id,T1,T1_date,T1_age,mrrc_id,mri_date,age_mri,T1T2_lag) %>%
  ungroup() %>% 
  rename(T2 = mrrc_id, T2_date = mri_date, T2_age = age_mri)

#Find third scan & add with T1_T2 info
T2_T3 <- T1_T2 %>% 
  left_join(df, by = "record_id")  %>% 
  filter(T1_date != mri_date) %>% #Take out T1s
  mutate(T2T3_lag = difftime(mri_date, T2_date, units="days")) %>% 
  filter(T2T3_lag != 0)  %>% 
  group_by(record_id) %>% 
  slice(which.min(T2T3_lag))  %>% 
  select(record_id,mrrc_id,mri_date,age_mri,T2T3_lag) %>%
  ungroup() %>% 
  rename(T3 = mrrc_id, T3_date = mri_date, T3_age = age_mri)

#Prep timepoints to be combined in tidy data set
T1 <- select(T1, T1) %>% mutate(mri_tp = "T1") %>% rename(mrrc_id = T1)
T2 <- select(T1_T2, T2) %>% mutate(mri_tp = "T2") %>% rename(mrrc_id = T2)
T3 <- select(T2_T3, T3) %>% mutate(mri_tp = "T3") %>% rename(mrrc_id = T3)

#Create final dataframe
long_mri <- bind_rows(T1,T2) %>% 
  bind_rows(T3) 

#Write out datatable
write.table(long_mri, file=paste("data_categories/mri_data/additional_mri_data/long_mri/long_mri_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)


#Create lag lookup table dataframe
long_mri_lag <- T1_T2 %>% 
  left_join(T2_T3, by = "record_id") %>% 
  select(record_id, T1T2_lag, T2T3_lag)
#Write out lag look up table 
write.table(long_mri_lag, file=paste("data_categories/mri_data/additional_mri_data/long_mri/long_mri_lag_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)

#Create mrrc_id list of T1s only
T1_mrrcIDs_anat <- T1 %>% select(mrrc_id) %>% arrange(mrrc_id) %>% filter(mrrc_id!="")
#Write out T1 mrrc_id list
write.table(T1_mrrcIDs_anat, file=paste("data_categories/mri_data/additional_mri_data/long_mri/T1_mrrcIDs_anat",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE,
            col.names = FALSE, quote = FALSE) 
#Create mrrc_id list of T2s only
T2_mrrcIDs_anat <- T2 %>% select(mrrc_id) %>% arrange(mrrc_id) %>% filter(mrrc_id!="")
#Write out T1 mrrc_id list
write.table(T2_mrrcIDs_anat, file=paste("data_categories/mri_data/additional_mri_data/long_mri/T2_mrrcIDs_anat",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE,
            col.names = FALSE, quote = FALSE) 
#Create mrrc_id list of T3s only
T3_mrrcIDs_anat <- T3 %>% select(mrrc_id) %>% arrange(mrrc_id) %>% filter(mrrc_id!="")
#Write out T1 mrrc_id list
write.table(T3_mrrcIDs_anat, file=paste("data_categories/mri_data/additional_mri_data/long_mri/T3_mrrcIDs_anat",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE,
            col.names = FALSE, quote = FALSE) 


#Create dataframe of all kids who have done an MRI and which years they've done - For Kate
df2 <- df %>%
  select(record_id, Subj, age_mri, mri_year) %>%
  mutate(age_mri = round(age_mri, 0))
#Write out df2
write.csv(df2, file=paste("data_categories/mri_data/additional_mri_data/long_mri/mri_subjs_by_year",Sys.Date(),".csv",sep=""), sep="\t", row.names = FALSE,
            col.names = FALSE, quote = FALSE) 

  
  

