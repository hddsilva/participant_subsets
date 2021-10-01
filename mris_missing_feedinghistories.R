library(dplyr)

#Load data
feeding_histories <- read.csv("data_categories/questionnaire/additional_questionnaire/feeding_histories/feeding_histories.csv", header = TRUE)
lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")
most_recent_rest_wAnaticor <- read.delim(dir("data_categories/mri_data/additional_mri_data/most_recent/",
                                             full.names=T, pattern="^most_recent_rest_wAnaticor_20"),header=TRUE, sep="\t")
most_recent_rest <- read.delim(dir("data_categories/mri_data/additional_mri_data/most_recent/",
                                             full.names=T, pattern="^most_recent_rest_20"),header=TRUE, sep="\t")

#Create dataframe
mris_missing_feedinghistories <- feeding_histories %>% 
  inner_join(lookup_table, by="Subj") %>% 
  inner_join(most_recent_rest, by="record_id") %>% 
  mutate(missing_info = case_when(is.na(breast_hl_mancoded) | is.na(breast_exc) | 
                                     is.na(form_hl_mancoded) | is.na(form_exc) ~ 1))


#Count missing
sum(mris_missing_feedinghistories$missing_info==1, na.rm = TRUE)

sum(mris_missing_feedinghistories$breast_hl_mancoded >= 6, na.rm = TRUE)
sum(mris_missing_feedinghistories$form_exc == 1, na.rm = TRUE)
