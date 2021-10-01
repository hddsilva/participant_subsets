#Create table to manually code feeding histories of everyone in the NHLP

library(dplyr)

#Load in data
lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")
questionnaire <- read.delim(dir("data_categories/questionnaire/",
                               full.names=T, pattern="^questionnaire_20"),header=TRUE, sep="\t")
feeding_histories <- read.csv("data_categories/questionnaire/additional_questionnaire/feeding_histories/feeding_histories.csv", header = TRUE)
old_mancode_feedhist <- read.delim("data_categories/questionnaire/additional_questionnaire/feeding_histories/gen_feedhist_mancode_table_old.txt", header = TRUE)

#Create dataframe
gen_feedhist_mancode_table_new <- lookup_table %>% 
  left_join(questionnaire, by = "record_id") %>% 
  left_join(feeding_histories, by = "Subj") %>% 
  arrange(Subj) %>% 
  select(Subj,pq102a,pq102ahl,pq102b,pq102bhl,breast_hl_mancoded,breast_exc,form_hl_mancoded,form_exc) %>% 
  rename(pq_breast = pq102a, pq_breast_hl = pq102ahl, pq_form = pq102b, pq_form_hl = pq102bhl)

#Write out dataframe
write.table(gen_feedhist_mancode_table_new, "data_categories/questionnaire/additional_questionnaire/feeding_histories/gen_feedhist_mancode_table.txt", sep="\t", row.names = FALSE)
