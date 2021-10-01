#Creates a list of each participant's use of government assistance.

#Government assistance is defined as use of food stamps, WIC, or Medicaid/Husky (no fee)
#1=uses at least one of the above programs, 0=does not use any above programs

library(dplyr)

#Load in data table
questionnaire <- read.delim(dir("data_categories/questionnaire/",
                                full.names=T, pattern="^questionnaire_20"),header=TRUE, sep="\t")

#Create data table
gov_assist <- questionnaire %>%
  select(record_id,pq19c,pq19d,pq19e) %>% 
  mutate(gov_assist = ifelse((is.na(pq19c)&is.na(pq19d)&is.na(pq19e)),NA,
                             ifelse((pq19c==1|pq19d==1|pq19e ==1), 1, 0))) %>% 
  select(record_id,gov_assist)

#Write out data table       
write.table(gov_assist, file=paste("data_categories/questionnaire/additional_questionnaire/SES/gov_assist_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
