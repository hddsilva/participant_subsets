#Creates a dataframe with information related to the mother's pregnancy with the child

#birth_after_37 = child was born at or after 37 weeks gestation
#drugs_preg_noknow = mother took drugs (see drug list below) before she knew she was pregnant
#drugs_preg_wknow = mother took drugs (see drug list below) after she knew she was pregnant
#drugs_during_pregnancy = mother took drugs (see drug list below) at any point during her pregnancy

#drug list = amphetamines, barbiturates, crack, cocaine, heroin, methadone, and ecstasy
  #note that this does not include marijuana or alcohol use

library(dplyr)

#Load in data table
questionnaire <- read.delim(dir("data_categories/questionnaire/",
                                full.names=T, pattern="^questionnaire_20"),header=TRUE, sep="\t")

questionnaire$pq97a <- as.numeric(as.character(questionnaire$pq97a))
pq <- subset(questionnaire, select=pq1:pq111yes___14)
questionnaire$blank_quest <- apply(pq, 1, function(x) all(is.na(x)))


#Create data table
pregnancy <- questionnaire %>%
  mutate(birth_after_37 = case_when(pq97a >= 37 ~ "yes",
                                     pq97a < 37 ~ "no",
                                     (is.na(pq97a)) & (pq97==1) ~ "yes",
                                     (is.na(pq97a)) & (pq97==0) ~ "no"),
         drugs_preg_noknow = case_when(
           (blank_quest==TRUE) ~ "NA",
           (is.na(pq96a_1))&(is.na(pq96b_1))&(is.na(pq96c_1))&(is.na(pq96d_1))&(is.na(pq96e_1))&(is.na(pq96f_1))&(is.na(pq96g_1)) ~ "no",
           (pq96a_1==1)|(pq96b_1==1)|(pq96c_1==1)|(pq96d_1==1)|(pq96e_1==1)|(pq96f_1==1)|(pq96g_1==1) ~ "yes",
           (pq96a_1==0)&(pq96b_1==0)&(pq96c_1==0)&(pq96d_1==0)&(pq96e_1==0)&(pq96f_1==0)&(pq96g_1==0) ~ "no",
           (is.na(pq96a_1))|(is.na(pq96b_1))|(is.na(pq96c_1))|(is.na(pq96d_1))|(is.na(pq96e_1))|(is.na(pq96f_1))|(is.na(pq96g_1)) ~ "no"),
         drugs_preg_wknow = case_when(
           (blank_quest==TRUE) ~ "NA",
           (is.na(pq96a_2))&(is.na(pq96b_2))&(is.na(pq96c_2))&(is.na(pq96d_2))&(is.na(pq96e_2))&(is.na(pq96f_2))&(is.na(pq96g_2)) ~ "no",
           (pq96a_2==1)|(pq96b_2==1)|(pq96c_2==1)|(pq96d_2==1)|(pq96e_2==1)|(pq96f_2==1)|(pq96g_2==1) ~ "yes",
           (pq96a_2==0)&(pq96b_2==0)&(pq96c_2==0)&(pq96d_2==0)&(pq96e_2==0)&(pq96f_2==0)&(pq96g_2==0) ~ "no",
           (is.na(pq96a_2))|(is.na(pq96b_2))|(is.na(pq96c_2))|(is.na(pq96d_2))|(is.na(pq96e_2))|(is.na(pq96f_2))|(is.na(pq96g_2)) ~ "no"),
         drugs_during_pregnancy = case_when(
           (drugs_preg_noknow=="yes")|(drugs_preg_wknow=="yes") ~ "yes",
           (drugs_preg_noknow=="no")&(drugs_preg_wknow=="no") ~ "no")) %>% 
  select(record_id, birth_after_37, drugs_during_pregnancy) 
    
                                   
#Write out data table       
write.table(pregnancy, file=paste("data_categories/questionnaire/additional_questionnaire/pregnancy/pregnancy_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
