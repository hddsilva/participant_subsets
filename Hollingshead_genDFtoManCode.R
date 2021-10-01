#Creates a modified Hollingshead index, based off the occupation categorization in the parent questionnaire

library(dplyr)

#Load in data
questionnaire <- read.delim(dir("data_categories/questionnaire/",
                                full.names=T, pattern="^questionnaire_20"),header=TRUE, sep="\t")

DFtoManCode <- questionnaire %>% 
  select(record_id, pq11:pq18) %>% 
  mutate_at(.vars = c("pq11","pq15"), ~ case_when(.==1 ~ 7, #Ranked categories according to the Hollingshead
                                              .==2 ~ 5,
                                              .==3 ~ 2,
                                              .==4 ~ 6,
                                              .==5 ~ 4,
                                              .==6 ~ 3,
                                              .==7 ~ 1,
                                              .==8 ~ 9,
                                              .==9 ~ 8,
                                              .==10 ~ 6)) %>% 
  rename_at(.vars = c("pq11","pq15"), ~ paste0(.,"_Hrank")) %>% 
  mutate(par1_occ = pq11_Hrank,
         par2_occ = pq15_Hrank)


#Write out data table for manual coding
write.table(DFtoManCode, file=paste("data_categories/questionnaire/additional_questionnaire/SES/Hollingshead/DFtoManCode_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
