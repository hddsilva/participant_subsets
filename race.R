#Creates a dataframe of each participant's race

#Race code:
#1 - African-American
#2 - American Indian / Alaska Native
#3 - Asian
#4 - Caucasian (White)
#5 - Pacific Islander
#6 - Hispanic / Latino
#7 - Mixed race
#8 - Other

library(dplyr)

#Load in data
questionnaire <- read.delim(dir("data_categories/questionnaire/",
                               full.names=T, pattern="^questionnaire_20"),header=TRUE, sep="\t")

#Create data table
race <- questionnaire %>%
  #includes checking for NAs in pq2 because many families didn't respond to pq2,
  #and then any race info after that would get coded as NA
  mutate(race = ifelse ((pq2 == 0|is.na(pq2)) & pq3___1==1 & pq3___2==0 & pq3___3==0 & pq3___4==0 & pq3___5==0 & pq3___6==0, "AA",
                 ifelse ((pq2 == 0|is.na(pq2)) & pq3___1==0 & pq3___2==1 & pq3___3==0 & pq3___4==0 & pq3___5==0 & pq3___6==0, "AI_AN",
                         ifelse ((pq2 == 0|is.na(pq2)) & pq3___1==0 & pq3___2==0 & pq3___3==1 & pq3___4==0 & pq3___5==0 & pq3___6==0, "As",
                                 ifelse ((pq2 == 0|is.na(pq2)) & pq3___1==0 & pq3___2==0 & pq3___3==0 & pq3___4==1 & pq3___5==0 & pq3___6==0, "Eur",
                                         ifelse ((pq2 == 0|is.na(pq2)) & pq3___1==0 & pq3___2==0 & pq3___3==0 & pq3___4==0 & pq3___5==1 & pq3___6==0, "PI",
                                                 ifelse (pq2 == 1 & pq3___1==0 & pq3___2==0 & pq3___3==0 & pq3___4==0 & pq3___5==0, "His_Lat",
                                                         #Because many hispanic families respond positively to both pq2 and pq3___6
                                                         ifelse ((sum(pq3___1,pq3___2,pq3___3,pq3___4,pq3___5,pq3___6)>=2 | sum(pq3___1,pq3___2,pq3___3,pq3___4,pq3___5,pq2)>=2), "Mix",
                                                                 ifelse ((pq2 == 0|is.na(pq2)) & pq3___1==0 & pq3___2==0 & pq3___3==0 & pq3___4==0 & pq3___5==0 & pq3___6==1, "Oth", "check")))))))),
         race = as.factor(race)) %>% 
  select(record_id,race)

#Write out data table
write.table(race, file=paste("data_categories/questionnaire/additional_questionnaire/race/race_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)



