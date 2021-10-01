#Calculates the Hollingshead Index from the semi-manually coded NHLP occupations

#Semi-manually coded because the NHLP questionnaire includes a question self-categorizing the parent's
#occupation, and the categories align fairly well with the Hollingshead. The occupation was manually 
#coded only if the parent did not self-categorize but did include information about their occupation
#or there was a clear miscategorization 


library(dplyr)
library(ggpubr)

options(digits = 4) #globally set decimals to hundredths

#Read in data
MancodedDF <- read.delim("data_categories/questionnaire/additional_questionnaire/SES/Hollingshead/DFtoManCode_JRM.txt") %>% 
  select(record_id, par1_occ, par2_occ)
questionnaire <- read.delim(dir("data_categories/questionnaire/",
                                full.names=T, pattern="^questionnaire_20"),header=TRUE, sep="\t")


#Create spreadsheet to estimate Hollingshead index
Hollingshead <- MancodedDF %>% 
  left_join(questionnaire, by="record_id") %>% 
  select(record_id, pq1.factor, rela_to_child_other, pq7a_1, pq7b_1, pq7c_1, pq7d_1,
         pq7g_1, pq7g_2.factor, pq7gh_1, pq7gh_2.factor, pgm_1, pq7i_2.factor, pq7j_1, pq7j_2.factor, pq7k_1, pq7k_2.factor,
         pq8_1.factor, pq8_2.factor, pq8_3.factor, pq8_4.factor, pq8_5.factor, pq8_6.factor, pq8_7.factor,
         pq8_8.factor, pq8_9.factor, pq8_10.factor, pq10.factor, par1_occ, par2_occ) %>% 
  mutate(household = paste(pq8_1.factor,pq8_2.factor,pq8_3.factor,pq8_4.factor,pq8_5.factor,pq8_6.factor,pq8_7.factor,pq8_8.factor,pq8_9.factor,pq8_10.factor),
         par1_occ_score = par1_occ * 5, #An individual's occupational score is given a weight of 5
         par2_occ_score = par2_occ * 5,
         par1_edu_score = (case_when(pq1.factor=="Birth mother" ~ pq7a_1, #An individual's educational score is given a weight of 3
                                    pq1.factor=="Birth father" ~ pq7b_1,
                                    (pq1.factor=="Grandmother") & (pq7g_2.factor=="Yes") ~ pq7g_1, #Maternal grandmother
                                    (pq1.factor=="Grandmother") & (pq7i_2.factor=="Yes") ~ pgm_1, #Paternal grandmother
                                    (pq1.factor=="Grandfather") & (pq7gh_2.factor=="Yes") ~ pq7gh_1, #Maternal grandfather
                                    (pq1.factor=="Grandfather") & (pq7j_2.factor=="Yes") ~ pq7j_1))*3, #Paternal grandfather), 
         par2_edu_score = (case_when((pq1.factor=="Birth mother") & grepl("Father",household) ~ pq7b_1,
                                    (pq1.factor=="Birth mother") & grepl("Stepfather",household) ~ pq7d_1,
                                    (pq1.factor=="Birth father") & grepl("Mother",household) ~ pq7a_1,
                                    (pq1.factor=="Birth father") & grepl("Stepmother",household) ~ pq7c_1,
                                    (pq1.factor=="Grandmother") & (pq7g_2.factor=="Yes") & grepl("Grandfather",household) ~ pq7gh_1,
                                    (pq1.factor=="Grandmother") & (pq7i_2.factor=="Yes") & grepl("Grandfather",household) ~ pq7j_1,
                                    (pq1.factor=="Grandfather") & (pq7gh_2.factor=="Yes") & grepl("Grandmother",household) ~ pq7g_1,
                                    (pq1.factor=="Grandfather") & (pq7j_2.factor=="Yes") & grepl("Grandmother",household) ~ pgm_1))*3,
         Hollingshead_raw = case_when((pq1.factor=="Birth mother") & (grepl("Father",household)|grepl("Stepfather",household)) & #Birth mother resp. [married, complete]
                                    !is.na(par1_occ_score) & !is.na(par2_occ_score) & !is.na(par1_edu_score) & !is.na(par2_edu_score) ~
                                    (par1_occ_score+par2_occ_score)/2 + (par1_edu_score+par2_edu_score)/2,
                                  (pq1.factor=="Birth mother") & (!grepl("Father",household)|!grepl("Stepfather",household)) & #Birth mother resp. [single, complete]
                                    !is.na(par1_occ_score) & !is.na(par1_edu_score) ~
                                    par1_occ_score + par1_edu_score,
                                  (pq1.factor=="Birth father") & (grepl("Mother",household)|grepl("Stepmother",household)) & #Birth father resp. [married, complete]
                                    !is.na(par1_occ_score) & !is.na(par2_occ_score) & !is.na(par1_edu_score) & !is.na(par2_edu_score) ~
                                    (par1_occ_score+par2_occ_score)/2 + (par1_edu_score+par2_edu_score)/2,
                                  (pq1.factor=="Birth father") & (!grepl("Mother",household)|!grepl("Stepmother",household)) & #Birth father resp. [single, complete]
                                    !is.na(par1_occ_score) & !is.na(par1_edu_score) ~
                                    par1_occ_score + par1_edu_score,
                                  (pq1.factor=="Grandmother") & (grepl("Grandfather",household)) & #Grandmother resp. [married, complete]
                                    !is.na(par1_occ_score) & !is.na(par2_occ_score) & !is.na(par1_edu_score) & !is.na(par2_edu_score) ~
                                    (par1_occ_score+par2_occ_score)/2 + (par1_edu_score+par2_edu_score)/2,
                                  (pq1.factor=="Grandmother") & (!grepl("Grandfather",household)) & #Grandmother resp. [single, complete]
                                    !is.na(par1_occ_score) & !is.na(par1_edu_score) ~
                                    par1_occ_score + par1_edu_score,
                                  (pq1.factor=="Grandfather") & (grepl("Grandmother",household)) & #Grandfather resp. [married, complete]
                                    !is.na(par1_occ_score) & !is.na(par2_occ_score) & !is.na(par1_edu_score) & !is.na(par2_edu_score) ~
                                    (par1_occ_score+par2_occ_score)/2 + (par1_edu_score+par2_edu_score)/2,
                                  (pq1.factor=="Grandfather") & (!grepl("Grandmother",household)) & #Grandfather resp. [single, complete]
                                    !is.na(par1_occ_score) & !is.na(par1_edu_score) ~
                                    par1_occ_score + par1_edu_score))

#How much needs to be imputed? 
sum(is.na(Hollingshead$Hollingshead_raw)) / length(Hollingshead$Hollingshead_raw)

#Create imputations for cases with missing data
#If occupation is missing, the imputed score is the average occupational score of those
#with the same educational score. Vice versa if education is missing.
##Imputing par1_occ_score
Hollingshead$par1_occ_score_imp <- numeric(length = length(Hollingshead$par1_occ_score))
for (i in seq_along(Hollingshead$par1_occ_score)) {
  if (!is.na(Hollingshead$par1_occ_score[i])){
    Hollingshead$par1_occ_score_imp[i] <- Hollingshead$par1_occ_score[i]
  } else if (is.na(Hollingshead$par1_occ_score[i]) & is.na(Hollingshead$par1_edu_score[i])){
    Hollingshead$par1_occ_score_imp[i] <- NA
  } else {
    edu_num <- Hollingshead$par1_edu_score[i]
    occ_vector <- c(Hollingshead$par1_occ_score[Hollingshead$par1_edu_score==edu_num],
                    Hollingshead$par2_occ_score[Hollingshead$par2_edu_score==edu_num])
    Hollingshead$par1_occ_score_imp[i] <- mean(occ_vector, na.rm = TRUE)
  }
}

##Imputing par2_occ_score
Hollingshead$par2_occ_score_imp <- numeric(length = length(Hollingshead$par2_occ_score))
for (i in seq_along(Hollingshead$par2_occ_score)) {
  if (!is.na(Hollingshead$par2_occ_score[i])){
    Hollingshead$par2_occ_score_imp[i] <- Hollingshead$par2_occ_score[i]
  } else if (is.na(Hollingshead$par2_occ_score[i]) & is.na(Hollingshead$par2_edu_score[i])){
    Hollingshead$par2_occ_score_imp[i] <- NA
  } else {
    edu_num <- Hollingshead$par2_edu_score[i]
    occ_vector <- c(Hollingshead$par1_occ_score[Hollingshead$par1_edu_score==edu_num],
                    Hollingshead$par2_occ_score[Hollingshead$par2_edu_score==edu_num])
    Hollingshead$par2_occ_score_imp[i] <- mean(occ_vector, na.rm = TRUE)
  }
}

##Imputing par1_edu_score
Hollingshead$par1_edu_score_imp <- numeric(length = length(Hollingshead$par1_edu_score))
for (i in seq_along(Hollingshead$par1_edu_score)) {
  if (!is.na(Hollingshead$par1_edu_score[i])){
    Hollingshead$par1_edu_score_imp[i] <- Hollingshead$par1_edu_score[i]
  } else if (is.na(Hollingshead$par1_edu_score[i]) & is.na(Hollingshead$par1_occ_score[i])){
    Hollingshead$par1_edu_score_imp[i] <- NA
  } else {
    occ_num <- Hollingshead$par1_occ_score[i]
    edu_vector <- c(Hollingshead$par1_edu_score[Hollingshead$par1_occ_score==occ_num],
                    Hollingshead$par2_edu_score[Hollingshead$par2_occ_score==occ_num])
    Hollingshead$par1_edu_score_imp[i] <- mean(edu_vector, na.rm = TRUE)
  }
}

##Imputing par2_edu_score
Hollingshead$par2_edu_score_imp <- numeric(length = length(Hollingshead$par2_edu_score))
for (i in seq_along(Hollingshead$par2_edu_score)) {
  if (!is.na(Hollingshead$par2_edu_score[i])){
    Hollingshead$par2_edu_score_imp[i] <- Hollingshead$par2_edu_score[i]
  } else if (is.na(Hollingshead$par2_edu_score[i]) & is.na(Hollingshead$par2_occ_score[i])){
    Hollingshead$par2_edu_score_imp[i] <- NA
  } else {
    occ_num <- Hollingshead$par2_occ_score[i]
    edu_vector <- c(Hollingshead$par1_edu_score[Hollingshead$par1_occ_score==occ_num],
                    Hollingshead$par2_edu_score[Hollingshead$par2_occ_score==occ_num])
    Hollingshead$par2_edu_score_imp[i] <- mean(edu_vector, na.rm = TRUE)
  }
}

Hollingshead <- Hollingshead %>% 
  mutate(Hollingshead_imp = round(case_when((pq1.factor=="Birth mother") & (grepl("Father",household)|grepl("Stepfather",household)) & #Birth mother resp. [married, complete]
                                 !is.na(par1_occ_score_imp) & !is.na(par2_occ_score_imp) & !is.na(par1_edu_score_imp) & !is.na(par2_edu_score_imp) ~ 
                                 (par1_occ_score_imp+par2_occ_score_imp)/2 + (par1_edu_score_imp+par2_edu_score_imp)/2, 
                               (pq1.factor=="Birth mother") & (!grepl("Father",household)|!grepl("Stepfather",household)) & #Birth mother resp. [single, complete]
                                 !is.na(par1_occ_score_imp) & !is.na(par1_edu_score_imp) ~ 
                                 par1_occ_score_imp + par1_edu_score_imp,
                               (pq1.factor=="Birth father") & (grepl("Mother",household)|grepl("Stepmother",household)) & #Birth father resp. [married, complete]
                                 !is.na(par1_occ_score_imp) & !is.na(par2_occ_score_imp) & !is.na(par1_edu_score_imp) & !is.na(par2_edu_score_imp) ~ 
                                 (par1_occ_score_imp+par2_occ_score_imp)/2 + (par1_edu_score_imp+par2_edu_score_imp)/2, 
                               (pq1.factor=="Birth father") & (!grepl("Mother",household)|!grepl("Stepmother",household)) & #Birth father resp. [single, complete]
                                 !is.na(par1_occ_score_imp) & !is.na(par1_edu_score_imp) ~ 
                                 par1_occ_score_imp + par1_edu_score_imp,
                               (pq1.factor=="Grandmother") & (grepl("Grandfather",household)) & #Grandmother resp. [married, complete]
                                 !is.na(par1_occ_score_imp) & !is.na(par2_occ_score_imp) & !is.na(par1_edu_score_imp) & !is.na(par2_edu_score_imp) ~ 
                                 (par1_occ_score_imp+par2_occ_score_imp)/2 + (par1_edu_score_imp+par2_edu_score_imp)/2, 
                               (pq1.factor=="Grandmother") & (!grepl("Grandfather",household)) & #Grandmother resp. [single, complete]
                                 !is.na(par1_occ_score_imp) & !is.na(par1_edu_score_imp) ~ 
                                 par1_occ_score_imp + par1_edu_score_imp,
                               (pq1.factor=="Grandfather") & (grepl("Grandmother",household)) & #Grandfather resp. [married, complete]
                                 !is.na(par1_occ_score_imp) & !is.na(par2_occ_score_imp) & !is.na(par1_edu_score_imp) & !is.na(par2_edu_score_imp) ~ 
                                 (par1_occ_score_imp+par2_occ_score_imp)/2 + (par1_edu_score_imp+par2_edu_score_imp)/2, 
                               (pq1.factor=="Grandfather") & (!grepl("Grandmother",household)) & #Grandfather resp. [single, complete]
                                 !is.na(par1_occ_score_imp) & !is.na(par1_edu_score_imp) ~ 
                                 par1_occ_score_imp + par1_edu_score_imp),2),
         avg_par_edu = case_when(!is.na(par1_edu_score) & !is.na(par2_edu_score) ~ (((rowSums(Hollingshead[,c("par1_edu_score","par2_edu_score")], na.rm=TRUE))/2)/3), 
                                 !is.na(par1_edu_score) & is.na(par2_edu_score) ~ par1_edu_score/3,
                                 is.na(par1_edu_score) & !is.na(par2_edu_score) ~ par2_edu_score/3))  %>%  
  select(record_id, Hollingshead_raw, Hollingshead_imp)

#How much is missing after imputation? 
sum(is.na(Hollingshead$Hollingshead_imp)) / length(Hollingshead$Hollingshead_imp)

#Write out dataframe
write.table(Hollingshead, file=paste("data_categories/questionnaire/additional_questionnaire/SES/Hollingshead/Hollingshead_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)




#Plot both parents education level with their occupational level (to check imputation looks reasonable)
ggscatter(Hollingshead,x = "par1_edu_score_imp",y = "par1_occ_score_imp",
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Parent 1 edu imp", ylab = "Parent 1 occ imp")

ggscatter(Hollingshead,x = "par1_edu_score",y = "par1_occ_score",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Parent 1 edu raw", ylab = "Parent 1 occ raw")

ggscatter(Hollingshead,x = "par2_edu_score_imp",y = "par2_occ_score_imp",
                          add = "reg.line", conf.int = TRUE,
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Parent 2 edu imp", ylab = "Parent 2 occ imp")

ggscatter(Hollingshead,x = "par2_edu_score",y = "par2_occ_score",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Parent 2 edu raw", ylab = "Parent 2 occ raw")
  
