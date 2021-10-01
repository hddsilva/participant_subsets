#Categorizes each participant's language history as either monolingual English (Mono_Engl),
#bilingual Spanish (Bi_Span), or neither. 

#Mono_Engls: Spanish is not spoken in the home and all of the caregivers are monolingual
#Bi_Span: Spanish is spoken at home and none of the caregivers speak a language other than
#English or Spanish

library(dplyr)

#Load in data
questionnaire <- read.delim(dir("data_categories/questionnaire/",
                                full.names=T, pattern="^questionnaire_20"),header=TRUE, sep="\t")

#For all caregiver bilingual designations:
#NoCare = is not a caregiver for the child
#Mono = Monolingual
#Spanish = Speaks Spanish (may or may not also speak English)
#AltLang = Speaks a foreign language other than Spanish

#Categorize the language for each of the child's caretakers 
caretaker_lang <- questionnaire %>%
  #select variables of interest
  select(record_id, pq7a_2, pq7a_5, pq7a_6, pq7a_6other,
         pq7b_2, pq7b_5, pq7b_6, pq7b_6other,
         pq7c_2, pq7c_5, pq7c_6, pq7c_6other,
         pq7d_2, pq7d_5, pq7d_6, pq7d_6other,
         pq7e_2, pq7e_5, pq7e_6, pq7e_6other,
         pq7f_2, pq7f_5, pq7f_6, pq7f_6other,
         pq7g_2, pq7g_5, pq7g_6, pq7g_6other,
         pq7gh_2, pq7gh_5, pq7gh_6, pq7h_6other,
         pq7i_2, pq7i_5, pq7i_6, pq7i_6other,
         pq7j_2, pq7j_5, pq7j_6, pq7j_6other,
         pq7k_2, pq7k_5, pq7k_6, pq7k_6other,
         pq7l_2, pq7l_5, pq7l_6, pq7l_6other,
         pq7m_2, pq7m_5, pq7m_6, pq7m_6other,
         bs_s1q1_1, bs_s1q1_2, bs_s1q1_3, bs_s1q1_6)  %>% 
  #create language designation for each family member
  mutate(pq7a_6other = tolower(pq7a_6other),
         #There are so many conditions listed because of inconsistencies in how the language questions were responded to.
         #ie. a Spanish-speaker might respond that they are bilingual and that they're Spanish/English bilingual. Or they 
         #might respond that they are not bilingual, that they are "other" in bilingual combination, and might respond 
         #"just Spanish" in the text field. 
         BioMotherLang = case_when(is.na(pq7a_5) & is.na(pq7a_6) & pq7a_6other=="" ~ "NA",
                                   is.na(pq7a_5) & is.na(pq7a_6) & is.na(pq7a_6other) ~ "NA",
                                   pq7a_2==0 | is.na(pq7a_2) ~ "NoCare",
                                   pq7a_2==1 & pq7a_5==0 & is.na(pq7a_6) & pq7a_6other=="" ~ "Mono",
                                   pq7a_2==1 & pq7a_5==0 & is.na(pq7a_6) & grepl(".*english.*",pq7a_6other) ~ "Mono",
                                   pq7a_2==1 & pq7a_5==1 & pq7a_6==1 ~ "Spanish",
                                   pq7a_2==1 & is.na(pq7a_5) & pq7a_6==1 ~ "Spanish",
                                   pq7a_2==1 & pq7a_5==0 & pq7a_6==1 ~ "Spanish",
                                   pq7a_2==1 & pq7a_5==0 & pq7a_6==2 & grepl(".*span.*",pq7a_6other) ~ "Spanish",
                                   pq7a_2==1 & is.na(pq7a_5) & pq7a_6==2 & grepl(".*span.*",pq7a_6other) ~ "Spanish",
                                   pq7a_2==1 & pq7a_5==0 & is.na(pq7a_6) & grepl(".*span.*",pq7a_6other) ~ "Spanish",
                                   pq7a_2==1 & is.na(pq7a_5) & is.na(pq7a_6) & grepl(".*span.*",pq7a_6other) ~ "Spanish",
                                   pq7a_2==1 & pq7a_5==1 & is.na(pq7a_6) & pq7a_6other=="" ~ "UnknownBiling",
                                   pq7a_2==1 & pq7a_5==1 & pq7a_6==2 ~ "AltLang"),
         pq7b_6other = tolower(pq7b_6other),
         BioFatherLang = case_when(is.na(pq7b_5) & is.na(pq7b_6) & pq7b_6other=="" ~ "NA",
                                   is.na(pq7b_5) & is.na(pq7b_6) & is.na(pq7b_6other) ~ "NA",
                                   pq7b_2==0 | is.na(pq7b_2) ~ "NoCare",
                                   pq7b_2==1 & pq7b_5==0 & is.na(pq7b_6) & pq7b_6other=="" ~ "Mono",
                                   pq7b_2==1 & pq7b_5==0 & is.na(pq7b_6) & grepl(".*english.*",pq7b_6other) ~ "Mono",
                                   pq7b_2==1 & pq7b_5==1 & pq7b_6==1 ~ "Spanish",
                                   pq7b_2==1 & is.na(pq7b_5) & pq7b_6==1 ~ "Spanish",
                                   pq7b_2==1 & pq7b_5==0 & pq7b_6==1 ~ "Spanish",
                                   pq7b_2==1 & pq7b_5==0 & pq7b_6==2 & grepl(".*span.*",pq7b_6other) ~ "Spanish",
                                   pq7b_2==1 & is.na(pq7b_5) & pq7b_6==2 & grepl(".*span.*",pq7b_6other) ~ "Spanish",
                                   pq7b_2==1 & pq7b_5==0 & is.na(pq7b_6) & grepl(".*span.*",pq7b_6other) ~ "Spanish",
                                   pq7b_2==1 & is.na(pq7b_5) & is.na(pq7b_6) & grepl(".*span.*",pq7b_6other) ~ "Spanish",
                                   pq7b_2==1 & pq7b_5==1 & is.na(pq7b_6) & pq7b_6other=="" ~ "UnknownBiling",
                                   pq7b_2==1 & pq7b_5==1 & pq7b_6==2 ~ "AltLang"),
         pq7c_6other = tolower(pq7c_6other),
         StepMotherLang = case_when(is.na(pq7c_5) & is.na(pq7c_6) & pq7c_6other=="" ~ "NA",
                                    is.na(pq7c_5) & is.na(pq7c_6) & is.na(pq7c_6other) ~ "NA",
                                   pq7c_2==0 | is.na(pq7c_2) ~ "NoCare",
                                   pq7c_2==1 & pq7c_5==0 & is.na(pq7c_6) & pq7c_6other=="" ~ "Mono",
                                   pq7c_2==1 & pq7c_5==0 & is.na(pq7c_6) & grepl(".*english.*",pq7c_6other) ~ "Mono",
                                   pq7c_2==1 & pq7c_5==1 & pq7c_6==1 ~ "Spanish",
                                   pq7c_2==1 & is.na(pq7c_5) & pq7c_6==1 ~ "Spanish",
                                   pq7c_2==1 & pq7c_5==0 & pq7c_6==1 ~ "Spanish",
                                   pq7c_2==1 & pq7c_5==0 & pq7c_6==2 & grepl(".*span.*",pq7c_6other) ~ "Spanish",
                                   pq7c_2==1 & is.na(pq7c_5) & pq7c_6==2 & grepl(".*span.*",pq7c_6other) ~ "Spanish",
                                   pq7c_2==1 & pq7c_5==0 & is.na(pq7c_6) & grepl(".*span.*",pq7c_6other) ~ "Spanish",
                                   pq7c_2==1 & is.na(pq7c_5) & is.na(pq7c_6) & grepl(".*span.*",pq7c_6other) ~ "Spanish",
                                   pq7c_2==1 & pq7c_5==1 & is.na(pq7c_6) & pq7c_6other=="" ~ "UnknownBiling",
                                   pq7c_2==1 & pq7c_5==1 & pq7c_6==2 ~ "AltLang"),
         pq7d_6other = tolower(pq7d_6other),
         StepFatherLang = case_when(is.na(pq7d_5) & is.na(pq7d_6) & pq7d_6other=="" ~ "NA",
                                    is.na(pq7d_5) & is.na(pq7d_6) & is.na(pq7d_6other) ~ "NA",
                                   pq7d_2==0 | is.na(pq7d_2) ~ "NoCare",
                                   pq7d_2==1 & pq7d_5==0 & is.na(pq7d_6) & pq7d_6other=="" ~ "Mono",
                                   pq7d_2==1 & pq7d_5==0 & is.na(pq7d_6) & grepl(".*english.*",pq7d_6other) ~ "Mono",
                                   pq7d_2==1 & pq7d_5==1 & pq7d_6==1 ~ "Spanish",
                                   pq7d_2==1 & is.na(pq7d_5) & pq7d_6==1 ~ "Spanish",
                                   pq7d_2==1 & pq7d_5==0 & pq7d_6==1 ~ "Spanish",
                                   pq7d_2==1 & pq7d_5==0 & pq7d_6==2 & grepl(".*span.*",pq7d_6other) ~ "Spanish",
                                   pq7d_2==1 & is.na(pq7d_5) & pq7d_6==2 & grepl(".*span.*",pq7d_6other) ~ "Spanish",
                                   pq7d_2==1 & pq7d_5==0 & is.na(pq7d_6) & grepl(".*span.*",pq7d_6other) ~ "Spanish",
                                   pq7d_2==1 & is.na(pq7d_5) & is.na(pq7d_6) & grepl(".*span.*",pq7d_6other) ~ "Spanish",
                                   pq7d_2==1 & pq7d_5==1 & is.na(pq7d_6) & pq7d_6other=="" ~ "UnknownBiling",
                                   pq7d_2==1 & pq7d_5==1 & pq7d_6==2 ~ "AltLang"),
         pq7e_6other = tolower(pq7e_6other),
         AdoptMotherLang = case_when(is.na(pq7e_5) & is.na(pq7e_6) & pq7e_6other=="" ~ "NA",
                                     is.na(pq7e_5) & is.na(pq7e_6) & is.na(pq7e_6other) ~ "NA",
                                   pq7e_2==0 | is.na(pq7e_2) ~ "NoCare",
                                   pq7e_2==1 & pq7e_5==0 & is.na(pq7e_6) & pq7e_6other=="" ~ "Mono",
                                   pq7e_2==1 & pq7e_5==0 & is.na(pq7e_6) & grepl(".*english.*",pq7e_6other) ~ "Mono",
                                   pq7e_2==1 & pq7e_5==1 & pq7e_6==1 ~ "Spanish",
                                   pq7e_2==1 & is.na(pq7e_5) & pq7e_6==1 ~ "Spanish",
                                   pq7e_2==1 & pq7e_5==0 & pq7e_6==1 ~ "Spanish",
                                   pq7e_2==1 & pq7e_5==0 & pq7e_6==2 & grepl(".*span.*",pq7e_6other) ~ "Spanish",
                                   pq7e_2==1 & is.na(pq7e_5) & pq7e_6==2 & grepl(".*span.*",pq7e_6other) ~ "Spanish",
                                   pq7e_2==1 & pq7e_5==0 & is.na(pq7e_6) & grepl(".*span.*",pq7e_6other) ~ "Spanish",
                                   pq7e_2==1 & is.na(pq7e_5) & is.na(pq7e_6) & grepl(".*span.*",pq7e_6other) ~ "Spanish",
                                   pq7e_2==1 & pq7e_5==1 & is.na(pq7e_6) & pq7e_6other=="" ~ "UnknownBiling",
                                   pq7e_2==1 & pq7e_5==1 & pq7e_6==2 ~ "AltLang"),
         pq7f_6other = tolower(pq7f_6other),
         AdoptFatherLang = case_when(is.na(pq7f_5) & is.na(pq7f_6) & pq7f_6other=="" ~ "NA",
                                     is.na(pq7f_5) & is.na(pq7f_6) & is.na(pq7f_6other) ~ "NA",
                                     pq7f_2==0 | is.na(pq7f_2) ~ "NoCare",
                                     pq7f_2==1 & pq7f_5==0 & is.na(pq7f_6) & pq7f_6other=="" ~ "Mono",
                                     pq7f_2==1 & pq7f_5==0 & is.na(pq7f_6) & grepl(".*english.*",pq7f_6other) ~ "Mono",
                                     pq7f_2==1 & pq7f_5==1 & pq7f_6==1 ~ "Spanish",
                                     pq7f_2==1 & is.na(pq7f_5) & pq7f_6==1 ~ "Spanish",
                                     pq7f_2==1 & pq7f_5==0 & pq7f_6==1 ~ "Spanish",
                                     pq7f_2==1 & pq7f_5==0 & pq7f_6==2 & grepl(".*span.*",pq7f_6other) ~ "Spanish",
                                     pq7f_2==1 & is.na(pq7f_5) & pq7f_6==2 & grepl(".*span.*",pq7f_6other) ~ "Spanish",
                                     pq7f_2==1 & pq7f_5==0 & is.na(pq7f_6) & grepl(".*span.*",pq7f_6other) ~ "Spanish",
                                     pq7f_2==1 & is.na(pq7f_5) & is.na(pq7f_6) & grepl(".*span.*",pq7f_6other) ~ "Spanish",
                                     pq7f_2==1 & pq7f_5==1 & is.na(pq7f_6) & pq7f_6other=="" ~ "UnknownBiling",
                                     pq7f_2==1 & pq7f_5==1 & pq7f_6==2 ~ "AltLang"),
         pq7g_6other = tolower(pq7g_6other),
         MatGrandmaLang = case_when(is.na(pq7g_5) & is.na(pq7g_6) & pq7g_6other=="" ~ "NA",
                                     is.na(pq7g_5) & is.na(pq7g_6) & is.na(pq7g_6other) ~ "NA",
                                     pq7g_2==0 | is.na(pq7g_2) ~ "NoCare",
                                     pq7g_2==1 & pq7g_5==0 & is.na(pq7g_6) & pq7g_6other=="" ~ "Mono",
                                     pq7g_2==1 & pq7g_5==0 & is.na(pq7g_6) & grepl(".*english.*",pq7g_6other) ~ "Mono",
                                     pq7g_2==1 & pq7g_5==1 & pq7g_6==1 ~ "Spanish",
                                     pq7g_2==1 & is.na(pq7g_5) & pq7g_6==1 ~ "Spanish",
                                     pq7g_2==1 & pq7g_5==0 & pq7g_6==1 ~ "Spanish",
                                     pq7g_2==1 & pq7g_5==0 & pq7g_6==2 & grepl(".*span.*",pq7g_6other) ~ "Spanish",
                                     pq7g_2==1 & is.na(pq7g_5) & pq7g_6==2 & grepl(".*span.*",pq7g_6other) ~ "Spanish",
                                     pq7g_2==1 & pq7g_5==0 & is.na(pq7g_6) & grepl(".*span.*",pq7g_6other) ~ "Spanish",
                                     pq7g_2==1 & is.na(pq7g_5) & is.na(pq7g_6) & grepl(".*span.*",pq7g_6other) ~ "Spanish",
                                     pq7g_2==1 & pq7g_5==1 & is.na(pq7g_6) & pq7g_6other=="" ~ "UnknownBiling",
                                     pq7g_2==1 & pq7g_5==1 & pq7g_6==2 ~ "AltLang"),
         pq7h_6other = tolower(pq7h_6other),
         MatGrandpaLang = case_when(is.na(pq7gh_5) & is.na(pq7gh_6) & pq7h_6other=="" ~ "NA",
                                     is.na(pq7gh_5) & is.na(pq7gh_6) & is.na(pq7h_6other) ~ "NA",
                                     pq7gh_2==0 | is.na(pq7gh_2) ~ "NoCare",
                                     pq7gh_2==1 & pq7gh_5==0 & is.na(pq7gh_6) & pq7h_6other=="" ~ "Mono",
                                     pq7gh_2==1 & pq7gh_5==0 & is.na(pq7gh_6) & grepl(".*english.*",pq7h_6other) ~ "Mono",
                                     pq7gh_2==1 & pq7gh_5==1 & pq7gh_6==1 ~ "Spanish",
                                     pq7gh_2==1 & is.na(pq7gh_5) & pq7gh_6==1 ~ "Spanish",
                                     pq7gh_2==1 & pq7gh_5==0 & pq7gh_6==1 ~ "Spanish",
                                     pq7gh_2==1 & pq7gh_5==0 & pq7gh_6==2 & grepl(".*span.*",pq7h_6other) ~ "Spanish",
                                     pq7gh_2==1 & is.na(pq7gh_5) & pq7gh_6==2 & grepl(".*span.*",pq7h_6other) ~ "Spanish",
                                     pq7gh_2==1 & pq7gh_5==0 & is.na(pq7gh_6) & grepl(".*span.*",pq7h_6other) ~ "Spanish",
                                     pq7gh_2==1 & is.na(pq7gh_5) & is.na(pq7gh_6) & grepl(".*span.*",pq7h_6other) ~ "Spanish",
                                     pq7gh_2==1 & pq7gh_5==1 & is.na(pq7gh_6) & pq7h_6other=="" ~ "UnknownBiling",
                                     pq7gh_2==1 & pq7gh_5==1 & pq7gh_6==2 ~ "AltLang"),
         pq7i_6other = tolower(pq7i_6other),
         PatGrandmaLang = case_when(is.na(pq7i_5) & is.na(pq7i_6) & pq7i_6other=="" ~ "NA",
                                    is.na(pq7i_5) & is.na(pq7i_6) & is.na(pq7i_6other) ~ "NA",
                                    pq7i_2==0 | is.na(pq7i_2) ~ "NoCare",
                                    pq7i_2==1 & pq7i_5==0 & is.na(pq7i_6) & pq7i_6other=="" ~ "Mono",
                                    pq7i_2==1 & pq7i_5==0 & is.na(pq7i_6) & grepl(".*english.*",pq7i_6other) ~ "Mono",
                                    pq7i_2==1 & pq7i_5==1 & pq7i_6==1 ~ "Spanish",
                                    pq7i_2==1 & is.na(pq7i_5) & pq7i_6==1 ~ "Spanish",
                                    pq7i_2==1 & pq7i_5==0 & pq7i_6==1 ~ "Spanish",
                                    pq7i_2==1 & pq7i_5==0 & pq7i_6==2 & grepl(".*span.*",pq7i_6other) ~ "Spanish",
                                    pq7i_2==1 & is.na(pq7i_5) & pq7i_6==2 & grepl(".*span.*",pq7i_6other) ~ "Spanish",
                                    pq7i_2==1 & pq7i_5==0 & is.na(pq7i_6) & grepl(".*span.*",pq7i_6other) ~ "Spanish",
                                    pq7i_2==1 & is.na(pq7i_5) & is.na(pq7i_6) & grepl(".*span.*",pq7i_6other) ~ "Spanish",
                                    pq7i_2==1 & pq7i_5==1 & is.na(pq7i_6) & pq7i_6other=="" ~ "UnknownBiling",
                                    pq7i_2==1 & pq7i_5==1 & pq7i_6==2 ~ "AltLang"),
         pq7j_6other = tolower(pq7j_6other),
         PatGrandpaLang = case_when(is.na(pq7j_5) & is.na(pq7j_6) & pq7j_6other=="" ~ "NA",
                                    is.na(pq7j_5) & is.na(pq7j_6) & is.na(pq7j_6other) ~ "NA",
                                    pq7j_2==0 | is.na(pq7j_2) ~ "NoCare",
                                    pq7j_2==1 & pq7j_5==0 & is.na(pq7j_6) & pq7j_6other=="" ~ "Mono",
                                    pq7j_2==1 & pq7j_5==0 & is.na(pq7j_6) & grepl(".*english.*",pq7j_6other) ~ "Mono",
                                    pq7j_2==1 & pq7j_5==1 & pq7j_6==1 ~ "Spanish",
                                    pq7j_2==1 & is.na(pq7j_5) & pq7j_6==1 ~ "Spanish",
                                    pq7j_2==1 & pq7j_5==0 & pq7j_6==1 ~ "Spanish",
                                    pq7j_2==1 & pq7j_5==0 & pq7j_6==2 & grepl(".*span.*",pq7j_6other) ~ "Spanish",
                                    pq7j_2==1 & is.na(pq7j_5) & pq7j_6==2 & grepl(".*span.*",pq7j_6other) ~ "Spanish",
                                    pq7j_2==1 & pq7j_5==0 & is.na(pq7j_6) & grepl(".*span.*",pq7j_6other) ~ "Spanish",
                                    pq7j_2==1 & is.na(pq7j_5) & is.na(pq7j_6) & grepl(".*span.*",pq7j_6other) ~ "Spanish",
                                    pq7j_2==1 & pq7j_5==1 & is.na(pq7j_6) & pq7j_6other=="" ~ "UnknownBiling",
                                    pq7j_2==1 & pq7j_5==1 & pq7j_6==2 ~ "AltLang"),
         pq7k_6other = tolower(pq7k_6other),
         FostMotherLang = case_when(is.na(pq7k_5) & is.na(pq7k_6) & pq7k_6other=="" ~ "NA",
                                    is.na(pq7k_5) & is.na(pq7k_6) & is.na(pq7k_6other) ~ "NA",
                                    pq7k_2==0 | is.na(pq7k_2) ~ "NoCare",
                                    pq7k_2==1 & pq7k_5==0 & is.na(pq7k_6) & pq7k_6other=="" ~ "Mono",
                                    pq7k_2==1 & pq7k_5==0 & is.na(pq7k_6) & grepl(".*english.*",pq7k_6other) ~ "Mono",
                                    pq7k_2==1 & pq7k_5==1 & pq7k_6==1 ~ "Spanish",
                                    pq7k_2==1 & is.na(pq7k_5) & pq7k_6==1 ~ "Spanish",
                                    pq7k_2==1 & pq7k_5==0 & pq7k_6==1 ~ "Spanish",
                                    pq7k_2==1 & pq7k_5==0 & pq7k_6==2 & grepl(".*span.*",pq7k_6other) ~ "Spanish",
                                    pq7k_2==1 & is.na(pq7k_5) & pq7k_6==2 & grepl(".*span.*",pq7k_6other) ~ "Spanish",
                                    pq7k_2==1 & pq7k_5==0 & is.na(pq7k_6) & grepl(".*span.*",pq7k_6other) ~ "Spanish",
                                    pq7k_2==1 & is.na(pq7k_5) & is.na(pq7k_6) & grepl(".*span.*",pq7k_6other) ~ "Spanish",
                                    pq7k_2==1 & pq7k_5==1 & is.na(pq7k_6) & pq7k_6other=="" ~ "UnknownBiling",
                                    pq7k_2==1 & pq7k_5==1 & pq7k_6==2 ~ "AltLang"),
         pq7l_6other = tolower(pq7l_6other),
         FostFatherLang = case_when(is.na(pq7l_5) & is.na(pq7l_6) & pq7l_6other=="" ~ "NA",
                                    is.na(pq7l_5) & is.na(pq7l_6) & is.na(pq7l_6other) ~ "NA",
                                    pq7l_2==0 | is.na(pq7l_2) ~ "NoCare",
                                    pq7l_2==1 & pq7l_5==0 & is.na(pq7l_6) & pq7l_6other=="" ~ "Mono",
                                    pq7l_2==1 & pq7l_5==0 & is.na(pq7l_6) & grepl(".*english.*",pq7l_6other) ~ "Mono",
                                    pq7l_2==1 & pq7l_5==1 & pq7l_6==1 ~ "Spanish",
                                    pq7l_2==1 & is.na(pq7l_5) & pq7l_6==1 ~ "Spanish",
                                    pq7l_2==1 & pq7l_5==0 & pq7l_6==1 ~ "Spanish",
                                    pq7l_2==1 & pq7l_5==0 & pq7l_6==2 & grepl(".*span.*",pq7l_6other) ~ "Spanish",
                                    pq7l_2==1 & is.na(pq7l_5) & pq7l_6==2 & grepl(".*span.*",pq7l_6other) ~ "Spanish",
                                    pq7l_2==1 & pq7l_5==0 & is.na(pq7l_6) & grepl(".*span.*",pq7l_6other) ~ "Spanish",
                                    pq7l_2==1 & is.na(pq7l_5) & is.na(pq7l_6) & grepl(".*span.*",pq7l_6other) ~ "Spanish",
                                    pq7l_2==1 & pq7l_5==1 & is.na(pq7l_6) & pq7l_6other=="" ~ "UnknownBiling",
                                    pq7l_2==1 & pq7l_5==1 & pq7l_6==2 ~ "AltLang"),
         pq7m_6other = tolower(pq7m_6other),
         OtherCaretakerLang = case_when(is.na(pq7m_5) & is.na(pq7m_6) & pq7m_6other=="" ~ "NA",
                                    is.na(pq7m_5) & is.na(pq7m_6) & is.na(pq7m_6other) ~ "NA",
                                    pq7m_2==0 | is.na(pq7m_2) ~ "NoCare",
                                    pq7m_2==1 & pq7m_5==0 & is.na(pq7m_6) & pq7m_6other=="" ~ "Mono",
                                    pq7m_2==1 & pq7m_5==0 & is.na(pq7m_6) & grepl(".*english.*",pq7m_6other) ~ "Mono",
                                    pq7m_2==1 & pq7m_5==1 & pq7m_6==1 ~ "Spanish",
                                    pq7m_2==1 & is.na(pq7m_5) & pq7m_6==1 ~ "Spanish",
                                    pq7m_2==1 & pq7m_5==0 & pq7m_6==1 ~ "Spanish",
                                    pq7m_2==1 & pq7m_5==0 & pq7m_6==2 & grepl(".*span.*",pq7m_6other) ~ "Spanish",
                                    pq7m_2==1 & is.na(pq7m_5) & pq7m_6==2 & grepl(".*span.*",pq7m_6other) ~ "Spanish",
                                    pq7m_2==1 & pq7m_5==0 & is.na(pq7m_6) & grepl(".*span.*",pq7m_6other) ~ "Spanish",
                                    pq7m_2==1 & is.na(pq7m_5) & is.na(pq7m_6) & grepl(".*span.*",pq7m_6other) ~ "Spanish",
                                    pq7m_2==1 & pq7m_5==1 & is.na(pq7m_6) & pq7m_6other=="" ~ "UnknownBiling",
                                    pq7m_2==1 & pq7m_5==1 & pq7m_6==2 ~ "AltLang"),
         #Create one variable that characterizes the caregiver language
         caretaker_lang = case_when(BioMotherLang == "AltLang" | BioFatherLang == "AltLang" |
                                      StepMotherLang == "AltLang" | StepFatherLang == "AltLang" |
                                      AdoptMotherLang == "AltLang" | AdoptFatherLang == "AltLang" |
                                      MatGrandmaLang == "AltLang" | MatGrandpaLang == "AltLang" |
                                      PatGrandmaLang == "AltLang" | PatGrandpaLang == "AltLang" |
                                      FostMotherLang == "AltLang" | FostFatherLang == "AltLang" |
                                      OtherCaretakerLang == "AltLang" ~ "AltLang",
                                    BioMotherLang == "UnknownBiling" | BioFatherLang == "UnknownBiling" |
                                      StepMotherLang == "UnknownBiling" | StepFatherLang == "UnknownBiling" |
                                      AdoptMotherLang == "UnknownBiling" | AdoptFatherLang == "UnknownBiling" |
                                      MatGrandmaLang == "UnknownBiling" | MatGrandpaLang == "UnknownBiling" |
                                      PatGrandmaLang == "UnknownBiling" | PatGrandpaLang == "UnknownBiling" |
                                      FostMotherLang == "UnknownBiling" | FostFatherLang == "UnknownBiling" |
                                      OtherCaretakerLang == "UnknownBiling" ~ "UnknownBiling",
                                    BioMotherLang == "Spanish" | BioFatherLang == "Spanish" |
                                      StepMotherLang == "Spanish" | StepFatherLang == "Spanish" |
                                      AdoptMotherLang == "Spanish" | AdoptFatherLang == "Spanish" |
                                      MatGrandmaLang == "Spanish" | MatGrandpaLang == "Spanish" |
                                      PatGrandmaLang == "Spanish" | PatGrandpaLang == "Spanish" |
                                      FostMotherLang == "Spanish" | FostFatherLang == "Spanish" |
                                      OtherCaretakerLang == "Spanish" ~ "Spanish",
                                    BioMotherLang == "Mono" | BioFatherLang == "Mono" |
                                      StepMotherLang == "Mono" | StepFatherLang == "Mono" |
                                      AdoptMotherLang == "Mono" | AdoptFatherLang == "Mono" |
                                      MatGrandmaLang == "Mono" | MatGrandpaLang == "Mono" |
                                      PatGrandmaLang == "Mono" | PatGrandpaLang == "Mono" |
                                      FostMotherLang == "Mono" | FostFatherLang == "Mono" |
                                      OtherCaretakerLang == "Mono" ~ "Mono"))  %>% 
  select(record_id,caretaker_lang)
#Make "NA"s in dataframe true NAs
caretaker_lang[caretaker_lang=="NA"]=NA

#Create dataframe
lang_cat <- questionnaire %>% 
  left_join(caretaker_lang, by = "record_id") %>% 
  mutate(bs_s1q1_1 = tolower(bs_s1q1_1), #convert to lowercase
         bs_s1q1_2 = tolower(bs_s1q1_2),
         bs_s1q1_1_uniform = case_when(grepl(".*span.*",bs_s1q1_1) ~ "Spanish", #catch "spanish" and "espanol"
                                       grepl(".*ngl.*",bs_s1q1_1) ~ "English", #catch "english and ingles"
                                       bs_s1q1_1 != "" ~ "OtherLang"),
         bs_s1q1_2_uniform = case_when(grepl(".*span.*",bs_s1q1_2) ~ "Spanish",
                                       grepl(".*ngl.*",bs_s1q1_2) ~ "English",
                                       bs_s1q1_2 != "" ~ "OtherLang"),
         Spanish_in_home = case_when(bs_s1q1_1_uniform=="Spanish" | bs_s1q1_2_uniform=="Spanish" ~ "yes"),
         lang_cat = case_when(caretaker_lang=="Mono" & is.na(Spanish_in_home) ~ "Mono_Engl", #some Spanish-speakers responded "monolingual", so must also use no Spanish_in_home
                              Spanish_in_home=="yes" & caretaker_lang!="AltLang" ~ "Bi_Span")) %>%  #must not be "AltLang" to make sure family speaking purely Spanish
  select(record_id,lang_cat)

#Write out data table
write.table(lang_cat, file=paste("data_categories/questionnaire/additional_questionnaire/language_categories/lang_cat_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
