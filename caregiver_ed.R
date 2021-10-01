#Creates a caregiver_ed score, the average educational attainment of the child's caregivers


library(dplyr)

#Load in data
questionnaire <- read.delim(dir("data_categories/questionnaire/",
                                full.names=T, pattern="^questionnaire_20"),header=TRUE, sep="\t")

#Create dataframe
caregiver_ed <- questionnaire %>% 
  select(record_id, 
         pq7a_1, #Birth mother
         pq7a_2,
         pq7b_1, #Birth father
         pq7b_2,
         pq7c_1, #Step mother
         pq7c_2,
         pq7d_1, #Step father
         pq7d_2,
         pq7e_1, #Adoptive mother
         pq7e_2,
         pq7f_1, #Adoptive father
         pq7f_2,
         pq7g_1, #Maternal grandmother
         pq7g_2,
         pq7gh_1, #Maternal grandfather
         pq7gh_2,
         pgm_1, #Paternal grandmother
         pq7i_2,
         pq7j_1, #Paternal grandfather
         pq7j_2,
         pq7k_1, #Foster mother
         pq7k_2,
         pq7l_1, #Foster father
         pq7l_2,
         pq7m_1, #Birth mother
         pq7m_2)  %>%   
  mutate(num_caregivers = rowSums(caregiver_ed[, c("pq7a_2","pq7b_2","pq7c_2","pq7d_2",
                                                   "pq7e_2","pq7f_2","pq7g_2","pq7gh_2",
                                                   "pq7i_2","pq7j_2","pq7k_2","pq7l_2","pq7m_2")], na.rm = TRUE))
