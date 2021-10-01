#Finds the cognitive assessment closest to the average age of MRIs (8 yrs old)

library(dplyr)

#Load in data
lookup_table <- read.delim(dir("data_categories/lookup_table/",
                             full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")
cognitives <- read.delim(dir("data_categories/cognitives/",
                               full.names=T, pattern="^cognitives_20"),header=TRUE, sep="\t")

#Create data table
cognitives_AvgAgeMRI <- cognitives  %>% 
  left_join(lookup_table, by = "record_id") %>%  
  mutate(#3208 is 365*8.79, which is the age we're trying to match to
         MRIage_cog_gap_abs = abs(dob_cog_gap-3208),
         MRIage_cog_gap = dob_cog_gap-3208) %>% 
  group_by(record_id) %>% 
  slice(which.min(MRIage_cog_gap_abs)) %>%
  ungroup() %>% 
  select(-Subj, -childsex, -childsex.factor, -childdob)

#Write out data table
write.table(cognitives_AvgAgeMRI, file=paste("data_categories/cognitives/additional_cognitives/cognitives_AvgAgeMRI_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
