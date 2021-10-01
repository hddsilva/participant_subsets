#Creates a table with each participant's record_id, subject id, and gender.
#Used to easily link the record_id to the subject id

library(dplyr)

#Rename family id into Subj
colnames(data)[colnames(data)=="family_id"] <- "Subj"
#Specify child dob as a date data type
data$childdob <- as.Date(as.character(data$childdob),"%Y-%m-%d")

#Create data table
lookup_table <- data %>%
  group_by(record_id) %>% 
  filter(!is.na(Subj)) %>% 
  select(record_id,
         Subj,
         childsex,
         childsex.factor,
         childdob) %>% 
  ungroup()

#Write out data table
write.table(lookup_table, file=paste("data_categories/lookup_table/lookup_table_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)


