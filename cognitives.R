#Creates a dataframe of all cognitive assessments

library(dplyr)

lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")

data$cogtest_date <- as.Date(as.character(data$cogtest_date),"%Y-%m-%d")

#Create cognitive assessment table
cognitives <- data %>%
  select(-childsex, -childsex.factor, -childdob) %>% 
  left_join(lookup_table, by = "record_id") %>%
  group_by(record_id) %>% 
  filter(grepl("cog",redcap_event_name)) %>%
  mutate(dob_cog_gap = abs(difftime(childdob,cogtest_date, units="days"))) %>% 
  select(record_id,
         cogtest_completed:cognitive_test_information_complete,
         wasi_completed:wasiii_complete,
         wisc_completed:ranras_complete,
         dob_cog_gap)

write.table(cognitives, file=paste("data_categories/cognitives/cognitives_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
