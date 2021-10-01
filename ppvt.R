#Creates a dataframe of ppvt assessments
#Easiest for PPVT to be separated b/c it's only done in pre, not post testing

library(dplyr)

lookup_table <- read.delim(dir("data_categories/lookup_table/",
                               full.names=T, pattern="^lookup_table_20"),header=TRUE, sep="\t")

#Create data table
ppvt <- data %>%
  select(-childsex, -childsex.factor, -childdob) %>% 
  left_join(lookup_table, by = "record_id") %>%
  group_by(record_id) %>% 
  filter(grepl("t1|t2|t3|t4|t5|p1|p2|p3|p4|p5",redcap_event_name),
         !is.na(ppvtrs)) %>% 
  mutate(dob_ppvt_gap = abs(difftime(childdob,ppvtdate, units="days"))) %>% 
  select(record_id,
         ppvt_completed:ppvt4_complete,
         dob_ppvt_gap)

#Write out data table
write.table(ppvt, file=paste("data_categories/ppvt/ppvt_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
