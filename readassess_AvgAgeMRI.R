#Creates a dataframe of the reading assessments closest to the average age
#for MRIs (8.79 years old)

library(dplyr)

#Load in data
reading_assessments <- read.delim(dir("data_categories/reading_assessments/",
                                full.names=T, pattern="^reading_assessments_20"),header=TRUE, sep="\t")

#Create data table
readassess_AvgAgeMRI <- reading_assessments %>%
  mutate(#3208 is 365*8.79, which is the age we're trying to match to
         MRIage_read_gap_abs = abs(dob_read_gap-3208),
         MRIage_read_gap = dob_read_gap-3208) %>% 
  group_by(record_id) %>% 
  slice(which.min(MRIage_read_gap_abs)) %>%  
  ungroup()

#Write out data table
write.table(readassess_AvgAgeMRI, file=paste("data_categories/reading_assessments/additional_readassess/readassess_AvgAgeMRI_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
