#Creates a dataframe of each child's earliest reading assessment

library(dplyr)

#Load in data
reading_assessments <- read.delim(dir("data_categories/reading_assessments/",
                                      full.names=T, pattern="^reading_assessments_20"),header=TRUE, sep="\t")

#Create data table
readassess_earliest <- reading_assessments %>%
  group_by(record_id) %>% 
  slice(which.min(read_age)) %>%  
  ungroup()

#Write out data table
write.table(readassess_earliest, file=paste("data_categories/reading_assessments/additional_readassess/readassess_earliest_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
