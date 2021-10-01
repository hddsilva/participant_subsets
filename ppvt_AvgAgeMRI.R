#Finds the PPVT closest to average age of MRIs (8 yrs old)

library(dplyr)

#Load in data
ppvt <- read.delim(dir("data_categories/ppvt/",
                               full.names=T, pattern="^ppvt_20"),header=TRUE, sep="\t")


#Create data table
ppvt_AvgAgeMRI <- ppvt %>% 
  mutate(#3208 is 365*8.79, which is the age we're trying to match to
         MRIage_ppvt_gap_abs = abs(dob_ppvt_gap-3208),
         MRIage_ppvt_gap = dob_ppvt_gap-3208) %>% 
  group_by(record_id) %>% 
  slice(which.min(MRIage_ppvt_gap_abs)) %>%
  ungroup()

#Write out data table
write.table(ppvt_AvgAgeMRI, file=paste("data_categories/ppvt/additional_ppvt/ppvt_AvgAgeMRI_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
