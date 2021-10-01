#Loop through cerebellum parcellation csv's
#Hailey 3/27/2020

library(dplyr)

setwd("data_categories/cerebellum_parcellations/csvs")
ID_list <- list.files(pattern = "*_MPRAGE_defaced.csv") %>%  
  lapply(substr, 1, 6)

cerebellum_parcellations <- list.files(pattern = "*_MPRAGE_defaced.csv") %>%  
  lapply(read.csv, sep = ";") %>% 
  bind_rows 
cerebellum_parcellations$mrrc_ID <- ID_list
cerebellum_parcellations <- cerebellum_parcellations %>% 
  select(mrrc_ID,everything(),-Sex,-Age,-Report.Date,-Patient.ID)
cerebellum_parcellations$mrrc_ID <- as.character(cerebellum_parcellations$mrrc_ID)
  
#Write out dataframe
write.table(cerebellum_parcellations, file=paste("data_categories/cerebellum_parcellations/cerebellum_parcellations_",Sys.Date(),".txt",sep=""), sep="\t", row.names = FALSE)
