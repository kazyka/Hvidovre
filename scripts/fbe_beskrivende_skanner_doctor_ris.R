
library(dplyr)
library(dbplyr)
library(odbc)
library(stringr)
library(tidyverse)
library(gridExtra)
library(grid)
library(hrbrthemes)
library(here)
library(vroom)




data_raw_loc <- paste0(here(), "/data/raw/FBE/rispacs")
processed_loc <-  paste0(here(), "/data/processed/FBE/rispacs")
save_data_loc <-  paste0(here(), "/output_data/")
save_fig_loc <-  paste0(here(), "/figures/")


pacs_data <- readRDS(paste0(processed_loc, '/pacs_data_afgr.rds')) 
ris_data <- readRDS(paste0(processed_loc, '/ris_data_afgr.rds')) 
sks_data <- readRDS(paste0(processed_loc, '/sks_beskrivelser.rds')) 

beskrivende_doctor <- read.csv2(
    paste0(data_raw_loc, "/ris_data_beskrivende_doctor.csv"),
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE,
    colClasses = "character",
    encoding = "UTF-8"
)



# sort( unique(beskrivende_doctor$Full.name))

unique(beskrivende_doctor$Full.name)[grep("Annette", unique(beskrivende_doctor$Full.name))]

beskrivende_doctor$sub_code <- substr(beskrivende_doctor$Billing.code, 1, 3)

#choosing only UXM codes (MR)

beskrivende_doctor_mr <- beskrivende_doctor[beskrivende_doctor$sub_code %in% "UXM", ]

mr_forskere <- tibble(
    Full.name = c("Jensen, Frank Krieger", "Buskov, Laura,", "Jensen, Annette Bøjer,",
              "Magnussen, Erland,", "Madsen, Camilla,", "Davtyan, Marine,", 
              "Mariana Kristensen,", "Joshi, Deepak,"),
    codes = c(rep(334, 3), rep(340, 5))
)

mr_forskere <- as.data.frame(mr_forskere)

beskrivende_doctor_mr <- merge(x = beskrivende_doctor_mr, y = mr_forskere, 
                               by = "Full.name", all.x = TRUE)





fordeling <- beskrivende_doctor_mr %>%
    count(codes, Full.name, Billing.code) %>%  
    mutate(andel_i_procent = round(n/sum(n)*100, 2)) 


fordeling[is.na(fordeling$codes), c("codes")] <- 0


pr_grp <- aggregate(fordeling$n, by=list(codes=fordeling$codes), FUN=sum)

fordeling <- merge(x = fordeling, y = pr_grp, by = "codes", all.x = TRUE)


fordeling$andel_i_grp <- round(fordeling$n / fordeling$x * 100, 2)

fordeling <- fordeling[, !(names(fordeling) %in% "x")]



save_file_name <- paste0(save_data_loc, "oversigt_beskrivetider.csv")
write.csv(as.matrix(fordeling), save_file_name, row.names = TRUE)


tmp <- fordeling[fordeling$codes %in% 0, c("Full.name", "Billing.code")]

save_file_name <- paste0(save_data_loc, "mr_beskrivelser_alt_andet_end_334_340.csv")
write.csv(as.matrix(tmp), save_file_name, row.names = TRUE)
