
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


tmp <- grep("Frank", beskrivende_doctor$Full.name)

print(length(tmp))


name_lookup <- "Jensen, Frank Krieger"

bd_frank <- beskrivende_doctor[beskrivende_doctor$Full.name %in% name_lookup, ]


bd_frank$sub_code <- substr(bd_frank$Billing.code, 1, 3)


fordeling <- bd_frank %>% 
    count(sub_code, sort=TRUE) %>%  
    mutate(andel = round(n/sum(n)*100, 2)) 

