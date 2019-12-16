
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


# First we need to load some functions that we will use later on

source(paste0(data_raw_loc, "/R/dog_combine_accession_numbers.r"))
source(paste0(data_raw_loc, "/R/dog_spread_SKS.r"))
source(paste0(data_raw_loc, "/R/dog_date_columns.r"))
source(paste0(data_raw_loc, "/R/dog_date_from_text.r"))
source(paste0(data_raw_loc, "/R/dog_double_count.r"))

# change the column name of the ris and pacs data
ris_mod <- ris_data[, c("Accession.number", "Billing.code", "Billing.code.modifier", "Procedure.priority", "Patient_RK", "Procedure.name")]
colnames(ris_mod) <- c("Accession_number", "Billing_code", "Billing_code_modifier", "Procedure_priority", "Patient_RK", "Procedure_name")


pacs_mod <- pacs_data[, c("Accession.number", "Study.date.time.PACS", "Source")]
colnames(pacs_mod) <- c("Accession_number", "Study_date_time_PACS", "Source")

pacs_mod$Study_date_time_PACS <- lubridate::ymd_hms(pacs_mod$Study_date_time_PACS)

