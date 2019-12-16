
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




# # Definere arbejdsmappe
# diskdrive = "L"
# 
# # Location to my folder
# my_folder <- ":/LovbeskyttetMapper/BogA - Analyse/Mirza/rispacsHvidovre"

####
#### FIX PATH
####

data_raw_loc <- paste0(here(), "/data/raw/")
save_processed_loc <-  paste0(here(), "/data/processed/")
save_data_loc <-  paste0(here(), "/output/")
save_fig_loc <-  paste0(here(), "/figures/")

# 
# if (!!(file_test("-f", paste0(save_processed_loc, "pacs_data.csv")) &  file_test("-f", paste0(save_processed_loc, "ris_data.csv")))) {
#     # Henter data fra databasen
Analyse <- dbConnect(odbc::odbc(),
                     Driver = "SQL server",
                     Server = "RGHPRODAPP007\\RGHPRODAPP007",
                     Database = "Analyse",
                     encoding = "latin1")

RIS_HVIDOVRE <- tbl(Analyse, in_schema("FBE", "RIS_HVIDOVRE_AFGR")) %>%
    collect()

PACS_HVIDOVRE <- tbl(Analyse, in_schema("FBE", "PACS_HVIDOVRE_AFGR")) %>%
    collect()

pacs_data <- PACS_HVIDOVRE
ris_data <- RIS_HVIDOVRE

dbDisconnect(Analyse) 

# Loader helligdage
load(file = paste0(here(), "/data/raw/FBE/rispacs/helligdag18.rda"))

helligdag <- c(helligdag18, as.Date("2019-01-01"), as.Date("2019-04-14"),
               as.Date("2019-04-18"), as.Date("2019-04-19"), as.Date("2019-04-21"),
               as.Date("2019-04-2"), as.Date("2019-05-17"), as.Date("2019-05-30"),
               as.Date("2019-06-09"), as.Date("2019-06-10"), as.Date("2019-12-25"),
               as.Date("2019-12-26"))

# Loader liste af skannertyper
skannertype <- read.csv2(
    paste0(data_raw_loc , "FBE/rispacs/skannertype_new.csv"),
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    colClasses = "character"
)

sks_navne_oversigt <- read.csv2(
    paste0(data_raw_loc , "FBE/rispacs/sks_navne.csv"),
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    colClasses = "character"
)


# Definere funktioner
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

dog_aabningstid <- function(df, x = NULL){
    suppressWarnings(
        df %>%
            dplyr::filter(!.data$date %in% x) %>%
            dplyr::filter(!.data$weekday %in% c(7, 1)) %>%
            dplyr::filter((dplyr::between(.data$hour_min, as.numeric(hms::as.hms("08:00:00")), as.numeric(hms::as.hms("14:59:59")))
                           & .data$weekday %in% c(2:5))  |
                              (.data$weekday %in% c(6)
                               & dplyr::between(.data$hour_min, as.numeric(hms::as.hms("08:00:00")), as.numeric(hms::as.hms("14:30:00")))))
    )
}

source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_combine_accession_numbers.r"))
source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_spread_SKS.r"))
source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_date_columns.r"))
source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_date_from_text.r"))
source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_double_count.r"))

# Hiver de korrekte kol. ud.
ris_mod <- ris_data[, c("Accession.number", "Billing.code", "Billing.code.modifier", "Procedure.priority", "Patient_RK", "Procedure.name")]
colnames(ris_mod) <- c("Accession_number", "Billing_code", "Billing_code_modifier", "Procedure_priority", "Patient_RK", "Procedure_name")


pacs_mod <- pacs_data[, c("Accession.number", "Study.date.time.PACS", "Source")]
colnames(pacs_mod) <- c("Accession_number", "Study_date_time_PACS", "Source")

pacs_mod$Study_date_time_PACS <- lubridate::ymd_hms(pacs_mod$Study_date_time_PACS)


# Now that we have ris and pacs, we have to merge pacs on ris. This is a left merge
rispacs_left_merged <- left_join(ris_mod, pacs_mod, by = "Accession_number") %>% 
    arrange(Accession_number, Procedure_priority)

# samler billing_code mm
# laver en dato kolonne
# merge by source, så jeg kender skannerne

rispacs_left_merged$Billing_code_alt <- paste(rispacs_left_merged$Billing_code, rispacs_left_merged$Billing_code_modifier, sep="_")
rispacs_left_merged$year <- lubridate::year(rispacs_left_merged$Study_date_time_PACS)
rispacs_left_merged <- merge(x = rispacs_left_merged, y = skannertype, by = "Source", all.x = TRUE)

colnames(rispacs_left_merged)[1] <- "Source_code"
colnames(rispacs_left_merged)[12] <- "Source"
dput(colnames(rispacs_left_merged))


# Preprocessing for the different sources. 
years_to_select_list <- unlist(sort(unique(rispacs_left_merged$year)))
years_to_select <- years_to_select_list

machines_to_select_list <- c("CT", "MR", "gennemlyser", "kaebeskanner", "RTG", 
                             "XRU", "UL", "PET")

kk <- 5

machines_to_select <- machines_to_select_list[kk]


print(years_to_select)
print(machines_to_select)

if (machines_to_select == "CT") {
    machines_selected <- c("CT1", "CT2", "CT3", "CT4")
    time_before_merge <- 30
} else if (machines_to_select == "PET") {
    machines_selected <- c("CTK1")
    time_before_merge <- 90
} else if (machines_to_select == "MR") {
    machines_selected <- c("Verio", "Avanto", "Prisma", "Espree")
    time_before_merge <- 90
} else if (machines_to_select == "gennemlyser") {
    machines_selected <- c("Gennemlyser")
    time_before_merge <- 90
} else if (machines_to_select == "kaebeskanner") {
    machines_selected <- c("Kæbeskanner")
    time_before_merge <- 60
} else if (machines_to_select == "RTG") {
    machines_selected <-
        c("XR1", "XR2", "XT2", "XT3", "XT4", "XR4", "XT4", "XT5")
    time_before_merge <- 90
} else if (machines_to_select == "XRU") {
    machines_selected <- c("XRUdefoto")
    time_before_merge <- 90
} else if (machines_to_select == "UL") {
    machines_selected <- c("UL1", "UL2", "UL3", "UL4", "UL5")
    time_before_merge <- 60
} else {
    print("CHECK YOUR machines to select")
}

rispacs_left <- rispacs_left_merged[(rispacs_left_merged$Source %in% machines_selected), ]


# technical stuff
affix_save_name <- paste0(c(years_to_select, machines_to_select), collapse = "_")
dir.create(file.path(save_fig_loc, machines_to_select))
dir.create(file.path(save_data_loc, machines_to_select))
dir.create(file.path(paste0(save_processed_loc, "FBE/rispacs/", machines_to_select)))





# Colors, Colours
farver <- c(rgb(148,182,210, m=255),rgb(221,128,71, m=255),rgb(165,171,129, m=255),rgb(216,178,92, m=255),rgb(123,167,157, m=255),rgb(150,140,140, m=255))

## Precossing stuff

# Combining Accesion numbers
# Bruger funktioner til at kombinere accesion numbers ud fra et pre-defineret tidsintaval
a <- dog_combine_accession_numbers(rispacs_left, CPR = Patient_RK, time_passed = time_before_merge)
b <- dog_spread_SKS(a, Billing_code = Billing_code_alt, Accession_number = Accession_number2, col_name = SKS)

one_row <- left_join(a, b, by = "Accession_number2") %>% 
    select(Accession_number2, SKS, Procedure_priority, Patient_RK, 
           Study_date_time_PACS, Source, Procedure_name) %>% 
    distinct(Accession_number2, .keep_all = TRUE)  %>% 
    mutate(Procedure_priority = if_else(is.na(Procedure_priority), "Priority missing", Procedure_priority),
           Source = if_else(is.na(Source), "Scanner ID missing", Source))

# Split up the sks and mod.
one_row$SKS_m_mod <- one_row$SKS
one_row$SKS <- ""
one_row$SKS_mod <- ""

for (i in 1:nrow(one_row)) {
    split_codes <- unlist(str_split(one_row$SKS_m_mod[i], "\\_|\\/"))
    sks_codes <- paste0(split_codes[c(TRUE, FALSE)], collapse = "/")
    sks_mod_codes <- paste0(split_codes[c(FALSE, TRUE)], collapse = "/")
    one_row$SKS[i] <- sks_codes
    one_row$SKS_mod[i] <- sks_mod_codes
    
    if (i %% 5000 == 0) {
        print(i)
    }
}

x <- one_row$Study_date_time_PACS
out <-  one_row %>%
    dplyr::mutate(year=lubridate::year(x),
                  yday=lubridate::yday(x),
                  weekday=lubridate::wday(x, label=FALSE),
                  weekday2=lubridate::wday(x, label=TRUE),
                  hour = lubridate::hour(x),
                  hour_min= hms::as_hms(lubridate::force_tz(x)),
                  week_num = lubridate::week(x),
                  month = lubridate::month(x),
                  date = lubridate::date(x),
                  day = lubridate::day(x))


lev_pri <- unique(out$Procedure_priority)
to_match <- c("Haste", "Fremskyndet", "Kræftpakke", "Rutine", "-1", "DO NOT REMOVE")
lev_pri <- lev_pri[order(match(lev_pri, to_match))]

day_lev <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
             "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
             "25", "26", "27", "28", "29", "30", "31")


# Only select dates up to Q2
out$quarters <- zoo::as.yearqtr(out$date, format = "%Y-%m-%d")
out <- out[which(out$quarters %in% dput(sort(unique(out$quarters))[1:(length(unique(out$quarters)) - 1)]) ), ]



save_file_loc <- paste0(save_processed_loc, "FBE/rispacs/", machines_to_select, "/aktivitet.rds")

saveRDS(out, save_file_loc)

