# Description of this file:
# Using pacs, ris, sks and scanning_type datasets
# The purpose of this file is to describe the sks and sks modifier
# for each subspeciality
#
#


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

skannertype <- read.csv2(
    paste0(data_raw_loc, "/skannertype.csv"),
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    colClasses = "character"
)


# First we need to load some functions that we will use later on

source(paste0(data_raw_loc, "/R/dog_combine_accession_numbers.r"))
source(paste0(data_raw_loc, "/R/dog_spread_SKS.r"))
source(paste0(data_raw_loc, "/R/dog_date_columns.r"))
source(paste0(data_raw_loc, "/R/dog_date_from_text.r"))
source(paste0(data_raw_loc, "/R/dog_double_count.r"))

# change the column name of the ris and pacs data
# we will give the name "_mod" as suffix instead of data
# so we do not have to load the data again.
ris_mod <- ris_data[, c("Accession.number", "Billing.code", "Billing.code.modifier", "Procedure.priority", "Patient_RK", "Procedure.name")]
colnames(ris_mod) <- c("Accession_number", "Billing_code", "Billing_code_modifier", "Procedure_priority", "Patient_RK", "Procedure_name")


pacs_mod <- pacs_data[, c("Accession.number", "Study.date.time.PACS", "Source")]
colnames(pacs_mod) <- c("Accession_number", "Study_date_time_PACS", "Source")

pacs_mod$Study_date_time_PACS <- lubridate::ymd_hms(pacs_mod$Study_date_time_PACS)


# Now that we have ris and pacs, we have to merge pacs on ris. This is a left merge
rispacs_left_merged <- left_join(ris_mod, pacs_mod, by = "Accession_number") %>% 
    arrange(Accession_number, Procedure_priority)


rispacs_left_merged$Billing_code_alt <- paste(rispacs_left_merged$Billing_code, rispacs_left_merged$Billing_code_modifier, sep="_")
rispacs_left_merged$year <- lubridate::year(rispacs_left_merged$Study_date_time_PACS)
rispacs_left_merged <- merge(x = rispacs_left_merged, y = skannertype, by = "Source", all.x = TRUE)

colnames(rispacs_left_merged)[1] <- "Source_code"
colnames(rispacs_left_merged)[12] <- "Source"
dput(colnames(rispacs_left_merged))

# Find out which years there are available
# Select only needed years
years_to_select_list <- unlist(sort(unique(rispacs_left_merged$year)))
years_to_select <- years_to_select_list[3:4]


# Selecting machines
machines_to_select_list <- c("CT", "MR", "gennemlyser", "kaebeskanner", "RTG", 
                             "XRU", "UL")

kk <- 2
machines_to_select <- machines_to_select_list[kk]

if (machines_to_select == "CT") {
    machines_selected <- c("CT1", "CT2", "CT3", "CT4")
    time_before_merge <- 30
} else if (machines_to_select == "PET") {
    machines_selected <- c("CTK1")
    time_before_merge <- 90
} else if (machines_to_select == "MR") {
    machines_selected <- c("MR1", "MR2", "MR3", "MR4")
    time_before_merge <- 90
} else if (machines_to_select == "gennemlyser") {
    machines_selected <- c("Gennemlyser")
    time_before_merge <- 90
} else if (machines_to_select == "kaebeskanner") {
    machines_selected <- c("Kæbeskanner")
    time_before_merge <- 60
} else if (machines_to_select == "RTG") {
    machines_selected <- c("XR1", "XR2", "XT2", "XT3", "XT4", "XR4", "XT4", "XT5")
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

# technical stuff
affix_save_name <- paste0(c(years_to_select, machines_to_select), collapse = "_")
dir.create(file.path(save_fig_loc, machines_to_select))
dir.create(file.path(save_data_loc, machines_to_select))


# Selected the wanted data.
rispacs_left <- rispacs_left_merged[(rispacs_left_merged$year %in% years_to_select) & (rispacs_left_merged$Source %in% machines_selected), ]

# Precossing stuff

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
# originally there exists two columns
# one with the SKS value and one with the SKS modifier
# In function dog_spread_SKS, the sks codes with the modifier are merged
# if they are mergable out of the criteria.
# The idea here is to split it up, so there is a column with SKS, one with the
# modifier and the last with sks and modifier, e.g.
# SKS: value1/value2
# SKS-w-mod: value1_NA/value2_UXM
# SKS-mod: NA/UXM
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

alle_r_en_r <- out %>% 
    mutate(Source= fct_relevel(Source, unique(out$Source)[order(match(unique(out$Source), machines_selected))]),
           Procedure_priority = fct_relevel(Procedure_priority, lev_pri)) 


# SKS oversigt

sks_df <- count(alle_r_en_r, SKS_m_mod, sort=TRUE)

# Getting a warning. 
# Ignore this for now.
suppressWarnings(
    df_tmp <- data.frame(sks_df,do.call(rbind,str_split(sks_df$SKS_m_mod,"/")))
)


tt_pre_colnames <- dput(colnames(df_tmp))

no_of_true <- sum(str_detect(tt_pre_colnames, "X"))

df <- df_tmp[, str_detect(tt_pre_colnames, "X")]


#kommenter denne del
for (row in 1:nrow(df)) {
    vec = df %>% slice(row) %>% unlist() %>% unname()
    #check for duplicates
    if(length(unique(vec)) != length(df)) {
        positions <- which(duplicated(vec) %in% c("TRUE"))
        #iterate through positions
        for(i in 1:length(positions)) {
            possible_new_values <- c(NA)
            df[row,positions[i]]  <- sample(possible_new_values
                                            [ ! possible_new_values %in% unique(vec)],1)
        }
    }
}



# kommenter her
df_final <- cbind(df_tmp[, c("SKS_m_mod")], df)

colnames(df_final) <- c("Koder_samlet", paste0("SKS", 1:no_of_true))

tt_colnames <- dput(colnames(df_final))
names_of_col <- colnames(df_final[, str_detect(tt_colnames, "SKS")])

# tmp_df <- as.data.frame(splitstackshape::cSplit(df_final[, names_of_col], names(df_final[, names_of_col]), 
#                                   sep = "_"))

tmp_df <- as.data.frame(splitstackshape::cSplit(df_final, names(df_final[, names_of_col]), 
                                                sep = "_"))

tt_colnames <- dput(colnames(tmp_df))
names_of_col_sks <- colnames(tmp_df[, str_detect(tt_colnames, "_1")])
names_of_col_sks_til <- colnames(tmp_df[, str_detect(tt_colnames, "_2")])

names_of_col_sks <- names_of_col
names_of_col_sks_til <- paste0("SKS", 1:no_of_true, "_til")
new_names <- c(rbind(names_of_col_sks, names_of_col_sks_til))

colnames(tmp_df) <- c(colnames(tmp_df)[1], new_names)


# next part
df_final_til <- tmp_df


tt_colnames <- dput(colnames(df_final_til))
names_of_col <- colnames(df_final_til[, str_detect(tt_colnames, "SKS")])


df_final_til$row_id <- 1:nrow(df_final_til)

df_overview <- df_final_til


sks_navne_oversigt <- sks_data[, c("SKS_Kode", "SKS_Txt")]

for (i in 1:length(names_of_col)) {
    to_append <- merge(x = df_final_til, y = sks_navne_oversigt, by.x = names_of_col[i], by.y = "SKS_Kode", all.x = TRUE)
    to_append <- to_append[, (ncol(to_append)-1):(ncol(to_append))]
    df_overview <- merge(x = df_overview, y = to_append, by="row_id", all = TRUE)
    #colnames(df_overview) <- c("row_id", "Koder_samlet", paste0("SKS", 1:no_of_true), paste0("Tekst", 1:i))
    df_overview <- df_overview[ , !(names(df_overview) %in% names_of_col[i])]
    colnames(df_overview)[length(df_overview)] <- names_of_col[i]
}


df_overview <- df_overview[!duplicated(df_overview$row_id),]


df_overview <- df_overview[ , order(names(df_overview))]

df_overview <- df_overview[,colSums(is.na(df_overview))<nrow(df_overview)]

df_overview <- df_overview[ , !(names(df_overview) %in% "row_id")]

df_overview <- df_overview[order(rowSums(is.na(df_overview))), ]

# df_overview[] <- t(apply(df_overview, 1, function(x) c(x[!is.na(x)], x[is.na(x)])))

df_overview$Tid_i_min <- 0


save_file_name <- paste0(save_data_loc, machines_to_select, "/", affix_save_name, "_sks_oversigt_hele_doegn.csv")
write.csv(as.matrix(df_overview), save_file_name, row.names = TRUE)

