# Description of this file:
# Dataset that is needed here is the activity.rds file
# that is located in each subspeciality in the processed folder
# for FBE.
# Here the output is a timeseries plot of scannings and sks codes used for each 
# day




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




machines_to_select_list <- c("CT", "MR", "gennemlyser", "kaebeskanner", "RTG", 
                             "XRU", "UL", "PET")

kk <- 1




data_raw_loc <- paste0(here(), "/data/raw/")





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


# dog_aabningstid <- function(df, x = NULL){
#     suppressWarnings(
#         df %>%
#             dplyr::filter(!.data$date %in% x) %>%
#             dplyr::filter(!.data$weekday %in% c(7, 1)) %>%
#             dplyr::filter((dplyr::between(.data$hour_min, as.numeric(hms::as.hms("08:00:00")), as.numeric(hms::as.hms("18:59:59")))
#                            & .data$weekday %in% c(2:5))  |
#                               (.data$weekday %in% c(6)
#                                & dplyr::between(.data$hour_min, as.numeric(hms::as.hms("08:00:00")), as.numeric(hms::as.hms("18:59:59")))))
#     )
# }

source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_combine_accession_numbers.r"))
source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_spread_SKS.r"))
source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_date_columns.r"))
source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_date_from_text.r"))
source(paste0(here(), "/data/raw/FBE/rispacs/R/dog_double_count.r"))






save_processed_loc <-  paste0(here(), "/data/processed/FBE/rispacs/")
save_data_loc <-  paste0(here(), "/output_data/")
save_fig_loc <-  paste0(here(), "/figures/")






farver <-
    c(
        rgb(148, 182, 210, m = 255),
        rgb(221, 128, 71, m = 255),
        rgb(165, 171, 129, m = 255),
        rgb(216, 178, 92, m = 255),
        rgb(123, 167, 157, m = 255),
        rgb(150, 140, 140, m = 255)
    )

for (kk in 1:length(machines_to_select_list)) {
    
machines_to_select <- machines_to_select_list[kk]
years_to_select <- c("2016", "2017", "2018", "2019")

print(years_to_select)
print(machines_to_select)

affix_save_name <- paste0(c(years_to_select, machines_to_select), collapse = "_")


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





read_file_loc <- paste0(save_processed_loc, machines_to_select, "/aktivitet.rds")   
ct_out <- readRDS(read_file_loc)
print(paste0("File read: ", read_file_loc))

alle_r_en_r <- ct_out


alle_r_en_r_aab <- alle_r_en_r %>% 
    dog_aabningstid()

n_hverdage <- alle_r_en_r_aab %>% 
    distinct(date) %>% 
    nrow()

n_aabningsdage <- alle_r_en_r_aab %>%
    count(date, Source) %>%
    count(Source) %>% 
    rename(Antal_aabningsdage= n) %>% 
    mutate(Antal_dage_lukket =  n_hverdage - Antal_aabningsdage) %>% 
    filter(Source != "Ingen ID") 

antal_scan <- alle_r_en_r_aab %>% 
    group_by(Source) %>%
    summarise(Antal_scanninger=n()) 

aab_acc_num <- alle_r_en_r_aab$Accession_number2

lukket_df <- alle_r_en_r %>% 
    filter(!Accession_number2 %in% aab_acc_num)


hverdage <- alle_r_en_r %>% 
    filter(!weekday %in% c(1,7)) %>% 
    filter(!date %in% helligdag)

data_nummer <- alle_r_en_r %>% 
    count(Source)

# Oversigt på dagplan

oversigt_dagsniveau <- alle_r_en_r_aab %>%
    count(date, Source) %>% 
    spread(Source, n)  %>%
    #(- `Ingen ID`) %>%
    replace(is.na(.), 0) 

save_file_name <- paste0(save_data_loc, machines_to_select, "/", affix_save_name, "_oversigt_dagplan_aabningstid.csv")
write.csv(as.matrix(oversigt_dagsniveau), save_file_name, row.names = TRUE)


til_sks_df <- alle_r_en_r_aab[ , c("date", "SKS")]
s <- strsplit(til_sks_df$SKS, split = "/")
tmp <- data.frame(date = rep(til_sks_df$date, sapply(s, length)), SKS = unlist(s))


sks_over_tid <- tmp %>%
    count(date, SKS)
sks_ag_tid <- aggregate(sks_over_tid$n, by=list(Category=sks_over_tid$date), FUN=sum)

maskin_over_tid <- oversigt_dagsniveau %>%
    mutate(sum = rowSums(.[2:ncol(oversigt_dagsniveau)]))


oversigt_tidserie <- sks_ag_tid[, c("Category", "x")]
colnames(oversigt_tidserie) <- c("date", "total_sks")
oversigt_tidserie <- merge(x = oversigt_tidserie, y = maskin_over_tid[ , c("date", "sum")], all = TRUE)
colnames(oversigt_tidserie) <- c("date", "total_sks", "total_skanninger")
oversigt_tidserie$quarters <- zoo::as.yearqtr(oversigt_tidserie$date, format = "%Y-%m-%d")

tmp_sks <- oversigt_tidserie %>%
    group_by(quarters) %>% summarise(sum(total_sks))

colnames(tmp_sks) <- c("quarters", "total_sks")

# tmp_sks$ratio_sks <- 0
#
# for (i in 2:nrow(tmp_sks)) {
#     tmp_sks$ratio_sks[i] <- ((tmp_sks$total_sks[i] - tmp_sks$total_sks[i-1]) / tmp_sks$total_sks[i-1])*100
# }



tmp_skanninger <- oversigt_tidserie %>%
    group_by(quarters) %>% summarise(sum(total_skanninger))

colnames(tmp_skanninger) <- c("quarters", "total_skanninger")

# tmp_skanninger$ratio_skan <- 0
#
# for (i in 2:nrow(tmp_skanninger)) {
#     tmp_skanninger$ratio_skan[i] <- ((tmp_skanninger$total_skanninger[i] - tmp_skanninger$total_skanninger[i-1]) / tmp_skanninger$total_skanninger[i-1])*100
# }


oversigt_kvart <- merge(x = tmp_sks, y = tmp_skanninger, by = "quarters", all = TRUE)



title_text <- "Udvikling over tid af SKS og Skanninger"
sub_title_text <- paste0("For ", machines_to_select, " skanninger")


test <- oversigt_kvart %>%
    gather(key = "variable", value = "value", -quarters)

p <- oversigt_kvart %>%
    gather(key = "variable", value = "value", -quarters) %>%
    ggplot(aes(x = quarters, y = value, colour = variable)) +
    zoo::scale_x_yearqtr(format="%YQ%q", n=5) +
    theme_ipsum_rc() +
    ylim(0, roundUpNice(max(oversigt_kvart$total_sks))) +
    geom_line() +
    geom_point() +
    geom_smooth(method = "lm", se=FALSE) +
    # geom_text(aes(label = round(value, 1)),
    #           vjust = "outward", hjust = "inward",
    #           show.legend = FALSE) +
    #scale_color_manual(values = farver) +
    labs(title=title_text,
         subtitle = sub_title_text,
         x="Kvartal",
         y="Antal",
         fill="") +
    scale_color_manual(values = farver, name = "", labels = c("Antal Skanninger", "Antal SKS koder"))


save_file_name <- paste0(save_fig_loc, machines_to_select, "/", affix_save_name, "_udvikling_over_tid.png")
ggsave(save_file_name, plot = p, width = 30, height = 20, units = "cm")
}
