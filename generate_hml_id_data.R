
rm(list = ls())

# Load Libraries -------------

library(tidyverse)

# Load recruitment lists ---------

mindcrowd_folder <- "C:/Users/Lisa/Box/[UA BOX Health] MindCrowd Inbound/"

files_dates <- list.files(mindcrowd_folder)
files_dates <- unique(str_sub(files_dates[grep(".csv", files_dates)], end = 10))
files_dates <- files_dates[grep("20", files_dates)]

most_recent_update <- files_dates[order(files_dates, decreasing = T)][1]

atl_dat <- read.csv(paste0(mindcrowd_folder, most_recent_update, "recruitment_list_atlanta.csv.gz"))

bal_dat <- read.csv(paste0(mindcrowd_folder, most_recent_update, "recruitment_list_baltimore.csv.gz"))

mia_dat <- read.csv(paste0(mindcrowd_folder, most_recent_update, "recruitment_list_miami.csv.gz"))

tuc_dat <- read.csv(paste0(mindcrowd_folder, most_recent_update, "recruitment_list_tucson.csv.gz"))

# Combine data, de-identify and select variables --------

dat <- rbind(atl_dat %>% mutate(area = "Atlanta"), 
             bal_dat %>% mutate(area = "Baltimore"), 
             mia_dat %>% mutate(area = "Miami"), 
             tuc_dat %>% mutate(area = "Tucson")
             ) %>%
  mutate(age_group = case_when(age %in% 50:59 ~ "50-59",
                               age %in% 60:69 ~ "60-69",
                               age %in% 70:79 ~ "70-79"),
         part_id = str_sub(participant_id, start = -8)) %>%
  select(participant_id, part_id, area, age_group, sex) %>%
  mutate(hml_id = NA,
         hml_id_created_date = NA)

# Remove participants who already have HML IDs



# Save data -------------

# Check to make sure you don't save over the saved HML IDs
# write.csv(dat, file = "ID Assignment Dashboard/deidentified_id_data.csv", row.names = F)

