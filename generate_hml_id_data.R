
rm(list = ls())

# Set file locations ----------

setwd("C:/Users/Lisa/Box/pan_dashboard_data/generate_hml_id/")

mindcrowd_folder <- "C:/Users/Lisa/Box/[UA BOX Health] MindCrowd Inbound/"

# Load Libraries -------------

library(tidyverse, quietly = T)

# section to connect to Google Drive
library(googledrive)
# file with info for service account 
googledrive::drive_auth(path = "pan-mindcrowd-uploads-ddf6b0dbe662.json")

# Option to silence the messages coming from the Google Drive library
options(googledrive_quiet = TRUE)

# Load recruitment lists ---------

files_dates <- list.files(mindcrowd_folder)
files_dates <- files_dates[str_detect(files_dates, "recruitment_list")]
files_dates <- unique(str_sub(files_dates[grep(".csv", files_dates)], end = 10))
files_dates <- files_dates[grep("20", files_dates)]

most_recent_update <- files_dates[order(files_dates, decreasing = T)][1]

print(paste0("Loading data from most recent update: ", most_recent_update, "."))

atl_dat <- read.csv(paste0(mindcrowd_folder, most_recent_update, "recruitment_list_atlanta.csv.gz"))

bal_dat <- read.csv(paste0(mindcrowd_folder, most_recent_update, "recruitment_list_baltimore.csv.gz"))

mia_dat <- read.csv(paste0(mindcrowd_folder, most_recent_update, "recruitment_list_miami.csv.gz"))

tuc_dat <- read.csv(paste0(mindcrowd_folder, most_recent_update, "recruitment_list_tucson.csv.gz"))

# Combine data, de-identify and select variables --------

new_dat <- rbind(atl_dat %>% mutate(area = "Atlanta"),
             bal_dat %>% mutate(area = "Baltimore"),
             mia_dat %>% mutate(area = "Miami"),
             tuc_dat %>% mutate(area = "Tucson")
             ) %>%
  mutate(age_group = case_when(age %in% 50:59 ~ "50-59",
                               age %in% 60:69 ~ "60-69",
                               age %in% 70:79 ~ "70-79"),
         part_id = str_sub(participant_id, start = -8),
         race_ethnicity = case_when(hispanic_latino == "Yes" ~ "Hispanic",
                                    race == "White" ~ "Non-Hispanic White",
                                    race == "Black or African American" ~ "Non-Hispanic Black",
                                    TRUE ~ "Other")) %>%
  select(participant_id, sex, race, hispanic_latino, area, age_group, memory_rank) %>%
  mutate(hml_id = NA, hml_id_created_date = NA,.after = participant_id) %>%
  mutate(hml_id_undo = NA,
         across(everything(), ~ifelse(. == "", NA, .)))

# Remove participants who already have HML IDs ------

# Downloads current deidentified_id_data.csv and overwrites existing file
with_drive_quiet(
  drive_download(as_id("https://drive.google.com/file/d/1GbJUTmt96x50L9fDH4RGIIt05zoOWEbo"), 
                 path = "deidentified_id_data.csv",
                 overwrite = TRUE)
)
dat <- read.csv("deidentified_id_data.csv")

# Updating data to contain more demographics:
## Only need to run once, keeping code for future reference:

dat <- dat %>%
  left_join(new_dat %>% select(participant_id, area, sex, age_group,
                               race, hispanic_latino, memory_rank)) 

# Filters out already existing participant ids and adds new IDs
updated_dat <- new_dat %>%
  filter(!participant_id %in% dat$participant_id) %>%
  rbind(dat)

print(paste0("Loaded ", nrow(new_dat), " rows of data from MindCrowd."))

print(paste0("Loaded ", nrow(updated_dat), " rows of data from app folder with ", sum(!is.na(updated_dat$hml_id)), " new HML IDs."))

# Save data -------------

write.csv(updated_dat, file = "deidentified_id_data.csv",
          row.names = F)
# Check to make sure you don't save over the saved HML IDs
# Saves the modified file to Google Drive into the HML Data shared drive
drive_put("deidentified_id_data.csv", path=drive_find(pattern="HML Data", corpus="allDrives"))

print("Saved updated data to app folder.")
