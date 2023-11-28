
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

# Load current list of participants and all_screening_data

with_drive_quiet(
  drive_download(as_id("https://drive.google.com/file/d/1NWUkcoIRuHBlw1h6_LWRlNpQSTcNa3fr"),
                 overwrite = TRUE)
)
load_data <- readRDS("generate_hml_id_dat.Rds")

dat <- load_data$dat
all_dat <- load_data$all_dat

# Combine location recruitment data, de-identify and select variables --------

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

## Updating data to contain more demographics:
## Only need to run once, keeping code for future reference:
# 
# dat <- dat %>%
#   left_join(new_dat %>% select(participant_id, area, sex, age_group,
#                                race, hispanic_latino, memory_rank),
#             by = join_by(participant_id, area, age_group, sex, race, hispanic_latino, memory_rank)) 

# Filters out already existing participant ids and adds new IDs
updated_dat <- new_dat %>%
  filter(!participant_id %in% dat$participant_id) %>%
  rbind(dat)

print(paste0("Loaded ", nrow(new_dat), " rows of data from MindCrowd."))

print(paste0("Loaded ", nrow(updated_dat), " rows of data from app folder with ", sum(!is.na(updated_dat$hml_id)), " HML IDs."))

# Create data for manual ID update ---------------

# Get all of the screening data from the recruitment dashboard data script
with_drive_quiet(
  drive_download(as_id("https://drive.google.com/file/d/1eTpEi3O7JFVuG4GkawK0C_DDOs93qVKW"),
                 path = "all_screening_data.Rdata",
                 overwrite = TRUE)
)
load("all_screening_data.Rdata")

all_dat <- all_screening_data %>%
  left_join(dat %>% select(participant_id_parent = participant_id, hml_id, hml_id_created_date),
            by = "participant_id_parent") %>%
  distinct(participant_id, .keep_all = TRUE) %>%
  mutate(participant_id_parent2 = ifelse(participant_id_parent == participant_id,
                                         participant_id_parent, NA),
         participant_id2 = ifelse(participant_id_parent != participant_id,
                                  participant_id, NA),
         participant_id_parent_short = ifelse(participant_id_parent == participant_id,
                                              str_sub(participant_id_parent, start = -8),
                                              NA),
         participant_id_short = ifelse(participant_id_parent != participant_id, 
                                       str_sub(participant_id, start = -8),
                                       NA)) %>%
  pivot_longer(cols = c(participant_id_parent2, participant_id2, participant_id_parent_short, participant_id_short),
               names_to = "id_source",
               values_to = "alternative_id") %>%
  filter(!is.na(alternative_id)) %>%
  select(-c(participant_id, id_source))

# Save deidentified id data and all screening data
saveRDS(list(dat = updated_dat, 
             all_dat = all_dat), file = "generate_hml_id_dat.Rds")
drive_put("generate_hml_id_dat.Rds", path=drive_find(pattern="HML Data", corpus="allDrives"))

# Move the new participant_id_parent and hml_ids to the REDCap ID upload folder -------

hml_id_files <- list.files(paste0(mindcrowd_folder, "/REDCap_ID_Assignment"))

processed_ids <- do.call(rbind,
                         lapply(paste0(mindcrowd_folder, "/REDCap_ID_Assignment/", hml_id_files),
                                read.csv, header = T)
                         )

updated_ids <- updated_dat %>%
  filter(!is.na(hml_id),
         !hml_id %in% processed_ids$hml_id)

if(nrow(updated_ids) > 0){
  write.csv(updated_ids, file = paste0(mindcrowd_folder, "/HML_ID_Assignment/hml_id_data.csv"),
            row.names = F)
  
  print("Saved updated data to REDCap ID creation folder.")
}else{
  print("No updated data to save to REDCap ID creation folder.")
}



