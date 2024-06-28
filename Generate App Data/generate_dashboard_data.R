
print("######################################")
print("Generating dashboard data")
print("######################################")

rm(list = ls())

# Set file locations ---------

setwd("C:/Users/Lisa/Box/pan_dashboard_data/generate_dashboard_data/")

mindcrowd_folder <- "C:/Users/Lisa/Box/[UA BOX Health] MindCrowd Inbound"

# Load Libraries -------------

library(tidyverse)

# section to connect to Google Drive
library(googledrive)
# file with info for service account 
googledrive::drive_auth(path = "pan-mindcrowd-uploads-d9b7ecb93e53.json")

# Option to silence the messages coming from the Google Drive library
options(googledrive_quiet = TRUE)

# Wrangle data -------------------

source("mindcrowd_data_processing_functions_230322.R")

print("Loading most recent participants and memory data.")

memory_data <- read.csv(paste0(mindcrowd_folder, "/Current/memory.csv"))

mindcrowd_data <- read.csv(paste0(mindcrowd_folder, "/Current/participants.csv"))

screening_data <- create_screening_data(mindcrowd_data, memory_data, campaign_code = T)

data <- screening_data %>%
  select(-email) %>%
  mutate(area = case_when(area %in% "tucson" ~ "Tucson",
                          area %in% "miami" ~ "Miami",
                          area %in% "baltimore" ~ "Baltimore",
                          area %in% "atlanta" ~ "Atlanta"))

print("Saved screening data to Google Drive.")

save(data, file = paste0("mindcrowd_screening_data.Rdata"))
drive_put("mindcrowd_screening_data.Rdata", path=drive_find(pattern="HML Data", corpus="allDrives"))

# Save all ID data

all_screening_data <- create_screening_data(mindcrowd_data, memory_data, dag_area = "Raw")

all_screening_data <- subset(all_screening_data, 
                             select = c(participant_id_parent, participant_id, 
                                        sex, age_group, race, hispanic_latino, task_group))

print("Saved ID data to Google Drive.")

save(all_screening_data, file = paste0("all_screening_data.Rdata"))
drive_put("all_screening_data.Rdata", path=drive_find(pattern="HML Data", corpus="allDrives"))

# Save campaign code data -----------------

recruitment_zip_codes <- read.csv("recruitment_zip_codes.csv")
mindcrowd_data$area <- dplyr::case_when(mindcrowd_data$mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "tucson")] ~ "Tucson",
                                        mindcrowd_data$mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "miami")] ~ "Miami",
                                        mindcrowd_data$mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "baltimore")] ~ "Baltimore",
                                        mindcrowd_data$mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "atlanta")] ~ "Atlanta")


codes <- mindcrowd_data %>% 
  mutate(campaign_code = toupper(campaign_code)) %>%
  filter(campaign_code != "") %>%
  mutate(area = ifelse(is.na(area), "Outside Screening Areas", area)) %>%
  select(participant_id, area, campaign_code)

print("Saved MindCrowd codes.")

save(codes, file = paste0("mindcrowd_campaign_codes.Rdata"))
drive_put("mindcrowd_campaign_codes.Rdata", path=drive_find(pattern="HML Data", corpus="allDrives"))

print("######################################")
print("Completed generating dashboard data.")
print("######################################")
