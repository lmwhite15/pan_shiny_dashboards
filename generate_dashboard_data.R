
rm(list = ls())

# Set file locations ---------

setwd("C:/Users/Lisa/Box/pan_dashboard_data/generate_dashboard_data/")

mindcrowd_folder <- "C:/Users/Lisa/Box/[UA BOX Health] MindCrowd Inbound"

# Load Libraries -------------

library(tidyverse)

source("mindcrowd_data_processing_functions_230322.R")

memory_data <- read.csv(paste0(mindcrowd_folder, "/Current/memory.csv"))

mindcrowd_data <- read.csv(paste0(mindcrowd_folder, "/Current/participants.csv"))

screening_data <- create_screening_data(mindcrowd_data, memory_data, campaign_code = T)

data <- screening_data %>%
  select(-email) %>%
  mutate(area = case_when(area %in% "tucson" ~ "Tucson",
                          area %in% "miami" ~ "Miami",
                          area %in% "baltimore" ~ "Baltimore",
                          area %in% "atlanta" ~ "Atlanta"))

save(data, file = paste0("PAN Pre-Screening Dashboard/mindcrowd_screening_data.Rdata"))

# Save campaign code data

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

save(codes, 
     file = paste0("PAN Pre-Screening Dashboard/mindcrowd_campaign_codes.Rdata"))

print("Saved Pre-Screening Dataset!")
