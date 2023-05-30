
# Notes:
# These are by site and we should have either a tab at the top or at the side of the app 
# that goes specifically to the site.
# 
# The App should have a drop-down that would have the mindcrowd IDs (parent) and the last 8 
# characters of the MindCrowd ID (I believe this is how we built out the survey app correct?) 
# and then show for that ID Sex and Age Group (as a check that they selected the right 
# MindCrowd ID. Once they select a MindCrowd ID we could ask for confirmation that this is the 
# right person, and that this person has passed all screening criteria and are eligible for the 
# study? once they confirm this we will then assign them an HML ID which should be: HML-XXXX with 
# XXXX being a number from 1 to 1700. We will need to store the MindCrowd and HML IDs in a file.

rm(list = ls())

# Load Libraries -------------

library(tidyverse)

# Load recruitment lists ---------

atl_dat <- read.csv("C:/Users/Lisa/Box/For Lisa/atlanta_recruitment_list_export.csv")

bal_dat <- read.csv("C:/Users/Lisa/Box/For Lisa/atlanta_recruitment_list_export.csv")

mia_dat <- read.csv("C:/Users/Lisa/Box/For Lisa/atlanta_recruitment_list_export.csv")

tuc_dat <- read.csv("C:/Users/Lisa/Box/For Lisa/atlanta_recruitment_list_export.csv")

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
  select(participant_id, part_id, area, age_group, sex)

# Save data -------------

save(dat, file = "ID Assignment Dashboard/deidentified_id_data.Rdata")

