# This R script generates data for the PAN Participant Tracker App


# This code is for pulling REDCap recruitment data from postgres database and saving deidentified 
# data to the Google Drive for the Participant Tracking dashboard.
# If running code from home, need to connect to UA VPN.
# Written: 28-Jan-24

rm(list = ls())

if (Sys.info()["sysname"] == "Windows") {
  setwd("D:/Precision Aging Network/pan_shiny_dashboards/Generate App Data/")
} else if (Sys.info()["sysname"] == "Linux") {
  setwd("/data/rscripts/pan_shiny_dashboards/Generate App Data/")
}

# Load libraries ---------------

library(DBI)
library(dplyr)

# section to connect to Google Drive
library(googledrive)
# file with info for service account 
googledrive::drive_auth(path = "pan-mindcrowd-uploads-d9b7ecb93e53.json")
options(googledrive_quiet = TRUE)

# Load data --------------------

con <- dbConnect(RPostgres::Postgres(),
                 user = 'pan_user',
                 password = Sys.getenv("DB_PASSWORD"),
                 dbname = 'pan_data',
                 host = 'bio5-pan-prod.cluster-c0xzlo6s7duc.us-west-2.rds.amazonaws.com',
                 port = '5432',
                 sslmode = 'verify-full',
                 sslrootcert = 'rds-ca-2019-root.pem')

demo <- dbReadTable(con, "p2_redcap_demographics") %>%
  select(record_id, hml_id, hml_id_created_date, api_import_date,
         age, decade, sex, race, hispanic_latino, group_id_number, memory_rank)  %>%
  # Create variables for app table/plots
  mutate(age_group = case_when(age %in% 50:59 ~ "50-59",
                               age %in% 60:69 ~ "60-69",
                               age %in% 70:79 ~ "70-79",
                               TRUE ~ as.character(age)),
         race_group = case_when(hispanic_latino == "Yes" ~ "Hispanic/Latino",
                                race == "White" ~ "Non-Hispanic White",
                                race == "Black or African American" ~ "Non-Hispanic Black",
                                TRUE ~ "Other"),
         memory_rank = stringr::str_to_upper(memory_rank))

phq9 <- dbReadTable(con, "p2_redcap_cognitive_function_phq9")

moca <- dbReadTable(con, "p2_redcap_cognitive_function_moca")

naart <- dbReadTable(con, "p2_redcap_cognitive_function_naart")

avlt <- dbReadTable(con, "p2_redcap_cognitive_function_avlt")

biometrics <- dbReadTable(con, "p2_redcap_biometrics")

neuropsych <- dbReadTable(con, "p2_redcap_recruitment_info") %>%
  select(hml_id, record_id, participant_id_parent = participant_id_parent_x, neuropsychology_file_upload)

base <- full_join(phq9, moca, by = c("hml_id", "record_id", "participant_id_parent")) %>%
  full_join(naart, by = c("hml_id", "record_id", "participant_id_parent")) %>%
  full_join(avlt, by = c("hml_id", "record_id", "participant_id_parent")) %>%
  full_join(biometrics, by = c("hml_id", "record_id", "participant_id_parent")) %>%
  full_join(neuropsych, by = c("hml_id", "record_id", "participant_id_parent")) %>%
  # One of the record_ids is missing a hml_id
  left_join(select(demo, record_id, hml_id) %>% rename(new_hml_id = hml_id), by = "record_id") %>%
  mutate(hml_id = ifelse(is.na(hml_id), new_hml_id, hml_id)) %>%
  select(-new_hml_id)

elig <- dbReadTable(con, "p2_redcap_eligibility") %>%
  select(record_id, hml_id, not_eligible, hif_eligible_behav, hif_eligible_mri, hif_eligible_bd) %>%
  # One of the record_ids is missing a hml_id
  left_join(select(demo, record_id, hml_id) %>% rename(new_hml_id = hml_id), by = "record_id") %>%
  mutate(hml_id = ifelse(is.na(hml_id), new_hml_id, hml_id)) %>%
  select(-new_hml_id)

cons <- dbReadTable(con, "p2_redcap_consent_form") %>%
  select(record_id, hml_id, main_consent_date, main_consent_upload, consent_form_complete) %>%
  # One of the record_ids is missing a hml_id
  left_join(select(demo, record_id, hml_id) %>% rename(new_hml_id = hml_id), by = "record_id") %>%
  mutate(hml_id = ifelse(is.na(hml_id), new_hml_id, hml_id)) %>%
  select(-new_hml_id)

# Get MindCrowd data -----------

# Load performance quartile groups
performance_groups <- read.csv("mindcrowd_performance_groups.csv")

# Load Mindcrowd participants data
participants <- dbReadTable(con, "participants") 

participants_subset <- participants %>%
  mutate(race_group = case_when(hispanic_latino == "Yes" ~ "Hispanic",
                                hispanic_latino == "No" & race == "Black or African American" ~ "Non-Hispanic Black",
                                hispanic_latino == "No" & race == "White" ~ "Non-Hispanic White",
                                TRUE ~ "Other"),
         age_group = age_decade) %>%
  select(participant_id, participant_id_parent, sex, race, race_group, hispanic_latino, age, age_group, 
         highest_education_level_completed, mailing_country) %>%
  distinct()

# Load paired associates game data
memory <- dbReadTable(con, "memory")

memory_subset <- memory %>%
  mutate(created_at = format(as.Date(created_date_game_result), "%Y-%m-%d")) %>%
  filter(game_status == "Completed") %>%
  select(participant_id, created_at, totalcorrect) %>%
  filter(!is.na(participant_id)) %>%
  distinct()

mindcrowd_data <- participants_subset %>%
  left_join(memory_subset, by = "participant_id", relationship = "many-to-many") %>%
  filter(!is.na(totalcorrect)) %>%
  # Get most recent memory game score
  group_by(participant_id_parent) %>% arrange(created_at) %>% slice(1) %>% ungroup() %>%
  # Attach performance groups quartile calculations to get paired associates quartile assignment
  mutate(age_bin = case_when(age %in% 18:29 ~ "18-29",
                             age %in% 30:49 ~ "30-49",
                             age %in% 50:64 ~ "50-64",
                             age %in% 65:74 ~ "65-74",
                             age >= 75 ~ "75+"),
         education_attainment_bin = case_when(highest_education_level_completed %in% c("Completed 8th Grade (Elementary or Primary School Graduate)", "Up to 8 Years", 
                                                                                       "Some High School", "High School Diploma (Baccalaureate)") ~ "1 & 2",
                                              highest_education_level_completed %in% c("Some College (Some University)") ~ "3",
                                              highest_education_level_completed %in% c("College Degree (University Graduate)") ~ "4",
                                              highest_education_level_completed %in% c("Post Graduate Degree (Masters or Doctorate)") ~ "5")) %>%
  left_join(performance_groups, by = c("age_bin", "sex", "education_attainment_bin")) %>%
  mutate(task_group = case_when(totalcorrect < q1 ~ "Q1",
                                totalcorrect >= q1 & totalcorrect < q2 ~ "Q2",
                                totalcorrect >= q2 & totalcorrect < q3 ~ "Q3",
                                totalcorrect >= q3 ~ "Q4")) %>%
  # Remove any no longer necessary variables
  # select(-c(age_bin, highest_education_level_completed, education_attainment_bin, n, q1, q2, q3)) %>%
  # Mark anything missing as "Missing"
  mutate(across(everything(), ~ifelse(is.na(.), "Missing", .)))

## Get screening data
source("mindcrowd_data_processing_functions_240530.R")

# Add recruitment site to participant data
site_data <- dbReadTable(con, "dm_recruitment_location") %>%
  rename(area = recruit_location)

mindcrowd_part_data <- participants %>%
  left_join(site_data, by = c("participant_id", "participant_id_parent"))

screening_data <- create_screening_data(mindcrowd_part_data, memory, campaign_code = T)

screening_data$area = stringr::str_to_title(screening_data$area)

# Consented HML data -------------

assess <- base %>%
  # Add in demo data
  full_join(select(demo, record_id, group_id_number, hml_id,
                   age_group, race_group, memory_rank, sex), by = c("record_id", "hml_id")) %>%
  # Filter for consented participants
  left_join(cons, by = c("record_id", "hml_id")) %>%
  filter(!is.na(main_consent_upload)) %>%
  # Get postgres update date
  mutate(update_date = format(Sys.Date(), "%y%m%d")) %>%
  # Create area variable
  mutate(area = case_when(group_id_number == 4669 ~ "Atlanta",
                          group_id_number == 4668 ~ "Baltimore",
                          group_id_number == 4667 ~ "Miami",
                          group_id_number == 4670 ~ "Tucson")) %>%
  # Cognitive Summaries ~~~~~~~~~~~~~~
  mutate(across(phq9_endorse_q9:avlt_trial_a7_zscore, ~ifelse(is.na(.), 1, 0))) %>%
  mutate("PHQ-9" = ifelse((phq9_endorse_q9 + phq9_total + phq9_complete) > 0, "Missing", "Non-Missing"),
         # 20-Feb-24: Removed the education bonus and mis_total variables since not used in final total:
         MoCA = ifelse(select(., moca_abstraction, starts_with("moca_attention"), moca_delay_recall, starts_with("moca_language_"),
                              moca_visuospatial, moca_naming, moca_orientation, moca_total) %>% rowSums() > 0, "Missing", "Non-Missing"),
         NAART = ifelse(select(., naart_total_correct) %>% rowSums() > 0, "Missing", "Non-Missing"),
         AVLT = ifelse(select(., starts_with("avlt_") & ends_with("_raw")) %>% rowSums() > 0, "Missing", "Non-Missing"),
         "Neuropsychology Upload" = ifelse(neuropsychology_file_upload == 1, "Missing", "Non-Missing"),
         "PHQ-9 Upload" = ifelse(phq9_file_upload == 1, "Missing", "Non-Missing")) %>%
  # Biometrics Form ~~~~~~~~~~~~~~~~~~
  mutate(across(bp_r_systolic_1:waist_hip_ratio, ~ifelse(is.na(.), 1, 0))) %>%
  mutate("Biometrics Form" = ifelse(select(., bp_r_systolic_1:waist_hip_ratio) %>% rowSums() > 0, "Missing", "Non-Missing")) %>%
  # All table variables
  mutate(across(c(`PHQ-9`, `MoCA`, `NAART`, `AVLT`, `Neuropsychology Upload`, `PHQ-9 Upload`, `Biometrics Form`), 
                ~factor(., levels = c("Non-Missing", "Missing")))) %>%
  # Get overall missingness:
  mutate(Overall = factor(ifelse(`PHQ-9` == "Non-Missing" & MoCA == "Non-Missing" & NAART == "Non-Missing" & 
                                   AVLT == "Non-Missing" & `Neuropsychology Upload` == "Non-Missing" & 
                                   `PHQ-9 Upload` == "Non-Missing" & `Biometrics Form` == "Non-Missing", "Complete Neuropsych and Biometrics", 
                                 "Incomplete Neuropsych and Biometrics"),
                          levels = c("Complete Neuropsych and Biometrics", "Incomplete Neuropsych and Biometrics"))) %>%
  select(record_id, hml_id = hml_id, area, update_date, age_group, race_group, memory_rank, sex, main_consent_date, 
         main_consent_upload, `PHQ-9`, `MoCA`, `NAART`, `AVLT`, `Neuropsychology Upload`, `PHQ-9 Upload`, `Biometrics Form`,
         Overall) %>%
  # Add in whether participant is eligible for study
  left_join(elig, by = c("record_id", "hml_id" = "hml_id")) %>%
  mutate(not_eligible = case_when(not_eligible == 0 ~"Not Eligible", 
                                  !is.na(hif_eligible_behav) ~ "Eligible"))

# Small edits to data for app ---------------

# All Mindcrowd data with paired associates tests
all_mindcrowd_data <- mindcrowd_data %>%
  rename(paired_associates = task_group) %>%
  mutate(area = ifelse(mailing_country == "United States", mailing_country, "Outside United States"),
         area = factor(area, levels = c("United States", "Outside United States")),
         # Create quarter time variable:
         day_time = as.Date(created_at, format = "%Y-%m-%d"),
         month_time = format(day_time, "%Y-%m"),
         month_name_time = format(day_time, "%Y %b"),
         quarter_time = paste0(format(day_time, "%Y"), " ", quarters(day_time)),
         year_time = format(day_time, "%Y")) %>%
  filter(sex != "Missing", age_group != "Missing", race_group != "Missing", paired_associates != "Missing",
         age_group %in% c("50-59", "60-69", "70-79")) 

# All HML ID assigned participants
hml_data <- demo %>%
  rename(paired_associates = memory_rank) %>%
  mutate(area = case_when(group_id_number == 4669 ~ "Atlanta",
                          group_id_number == 4668 ~ "Baltimore",
                          group_id_number == 4667 ~ "Miami",
                          group_id_number == 4670 ~ "Tucson"),
         day_time = ifelse(is.na(hml_id_created_date), api_import_date, hml_id_created_date),
         day_time = as.Date(day_time, format = "%Y-%m-%d"),
         month_time = as.character(format(day_time, "%Y-%m")),
         month_name_time = format(day_time, "%Y %b"),
         quarter_time = paste0(format(day_time, "%Y"), " ", quarters(day_time)),
         update_date = format(Sys.Date(), "%y%m%d"))

# All consented participants
consented <- assess %>%
  rename(paired_associates = memory_rank) %>%
  mutate(day_time = as.Date(main_consent_date, format = "%Y-%m-%d"),
         month_time = as.character(format(day_time, "%Y-%m")),
         month_name_time = format(day_time, "%Y %b"),
         quarter_time = paste0(format(day_time, "%Y"), " ", quarters(day_time)))

# Get all screening data for HML ID Assignment

all_screening_data <- create_screening_data(mindcrowd_part_data, memory, dag_area = "Raw")

all_screening_data <- subset(all_screening_data, 
                             select = c(participant_id_parent, participant_id, 
                                        sex, age_group, race, hispanic_latino, task_group))

# Save data --------------------

save(all_mindcrowd_data, all_screening_data, screening_data, hml_data, consented, file = "hml_redcap_data.Rdata")

drive_put("hml_redcap_data.Rdata", path=drive_find(pattern="HML Data", corpus="allDrives"))
