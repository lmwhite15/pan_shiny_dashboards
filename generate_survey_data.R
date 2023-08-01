
rm(list = ls())

# Set file locations ---------

setwd("C:/Users/Lisa/Box/pan_dashboard_data/generate_survey_data/")

mindcrowd_folder <- "C:/Users/Lisa/Box/[UA BOX Health] MindCrowd Inbound"

# Load Libraries -------------

library(tidyverse)
library(openxlsx)

# section to connect to Google Drive
library(googledrive)
# file with info for service account 
googledrive::drive_auth(path = "pan-mindcrowd-uploads-ddf6b0dbe662.json")

# Load functions ------------

source("survey_logic_branching.R")

source("survey_redcap_formatting.R")

# Load data ------------------

## Recruitment zone zipcodes
recruitment_zip_codes <- read.csv("recruitment_zip_codes.csv")

## Participant data ~~~~
raw_participant_data <- read.csv(paste0(mindcrowd_folder, "/Current/participants.csv")) 

## Use email to match data to survey data 
## (some participant level data doesn't show participant_id so need to use email, hopefully temporary?)
participant_data <- raw_participant_data %>%
  mutate(email = toupper(email),
         area = case_when(mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "tucson")] ~ "tucson",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "miami")] ~ "miami",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "baltimore")] ~ "baltimore",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "atlanta")] ~ "atlanta")) %>%
  select(participant_id, participant_id_parent, area, email, created_date_participant)

## Survey Data ~~~~~

files_dates <- list.files(mindcrowd_folder)
files_dates <- unique(str_sub(files_dates[grep(".csv", files_dates)], end = 10))
files_dates <- files_dates[grep("20", files_dates)]

most_recent_update <- files_dates[order(files_dates, decreasing = T)][1]

names <- c("adl", "anxiety", "brain_disease", "covid", "diet", "fhad", 
           "health_medical", "perceived_stress", "qpar", "ses", "sleep",
           "social_stressor", "social_support", "subjective_english", "swls")

files <- paste0(mindcrowd_folder, "/", most_recent_update, names, ".csv.gz")

raw_files_list <- lapply(files, 
                         function(x){read.csv(x) %>% mutate_all(~ifelse(. == "", NA, .))})

## HML Recruited Participant IDs
## Using csv from New_HML_IDs folder in Box
hml_id_files <- list.files(paste0(mindcrowd_folder, "/New_HML_IDs"))
hml_id_files <- hml_id_files[-which(hml_id_files == "log")]
hml_id_files <- hml_id_files[order(hml_id_files, decreasing = TRUE)]

hml_ids <- read.csv(paste0(mindcrowd_folder, "/New_HML_IDs/", hml_id_files[1])) %>%
  rename(record_id = REDCap_id, 
         study_id = HML_id) %>%
  filter(!str_detect(record_id, "TEST"))

## Match participant data to hml_ids
hml_participant_data <- hml_ids %>%
  select(record_id, study_id, participant_id_parent) %>%
  left_join(participant_data, by = "participant_id_parent") %>%
  group_by(participant_id_parent) %>%
  arrange(desc(created_date_participant)) %>% slice(1) %>% ungroup() %>% select(-created_date_participant) %>%
  mutate(area = ifelse(is.na(area), "tucson", area))

# Format survey data ----------------------

# Capitalize all email addresses to make matching easier and select most recent survey
# Match surveys to participant data within recruitment areas

names(raw_files_list) <- names

files_list <- lapply(raw_files_list, function(x){
  new_x <- x %>%
    mutate(email = toupper(email)) %>%
    group_by(email) %>% 
    arrange(desc(created_date_survey)) %>% 
    slice(1) %>% 
    ungroup() %>%
    rename(survey_participant_id = participant_id) %>%
    right_join(hml_participant_data, by = "email") %>%
    mutate(survey_participant_id = ifelse(survey_participant_id == "" | is.na(survey_participant_id),
                                          participant_id_parent, 
                                          survey_participant_id)) %>%
    mutate(survey_participant_id = str_replace(survey_participant_id, ".*(?=0{5})", ""),
           survey_participant_id = str_replace(survey_participant_id, "0{5}", "")) %>%
    select(record_id, study_id, area, everything(),
           -c(survey_id, email, contains("participant_id"))) %>%
    mutate(across(-created_date_survey, ~ifelse(!is.na(created_date_survey) & is.na(.), "", .))) %>%
    mutate(created_date_survey = as.Date(created_date_survey))
  if(sum(colnames(new_x) %in% c("Last Modified Date", "Not Listed", "Not.Listed", "Last.Modified.Date")) > 0){
    new_x <- new_x[-which(colnames(new_x) %in% c("Last Modified Date", "Not Listed", "Not.Listed", "Last.Modified.Date"))]
  }
  colnames(new_x)[which(colnames(new_x) == "created_date_survey")] <- paste0(str_replace(colnames(new_x)[ncol(new_x)], "_.*", ""), "_", "timestamp")
  
  new_x <- new_x %>%
    select(record_id, study_id, area, contains("_timestamp"),
           paste0(str_extract(names(.)[ncol(.)], ".*(?=_)"), "_v1.0.", 1:(ncol(.)-4))) %>%
    arrange(as.numeric(record_id))
  
  new_x
})

# Load and format data dictionary set -----------------

data_dictionary_file <- "Survey Info Document.xlsx"

sheet_names <- getSheetNames(data_dictionary_file)

survey_data_dictionary <- NULL

for(sheet in sheet_names){
  sheet_dictionary <- read.xlsx(data_dictionary_file, sheet = sheet) %>%
    rename(item = 1, question = 2, type = 3) %>%
    filter(str_detect(item, "_")) %>%
    mutate(question = str_replace(question, "__c", ""),
           survey = sheet)
  
  survey_data_dictionary <- rbind(survey_data_dictionary, sheet_dictionary)
}

# Check logic branches to determine whether data is actually missing or just not applicable
files_list  <- survey_logic_branching(files_list)

# Formatting for REDCap import -----------

redcap_files_list <- lapply(files_list, function(x){
  completed_x <- x %>%
    mutate(across(-c(record_id, ends_with("_timestamp")), 
                  ~ifelse(is.na(.) | . == "", 1, 0))) %>%
    mutate(missing = select(., -c(record_id, ends_with("_timestamp"))) %>%
             rowSums(na.rm = T),
           completed = ifelse(missing == 0, "1", "0")) %>%
    rename(!!paste0(str_extract(names(.)[ncol(.)-4], ".*(?=_)"), "_complete") := completed) %>%
    select(record_id, ends_with("_complete"))
  
  new_x <- x %>%
    left_join(completed_x, by = "record_id") %>%
    select(record_id, study_id,
           contains("_timestamp"),
           paste0(str_extract(names(.)[ncol(.)], ".*(?=_)"), "_v1.0.", 1:(ncol(.)-5)),
           ends_with("_complete")) %>%
    rename_all(~tolower(str_replace_all(., "\\.", ""))) %>%
    arrange(as.numeric(record_id))
  
  new_x
})

redcap_data <- redcap_formatting(redcap_files_list)

# Save files -------------

files_list <- lapply(files_list, function(x){
  new_x <- x %>% select(-record_id)
})

save(names, files_list, survey_data_dictionary, redcap_data,
     file = "pan_survey_files_list.Rdata")

drive_put("pan_survey_files_list.Rdata", path=drive_find(pattern="HML Data", corpus="allDrives"))

print("Saved Survey Dataset!")

