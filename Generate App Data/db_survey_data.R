
print("######################################")
print("Generating survey data")
print("######################################")

rm(list = ls())

if (Sys.info()["sysname"] == "Windows") {
  setwd("D:/Precision Aging Network/pan_shiny_dashboards/Generate App Data/")
} else if (Sys.info()["sysname"] == "Linux") {
  setwd("/data/rscripts/pan_shiny_dashboards/Generate App Data/")
}

library(DBI) # Connecting to database
library(tidyverse)
library(openxlsx)
# section to connect to Google Drive
library(googledrive)
# file with info for service account 
googledrive::drive_auth(path = "pan-mindcrowd-uploads-d9b7ecb93e53.json")

# Load functions ------------

source("survey_logic_branching.R")

source("survey_redcap_formatting.R")

# Load data ------------------

con <- dbConnect(RPostgres::Postgres(),
                 user = 'pan_user',
                 password = Sys.getenv("DB_PASSWORD"),
                 dbname = 'pan_data',
                 host = 'bio5-pan-prod.cluster-c0xzlo6s7duc.us-west-2.rds.amazonaws.com',
                 port = '5432',
                 # sslmode = 'verify-full',
                 sslrootcert = 'global-bundle.pem')

## Participant data ~~~~
print("Loading current participants data.")

# HML IDs and hashed emails
hml_dat <- dbReadTable(con, "views_hml_match")

# Record IDs
record_id_dat <- dbReadTable(con, "redcap_id_assignment") 

# Recruitment Sites
site_dat <- dbReadTable(con, "info_hml_id_data") %>% 
  select(hml_id, area)

# Set up participant data
participant_data <- hml_dat %>%
  # Add record_ids
  left_join(record_id_dat, by = c("hml_id", "participant_id_parent")) %>%
  # Add recruitment sites
  left_join(site_dat, by = c("hml_id")) %>%
  select(record_id = redcap_record_id, hml_id, area, email) %>%
  mutate(area = tolower(area))

## Survey Data ~~~~~

names <- c("adl", "anxiety", "brain_disease", "covid", "diet", "fhad", 
           "health_medical", "perceived_stress", "qpar", "ses", "sleep",
           "social_stressor", "social_support", "subjective_english", "swls")

raw_files_list <- lapply(names, 
                         function(x){dbReadTable(con, x) %>% mutate_all(~ifelse(. == "", NA, .))})

names(raw_files_list) <- names

# Format survey data ----------------------

# Capitalize all email addresses to make matching easier and select most recent survey
# Match surveys to participant data within recruitment areas

files_list <- lapply(raw_files_list, function(x){
  new_x <- participant_data %>%
    filter(!is.na(email)) %>%
    left_join(x, by = "email") %>%
    select(record_id, hml_id, area, everything(),
           -c(survey_id, email, contains("participant_id"))) %>%
    mutate(across(-c(created_date_survey, record_id), ~ifelse(!is.na(created_date_survey) & is.na(.), "", .))) %>%
    mutate(created_date_survey = as.Date(created_date_survey)) %>%
    # Select most recent survey for each participant
    group_by(record_id) %>%
    arrange(desc(created_date_survey)) %>% slice(1) %>% ungroup()
  if(sum(colnames(new_x) %in% c("Last Modified Date", "Not Listed", "Not.Listed", "Last.Modified.Date")) > 0){
    new_x <- new_x[-which(colnames(new_x) %in% c("Last Modified Date", "Not Listed", "Not.Listed", "Last.Modified.Date"))]
  }
  
  colnames(new_x)[which(colnames(new_x) == "created_date_survey")] <- paste0(str_replace(colnames(new_x)[ncol(new_x)], "_.*", ""), "_", "timestamp")
  
  survey_name <- names(new_x)[str_detect(names(new_x), "_v1.")][1]
  survey_name <- str_extract(survey_name, ".*(?=_)")
  item_list <- names(new_x)[which(str_detect(names(new_x), survey_name) & !endsWith(names(new_x), "timestamp"))]
  max_item_number <- max(as.numeric(str_extract(item_list, "(?<=\\.)\\d+$")))
  
  new_x <- new_x %>%
    select(record_id, hml_id, area, contains("_timestamp"),
           paste0(survey_name, "_v1.0.", 1:max_item_number)) %>%
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

print("Formatting data for REDCap.")

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
    mutate(record_id = as.character(record_id)) %>%
    select(record_id, hml_id,
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

batch_data <- dbReadTable(con, "batch_id") %>%
  # Remove updates from HML ID script
  mutate(last_digits = str_sub(batchid, start = -3)) %>%
  filter(last_digits != 999) %>%
  # Get latest update date
  filter(batchid == max(batchid))

latest_data_date <- as.Date(batch_data$timestamp[1], format = "%m/%d/%Y") %>%
  format("%b %d, %Y")

survey_update_date <- format(Sys.Date(), "%b %d, %Y")

print("Saving survey files to Google Drive.")

save(names, latest_data_date, survey_update_date, files_list, survey_data_dictionary, redcap_data,
     file = "pan_survey_files_list.Rdata")

drive_put("pan_survey_files_list.Rdata", 
          path=drive_find(pattern="HML Data", corpus="allDrives"))

print("######################################")
print("Completed generating survey data")
print("######################################")

