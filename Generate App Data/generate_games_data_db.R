
print("######################################")
print("Generating games data.")
print("######################################")

# Pull games data and create data frame for Games dashboard

rm(list = ls())

# Set file locations ---------

setwd("C:/Users/Lisa/Box/pan_dashboard_data/generate_games_data/")

mindcrowd_folder <- "C:/Users/Lisa/Box/[UA BOX Health] MindCrowd Inbound"

# Load libraries --------------

library(DBI) # Connecting to database
library(tidyverse)
library(openxlsx)

# section to connect to Google Drive
library(googledrive)
# file with info for service account 
googledrive::drive_auth(path = "pan-mindcrowd-uploads-d9b7ecb93e53.json")

# Option to silence the messages coming from the Google Drive library
options(googledrive_quiet = TRUE)

con <- dbConnect(RPostgres::Postgres(),
                 user = 'pan_user',
                 password = 'PANdataBase!',
                 dbname = 'pan_data',
                 host = 'bio5-pan-prod.cluster-c0xzlo6s7duc.us-west-2.rds.amazonaws.com',
                 port = '5432',
                 sslmode = 'verify-full',
                 sslrootcert = 'rds-ca-2019-root.pem')


# Load data --------------------

print("Loading current participants data.")

recruitment_zip_codes <- read.csv("recruitment_zip_codes.csv")

participant_data <- dbReadTable(con, "participants") %>%
  mutate(area = case_when(mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "tucson")] ~ "tucson",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "miami")] ~ "miami",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "baltimore")] ~ "baltimore",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "atlanta")] ~ "atlanta")) %>%
  select(participant_id, participant_id_parent, area, created_date_participant, email)
## Load results data

# 08-Feb-24: Most recent files no longer contain update date
# names <- list.files(mindcrowd_folder)
# 
# files_dates <- unique(str_sub(names[grep(".csv", names)], end = 10))
# files_dates <- files_dates[grep("20", files_dates)]
#
# most_recent_update <- files_dates[order(files_dates, decreasing = T)][1]
# 
# names <- str_replace(names[which(str_detect(names, "responses") & str_detect(names, most_recent_update))], "_responses", "")
# names <- str_sub(names, start = 11, end = -8)
# names <- names[-which(names == "memory")]
# 
# files <- paste0(mindcrowd_folder, "/", most_recent_update, names, ".csv.gz")

names <- c("attention", "faces_names", "focus", "keep_track", "objects", "objects_spatial", 
           "objects_temporal", "react", "shapes", "switching", "word_pairs")

raw_files_list <- lapply(names, 
                         function(x){dbReadTable(con, x)})

## Load responses data

response_raw_files_list <- lapply(names, 
                                  function(x){dbReadTable(con, paste0(x, "_responses"))})

## HML Recruited Participant IDs -------------
## Using csv from REDCap ID Assignment folder in Box
hml_id_files <- list.files(paste0(mindcrowd_folder, "/REDCap_ID_Assignment"))

hml_ids <- do.call(rbind, 
                   lapply(paste0(mindcrowd_folder, "/REDCap_ID_Assignment/", hml_id_files), 
                          read.csv)) %>%
  rename(record_id = redcap_record_id, 
         study_id = hml_id) %>%
  mutate(record_id = as.character(record_id))

## Match participant data to hml_ids
hml_participant_data <- hml_ids %>%
  left_join(participant_data, by = "participant_id_parent",
            relationship = "many-to-many") %>%
  # Replace any times with missing area observations with another time area
  group_by(participant_id_parent) %>% fill(area, .direction = "downup") %>% ungroup() %>%
  # # Select most recent survey for each participant
  # group_by(participant_id_parent) %>%
  # arrange(desc(created_date_participant)) %>% slice(1) %>% ungroup() %>% 
  select(-created_date_participant) %>%
  mutate(participant_id = ifelse(is.na(participant_id), participant_id_parent, participant_id))

# Attach missing areas/emails to participants outside of recruitment zone ---------

hml_id_area_files <- list.files(paste0(mindcrowd_folder, "/HML_ID_Assignment/Archived_Files"))

hml_id_areas <- do.call(rbind, 
                        lapply(paste0(mindcrowd_folder, "/HML_ID_Assignment/Archived_Files/", hml_id_area_files), 
                               function(x){read.csv(x) %>% select(hml_id, area)})) %>%
  rename(study_id = hml_id) %>% distinct() %>% mutate(area = tolower(area))

# Add missing areas

missing_area_ids <- filter(hml_participant_data, is.na(area)) %>%
  select(-area) %>% left_join(hml_id_areas, by = "study_id")

hml_participant_data <- hml_participant_data %>%
  filter(!is.na(area)) %>% rbind(missing_area_ids)

# Add missing emails

missing_emails <- filter(hml_participant_data, is.na(email)) %>%
  select(-email) %>% left_join(select(participant_data, participant_id, email), by = "participant_id")

hml_participant_data <- hml_participant_data %>%
  filter(!is.na(email)) %>% rbind(missing_emails)

# Format games data ----------------------

# Capitalize all email addresses to make matching easier and select most recent game
# Match games to participant data within recruitment areas

names(raw_files_list) <- names

files_list <- lapply(raw_files_list, function(x){
  if(sum(str_detect(names(x), "created_date_game_session") > 0)){
    names(x)[which(names(x) == "created_date_game_session")] <- "created_date_game_result"
  }
  
  # Select most recent game
  recent_x <- x %>%
    group_by(participant_id) %>% arrange(desc(created_date_game_result)) %>% 
    slice(1) %>% ungroup()
  
  new_x <- hml_participant_data %>%
    left_join(recent_x, by = "participant_id") %>%
    select(study_id, area, everything(), -email) %>%
    mutate(across(-created_date_game_result, ~ifelse(!is.na(created_date_game_result) & is.na(.), "", .))) %>%
    mutate(created_date_game_result = as.Date(created_date_game_result))
  
  game_name <- new_x$game_name[which(!is.na(new_x$game_name))][1] %>% 
    str_to_lower() %>% str_replace(" ", "\\_") %>% str_remove_all("[^[:alpha:]|\\_]")
  
  game_name <- case_when(game_name == "objects_space" ~ "objects_spatial",
                         game_name == "objects_time" ~ "objects_temporal",
                         TRUE ~ game_name)
  
  names(new_x)[which(names(new_x) == "game_status")] <- paste0(game_name, "_game_status")
  names(new_x)[which(names(new_x) == "created_date_game_result")] <- paste0(game_name, "_", "timestamp")
  
  new_x
})

# Format responses data for REDCap import ------------

# print("Formatting games data for REDcap.")

names(response_raw_files_list) <- names

response_files_list <- response_raw_files_list[c("word_pairs", "keep_track", "shapes", "faces_names", "focus", "switching", "react")]

## Word Pairs

response_files_list[["word_pairs"]] <- files_list[["word_pairs"]] %>%
  select(study_id, participant_id, game_result) %>%
  left_join(response_files_list[["word_pairs"]] %>% filter(!is.na(game_result)), by = "game_result",
            relationship = "many-to-many") %>%
  mutate(question_number = NA) %>% # because "question_number" not found in word pairs data
  select(study_id, participant_id, game_result, created_date_game_response = created_date_game_result,
         WP_round = round,
         WP_trial_type = difficulty,
         WP_trial = question_number, 
         WP_stimuli = stimuli,
         WP_correct_resp = right_answer,
         WP_response = response,
         WP_correct = is_correct,
         WP_rt = rt,
         WP_time_to_first_key = time_to_first_keystroke) %>%
  mutate(WP_trial_type = case_when(WP_trial_type == "easy" ~ 1,
                                   WP_trial_type == "hard" ~ 2),
         WP_correct = case_when(WP_correct == "True" ~ 1,
                                WP_correct == "False" ~ 2))

## Keep Track

response_files_list[["keep_track"]] <- files_list[["keep_track"]] %>%
  select(study_id, participant_id, game_result) %>%
  left_join(response_files_list[["keep_track"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(study_id, participant_id, game_result, created_date_game_response,
         KT_round = round,
         KT_trial = question_number,
         KT_correct_resp = right_answer,
         KT_response = response,
         KT_correct = is_correct,
         KT_rt = rt)

## Shapes

response_files_list[["shapes"]] <- files_list[["shapes"]] %>%
  select(study_id, participant_id, game_result) %>%
  left_join(response_files_list[["shapes"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(study_id, participant_id, game_result, created_date_game_response,
         S_round = round,
         S_trial = question_number,
         S_condition = difficulty,
         S_correct = is_correct,
         image_name,
         S_correct_resp = right_answer,
         S_rt = rt,
         S_trial_type = shape) %>%
  separate(image_name, c("S_image_left", "S_image_right"), sep = ",") %>%
  mutate(S_condition = case_when(S_condition == "hard" ~ "HA",
                                 S_condition == "easy" ~ "LA"),
         S_correct = case_when(S_correct == "True" ~ 1,
                               S_correct == "False" ~ 2),
         S_image_left = str_remove(str_sub(S_image_left, end = -2), "\\{\"leftImage\":\""),
         S_image_right = str_remove(str_sub(S_image_right, end = -3), "\"rightImage\":\""),
         S_correct_resp = case_when(S_correct_resp == "f" ~ "match",
                                    S_correct_resp == "j" ~ "non-match"))

## Face Names

response_files_list[["faces_names"]] <- files_list[["faces_names"]] %>%
  select(study_id, participant_id, game_result) %>%
  left_join(response_files_list[["faces_names"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(study_id, participant_id, game_result, created_date_game_response,
         FN_round = round,
         FN_trial = question_number,
         FN_trial_type = question_type,
         FN_correct_resp = right_answer,
         FN_response = response,
         FN_correct = is_correct,
         FN_rt = rt,
         FN_total_time = total_time_answers,
         FN_time_to_first_key = first_input_time) %>%
  mutate(FN_trial_type = case_when(FN_trial_type == "name" ~ 0,
                                   FN_trial_type == "occupation" ~ 1),
         FN_correct = case_when(FN_correct == "True" ~ 1,
                                FN_correct == "False" ~ 2))

## Focus

response_files_list[["focus"]] <- files_list[["focus"]] %>%
  select(study_id, participant_id, game_result) %>%
  left_join(response_files_list[["focus"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(study_id, participant_id, game_result, created_date_game_response,
         F_round = round,
         F_trial_type = congruent,
         F_trial = question_number,
         F_correct_resp = right_answer,
         F_response = response,
         F_correct = is_correct,
         F_RT = rt) %>%
  mutate(F_trial_type = case_when(F_trial_type == "True" ~ 1,
                                  F_trial_type == "False" ~ 2),
         F_correct = case_when(F_correct == "True" ~ 1,
                               F_correct == "False" ~ 2))

## Switching

response_files_list[["switching"]] <- files_list[["switching"]] %>%
  select(study_id, participant_id, game_result) %>%
  left_join(response_files_list[["switching"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(study_id, participant_id, game_result, created_date_game_response,
         SW_round = round,
         SW_trial_type = round_descriptive,
         SW_trial = question_number,
         SW_quadrant = quadrant,
         SW_stimuli = stimuli,
         SW_correct = is_correct,
         SW_rt = rt) %>%
  mutate(SW_correct = case_when(SW_correct == "True" ~ 1,
                                SW_correct == "False" ~ 2))

## React

response_files_list[["react"]] <- files_list[["react"]] %>%
  select(study_id, participant_id, game_result) %>%
  left_join(response_files_list[["react"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(study_id, participant_id, game_result, created_date_game_response,
         R_round = round,
         R_trial_type = trial_type,
         R_trial = question_number,
         R_correct_resp = right_answer,
         R_correct = is_correct,
         R_rt = rt,
         R_ISI= isi) %>%
  mutate(R_trial_type = case_when(R_trial_type == "simple" ~ 0,
                                  R_trial_type == "complex" ~ 1),
         R_correct = case_when(R_correct == "True" ~ 1,
                               R_correct == "False" ~ 2))

# Save data ----------

redcap_data <- response_files_list

games_update_date <- format(Sys.Date(), "%b %d, %Y")
games_update_date <- "Jun 28, 2024"

save(games_update_date, names, files_list, redcap_data, file = paste0("pan_games_files_list.Rdata"))

drive_put("pan_games_files_list.Rdata", path=drive_find(pattern="HML Data", corpus="allDrives"))

print("Saved games data to Google Drive.")

print("######################################")
print("Completed generating games data.")
print("######################################")
