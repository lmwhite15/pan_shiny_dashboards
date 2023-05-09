
# Pull games data and create data frame for Games dashboard

rm(list = ls())

# Load libraries --------------

library(tidyverse)
library(openxlsx)

# Load data --------------------

# system_date <- format(Sys.Date(), "%Y-%m-%d")

mindcrowd_folder <- paste0("C:/Users/Lisa/Box/[UA BOX Health] MindCrowd Inbound")

participant_data <- read.csv(paste0(mindcrowd_folder, "/Current/participants.csv")) 

names <- list.files(mindcrowd_folder)

## Load results data

files_dates <- unique(str_sub(names[grep(".csv", names)], end = 10))
files_dates <- files_dates[grep("20", files_dates)]

most_recent_update <- files_dates[order(files_dates, decreasing = T)][1]

names <- str_replace(names[which(str_detect(names, "responses") & str_detect(names, most_recent_update))], "_responses", "")
names <- str_sub(names, start = 11, end = -8)
names <- names[-which(names == "memory")]

files <- paste0(mindcrowd_folder, "/", most_recent_update, names, ".csv.gz")

raw_files_list <- lapply(files, 
                         function(x){read.csv(x)})

## Load responses data

response_files <- paste0(mindcrowd_folder, "/", most_recent_update, names, "_responses.csv.gz")

response_raw_files_list <- lapply(response_files, 
                                  function(x){read.csv(x)})

# Set up participant data --------------

recruitment_zip_codes <- read.csv("recruitment_zip_codes.csv")

participant_data$area <- dplyr::case_when(participant_data$mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "tucson")] ~ "tucson",
                                          participant_data$mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "miami")] ~ "miami",
                                          participant_data$mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "baltimore")] ~ "baltimore",
                                          participant_data$mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "atlanta")] ~ "atlanta")

participant_data_subset <- participant_data %>%
  mutate(email = toupper(email),
         area = case_when(mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "tucson")] ~ "tucson",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "miami")] ~ "miami",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "baltimore")] ~ "baltimore",
                          mailing_postalcode %in% recruitment_zip_codes$zip_code[which(recruitment_zip_codes$recruit_location == "atlanta")] ~ "atlanta")) %>%
  filter(contactable %in% c(TRUE, "True"), unsubscribed %in% c(FALSE, "False"), !is.na(area)) %>%
  group_by(participant_id_parent) %>%
  arrange(desc(created_date_participant)) %>% slice(1) %>% ungroup() %>%
  select(participant_id, participant_id_parent, email, area)

# Format games data ----------------------

# Capitalize all email addresses to make matching easier and select most recent game
# Match games to participant data within recruitment areas

names(raw_files_list) <- names

files_list <- lapply(raw_files_list, function(x){
  if(sum(str_detect(names(x), "created_date_game_session") > 0)){
    names(x)[which(names(x) == "created_date_game_session")] <- "created_date_game_result"
  }
  
  new_x <- x %>%
    filter(!is.na(participant_id)) %>%
    right_join(participant_data_subset %>% 
                 select(participant_id_parent, area), 
               by = c("participant_id" = "participant_id_parent")) %>%
    group_by(participant_id) %>%
    arrange(desc(created_date_game_result)) %>%
    slice(1) %>% ungroup() %>%
    mutate(participant_id = str_replace(participant_id, ".*(?=0{5})", ""),
           participant_id = str_replace(participant_id, "0{5}", "")) %>%
    select(participant_id, area, everything()) %>%
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

names(response_raw_files_list) <- names

response_files_list <- response_raw_files_list[c("word_pairs", "keep_track", "shapes", "faces_names", "focus", "switching", "react")]

## Word Pairs

response_files_list[["word_pairs"]] <- files_list[["word_pairs"]] %>%
  select(participant_id, game_result) %>%
  left_join(response_files_list[["word_pairs"]], by = "game_result") %>%
  mutate(question_number = NA) %>% # "question_number" not found in word pairs data
  select(participant_id, game_result, created_date_game_response = created_date_game_result,
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
  select(participant_id, game_result) %>%
  left_join(response_files_list[["keep_track"]], by = "game_result") %>%
  select(participant_id, game_result, created_date_game_response,
         KT_round = round,
         KT_trial = question_number,
         KT_correct_resp = right_answer,
         KT_response = response,
         KT_correct = is_correct,
         KT_rt = rt)

## Shapes

response_files_list[["shapes"]] <- files_list[["shapes"]] %>%
  select(participant_id, game_result) %>%
  left_join(response_files_list[["shapes"]], by = "game_result") %>%
  select(participant_id, game_result, created_date_game_response,
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
  select(participant_id, game_result) %>%
  left_join(response_files_list[["faces_names"]], by = "game_result") %>%
  select(participant_id, game_result, created_date_game_response,
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
  select(participant_id, game_result) %>%
  left_join(response_files_list[["focus"]], by = "game_result") %>%
  select(participant_id, game_result, created_date_game_response,
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
  select(participant_id, game_result) %>%
  left_join(response_files_list[["switching"]], by = "game_result") %>%
  select(participant_id, game_result, created_date_game_response,
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
  select(participant_id, game_result) %>%
  left_join(response_files_list[["react"]], by = "game_result") %>%
  select(participant_id, game_result, created_date_game_response,
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

save(names, files_list, redcap_data,
     file = paste0("Games Dashboard/pan_games_files_list.Rdata"))

print("Saved Games Dataset!")
