
# Format responses data for REDCap import ------------

## Word Pairs

response_files_list[["word_pairs"]] <- files_list[["word_pairs"]] %>%
  filter(hml_id == picked_id) %>%
  filter(word_pairs_game_status != "Date Out of Range") %>%
  select(hml_id, participant_id_parent, game_result) %>%
  left_join(response_files_list[["word_pairs"]] %>% filter(!is.na(game_result)), by = "game_result",
            relationship = "many-to-many") %>%
  mutate(question_number = NA) %>% # because "question_number" not found in word pairs data
  select(hml_id, participant_id_parent, game_result, created_date_game_response = created_date_game_result,
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
                                WP_correct == "False" ~ 2)) %>%
  mutate(across(everything(), as.character))

## Keep Track

response_files_list[["keep_track"]] <- files_list[["keep_track"]] %>%
  filter(hml_id == picked_id) %>%
  filter(keep_track_game_status != "Date Out of Range") %>%
  select(hml_id, participant_id_parent, game_result) %>%
  left_join(response_files_list[["keep_track"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(hml_id, participant_id_parent, game_result, created_date_game_response,
         KT_round = round,
         KT_trial = question_number,
         KT_correct_resp = right_answer,
         KT_response = response,
         KT_correct = is_correct,
         KT_rt = rt) %>%
  mutate(across(everything(), as.character))

## Shapes

response_files_list[["shapes"]] <- files_list[["shapes"]] %>%
  filter(hml_id == picked_id) %>%
  filter(shapes_game_status != "Date Out of Range") %>%
  select(hml_id, participant_id_parent, game_result) %>%
  left_join(response_files_list[["shapes"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(hml_id, participant_id_parent, game_result, created_date_game_response,
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
                                    S_correct_resp == "j" ~ "non-match")) %>%
  mutate(across(everything(), as.character))

## Face Names

response_files_list[["faces_names"]] <- files_list[["faces_names"]] %>%
  filter(hml_id == picked_id) %>%
  filter(faces_names_game_status != "Date Out of Range") %>%
  select(hml_id, participant_id_parent, game_result) %>%
  left_join(response_files_list[["faces_names"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(hml_id, participant_id_parent, game_result, created_date_game_response,
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
                                FN_correct == "False" ~ 2)) %>%
  mutate(across(everything(), as.character))

## Focus

response_files_list[["focus"]] <- files_list[["focus"]] %>%
  filter(hml_id == picked_id) %>%
  filter(focus_game_status != "Date Out of Range") %>%
  select(hml_id, participant_id_parent, game_result) %>%
  left_join(response_files_list[["focus"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(hml_id, participant_id_parent, game_result, created_date_game_response,
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
                               F_correct == "False" ~ 2)) %>%
  mutate(across(everything(), as.character))

## Switching

response_files_list[["switching"]] <- files_list[["switching"]] %>%
  filter(hml_id == picked_id) %>%
  filter(switching_game_status != "Date Out of Range") %>%
  select(hml_id, participant_id_parent, game_result) %>%
  left_join(response_files_list[["switching"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(hml_id, participant_id_parent, game_result, created_date_game_response,
         SW_round = round,
         SW_trial_type = round_descriptive,
         SW_trial = question_number,
         SW_quadrant = quadrant,
         SW_stimuli = stimuli,
         SW_correct = is_correct,
         SW_rt = rt) %>%
  mutate(SW_correct = case_when(SW_correct == "True" ~ 1,
                                SW_correct == "False" ~ 2)) %>%
  mutate(across(everything(), as.character))

## React

response_files_list[["react"]] <- files_list[["react"]] %>%
  filter(hml_id == picked_id) %>%
  filter(react_game_status != "Date Out of Range") %>%
  select(hml_id, participant_id_parent, game_result) %>%
  left_join(response_files_list[["react"]], by = "game_result",
            relationship = "many-to-many") %>%
  select(hml_id, participant_id_parent, game_result, created_date_game_response,
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
                               R_correct == "False" ~ 2)) %>%
  mutate(across(everything(), as.character))