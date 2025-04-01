
# Get data update date --------------

# Get date data were updated (to display in sidebar)
batch_data <- dbReadTable(con, "batch_id") %>%
  # Get latest update date
  mutate(new_timestamp = as.Date(timestamp, format = "%m/%d/%Y")) %>%
  filter(new_timestamp == max(new_timestamp))

latest_data_date <- as.Date(batch_data$timestamp[1], format = "%m/%d/%Y") %>%
  format("%b %d, %Y")

# Get participant data --------------------

# HML ID created dates, ids, and recruitment site
info_hml_id_data <- dbReadTable(con, "info_hml_id_data") %>%
  select(hml_id, hml_id_created_date, participant_id_parent = participant_id, site = area)

## Get secondary participant_ids
participant_id_dat <- dbReadTable(con, "views_all_ids") %>%
  select(hml_id, participant_id = participant_ids) %>%
  separate_rows(participant_id)

# Get eligibility date
participant_elig_dat <- dbReadTable(con, "p2_redcap_consent_form") %>%
  select(hml_id, main_consent_date)

# Date buffer for filtering games +/- days around participant consent date
date_buffer <- 90

# HML ID app created date
hml_app_start_date <- as.Date("2023-10-01")

# Combine id data and generate filtering dates for games
participant_data <- info_hml_id_data %>%
  left_join(participant_id_dat, by = "hml_id") %>%
  left_join(participant_elig_dat, by = "hml_id") %>%
  mutate(
    # A date was saved in an incorrect format:
    hml_id_created_date = ifelse(hml_id_created_date == "9/5/24", "2024-09-05", hml_id_created_date),
    hml_id_created_date = as.Date(hml_id_created_date),
    main_consent_date = as.Date(main_consent_date),
    filter_date = as.Date(case_when(
      # Use consent date if participant was consented before app was created
      hml_id_created_date < hml_app_start_date ~ main_consent_date,
      # Use HML ID if pre-app participant is missing consent date
      is.na(main_consent_date) ~ hml_id_created_date,
      # Use HML ID created date generally
      TRUE ~ hml_id_created_date)),
    # Some participants were missed in the previous filter
    filter_date = as.Date(ifelse(is.na(filter_date), hml_id_created_date, filter_date)),
    study_start_date_before = as.Date(hml_id_created_date - date_buffer),
    study_start_date_after = as.Date(hml_id_created_date + date_buffer)) %>%
  select(-c(hml_id_created_date, main_consent_date, filter_date)) %>%
  # 2025-03-21: Participants logged into MC under different emails than what was used to assign HML IDs.
  mutate(participant_id = case_when(hml_id == "HML0765" ~ "003Vu00000hTICAIA4",
                                    hml_id == "HML0793" ~ "003Vu00000lDflWIAS",
                                    TRUE ~ participant_id))

# Get main games data ----------------------

# This chunk pulls and formats the main* games data 
# *not the response-level data, that will be pulled when the users requests to download participant data

# Load games data -----------------------

names <- c("attention", "faces_names", "focus", "keep_track", "objects", "objects_spatial", 
           "objects_temporal", "react", "shapes", "switching", "word_pairs")

# Get main games data
raw_files_list <- lapply(names, 
                         function(x){
                           # Get all games and only load rows with participant_ids matching participant_data$participant_id
                           dbGetQuery(con, paste0("SELECT * FROM ", x, " WHERE participant_id IN ('", 
                                                  paste0(participant_data$participant_id, collapse = "', '"), "')"))})
names(raw_files_list) <- names

# Format games data ----------------------

# Match games to participant data within recruitment sites

files_list <- lapply(raw_files_list, function(x){
  if(sum(str_detect(names(x), "created_date_game_session") > 0)){
    names(x)[which(names(x) == "created_date_game_session")] <- "created_date_game_result"
  }
  
  # Select most recent game
  recent_x <- x %>%
    group_by(participant_id) %>% arrange(desc(created_date_game_result)) %>% 
    slice(1) %>% ungroup()
  
  new_x <- participant_data %>%
    left_join(recent_x, by = "participant_id") %>%
    select(hml_id, site, everything()) %>%
    filter(!is.na(created_date_game_result)) %>%
    mutate(game_status = ifelse(created_date_game_result >= study_start_date_before & created_date_game_result <= study_start_date_after,
                                game_status, "Date Out of Range")) %>%
    mutate(across(-c(contains("date")), ~ifelse(is.na(.), "", .))) %>%
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
