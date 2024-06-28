# PAN MindCrowd Data Wrangling Functions

# Import people who've already been contacted --------------------------------------------------------
# Input: none
# Output: data.frame of participants already in REDCap
filter_no_contact <- function(){
  # Required Libraries: REDCapR
  
  # Get list of people who've already been sent to REDCap to filter out of potential recruitment list:
  # Note: this will produce an error the first time it is run with no data in REDCap
  
  redcapResult <- redcap_read_oneshot(
    redcap_uri = url,                        # Path to REDCap server
    token = token,                           # Security token
    fields = c("record_id", "subject_id", "user_id", "recruited"),
    export_data_access_groups = TRUE,
    raw_or_label = 'raw',
    raw_or_label_headers = 'raw',
    verbose = TRUE
  )
  # print("REDCap read results:")
  # print(redcapResult)
  
  export_data <- redcapResult$data
  # print("Export data:")
  # print(export_data)
  
  remove_people <- as.data.frame(export_data$user_id)
  # print("Removed people:")
  # print(remove_people)
  
  return(remove_people)
}

# REMOVE THIS LATER
# Used this code for testing function within this file
# mindcrowd_data <- read.csv("participants.csv/participants.csv")
# memory_data <- read.csv("memory.csv/memory.csv")

# Get raw data from MindCrowd and determine who is eligible for recruitment ---------------------------
# Input: mindcrowd participant data.frame and memory "game" data.frame, data access group string (default: "All")
# Output: data.frame of screening data for recruitment eligibility dashboard or creating prioritized list for site recruiters
create_screening_data <- function(mindcrowd_data, memory_data, dag_area = "All", campaign_code = FALSE){
  # Required Libraries: tidyverse (for now? case_when uses dplyr)
  
  # Creating race group, age group, and area variables --------------------------------------------------------
  
  mindcrowd_data$race_group <- dplyr::case_when(mindcrowd_data$hispanic_latino == "Yes" ~ "Hispanic",
                                         mindcrowd_data$hispanic_latino == "No" & mindcrowd_data$race == "Black or African American" ~ "Non-Hispanic Black",
                                         mindcrowd_data$hispanic_latino == "No" & mindcrowd_data$race == "White" ~ "Non-Hispanic White")
  mindcrowd_data$age_group <- mindcrowd_data$age_decade

  memory_data$created_at <- format(as.Date(memory_data$created_date_game_result), "%Y-%m-%d")
  
  # Select variables for filtering participants and merge datasets ---------------------------------------------
  
  if(dag_area == "Raw"){
    mindcrowd_data_subset <- subset(mindcrowd_data,
                                    select = c(participant_id, participant_id_parent, area, 
                                               sex, race, hispanic_latino, age, age_group, highest_education_level_completed))
  }else{
    if(campaign_code){
      mindcrowd_data_subset <- subset(mindcrowd_data,
                                      contactable %in% c(TRUE, "True") &
                                        !is.na(area),
                                      select = c(participant_id, participant_id_parent, email, mailing_postalcode, contactable,
                                                 area, sex, race, race_group, age, age_group, campaign_code, highest_education_level_completed))
    }else{
      mindcrowd_data_subset <- subset(mindcrowd_data,
                                      contactable %in% c(TRUE, "True") &
                                        !is.na(area),
                                      select = c(participant_id, participant_id_parent, email, mailing_postalcode, contactable,
                                                 area, sex, race, race_group, age, age_group, highest_education_level_completed))
    }
    
  }
  
  memory_data_subset <- subset(memory_data, 
                        game_status == "Completed",
                        select = c(participant_id, created_at, totalcorrect))
  
  # Merge participant demographics with memory test data
  if(dag_area == "Raw"){
    combined_data_parent <- merge(mindcrowd_data_subset, memory_data_subset, by = "participant_id", all.x = T)
    
  }else{
    combined_data <- merge(mindcrowd_data_subset, memory_data_subset, by = "participant_id") 
    
    # Filter latest test taken by participant ID parent
    combined_data_parent <- combined_data[order(combined_data$created_at),]
    combined_data_parent <- aggregate(combined_data_parent, list(combined_data_parent$participant_id_parent), tail, 1)
    combined_data_parent <- combined_data_parent[-which(names(combined_data_parent) == "Group.1")]
  }
  
  # Assigning performance groups --------------------------------------------------------
  
  combined_data_parent$education_attainment_bin <- case_when(combined_data_parent$highest_education_level_completed %in% c("Completed 8th Grade (Elementary or Primary School Graduate)", "Up to 8 years", 
                                                                                                                           "Some High School", "High School Diploma (Baccalaureate)") ~ "1 & 2",
                                                             combined_data_parent$highest_education_level_completed %in% c("Some College (Some University)") ~ "3",
                                                             combined_data_parent$highest_education_level_completed %in% c("College Degree (University Graduate)") ~ "4",
                                                             combined_data_parent$highest_education_level_completed %in% c("Post Graduate Degree (Masters or Doctorate)") ~ "5")
  
  combined_data_parent$age_bin <- case_when(combined_data_parent$age %in% 18:29 ~ "18-29",
                                            combined_data_parent$age %in% 30:49 ~ "30-49",
                                            combined_data_parent$age %in% 50:64 ~ "50-64",
                                            combined_data_parent$age %in% 65:74 ~ "65-74",
                                            combined_data_parent$age >= 75 ~ "75+")
  
  performance_groups <- read.csv("mindcrowd_performance_groups.csv")

  performance_grouped_data <- merge(combined_data_parent, performance_groups, all.x = T,
                                    by = c("age_bin", "sex", "education_attainment_bin"))
  
  performance_grouped_data$task_group <- case_when(performance_grouped_data$totalcorrect >= performance_grouped_data$q1 & 
                                                     performance_grouped_data$totalcorrect < performance_grouped_data$q2 ~ "Q2",
                                                   performance_grouped_data$totalcorrect >= performance_grouped_data$q2 & 
                                                     performance_grouped_data$totalcorrect < performance_grouped_data$q3 ~ "Q3",
                                                   performance_grouped_data$totalcorrect >= performance_grouped_data$q3 ~ "Q4")
  
  performance_grouped_data <- subset(performance_grouped_data,
                                     select = -c(age_bin, education_attainment_bin, highest_education_level_completed, n:q3))
  
  # Selecting individuals eligible for recruitment --------------------------------------------------------
  
  
  
  # Filter for the correct DAG,if one is specified.
  # If DAG is 'Unknown' then no records will be returned.
  # If DAG is 'All' then no DAG filtering will be done.
  if (dag_area != 'All') {
    if(dag_area == "Raw"){
      screening_data <- performance_grouped_data
    }else{
      screening_data <- subset(performance_grouped_data, 
                               !is.na(race_group) & !is.na(task_group) &
                                 age_group %in% c("50-59", "60-69", "70-79"))
      
      screening_data <- subset(screening_data, area %in% dag_area)
    }
  }else{
    screening_data <- subset(performance_grouped_data, 
                             !is.na(race_group) & !is.na(task_group) &
                               age_group %in% c("50-59", "60-69", "70-79"))
  }
  
  return(screening_data)
  
}

# Sample screening data for people to send to recruitment stage --------------------------------------
# Input: data.frame generated from create_screening_data and data.frame generated by filter_no_contact
# Output: data.frame of selected participants for recruiters to contact
create_prioritized_lists <- function(screening_data, remove_people, 
                                     priority_list = TRUE, dag_area = "All"){
  # Required library: tidyverse (for now?)
  
  print(paste0("Screening data before: ", nrow(screening_data)))
  # Remove the people who have previously been sent to REDCap 
  screening_data <- subset(screening_data, !participant_id %in% remove_people)
  print(paste0("Screening data after: ", nrow(screening_data)))
  
  if(priority_list){
    
    # Get list of group proportions to select for recruitment:
    selected_list <- read_csv("prioritized_group_numbers.csv", show_col_types = FALSE)[-1] %>%
      pivot_longer(cols = -race_group, names_to = "age_group")
    
    # Sample rows based on csv numbers:
    selected_data <- screening_data %>%
      left_join(selected_list, by = c("race_group", "age_group")) %>%
      group_by(area, race_group, age_group) %>%
      # Assigns random order of counts to samples
      mutate(samp = sample(n())) %>%
      # Selects requested number of prioritized participants
      filter(samp <= ceiling(value)) %>%
      ungroup() %>%
      select(-c(value, samp))
    
  }else{
    # No prioritization selection
    if(dag_area != "All"){ # If pulling from specific data access group
      
      # Load list of counts for area
      dag_count <- read.csv("dag_pull_config_221109.csv")
      dag_count <- dag_count$count[which(dag_count$dag == dag_area)]
      
      # Order created_at by oldest to newest record
      ordered_data <- screening_data[order(screening_data$created_at), ]
      
      # Select area and specificied number of records
      selected_data <- subset(ordered_data, area == dag_area)[1:dag_count,]
      
    }else{ # Pulling 40 from all access groups
      
      # Order created_at by oldest to newest record
      # Select area and specified number of records
      # Note: should find base R way to select multiple rows by area
      selected_data <- screening_data %>%
        arrange(created_at) %>%
        group_by(area) %>%
        slice(1:40)
      
    }
  }
  
  return(selected_data)
}

# Import screening data into REDCap ------------------------------------------------------------
# Input: data.frame of prioritized list of eligible participants
# Output: vector of changed records in REDCap
import_into_redcap <- function(selected_data){
  
  # Edit selected data to the correct format for import into REDCap:
  
  print("Inside import_into_redcap")
  
  # Convert factors to numerical **TODO: Need to remove unnecessary variables from REDCap
  import_data <- selected_data %>%
    mutate(subject_id = NA_character_, mri_eligible = NA_character_, recruited = NA_character_,
            region = NA_character_, city = NA_character_, educ_yrs = NA,
           speak_english = NA_character_, first_name = NA_character_, last_name = NA_character_,
           history_complete = "1", cognitive_complete = "1",
           sex = ifelse(sex == "Female", 1, 2),
           decade = case_when(age_group == "50-59" ~ "1",
                              age_group == "60-69" ~ "2",
                              age_group == "70-79" ~ "3"),
           ethnicity = ifelse(race_group == "Hispanic", "1", "0"),
           race = case_when(race == 'American Indian or Alaska Native' ~ '1',
                            race == 'Asian' ~ '2',
                            race == 'Black or African American' ~ '3',
                            race == 'Multiracial or Mixed Race' ~ '4',
                            race == 'Native Hawaiian or Other Pacific Islander' ~ '5',
                            race == 'White' ~ '6'),
           race_group = case_when(race_group == "Non-Hispanic Black" ~ 1,
                                  race_group == "Non-Hispanic White" ~ 2,
                                  race_group == "Hispanic" ~ 3),
           base_mem_score = case_when(task_group == "Q2" ~ 2,
                                      task_group == "Q3" ~ 3,
                                      task_group == "Q4" ~ 4),
           redcap_data_access_group = area,
           record_id = NA) %>%
    select(record_id, redcap_data_access_group, subject_id, user_id = participant_id, 
           age, decade, sex, race, ethnicity,
           race_group, email, region, zip = mailing_postalcode, educ_yrs, date_collected = created_at, #as.Date(created_at),
           speak_english, mri_eligible, recruited, history_complete, totalcorrect,
           base_mem_score, cognitive_complete, first_name, last_name)
  
  # Sample some people:
  import_sample <- import_data %>% # Change sample size here!
    # redcap_next_free_record_name provides next record number for REDCap to prevent overwriting other records
    mutate(record_id = redcap_next_free_record_name(redcap_uri = url, token = token, verbose = TRUE)) 
  
  recCount <- 0 # Initialize
  
  redcapReturn <- redcap_write_oneshot(
    ds = import_sample,                      # Dataframe to import
    redcap_uri = url,                        # Path to REDCap server
    token = token,                           # Security token
    overwrite_with_blanks = FALSE,           # Don't overwrite existing data with blanks
    convert_logical_to_integer = FALSE,
    verbose = TRUE,                          # All messages to console
    config_options = NULL
  )
  
  print("Result of import:")
  print(redcapReturn)
  
  recCount <- redcapReturn$records_affected_count
  
  return(recCount)
}
