# This file contains the functions needed to format the survey files for import into REDCap.

checkbox_function <- function(data, variable, checkboxes, none = "Not missing", keep = FALSE){
  
  for(i in 1:length(checkboxes)){
    data <- mutate(data, 
                   !!paste0(variable, "___", i) := ifelse(str_detect(!!as.symbol(variable), checkboxes[i]), "1", "0"),
                   .before = all_of(variable))
  }
  
  if(!is.na(none)){
    data <- mutate(data,
                   !!paste0(variable, "___0") := ifelse(!!as.symbol(variable) == none, "1", "0"),
                   .before = all_of(variable))
  }
  
  if(keep){
    return(data)
  }else{
    return(data %>% select(-all_of(variable)))
  }
  
}

redcap_formatting <- function(redcap_files_list){
  
  # ADL ------
  
  redcap_files_list[["adl"]] <- redcap_files_list[["adl"]] %>%
    mutate(across(adl_v101:adl_v1011,
                  ~as.numeric(factor(.,
                                     levels = c("Never did this activity",
                                                "Still do this activity",
                                                "Stopped doing this activity",
                                                "Stopped only due to COVID-19")))-1)) %>%
    checkbox_function(variable = "adl_v1012",
                      checkboxes = c("Walking", "Dressing", "Bathing", "Toileting", "Meal preparation", "Finances",
                                     "Using the telephone", "Shopping", "Housekeeping", "Laundry", "Medications"))
  
  # Anxiety ------
  
  redcap_files_list[["anxiety"]] <- redcap_files_list[["anxiety"]] %>%
    mutate(across(starts_with("anx_v"),
                  ~ifelse(str_detect(., "[:digit:](?= \\-)"), ., ""))) %>%
    mutate(across(starts_with("anx_v"),
                  ~str_extract(., ".*(?=-)") %>% str_trim())) %>%
    rename(anxiety_complete = anx_complete)
  
  # Brain Disease ------
  
  for(i in 1:73){
    redcap_files_list[["brain_disease"]] <- checkbox_function(redcap_files_list[["brain_disease"]], 
                                                              variable = paste0("bd_v10", i), 
                                                              checkboxes = c("Myself", "Mother", "Father", "Sibling", "Unknown"),
                                                              none = "None")
  }
  
  redcap_files_list[["brain_disease"]] <- redcap_files_list[["brain_disease"]] %>%
    select(record_id, hml_id, bd_timestamp, 
           paste0(rep(paste0("bd_v10", 1:73), each = 6), "___", c(1,2,3,4,0,5)),
           brain_disease_complete = bd_complete)
  
  # Covid ------
  
  redcap_files_list[["covid"]] <- redcap_files_list[["covid"]] %>%
    mutate(c19_v102 = str_remove_all(c19_v102, ",+$"),
           c19_v102 = str_replace_all(c19_v102, ",", "; ")) %>%
    mutate(across(c(c19_v101, c19_v104, c19_v106, c19_v107, c19_v108, c19_v109, c19_v1012:c19_v1035,
                    c19_v1039), ~case_when(. == "Yes" ~ 1,
                                           . == "No" ~ 0,
                                           . == "I am not sure" ~ 333,
                                           . == "I prefer not to answer" ~ 999))) %>%
    checkbox_function(variable = "c19_v103", 
                      checkboxes = c("Bluish lips or face", "Bluish or purple toes", "Brain Fog", "Chest pain",
                                     "Confusion", "Congestion", "Cough", "Diarrhea", "Exhaustion", "Fatigue",
                                     "Fever", "Hallucinations", "Headache", "Loss of smell", "Loss of taste",
                                     "Muscle or body aches", "Nausea", "Shortness of breath", "Sore throat",
                                     "Vomiting"),
                      none = "None of these symptoms") %>%
    checkbox_function(variable = "c19_v1011",
                      checkboxes = c("I did not experience", "Sore throat", "Cough", "Fever", "Shortness of breath",
                                     "Vomiting", "Nausea", "Headache", "Diarrhea", "Confusion", "Exhaustion", "Loss of smell",
                                     "Loss of taste", "Muscle or body aches", "Congestion", "Chest pain", "Bluish lips", 
                                     "Bluish or purple toes", "Hallucinations"),
                      none = NA) %>%
    checkbox_function(variable = "c19_v1010",
                      checkboxes = c("Fatigue", "Shortness of breath", "Nausea", "Headaches",
                                     "Diarrhea", "Confusion", "Exhaustion", "Loss of smell", "Loss of taste", 
                                     "Muscle or body aches", "Congestion", "Chest pain", "Bluish lips", 
                                     "Bluish or purple toes", "Hallucinations", "Brain Fog"),
                      none = "None of these symptoms") %>%
    checkbox_function(variable = "c19_v1036",
                      checkboxes = c("Moderna", "Pfizer", "Janssen", "AstraZeneca"),
                      none = NA,
                      keep = TRUE) %>%
    mutate(c19_v1036___555 = ifelse(str_detect(c19_v1036, "Other"), "1", "0"),
           c19_v1035_oth = ifelse(c19_v1036___555 == "1", c19_v1036, ""),
           .before = c19_v1036) %>%
    select(-c19_v1036)%>%
    checkbox_function(variable = "c19_v1040",
                      checkboxes = c("Moderna", "Pfizer", "Janssen", "AstraZeneca"),
                      none = NA) %>%
    mutate(across(-c19_timestamp, ~ifelse(. == "Not missing", "", .))) %>%
    rename(covid_complete = c19_complete)
  
  # Diet ------
  
  redcap_files_list[["diet"]] <- redcap_files_list[["diet"]] %>%
    mutate(diet_v101 = case_when(diet_v101 == "Poor" ~ 1,
                                 diet_v101 == "Fair" ~ 2,
                                 diet_v101 == "Good" ~ 3,
                                 diet_v101 == "Very Good" ~ 4,
                                 diet_v101 == "Excellent" ~ 5)) %>%
    mutate(across(diet_v102:diet_v106, ~case_when(. == "Less than 1" ~ 1,
                                                  . == "1 time" ~ 2,
                                                  . == "2-3 times" ~ 3,
                                                  . == "4-5 times" ~ 4,
                                                  . == "6 or more times" ~ 5))) %>%
    mutate(across(diet_v107:diet_v109, ~case_when(. == "Less than 1" ~ 1,
                                                  . == "1 time" ~ 2,
                                                  . == "2 times" ~ 3,
                                                  . == "3 times" ~ 4,
                                                  . == "4 or more" ~ 5))) %>%
    checkbox_function(variable = "diet_v1010",
                      checkboxes = c("Western Diet", "Western-Style Diet",
                                     "Mediterranean", "Pescatarian", "Vegetarian",
                                     "Vegan", "Paleo", "Medical", "Calorie-resricted",
                                     "Intermittent Fasting", "Gluten-free"),
                      none = "None of the above")
  
  # FHAD ------
  
  redcap_files_list[["fhad"]] <- redcap_files_list[["fhad"]] %>%
    mutate(across(c(fhad_v101:fhad_v103, fhad_v1011, fhad_v1012), 
                  ~case_when(. == "Yes" ~ 1,
                             . == "No" ~ 0))) %>%
    checkbox_function(variable = "fhad_v104",
                      checkboxes = c("Mother", "Father", "Sister", "Brother"),
                      none = NA,
                      keep = TRUE) %>%
    mutate(fhad_v104___5 = ifelse(str_count(fhad_v104, "Additional") > 0, "1", "0"),
           fhad_v104___6 = ifelse(str_count(fhad_v104, "Additional") > 1 , "1", "0"),
           .before = fhad_v104) %>%
    mutate(fhad_v1013 = case_when(fhad_v1013 == 0 ~ 0,
                                  fhad_v1013 == 1 ~ 1,
                                  fhad_v1013 == 2 ~ 2,
                                  fhad_v1013 == "Prefer not to answer" ~ 999)) %>%
    select(-fhad_v104)
  
  # Health and Medical ------
  
  redcap_files_list[["health_medical"]] <- redcap_files_list[["health_medical"]] %>%
    mutate(across(c(hm_v103:hm_v106, hm_v108, hm_v109, hm_v1012, hm_v1014, hm_v1016, hm_v1018,
                    hm_v1021, hm_v1024, hm_v1026, hm_v1027, hm_v1028, hm_v1030:hm_v1032, hm_v1034,
                    hm_v1036:hm_v1039, hm_v1042, hm_v1044, hm_v1046, hm_v1047, hm_v1050, hm_v1052, 
                    hm_v1054, hm_v1057),
                  ~case_when(. == "Yes" ~ 1,
                             . == "No" ~ 0,
                             . == "I don't know" ~ 333,
                             . == "Prefer not to answer" ~ 999)),
           hm_v107 = case_when(hm_v107 == "No" ~ 0,
                               hm_v107 == "Hearing aid" ~ 1,
                               hm_v107 == "Cochlear implant" ~ 2),
           hm_v1010 = case_when(hm_v1010 == "1 Not controlled" ~ 1,
                                hm_v1010 == "2" ~ 2,
                                hm_v1010 == "3 Somewhat controlled" ~ 3,
                                hm_v1010 == "4" ~ 4,
                                hm_v1010 == "5 Very well controlled" ~ 5),
           hm_v1033 = case_when(hm_v1033 == "A. With medication" ~ 1,
                                hm_v1033 == "B. With diet/exercise or other non-pharmaceutical means" ~ 2,
                                hm_v1033 == "C. Both A and B" ~ 3)) %>%
    checkbox_function(variable = "hm_v1017",
                      checkboxes = c("Grand Mal", "Absence", "Myoclonic",
                                     "Clonic", "Tonic", "Atonic", "Childhood febrile",
                                     "I dont know"),
                      none = NA, keep = T) %>%
    mutate(hm_v1017___555 = ifelse(str_detect(hm_v1017, "Other"), "1", "0"),
           hm_v1017_oth = ifelse(hm_v1017___555 == "1", hm_v1017, ""),
           .before = hm_v1017) %>%
    select(-hm_v1017) %>%
    checkbox_function(variable = "hm_v1019",
                      checkboxes = c("Migraines", "Tension", "Cluster", "Thunderclap",
                                     "Post Head Trauma"),
                      none = NA, keep = T) %>%
    mutate(hm_v1019___555 = ifelse(str_detect(hm_v1019, "Other"), "1", "0"),
           hm_v1019_oth = ifelse(hm_v1019___555 == "1", hm_v1019, ""),
           .before = hm_v1019) %>%
    select(-hm_v1019) %>%
    checkbox_function(variable = "hm_v1020",
                      checkboxes = c("Family physician", "Neurologist", "Self"),
                      none = NA, keep = T) %>%
    mutate(hm_v1020___4 = ifelse(str_detect(hm_v1020, "Other"), "1", "0"),
           hm_v1020_oth = ifelse(hm_v1020___4 == "1", hm_v1020, ""),
           .before = hm_v1020) %>%
    select(-hm_v1020) %>%
    checkbox_function(variable = "hm_v1022",
                      checkboxes = c("Ataxia", "Cerebral palsy", "Chorea", "Huntingtons disease", "Tardive dyskinesia",
                                     "Dystonia", "Essential tremor", "Parkinsons disease", "Restless legs syndrome",
                                     "Spasms", "Stereotypy", "Tic disorders", "Tourettes syndrome", "Wilsons disease"),
                      none = NA, keep = T) %>%
    mutate(hm_v1022___15 = ifelse(str_detect(hm_v1022, "Other"), "1", "0"),
           hm_v1022_oth = ifelse(hm_v1022___15 == "1", hm_v1022, ""),
           .before = hm_v1022) %>%
    select(-hm_v1022) %>%
    mutate(hm_v1023 = str_remove_all(hm_v1023, ","),
           hm_v1023_oth = ifelse(str_detect(hm_v1023, "Other"), hm_v1023, ""),
           .after = hm_v1023) %>%
    mutate(hm_v1023 = case_when(hm_v1023 == "Insomnia" ~ 1,
                                hm_v1023 == "Sleep apnea" ~ 2,
                                str_detect(hm_v1023, "Other") ~ 555,
                                hm_v1023 == "None of the above" ~ 999)) %>%
    checkbox_function(variable = "hm_v1025",
                      checkboxes = c("Heart Disease", "Heart Attack", "Liver disease", "Kidney disease",
                                     "Vascular disease", "Asthma", "Lung disease"),
                      none = "None of the above") %>%
    rename(hm_v1025___999 = hm_v1025___0) %>%
    checkbox_function(variable = "hm_v1029",
                      checkboxes = c("Depression", "Bipolar disorder", "Post-Traumatic Stress",
                                     "General anxiety", "Panic attacks", "Phobia", "Schizophrenia",
                                     "Substance use disorder", "Eating disorder"),
                      none = NA, keep = T) %>%
    mutate(hm_v1029___555 = ifelse(str_detect(hm_v1029, "Other"), "1", "0"),
           hm_v1029_oth = ifelse(hm_v1029___555 == "1", 
                                 str_replace(hm_v1029, ".*Other \\(please specify\\):,", ""),
                                 ""),
           .before = hm_v1029) %>%
    select(-hm_v1029) %>%
    mutate(hm_v1041 = case_when(hm_v1041 == "1 No Stress" ~ "1",
                                hm_v1041 == "3 â\u0080\u0093 Moderate Stress" ~ "3",
                                hm_v1041 == "3 Moderate Stress" ~ "3",
                                hm_v1041 == "5 Great Deal of Stress" ~ "5",
                                TRUE ~ hm_v1041),
           hm_v1042 = ifelse(hm_v1042 == "I donât know", "I don't know", hm_v1042)) %>%
    # Medication assignments (hm_v1055)
    mutate(# Remove end commas and "No medications"
      hm_v1055 = str_remove(hm_v1055, ",$"),
      hm_v1055 = ifelse(hm_v1055 %in% c("No medications", ""), "", hm_v1055),
      # Remove spaces around commas
      hm_v1055 = str_remove_all(hm_v1055, "[:space:](?=,)|(?<=,)[:space:]"),
      # First medication assignment
      hm_v1055_1 = str_remove(hm_v1055, ",.*"),
      hm_v1055 = str_remove(hm_v1055, "^.*?,"),
      hm_v1055 = ifelse(hm_v1055 == hm_v1055_1, "", hm_v1055),
      # Second medication assignment
      hm_v1055_2 = str_remove(hm_v1055, ",.*"),
      hm_v1055 = str_remove(hm_v1055, "^.*?,"),
      hm_v1055 = ifelse(hm_v1055 == hm_v1055_2, "", hm_v1055),
      # Third medication assignment
      hm_v1055_3 = str_remove(hm_v1055, ",.*"),
      hm_v1055 = str_remove(hm_v1055, "^.*?,"),
      hm_v1055 = ifelse(hm_v1055 == hm_v1055_3, "", hm_v1055),
      # Fourth medication assignment
      hm_v1055_4 = str_remove(hm_v1055, ",.*"),
      .after = hm_v1055) %>%
    select(-hm_v1055) %>%
    rename(hm_v1055 = hm_v1055_1) %>%
    # Dose assignments (hm_v1056)
    mutate(# Remove end commas and "Not missing"
      hm_v1056 = str_remove(hm_v1056, ",$"),
      hm_v1056 = ifelse(hm_v1056 %in% c("Not missing", ""), "", hm_v1056),
      # Remove spaces around commas
      hm_v1056 = str_remove_all(hm_v1056, "[:space:](?=,)|(?<=,)[:space:]"),
      # First medication assignment
      hm_v1056_1 = str_remove(hm_v1056, ",.*"),
      hm_v1056 = str_remove(hm_v1056, "^.*?,"),
      hm_v1056 = ifelse(hm_v1056 == hm_v1056_1, "", hm_v1056),
      # Second medication assignment
      hm_v1056_2 = str_remove(hm_v1056, ",.*"),
      hm_v1056 = str_remove(hm_v1056, "^.*?,"),
      hm_v1056 = ifelse(hm_v1056 == hm_v1056_2, "", hm_v1056),
      # Third medication assignment
      hm_v1056_3 = str_remove(hm_v1056, ",.*"),
      hm_v1056 = str_remove(hm_v1056, "^.*?,"),
      hm_v1056 = ifelse(hm_v1056 == hm_v1056_3, "", hm_v1056),
      # Fourth medication assignment
      hm_v1056_4 = str_remove(hm_v1056, ",.*"),
      .after = hm_v1056) %>%
    select(-hm_v1056) %>% rename(hm_v1056 = hm_v1056_1) %>%
    # Is this medication prescribed by a physician? (Survey data only had one entry)
    mutate(hm_v1057_2 = NA,
           hm_v1057_3 = NA,
           hm_v1057_4 = NA,
           .after = hm_v1057) %>%
    # Length of medication assignments (hm_v1058)
    mutate(# Remove end commas and "Not missing"
      hm_v1058 = str_remove(hm_v1058, ",$"),
      hm_v1058 = ifelse(hm_v1058 %in% c("Not missing", ""), "", hm_v1058),
      # Remove spaces around commas
      hm_v1058 = str_remove_all(hm_v1058, "[:space:](?=,)|(?<=,)[:space:]"),
      # First medication assignment
      hm_v1058_1 = str_remove(hm_v1058, ",.*"),
      hm_v1058 = str_remove(hm_v1058, "^.*?,"),
      hm_v1058 = ifelse(hm_v1058 == hm_v1058_1, "", hm_v1058),
      # Second medication assignment
      hm_v1058_2 = str_remove(hm_v1058, ",.*"),
      hm_v1058 = str_remove(hm_v1058, "^.*?,"),
      hm_v1058 = ifelse(hm_v1058 == hm_v1058_2, "", hm_v1058),
      # Third medication assignment
      hm_v1058_3 = str_remove(hm_v1058, ",.*"),
      hm_v1058 = str_remove(hm_v1058, "^.*?,"),
      hm_v1058 = ifelse(hm_v1058 == hm_v1058_3, "", hm_v1058),
      # Fourth medication assignment
      hm_v1058_4 = str_remove(hm_v1058, ",.*"),
      .after = hm_v1058) %>%
    select(-hm_v1058) %>% rename(hm_v1058 = hm_v1058_1) %>%
    # Reason for taking medication assignments (hm_v1059)
    mutate(# Remove end commas and "Not missing"
      hm_v1059 = str_remove(hm_v1059, ",$"),
      hm_v1059 = ifelse(hm_v1059 %in% c("Not missing", ""), "", hm_v1059),
      # Remove spaces around commas
      hm_v1059 = str_remove_all(hm_v1059, "[:space:](?=,)|(?<=,)[:space:]"),
      # First medication assignment
      hm_v1059_1 = str_remove(hm_v1059, ",.*"),
      hm_v1059 = str_remove(hm_v1059, "^.*?,"),
      hm_v1059 = ifelse(hm_v1059 == hm_v1059_1, "", hm_v1059),
      # Second medication assignment
      hm_v1059_2 = str_remove(hm_v1059, ",.*"),
      hm_v1059 = str_remove(hm_v1059, "^.*?,"),
      hm_v1059 = ifelse(hm_v1059 == hm_v1059_2, "", hm_v1059),
      # Third medication assignment
      hm_v1059_3 = str_remove(hm_v1059, ",.*"),
      hm_v1059 = str_remove(hm_v1059, "^.*?,"),
      hm_v1059 = ifelse(hm_v1059 == hm_v1059_3, "", hm_v1059),
      # Fourth medication assignment
      hm_v1059_4 = str_remove(hm_v1059, ",.*"),
      .after = hm_v1059) %>%
    select(-hm_v1059) %>% rename(hm_v1059 = hm_v1059_1) %>%
    rename(healthmed_complete = hm_complete)
  
  # Perceived Stress ------
  
  redcap_files_list[["perceived_stress"]] <- redcap_files_list[["perceived_stress"]] %>%
    mutate(across(starts_with("stress_v"), ~str_extract(., ".*(?=\\=)") %>% str_trim()))
  
  # QPAR ------
  
  redcap_files_list[["qpar"]] <- redcap_files_list[["qpar"]] %>%
    mutate(across(qpar_v101:qpar_v108, 
                  ~case_when(. == "0 Days" ~ "0",
                             . == "1-2 Days" ~ "1",
                             . == "3-4 Days" ~ "2",
                             . == "5-7 Days" ~ "3")),
           across(qpar_v109:qpar_v1016, 
                  ~case_when(. == "Less than 1 hour" ~ "0",
                             . == "1-2 hours" ~ "1",
                             . == "More than 2 hours" ~ "2")))
  
  # SES ------
  
  redcap_files_list[["ses"]] <- redcap_files_list[["ses"]] %>%
    mutate(across(c(ses_v108, ses_v1011, ses_v1013),
                  ~case_when(. == "Yes" ~ 1,
                             . == "No" ~ 0)),
           ses_v103 = case_when(ses_v103 == "High School Graduate or GED (General Education Diploma" ~ "High School Graduate or GED (General Education Diploma)",
                                ses_v103 == "Didnâ\u0080\u0099t Finish High School" ~ "Didn't Finish High School",
                                ses_v103 == "Didnâ\u0080\u0099t Finish High School, but completed a technical/vocational program" ~ 
                                  "Didn't Finish High School, but completed a technical/vocational program",
                                ses_v103 == "Masterâ\u0080\u0099s degree (or other post-graduate training" ~ "Master's degree (or other post-graduate training)",
                                ses_v103 == "Doctoral degree (PhD, MD, EdD, DVM, DDS, JD, etc" ~ "Doctoral degree (PhD, MD, EdD, DVM, DDS, JD, etc)",
                                TRUE ~ ses_v103),
           ses_v103 = case_when(ses_v103 == "Didn't Finish High School" ~ 1,
                                ses_v103 == "Didn't Finish High School, but completed a technical/vocational program" ~ 2,
                                ses_v103 == "High School Graduate or GED (General Education Diploma)" ~ 3,
                                ses_v103 == "Completed High School and a technical/vocational program" ~ 4,
                                ses_v103 == "Less than 2 Years of College" ~ 5,
                                ses_v103 == "2 Years of College or more/ including associate degree or equivalent" ~ 6,
                                ses_v103 == "College graduate (4 or 5-year program)" ~ 7,
                                ses_v103 == "Master's degree (or other post-graduate training)" ~ 8,
                                ses_v103 == "Doctoral degree (PhD, MD, EdD, DVM, DDS, JD, etc)" ~ 9),
           ses_v105 = case_when(ses_v105 == "Working full time" ~ 1,
                                ses_v105 == "Working part time" ~ 2,
                                ses_v105 == "Self-employed" ~ 3,
                                ses_v105 == "Not currently employed" ~ 4,
                                ses_v105 == "Retired" ~ 5,
                                ses_v105 == "Homemaker" ~ 6,
                                ses_v105 == "Disabled" ~ 7),
           ses_v106 = case_when(ses_v106 == "Single" ~ 1,
                                str_detect(ses_v106, "Married") ~ 2,
                                ses_v106 == "Separated or divorced" ~ 3,
                                ses_v106 == "Widowed" ~ 4),
           ses_v107 = case_when(ses_v107 == "Less than $5,000" ~ 1,
                                ses_v107 == "$5,000 - $9,999" ~ 2,
                                ses_v107 == "$10,000 - $14,999" ~ 3,
                                ses_v107 == "$15,000 - $19,999" ~ 4,
                                ses_v107 == "$20,000 - $29,999" ~ 5,
                                ses_v107 == "$30,000 - $39,999" ~ 6,
                                ses_v107 == "$40,000 - $49,999" ~ 7,
                                ses_v107 == "$50,000 - $59,999" ~ 8,
                                ses_v107 == "$60-000 - $74,999" ~ 9,
                                ses_v107 == "$75,000 - $99,999" ~ 10,
                                ses_v107 == "$100,000 - $124,999" ~ 11,
                                ses_v107 == "$125,000 - $149,999" ~ 12,
                                ses_v107 == "$150,000 -$299,000" ~ 13,
                                ses_v107 == "Above $300,000" ~ 14))
  
  # Sleep ------
  
  redcap_files_list[["sleep"]] <- redcap_files_list[["sleep"]] %>%
    mutate(across(c(sleep_v101:sleep_v103, sleep_v1017:sleep_v1020),
                  ~case_when(. == "Yes" ~ 1, 
                             . == "No" ~ 0)),
           sleep_v106 = ifelse(sleep_v106 == "None", 0, sleep_v106)) %>%
    checkbox_function(variable = "sleep_v104",
                      checkboxes = c("Sleep apnea", "Restless leg syndrome", "Narcolepsy",
                                     "REM Sleep behavior disorder", "Parasomnias", "Slow wave sleep disorders"),
                      none = NA, keep = T) %>%
    mutate(sleep_v104___555 = ifelse(str_detect(sleep_v104, "Other"), "1", "0"),
           .after = sleep_v104) %>%
    select(-sleep_v104) %>%
    checkbox_function(variable = "sleep_v1021",
                      checkboxes = c("Children / Pets", "Hobbies", "Other"),
                      none = NA) %>%
    rename(sleep_v1021___555 = sleep_v1021___3,
           sleep_v1021_oth = sleep_v1033) %>%
    mutate(across(sleep_v1023:sleep_v1032,
                  ~case_when(. == "All of the time" ~ "1",
                             . == "Most of the time" ~ "2",
                             . == "A good bit of the time" ~ "3",
                             . == "Some of the time" ~ "4",
                             . == "A little of the time" ~ "5",
                             . == "None of the time" ~ "6")))
  
  # Social Stressors ------
  
  redcap_files_list[["social_stressor"]] <- redcap_files_list[["social_stressor"]] %>%
    mutate(socstress_v101_yes = ifelse(socstress_v101 != "No", socstress_v101, NA),
           socstress_v102_yes = ifelse(socstress_v102 != "No", socstress_v102, NA),
           socstress_v103_yes = ifelse(socstress_v103 != "No", socstress_v103, NA),
           socstress_v104_yes = ifelse(socstress_v104 != "No", socstress_v104, NA),
           socstress_v105_yes = ifelse(socstress_v105 != "No", socstress_v105, NA),
           socstress_v106_yes = ifelse(socstress_v106 != "No", socstress_v106, NA),
           socstress_v107_yes = ifelse(socstress_v107 != "No", socstress_v107, NA),
           socstress_v108_yes = ifelse(socstress_v108 != "No", socstress_v108, NA),
           socstress_v109_yes = ifelse(socstress_v109 != "No", socstress_v109, NA),
           socstress_v1010_yes = ifelse(socstress_v1010 != "No", socstress_v1010, NA),
           socstress_v1011_yes = ifelse(socstress_v1011 != "No", socstress_v1011, NA),
           socstress_v1012_yes = ifelse(socstress_v1012 != "No", socstress_v1012, NA),
           across(socstress_v101:socstress_v1012, ~ifelse(. != "No", "Yes", .))) %>%
    mutate(across(starts_with("socstress_v"), 
                  ~case_when(. == "No" ~ 0,
                             . == "Yes" ~ 1,
                             . == "Mildly Stressful" ~ 1,
                             . == "Stressful" ~ 2,
                             . == "Very Stressful" ~ 3))) %>%
    select(record_id, hml_id, socstress_timestamp, 
           paste0("socstress_v10", rep(1:12, each = 2), c("", "_yes")),
           socstress_complete)
  
  # Social Support ------
  
  redcap_files_list[["social_support"]] <- redcap_files_list[["social_support"]] %>%
    mutate(across(socsupp_v101:socsupp_v103, ~case_when(. == "Hardly Ever" ~ 1,
                                                        . == "Some of the Time" ~ 2,
                                                        . == "Often" ~ 3)),
           across(socsupp_v104:socsupp_v1015, ~str_extract(., ".*(?=\\=)") %>% str_trim()))
  
  # Subjective English ------
  
  redcap_files_list[["subjective_english"]] <- redcap_files_list[["subjective_english"]] %>%
    mutate(subeng_v101 = case_when(subeng_v101 == "Yes" ~ 1,
                                   subeng_v101 == "No" ~ 0),
           across(subeng_v102:subeng_v104, 
                  ~case_when(. == "Not Very Comfortable" ~ 1,
                             . == "Somewhat Comfortable" ~ 2,
                             . == "Very Comfortable" ~ 3)))
  
  # SWLS ------
  
  redcap_files_list[["swls"]] <- redcap_files_list[["swls"]] %>%
    mutate(across(starts_with("swls_v"), ~str_extract(., ".*(?=-)") %>% str_trim()))
  
  
  # Combine surveys -------
  
  redcap_data <- redcap_files_list %>% 
    reduce(full_join, by = c("record_id", "hml_id")) %>%
    rename(c19_v1036_oth = c19_v1035_oth) %>%
    mutate(across(where(is.character), ~ifelse(. %in% c("Not missing", NA), "", .)),
           across(contains("_timestamp"), ~format(as.Date(.), "%m/%d/%Y")),
           across(contains("_timestamp"), as.character),
           across(everything(), ~ifelse(is.na(.), "", .))) %>%
    mutate(redcap_event_name = "hml_survey_tracker_arm_1", .after = record_id)
  
  return(redcap_data)
}
