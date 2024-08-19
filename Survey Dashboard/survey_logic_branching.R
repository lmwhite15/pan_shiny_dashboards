# This function goes through survey responses for participants and marks whether missing data
# is actually missing or due to logic branching.

survey_logic_branching <- function(files_list){
  
  # ADL ------------------------
  
  # Do you currently need assistance with any of the following activities?
  files_list[["adl"]]$ADL_v1.0.12 <- ifelse(files_list[["adl"]]$ADL_v1.0.12 == "",
                                            "Not missing", files_list[["adl"]]$ADL_v1.0.12)
  
  # Covid --------
  
  ## Note for future: remove for loop (assign values by vectors all at once)
  
  for(i in 1:nrow(files_list[["covid"]])){
    # If participant completed survey
    if(!is.na(files_list[["covid"]]$created_date_survey[i])){
      # If answered yes to testing positive for Covid
      if(files_list[["covid"]]$C19_v1.0.1[i] == "No"){
        files_list[["covid"]]$C19_v1.0.2[i] <- "Not missing"
        files_list[["covid"]]$C19_v1.0.3[i] <- "Not missing"
        files_list[["covid"]]$C19_v1.0.4[i] <- "Not missing"
        files_list[["covid"]]$C19_v1.0.5[i] <- "Not missing"
        files_list[["covid"]]$C19_v1.0.6[i] <- "Not missing"
        files_list[["covid"]]$C19_v1.0.7[i] <- "Not missing"
        files_list[["covid"]]$C19_v1.0.8[i] <- "Not missing"
        files_list[["covid"]]$C19_v1.0.9[i] <- "Not missing"
        files_list[["covid"]]$C19_v1.0.10[i] <- "Not missing"
      }else if(files_list[["covid"]]$C19_v1.0.1[i] == "Yes"){
        # If they were diagnosed with Covid
        # If they were hospitalized
        if(files_list[["covid"]]$C19_v1.0.4[i] == "No"){
          files_list[["covid"]]$C19_v1.0.5[i] <- "Not missing"
          files_list[["covid"]]$C19_v1.0.6[i] <- "Not missing"
          files_list[["covid"]]$C19_v1.0.7[i] <- "Not missing"
          files_list[["covid"]]$C19_v1.0.8[i] <- "Not missing"
          files_list[["covid"]]$C19_v1.0.9[i] <- "Not missing"
          files_list[["covid"]]$C19_v1.0.10[i] <- "Not missing"
        }
        files_list[["covid"]]$C19_v1.0.11[i] <- "Not missing"
      }
    }
  }
  
  #If answered yes to currently smoking tobacco
  files_list[["covid"]]$C19_v1.0.33 <- ifelse(files_list[["covid"]]$C19_v1.0.33 == "" &
                                                files_list[["covid"]]$C19_v1.0.32 == "Yes",
                                              "Not missing",
                                              files_list[["covid"]]$C19_v1.0.33)
  
  # IF yes to receiving a vaccine
  files_list[["covid"]]$C19_v1.0.36 <- ifelse(files_list[["covid"]]$C19_v1.0.36 == "" &
                                                files_list[["covid"]]$C19_v1.0.35 == "No",
                                              "Not missing",
                                              files_list[["covid"]]$C19_v1.0.36)
  files_list[["covid"]]$C19_v1.0.37 <- ifelse(files_list[["covid"]]$C19_v1.0.37 == "" &
                                                files_list[["covid"]]$C19_v1.0.35 == "No",
                                              "Not missing",
                                              files_list[["covid"]]$C19_v1.0.37)
  files_list[["covid"]]$C19_v1.0.38 <- ifelse(files_list[["covid"]]$C19_v1.0.38 == "" &
                                                files_list[["covid"]]$C19_v1.0.35 == "No",
                                              "Not missing",
                                              files_list[["covid"]]$C19_v1.0.38)
  
  
  # IF yes to receiving a booster
  files_list[["covid"]]$C19_v1.0.40 <- ifelse(files_list[["covid"]]$C19_v1.0.40 == "" &
                                                files_list[["covid"]]$C19_v1.0.39 == "No",
                                              "Not missing",
                                              files_list[["covid"]]$C19_v1.0.40)
  files_list[["covid"]]$C19_v1.0.41 <- ifelse(files_list[["covid"]]$C19_v1.0.41 == "" &
                                                files_list[["covid"]]$C19_v1.0.39 == "No",
                                              "Not missing",
                                              files_list[["covid"]]$C19_v1.0.41)
  
  # FHAD ----------
  
  # If yes to question 1:
  files_list[["fhad"]]$FHAD_v1.0.2 <- ifelse(files_list[["fhad"]]$FHAD_v1.0.2 == "" &
                                               files_list[["fhad"]]$FHAD_v1.0.1 == "No",
                                             "Not missing",
                                             files_list[["fhad"]]$FHAD_v1.0.2)
  
  # If yes to question 2:
  files_list[["fhad"]]$FHAD_v1.0.3 <- ifelse(files_list[["fhad"]]$FHAD_v1.0.3 == "" & 
                                               (files_list[["fhad"]]$FHAD_v1.0.2 == "No" |
                                                  files_list[["fhad"]]$FHAD_v1.0.2 == "Not missing"),
                                             "Not missing",
                                             files_list[["fhad"]]$FHAD_v1.0.3)
  files_list[["fhad"]]$FHAD_v1.0.4 <- ifelse(files_list[["fhad"]]$FHAD_v1.0.4 == "" & 
                                               (files_list[["fhad"]]$FHAD_v1.0.2 == "No" |
                                                  files_list[["fhad"]]$FHAD_v1.0.2 == "Not missing"),
                                             "Not missing",
                                             files_list[["fhad"]]$FHAD_v1.0.4)
  
  # If selected family member, enter their ages
  files_list[["fhad"]]$FHAD_v1.0.5 <- ifelse(((files_list[["fhad"]]$FHAD_v1.0.5 == "") & 
                                                files_list[["fhad"]]$FHAD_v1.0.4 != "Mother:") |
                                               files_list[["fhad"]]$FHAD_v1.0.4 == "Not missing",
                                             "Not missing",
                                             files_list[["fhad"]]$FHAD_v1.0.5)
  files_list[["fhad"]]$FHAD_v1.0.6 <- ifelse(((files_list[["fhad"]]$FHAD_v1.0.6 == "") & 
                                                files_list[["fhad"]]$FHAD_v1.0.4 != "Father:") |
                                               files_list[["fhad"]]$FHAD_v1.0.4 == "Not missing",
                                             "Not missing",
                                             files_list[["fhad"]]$FHAD_v1.0.6)
  files_list[["fhad"]]$FHAD_v1.0.7 <- ifelse(((files_list[["fhad"]]$FHAD_v1.0.7 == "") & 
                                                files_list[["fhad"]]$FHAD_v1.0.4 != "Sister:") |
                                               files_list[["fhad"]]$FHAD_v1.0.4 == "Not missing",
                                             "Not missing",
                                             files_list[["fhad"]]$FHAD_v1.0.7)
  files_list[["fhad"]]$FHAD_v1.0.8 <- ifelse(((files_list[["fhad"]]$FHAD_v1.0.8 == "") & 
                                                files_list[["fhad"]]$FHAD_v1.0.4 != "Brother:") |
                                               files_list[["fhad"]]$FHAD_v1.0.4 == "Not missing",
                                             "Not missing",
                                             files_list[["fhad"]]$FHAD_v1.0.8)
  files_list[["fhad"]]$FHAD_v1.0.9 <- ifelse(((files_list[["fhad"]]$FHAD_v1.0.9 == "") & 
                                                files_list[["fhad"]]$FHAD_v1.0.4 != "Additional Siblings(s):") |
                                               files_list[["fhad"]]$FHAD_v1.0.4 == "Not missing",
                                             "Not missing",
                                             files_list[["fhad"]]$FHAD_v1.0.9)
  files_list[["fhad"]]$FHAD_v1.0.10 <- ifelse(((files_list[["fhad"]]$FHAD_v1.0.10 == "") & 
                                                 files_list[["fhad"]]$FHAD_v1.0.4 != "Additional Siblings(s):") |
                                                files_list[["fhad"]]$FHAD_v1.0.4 == "Not missing",
                                              "Not missing",
                                              files_list[["fhad"]]$FHAD_v1.0.10)
  
  # If answered yes or no to question 2 then should answer:
  files_list[["fhad"]]$FHAD_v1.0.11 <- ifelse(files_list[["fhad"]]$FHAD_v1.0.11 == "" & 
                                                files_list[["fhad"]]$FHAD_v1.0.1 == "No",
                                              "Not missing",
                                              files_list[["fhad"]]$FHAD_v1.0.11)
  
  # If yes to 6 then should answer:
  files_list[["fhad"]]$FHAD_v1.0.13 <- ifelse(files_list[["fhad"]]$FHAD_v1.0.13 == "" & 
                                                files_list[["fhad"]]$FHAD_v1.0.12 == "No",
                                              "Not missing",
                                              files_list[["fhad"]]$FHAD_v1.0.13)
  
  # Health and Medical ------------------
  
  files_list[["health_medical"]] <- mutate_all(files_list[["health_medical"]], as.character)
  
  # if they don't have problems with hearing
  files_list[["health_medical"]]$HM_v1.0.7 <- ifelse(files_list[["health_medical"]]$HM_v1.0.7 == "" &
                                                       files_list[["health_medical"]]$HM_v1.0.6 == "No",
                                                     "Not missing",
                                                     files_list[["health_medical"]]$HM_v1.0.7)
  
  # if not diabetic
  files_list[["health_medical"]]$HM_v1.0.10 <- ifelse(files_list[["health_medical"]]$HM_v1.0.10 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.9 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.10)
  files_list[["health_medical"]]$HM_v1.0.11 <- ifelse(files_list[["health_medical"]]$HM_v1.0.11 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.9 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.11)
  
  # loss of consciousness
  files_list[["health_medical"]]$HM_v1.0.13 <- ifelse(files_list[["health_medical"]]$HM_v1.0.13 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.12 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.13)
  
  
  # medically diagnosed concussion
  files_list[["health_medical"]]$HM_v1.0.15 <- ifelse(files_list[["health_medical"]]$HM_v1.0.15 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.14 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.15)
  
  # diagnosed with seizures or epilepsy
  files_list[["health_medical"]]$HM_v1.0.17 <- ifelse(files_list[["health_medical"]]$HM_v1.0.17 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.16 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.17)
  
  # chronic headaches
  files_list[["health_medical"]]$HM_v1.0.19 <- ifelse(files_list[["health_medical"]]$HM_v1.0.19 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.18 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.19)
  files_list[["health_medical"]]$HM_v1.0.20 <- ifelse(files_list[["health_medical"]]$HM_v1.0.20 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.18 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.20)
  
  # movement disorder
  files_list[["health_medical"]]$HM_v1.0.22 <- ifelse(files_list[["health_medical"]]$HM_v1.0.22 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.21 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.22)
  
  # high cholesterol
  files_list[["health_medical"]]$HM_v1.0.27 <- ifelse(files_list[["health_medical"]]$HM_v1.0.27 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.26 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.27)
  
  # condition related to your mental health
  files_list[["health_medical"]]$HM_v1.0.29 <- ifelse(files_list[["health_medical"]]$HM_v1.0.29 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.28 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.29)
  
  # high blood pressure
  files_list[["health_medical"]]$HM_v1.0.32 <- ifelse(files_list[["health_medical"]]$HM_v1.0.32 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.31 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.32)
  files_list[["health_medical"]]$HM_v1.0.33 <- ifelse(files_list[["health_medical"]]$HM_v1.0.33 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.31 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.33)
  
  # problems with your memory
  files_list[["health_medical"]]$HM_v1.0.35 <- ifelse(files_list[["health_medical"]]$HM_v1.0.35 == "" &
                                                        files_list[["health_medical"]]$HM_v1.0.34 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.35)
  
  # diagnosed with any other disorder affecting your brain?
  files_list[["health_medical"]]$HM_v1.0.40 <- ifelse((files_list[["health_medical"]]$HM_v1.0.40 == "") &
                                                        files_list[["health_medical"]]$HM_v1.0.39 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.40)
  # are you a twin?
  files_list[["health_medical"]]$HM_v1.0.43 <- ifelse((files_list[["health_medical"]]$HM_v1.0.43 == "") &
                                                        files_list[["health_medical"]]$HM_v1.0.42 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.43)
  
  # do you drink alcohol
  files_list[["health_medical"]]$HM_v1.0.45 <- ifelse((files_list[["health_medical"]]$HM_v1.0.45 == "") &
                                                        files_list[["health_medical"]]$HM_v1.0.44 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.45)
  files_list[["health_medical"]]$HM_v1.0.46 <- ifelse((files_list[["health_medical"]]$HM_v1.0.46 == "") &
                                                        files_list[["health_medical"]]$HM_v1.0.44 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.46)
  
  # do you currently smoke cigarettes?
  files_list[["health_medical"]]$HM_v1.0.48 <- ifelse((files_list[["health_medical"]]$HM_v1.0.48 == "") &
                                                        files_list[["health_medical"]]$HM_v1.0.47 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.48)
  files_list[["health_medical"]]$HM_v1.0.49 <- ifelse((files_list[["health_medical"]]$HM_v1.0.49 == "") &
                                                        files_list[["health_medical"]]$HM_v1.0.47 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.49)
  
  # do you not smoke currently
  files_list[["health_medical"]]$HM_v1.0.51 <- ifelse((files_list[["health_medical"]]$HM_v1.0.51 == "") &
                                                        files_list[["health_medical"]]$HM_v1.0.50 == "No",
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.51)
  
  # do you use marijuana products
  files_list[["health_medical"]]$HM_v1.0.53 <- ifelse((files_list[["health_medical"]]$HM_v1.0.53 == "") &
                                                        files_list[["health_medical"]]$HM_v1.0.52 %in% c("No", "Prefer not to answer"),
                                                      "Not missing",
                                                      files_list[["health_medical"]]$HM_v1.0.53)
  
  # medication list
  files_list[["health_medical"]]$HM_v1.0.55 <- ifelse(files_list[["health_medical"]]$HM_v1.0.55 == "",
                                                      "No medications",
                                                      files_list[["health_medical"]]$HM_v1.0.55)
  
  for(i in 1:nrow(files_list[["health_medical"]])){
    # If participant completed survey:
    if(!is.na(files_list[["health_medical"]]$created_date_survey[i])){
      if(files_list[["health_medical"]]$HM_v1.0.55[i] == "No medications"){
        files_list[["health_medical"]]$HM_v1.0.56[i] <- "Not missing"
        files_list[["health_medical"]]$HM_v1.0.57[i] <- "Not missing"
        files_list[["health_medical"]]$HM_v1.0.58[i] <- "Not missing"
      }
    }
  }
  
  
  
  
  # QPAR ----------
  
  ## Change this from for loop later!
  
  # Recording length of time for each type of activity
  for(i in 1:nrow(files_list[["qpar"]])){
    for(col in 1:8){
      files_list[["qpar"]][i, paste0("QPAR_v1.0.", col+8)] <- ifelse(
        files_list[["qpar"]][i, paste0("QPAR_v1.0.", col)] == "0 Days",
        "Not missing",
        files_list[["qpar"]][i, paste0("QPAR_v1.0.", col+8)])
    }
  }
  
  # SES --------------
  files_list[["ses"]]$SES_v1.0.9 <- ifelse(files_list[["ses"]]$SES_v1.0.9 == "" &
                                             files_list[["ses"]]$SES_v1.0.8 == "No",
                                           "Not missing",
                                           files_list[["ses"]]$SES_v1.0.9) 
  files_list[["ses"]]$SES_v1.0.12 <- ifelse(files_list[["ses"]]$SES_v1.0.11 == "No",
                                            "Not missing",
                                            files_list[["ses"]]$SES_v1.0.12) 
  
  # Sleep ----------
  
  # If yes, what types of sleep disorders do you have?
  files_list[["sleep"]]$SLEEP_v1.0.4 <- ifelse(files_list[["sleep"]]$SLEEP_v1.0.4 == "" &
                                                 files_list[["sleep"]]$SLEEP_v1.0.3 == "No", 
                                               "Not missing",
                                               files_list[["sleep"]]$SLEEP_v1.0.4)
  # If selected other for question 4:
  files_list[["sleep"]]$SLEEP_v1.0.5 <- case_when(
    files_list[["sleep"]]$SLEEP_v1.0.4 == "Not missing" ~ "Not missing", 
    files_list[["sleep"]]$SLEEP_v1.0.5 == "" & !str_detect(files_list[["sleep"]]$SLEEP_v1.0.4, "Other") ~ "Not missing",
    TRUE ~ files_list[["sleep"]]$SLEEP_v1.0.5
  )
  
  
  # If no, please check the reasons why you cannot freely choose your wake-up times on Free Days?
  files_list[["sleep"]]$SLEEP_v1.0.21 <- ifelse(files_list[["sleep"]]$SLEEP_v1.0.21 == "" &
                                                  files_list[["sleep"]]$SLEEP_v1.0.20 == "No", 
                                                "Not missing",
                                                files_list[["sleep"]]$SLEEP_v1.0.21)
  
  files_list[["sleep"]]$SLEEP_v1.0.33 <- case_when(
    files_list[["sleep"]]$SLEEP_v1.0.21 == "Not missing" ~ "Not missing",
    files_list[["sleep"]]$SLEEP_v1.0.33 == "" & files_list[["sleep"]]$SLEEP_v1.0.21 != "" & !str_detect(files_list[["sleep"]]$SLEEP_v1.0.21, "Other") ~ "Not missing",
    TRUE ~ files_list[["sleep"]]$SLEEP_v1.0.33)
  
  # Subjective English ------
  
  # If selected that English is native language
  for(i in 1:nrow(files_list[["subjective_english"]])){
    # If participant completed survey
    if(!is.na(files_list[["subjective_english"]]$created_date_survey[i])){
      if(files_list[["subjective_english"]]$SUBENG_v1.0.1[i] == "Yes"){
        files_list[["subjective_english"]]$SUBENG_v1.0.2[i] <- "Not missing"
        files_list[["subjective_english"]]$SUBENG_v1.0.3[i] <- "Not missing"
        files_list[["subjective_english"]]$SUBENG_v1.0.4[i] <- "Not missing"
      }
    }
  }
  
  return(files_list)
  
}
