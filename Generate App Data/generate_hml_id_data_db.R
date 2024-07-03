
print("######################################")
print("Generating HML IDs")
print("######################################")

rm(list = ls())

# Set file locations ----------

# Set the working directory to the script's directory
if (Sys.info()["sysname"] == "Windows") {
  setwd("D:/Precision Aging Network/pan_shiny_dashboards/Generate App Data/")
} else if (Sys.info()["sysname"] == "Linux") {
  setwd("/data/rscripts/pan_shiny_dashboards/Generate App Data/")
}

library(RPostgres)
con <- dbConnect(RPostgres::Postgres(),
                 user = 'pan_user',
                 password = Sys.getenv("DB_PASSWORD"),
                 dbname = 'pan_data',
                 host = 'bio5-pan-prod.cluster-c0xzlo6s7duc.us-west-2.rds.amazonaws.com',
                 port = '5432',
                 sslmode = 'verify-full',
                 sslrootcert = 'rds-ca-2019-root.pem')

# Define Box FTPS details
# URL encode the path
encoded_path <- URLencode("[UA BOX Health] MindCrowd Inbound/HML_ID_Assignment/")

# Define the FTPS base URL
ftps_base_url <- "ftps://ftp.box.com/"

# Combine the base URL with the encoded path
ftps_url <- paste0(ftps_base_url, encoded_path)
userpwd <- Sys.getenv("FTP_PASSWORD")

# Define the file path on your local system
local_file <- "hml_id_data.csv"
output_file_path <- file.path(getwd(), local_file)

# Load Libraries -------------
library(RCurl)
library(tidyverse, quietly = T)

# section to connect to Google Drive
library(googledrive)
# file with info for service account 
googledrive::drive_auth(path = "pan-mindcrowd-uploads-d9b7ecb93e53.json")

# Option to silence the messages coming from the Google Drive library
options(googledrive_quiet = TRUE)

# Load recruitment lists ---------

atl_dat <- dbReadTable(con, "recruitment_list_atlanta")

bal_dat <- dbReadTable(con, "recruitment_list_baltimore")

mia_dat <- dbReadTable(con, "recruitment_list_miami")

tuc_dat <- dbReadTable(con, "recruitment_list_tucson")

print(paste0("Last participant created: ", 
             format(as.Date(max(tuc_dat$created_date_participant, na.rm = T)), "%d-%b-%Y")))

# Load current list of participants and all_screening_data

with_drive_quiet(
  drive_download(as_id("https://drive.google.com/file/d/1NWUkcoIRuHBlw1h6_LWRlNpQSTcNa3fr"),
                 overwrite = TRUE)
)
load_data <- readRDS("generate_hml_id_dat.Rds")

dat <- load_data$dat
all_dat <- load_data$all_dat

# Combine location recruitment data, de-identify and select variables --------

new_dat <- rbind(atl_dat %>% mutate(area = "Atlanta"),
             bal_dat %>% mutate(area = "Baltimore"),
             mia_dat %>% mutate(area = "Miami"),
             tuc_dat %>% mutate(area = "Tucson")
             ) %>%
  mutate(age_group = case_when(age %in% 50:59 ~ "50-59",
                               age %in% 60:69 ~ "60-69",
                               age %in% 70:79 ~ "70-79"),
         part_id = str_sub(participant_id, start = -8),
         race_ethnicity = case_when(hispanic_latino == "Yes" ~ "Hispanic",
                                    race == "White" ~ "Non-Hispanic White",
                                    race == "Black or African American" ~ "Non-Hispanic Black",
                                    TRUE ~ "Other")) %>%
  select(participant_id, sex, race, hispanic_latino, area, age_group, memory_rank) %>%
  mutate(hml_id = NA, hml_id_created_date = NA,.after = participant_id) %>%
  mutate(hml_id_undo = NA,
         across(everything(), ~ifelse(. == "", NA, .)))

# Remove participants who already have HML IDs ------

# Filters out already existing participant ids and adds new IDs
updated_dat <- new_dat %>%
  filter(!participant_id %in% dat$participant_id) %>%
  rbind(dat)

print(paste0("Loaded ", nrow(new_dat), " rows of data from MindCrowd."))

print(paste0("Loaded ", nrow(updated_dat), " rows of data from app folder with ", sum(!is.na(updated_dat$hml_id)), " HML IDs."))

# Create data for manual ID update ---------------

print("Loading screening data.")

# Get all of the screening data from the recruitment dashboard data script
with_drive_quiet(
  drive_download(as_id("https://drive.google.com/file/d/1eTpEi3O7JFVuG4GkawK0C_DDOs93qVKW"),
                 path = "all_screening_data.Rdata",
                 overwrite = TRUE)
)
load("all_screening_data.Rdata")

all_dat <- all_screening_data %>%
  left_join(dat %>% select(participant_id_parent = participant_id, hml_id, hml_id_created_date) %>% distinct(),
            by = "participant_id_parent") %>%
  distinct(participant_id, .keep_all = TRUE) %>%
  mutate(participant_id_parent2 = ifelse(participant_id_parent == participant_id,
                                         participant_id_parent, NA),
         participant_id2 = ifelse(participant_id_parent != participant_id,
                                  participant_id, NA),
         participant_id_parent_short = ifelse(participant_id_parent == participant_id,
                                              str_sub(participant_id_parent, start = -8),
                                              NA),
         participant_id_short = ifelse(participant_id_parent != participant_id, 
                                       str_sub(participant_id, start = -8),
                                       NA)) %>%
  pivot_longer(cols = c(participant_id_parent2, participant_id2, participant_id_parent_short, participant_id_short),
               names_to = "id_source",
               values_to = "alternative_id") %>%
  filter(!is.na(alternative_id)) %>%
  select(-c(participant_id, id_source))

# Save deidentified id data and all screening data
print("Saving HML ID data.")
saveRDS(list(dat = updated_dat, 
             all_dat = all_dat), file = "generate_hml_id_dat.Rds")
drive_put("generate_hml_id_dat.Rds", path=drive_find(pattern="HML Data", corpus="allDrives"))

# Move the new participant_id_parent and hml_ids to the REDCap ID upload folder -------

hml_id_files <- dbReadTable(con, "redcap_id_assignment")

updated_ids <- updated_dat %>%
  filter(!is.na(hml_id),
         !hml_id %in% hml_id_files$hml_id)

if(nrow(updated_ids) > 0){
  write.csv(updated_ids, file = output_file_path,
            row.names = F)
  # Upload the file
  ftpUpload(
    what = output_file_path,
    to = paste0(ftps_url, basename(output_file_path)),
    userpwd = userpwd,
    verbose = TRUE
  )
  print("Saved updated data to REDCap ID creation folder.")
}else{
  print("No updated data to save to REDCap ID creation folder.")
}
