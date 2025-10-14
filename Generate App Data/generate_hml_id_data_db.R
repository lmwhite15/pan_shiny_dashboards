
print("######################################")
print("Generating HML IDs")
print("######################################")

rm(list = ls())

# Load Libraries -------------
library(RCurl)
library(tidyverse, quietly = T)
library(RPostgres)

# Set file locations ----------

# Set the working directory to the script's directory
if (Sys.info()["sysname"] == "Windows") {
  setwd("D:/Precision Aging Network/pan_shiny_dashboards/Generate App Data/")
} else if (Sys.info()["sysname"] == "Linux") {
  setwd("/data/rscripts/pan_shiny_dashboards/Generate App Data/")
}

# Connect to PAN db
con <- dbConnect(RPostgres::Postgres(),
                 user = 'pan_user',
                 password = Sys.getenv("DB_PASSWORD"),
                 dbname = 'pan_data',
                 host = 'bio5-pan-prod.cluster-c0xzlo6s7duc.us-west-2.rds.amazonaws.com',
                 port = '5432',
                 # sslmode = 'verify-full',
                 sslrootcert = 'global-bundle.pem')

# Define Box FTPS details
# URL encode the path
encoded_path <- URLencode("[UA BOX Health] MindCrowd Inbound/HML_ID_Assignment/")

# Define the FTPS base URL
ftps_base_url <- "ftps://ftp.box.com/"

# Combine the base URL with the encoded path
ftps_url <- paste0(ftps_base_url, encoded_path)
userpwd <- Sys.getenv("FTP_PASSWORD")

# Define the file path on your local system (file to save to HML ID assignment folder)
local_file <- "hml_id_data.csv"
output_file_path <- file.path(getwd(), local_file)

# Load ID data ------------------------------------------------------------------------

updated_dat <- dbReadTable(con, "dm_hml_id_app_data")

hml_id_files <- dbReadTable(con, "redcap_id_assignment")

updated_ids <- updated_dat %>%
  filter(!is.na(hml_id),
         !hml_id %in% hml_id_files$hml_id) %>%
  select(participant_id,
         hml_id,
         hml_id_created_date,
         hml_id_undo,
         sex,
         race,
         hispanic_latino,
         area = site,
         age_group,
         memory_rank)

# Save data to REDCap ID creation folder ---------------------------------------------

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
