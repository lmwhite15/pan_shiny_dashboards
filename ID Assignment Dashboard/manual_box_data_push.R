# This code is used to manually push newly assigned HML IDs to the REDCap assignment folder

library(DBI) # Connecting to database
library(tidyverse) # Data wrangling
library(RCurl) # Connecting to Box Health to generate new REDCap ID

con <- dbConnect(RPostgres::Postgres(),
                 user = Sys.getenv("user")
                 , password = Sys.getenv("password")
                 , dbname = Sys.getenv("dbname")
                 , host = Sys.getenv("host")
                 , port = Sys.getenv("port")
                 , sslrootcert = 'global-bundle.pem'
)

id_dat <- dbReadTable(con, "dm_hml_id_app_data")

# Get any HML IDs that need REDCap IDs generated
hml_id_files <- dbReadTable(con, "redcap_id_assignment")

updated_ids <- id_dat %>%
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

# If there are new IDs then upload list to Box for REDCap ID generation
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
}