
# Function that converts the file names to dates in YYYY-MM-DD format for 
# file_date column in final file
parse_date <- function(f) {
  # Pattern 1: M.D.YY or M.D.YYYY  (e.g. "1.8.26")
  m <- regmatches(f, regexec("^(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2,4})\\b", f))[[1]]
  if (length(m) == 4) {
    mo <- as.integer(m[2]); da <- as.integer(m[3]); yr <- m[4]
    yr <- if (nchar(yr) == 2) as.integer(yr) + 2000 else as.integer(yr)
    return(sprintf("%04d-%02d-%02d", yr, mo, da))
  }
  # Pattern 2: M.YYYY, no day  (e.g. "3.2025")
  m <- regmatches(f, regexec("^(\\d{1,2})\\.(\\d{4})\\b", f))[[1]]
  if (length(m) == 3) {
    mo <- as.integer(m[2]); yr <- as.integer(m[3])
    return(sprintf("%04d-%02d-01", yr, mo))
  }
  # Pattern 3: Month name YYYY, no day  (e.g. "JAN 2026", "July 2026")
  m <- regmatches(f, regexec("^([A-Za-z]+)\\.?\\s+(\\d{4})\\b", f))[[1]]
  if (length(m) == 3) {
    mo <- match(toupper(substr(m[2], 1, 3)), toupper(month.abb))
    yr <- as.integer(m[3])
    return(sprintf("%04d-%02d-01", yr, mo))
  }
  NA_character_
}

cytokine_reads_function <- function(file_names, file_data, limit_data){
  
  # Load Cytokine Data -----------------------------------------------------------
  
  xlsx_files <- file_names
  xlsx_data <- file_data
  lod_data <- limit_data
  
  # List cytokine files
  run_files <- xlsx_files
  
  # Format file names to YYYY-MM-DD
  file_dates <- sapply(xlsx_files, parse_date, USE.NAMES = FALSE)
  
  # Combine files into one data frame
  combined_data <- NULL
  
  for(file in 1:length(run_files)){
    new_file <- xlsx_data[[file]] %>%
      # Detect new format (X1 = Date Run) vs old format (X1 = BARCODE)
      { if ("X3" %in% names(.) && any(!is.na(.$X2))) {
        # New format: X1=Date Run, X2=BARCODE, X3=SAMPLE ID
        fill(., X1, .direction = "down") %>%
          mutate(date_run = coalesce(
            as.Date(X1, format = "%m.%d.%y"),
            as.Date(file_dates[file], format = "%Y-%m-%d")
          )) %>%
          mutate(date_run = as.character(date_run),
                 file_date = file_dates[file]) %>%
          rename(BARCODE = X2) %>%
          select(-X1, -X3)
      } else {
        # Old format: X1=BARCODE, date from filename
        mutate(., 
               date_run = as.character(file_dates[file]),
               file_date = date_run ) %>%
          rename(BARCODE = X1)
      }
      } %>%
      filter(!BARCODE %in% c("SAMPLE ID", "SAMPLE BARCODE", "PID")) %>%
      mutate(BARCODE = str_replace_all(BARCODE, "ND", "-"))
    
    if("X2" %in% names(new_file)){
      new_file <- rename(new_file, hml_id = BARCODE, BARCODE = X2) %>%
        select(-hml_id)
    }
    
    if (any(is.na(new_file$date_run))) {
      cat("⚠️  date_run is NA for file:", run_files[file], "\n")
    }
    
    combined_data <- rbind(combined_data, new_file)
  }
  
  # Get Cytokine LODs ------------------------------------------------------------
  
  # Load LODs by file:
  lods <- NULL
  
  for(file in 1:length(run_files)){
    plate_lods <- lod_data[[file]] %>%
      rename(analyte = Analyte.ID,
             lod = "Overnight.Protocol.Minimum.Detectable.Concentration.(MinDC.[pg/mL])") %>%
      select(analyte, lod) %>%
      filter(!is.na(lod)) %>%
      mutate(file_date = file_dates[file])
    
    lods <- rbind(lods, plate_lods)
  }
  
  cytokines <- unique(lods$analyte)
  
  # Get Plate Means --------------------------------------------------------------
  
  # Get means of each cytokine by plate
  mean_dat <- combined_data %>%
    select(file_date, all_of(cytokines)) %>%
    mutate(across(-file_date, ~ifelse(. %in% c("ND", "%MFIe", "%MFI", "QCe", "-", "pg/ml"), NA, .))) %>%
    mutate(across(-file_date, ~log(as.numeric(.)))) %>%
    group_by(file_date) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_longer(cols = -file_date,
                 names_to = "analyte",
                 values_to = "cytokine_date_mean")
  
  return(list(cytokines = cytokines, 
              readings = combined_data, 
              means = mean_dat, 
              lods = lods))
  
}
