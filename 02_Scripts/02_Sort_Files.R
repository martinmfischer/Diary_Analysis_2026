################################################################################
# Project: Tagebuchstudie
# File:    02_Sort_Files.R
#
# Ziel:
#   Erstellt eine übersichtliche Ordnerstruktur für alle Teilnehmenden und
#   kopiert die hochgeladenen Screenshots in die passenden Ordner.
#
# Zielstruktur:
#
#   05_Participants/
#   ├── participant_1/
#   │   ├── Tag_1/
#   │   │   ├── participant_1_Tag_1_Photo_1.jpg
#   │   │   ├── participant_1_Tag_1_Photo_2.png
#   │   │   └── ...
#   │   ├── Tag_2/
#   │   └── ...
#   └── participant_2/
################################################################################


#===============================================================================
# Packages
#===============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  fs,
  tools
)


#===============================================================================
# Paths
#===============================================================================

data_file <- "01_Data/taeglicher_fragebogen_screenshot_upload.rds"

source_folder <- "01_Data/files"

target_folder <- "05_Participants"


#===============================================================================
# Load data
#===============================================================================

daily <- readRDS(data_file)


#===============================================================================
# Determine study day for every participant
#===============================================================================

daily <- daily %>%
  mutate(
    scheduled_date = as.Date(scheduled)
  ) %>%
  arrange(personalParticipantCode, scheduled_date) %>%
  group_by(personalParticipantCode) %>%
  mutate(
    study_day = dense_rank(scheduled_date)
  ) %>%
  ungroup()


#===============================================================================
# Create folder structure and copy screenshots
#===============================================================================

dir_create(target_folder)

for (i in seq_len(nrow(daily))) {
  
  row <- daily[i, ]
  
  participant <- row$personalParticipantCode
  day <- row$study_day
  
  # Prüfen, ob überhaupt Screenshots vorhanden sind
  screenshot_columns <- paste0("daily_", 1:10, "_screenshot")
  
  screenshots <- unlist(row[screenshot_columns])
  
  screenshots <- screenshots[
    !is.na(screenshots) &
      screenshots != ""
  ]
  
  # Keine Screenshots -> Datensatz überspringen
  if (length(screenshots) == 0) {
    next
  }
  
  participant_folder <- path(target_folder, participant)
  day_folder <- path(participant_folder, paste0("Tag_", day))
  
  dir_create(participant_folder)
  dir_create(day_folder)
  
  # Einzelne Screenshots kopieren
  for (photo in 1:10) {
    
    column_name <- paste0("daily_", photo, "_screenshot")
    
    filename <- row[[column_name]]
    
    if (is.na(filename) || filename == "") {
      next
    }
    
    source_file <- path(source_folder, filename)
    
    if (!file_exists(source_file)) {
      warning("File not found: ", source_file)
      next
    }
    
    extension <- file_ext(filename)
    
    destination <- path(
      day_folder,
      paste0(
        participant,
        "_Tag_",
        day,
        "_Photo_",
        photo,
        ".",
        extension
      )
    )
    
    file_copy(
      path = source_file,
      new_path = destination,
      overwrite = TRUE
    )
    
  }
  
}

message("Finished copying screenshots.")