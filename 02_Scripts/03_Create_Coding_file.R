################################################################################
# Project: Tagebuchstudie
# File:    03_Create_Coding_File.R
#
# Purpose:
#   Erstellt die Coding-Datei für die manuelle Inhaltsanalyse.
#
#   - Eine Zeile = ein Screenshot
#   - Übernimmt alle automatisch verfügbaren Informationen
#   - Legt leere Spalten für die spätere Codierung an
#
# Input:
#   01_Data/taeglicher_fragebogen_screenshot_upload.rds
#
# Output:
#   06_Coding/coding_sheet.xlsx
#   06_Coding/coding_sheet.rds
################################################################################


#===============================================================================
# Packages
#===============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  openxlsx,
  tools
)


#===============================================================================
# Paths
#===============================================================================

data_file <- "01_Data/taeglicher_fragebogen_screenshot_upload.rds"

output_folder <- "06_Coding"

dir.create(output_folder, showWarnings = FALSE)


#===============================================================================
# Load data
#===============================================================================

daily <- readRDS(data_file)


#===============================================================================
# Harmonize variable types
#===============================================================================

daily <- daily %>%
  mutate(
    across(matches("^daily_\\d+_account$"), as.character),
    across(matches("^daily_\\d+_topic$"), as.character)
  )


#===============================================================================
# Determine study day
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
# Create screenshot-level dataset
#===============================================================================

coding <- purrr::map_dfr(seq_len(nrow(daily)), function(i){
  
  row <- daily[i,]
  
  participant <- row$personalParticipantCode
  day <- row$study_day
  
  purrr::map_dfr(1:10, function(photo){
    
    screenshot <- row[[paste0("daily_", photo, "_screenshot")]]
    
    if(is.na(screenshot) || screenshot == ""){
      return(NULL)
    }
    
    extension <- tools::file_ext(screenshot)
    
    new_filename <- paste0(
      participant,
      "_Tag_",
      day,
      "_Photo_",
      photo,
      ".",
      extension
    )
    
    tibble(
      
      ##########################################################################
      # IDs
      ##########################################################################
      
      participant = participant,
      study_day = day,
      photo = photo,
      
      ##########################################################################
      # File information
      ##########################################################################
      
      original_filename = screenshot,
      
      filename = new_filename,
      
      filepath = file.path(
        "05_Participants",
        participant,
        paste0("Tag_", day),
        new_filename
      ),
      
      ##########################################################################
      # Automatic diary variables
      ##########################################################################
      
      platform = as.numeric(row[[paste0("daily_", photo, "_platform")]]),
      
      account = as.character(row[[paste0("daily_", photo, "_account")]]),
      
      topic_participant = as.character(row[[paste0("daily_", photo, "_topic")]]),
      
      incidentality = as.numeric(row[[paste0("daily_", photo, "_incidentality")]]),
      
      interaction_read =
        as.numeric(row[[paste0("daily_", photo, "_interaction_1")]]),
      
      interaction_research =
        as.numeric(row[[paste0("daily_", photo, "_interaction_2")]]),
      
      interaction_engagement =
        as.numeric(row[[paste0("daily_", photo, "_interaction_3")]]),
      
      locality =
        as.numeric(row[[paste0("daily_", photo, "_locality")]]),
      
      situation =
        as.numeric(row[[paste0("daily_", photo, "_situation")]]),
      
      ##########################################################################
      # Manual coding
      ##########################################################################
      
      topic_main = NA_character_,
      topic_sub = NA_character_,
      source = NA_character_,
      source_name = NA_character_,
      media_format = NA_character_,
      notes = NA_character_,
      coder = NA_character_,
      
      ##########################################################################
      # Workflow
      ##########################################################################
      
      coding_completed = FALSE,
      coding_date = as.Date(NA)
      
    )
    
  })
  
})


#===============================================================================
# Export
#===============================================================================

saveRDS(
  coding,
  file = file.path(
    output_folder,
    "coding_sheet.rds"
  )
)

openxlsx::write.xlsx(
  coding,
  file = file.path(
    output_folder,
    "coding_sheet.xlsx"
  ),
  overwrite = TRUE
)


message(
  nrow(coding),
  " screenshots exported to coding_sheet.xlsx"
)