################################################################################
# Project: Tagebuchstudie – Datenimport
# File:    01_Read_Clean.R
#
#   - Liest die Rohdaten aus CSV-Dateien im Ordner "01_Data" ein.
#   - Speichert die Datensätze als RDS-Dateien im gleichen Ordner.
#
################################################################################

rm(list = ls())

# ==============================================================================
# Packages
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load("readr", "tidyverse")


# ==============================================================================
# Paths
# ==============================================================================

data_dir <- "01_Data"

screening_file <- file.path(
  data_dir,
  "screening-befragung tagebuchstudie.csv"
)

diary_file <- file.path(
  data_dir,
  "täglicher fragebogen + screenshot-upload.csv"
)

outro_file <- file.path(
  data_dir,
  "abschlussbefragung tagebuchstudie.csv"
)

# ==============================================================================
# Read CSV files
# ==============================================================================

screening <- read_delim(
  screening_file,
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

diary <- read_delim(
  diary_file,
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

outro <- read_delim(
  outro_file,
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

# ==============================================================================
# Clean Empty entries
# ==============================================================================

## Empty entries entfernen

empty_diary <- diary %>% filter(is.na(firstOpened))

n_diary_empty <- sum(is.na(diary$firstOpened))
n_outro_empty <- sum(is.na(outro$firstOpened))

diary <- diary %>% filter(!is.na(firstOpened))
outro <- outro %>% filter(!is.na(firstOpened))

message("Leere Diary-Einträge entfernt: ", n_diary_empty)
message("Leere Outro-Einträge entfernt: ", n_outro_empty)


## Ausgeschlossene Teilnehmende bestimmen

screening_eliminated <- screening %>%
  filter(!intro_stop_age | !intro_stop_usage)

screening <- screening %>%
  filter(intro_stop_age & intro_stop_usage)

users_to_remove <- unique(screening_eliminated$personalParticipantCode)

message("Ausgeschlossene TN: ", length(users_to_remove))


## Aus Diary und Outro entfernen

n_diary_before <- nrow(diary)
n_outro_before <- nrow(outro)

diary <- diary %>%
  filter(!personalParticipantCode %in% users_to_remove)

outro <- outro %>%
  filter(!personalParticipantCode %in% users_to_remove)

message("Entfernte Diary-Zeilen: ", n_diary_before - nrow(diary))
message("Entfernte Outro-Zeilen: ", n_outro_before - nrow(outro))
message("Verbleibende TN im Screening: ",
        n_distinct(screening$personalParticipantCode))

# ==============================================================================
# Save as RDS
# ==============================================================================

saveRDS(
  screening,
  file = file.path(
    data_dir,
    "screening-befragung_tagebuchstudie.rds"
  )
)

saveRDS(
  diary,
  file = file.path(
    data_dir,
    "taeglicher_fragebogen_screenshot_upload.rds"
  )
)

saveRDS(
  outro,
  file = file.path(
    data_dir,
    "abschlussbefragung tagebuchstudie.rds"
  )
)

# ==============================================================================
# Finished
# ==============================================================================

message("CSV-Dateien erfolgreich eingelesen und als RDS gespeichert.")