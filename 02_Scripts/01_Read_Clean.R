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

source(file.path("02_Scripts", "00_Helpers.R"))


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

message("Empty diary entries removed: ", n_diary_empty)
message("Empty outro entries removed: ", n_outro_empty)


## Ausgeschlossene Teilnehmende bestimmen
## Stop-Items robust als logische Werte parsen (true/false, 1/0, ja/nein).

screening <- screening %>%
  mutate(
    eligible_age = as_logical_safe(intro_stop_age),
    eligible_usage = as_logical_safe(intro_stop_usage)
  )

screening_eliminated <- screening %>%
  filter(!(eligible_age %in% TRUE) | !(eligible_usage %in% TRUE))

screening <- screening %>%
  filter(eligible_age %in% TRUE, eligible_usage %in% TRUE)

users_to_remove <- unique(screening_eliminated$personalParticipantCode)

message("Excluded participants: ", length(users_to_remove))


## Aus Diary und Outro entfernen

n_diary_before <- nrow(diary)
n_outro_before <- nrow(outro)

diary <- diary %>%
  filter(!personalParticipantCode %in% users_to_remove)

outro <- outro %>%
  filter(!personalParticipantCode %in% users_to_remove)

message("Removed diary rows: ", n_diary_before - nrow(diary))
message("Removed outro rows: ", n_outro_before - nrow(outro))



# ==============================================================================
# Remove incomplete outro participants
# ==============================================================================

nrow_outro_before <- nrow(outro)
outro <- outro %>% filter(!is.na(committed))
nrow_outro_after <- nrow(outro)

outro_users <- outro$personalParticipantCode

screening <- screening %>% filter(personalParticipantCode %in% outro_users)
diary <- diary %>% filter(personalParticipantCode %in% outro_users)

message("Removed another ", nrow_outro_before - nrow_outro_after, " users that did not finish the outro survey")
message("Remaining participants in screening: ",
        n_distinct(diary$personalParticipantCode))
message("Remaining participants in daily: ",
        n_distinct(screening$personalParticipantCode))
message("Remaining participants in outro: ",
        n_distinct(outro$personalParticipantCode))
message("Number of diary entries: ", nrow(diary))

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
    "abschlussbefragung_tagebuchstudie.rds"
  )
)

# ==============================================================================
# Finished
# ==============================================================================

message("CSV files successfully read and saved as RDS.")