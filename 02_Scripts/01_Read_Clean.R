################################################################################
# Project: Tagebuchstudie – Datenimport
# File:    01_Read_Clean.R
#
#   - Liest die Rohdaten aus CSV-Dateien im Ordner "01_Data" ein.
#   - Speichert die Datensätze als RDS-Dateien im gleichen Ordner.
#
################################################################################


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

empty_diary <- diary %>% filter(is.na(firstOpened)) ## lets save them just in case

diary <- diary %>% filter(!is.na(firstOpened)) ## for some reason, we get a ton of empty entries.

outro <- outro %>% filter(!is.na(firstOpened)) ## for some reason, we get a ton of empty entries.



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