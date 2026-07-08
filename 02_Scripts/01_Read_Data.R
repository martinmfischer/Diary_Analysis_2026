################################################################################
# Project: Tagebuchstudie – Datenimport
# File:    01_Read_Data.R
#
#   - Liest die Rohdaten aus CSV-Dateien im Ordner "01_Data" ein.
#   - Speichert die Datensätze als RDS-Dateien im gleichen Ordner.
#
################################################################################


# ==============================================================================
# Packages
# ==============================================================================

#install.packages("pacman")
pacman::p_load("readr")


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


# ==============================================================================
# Finished
# ==============================================================================

message("CSV-Dateien erfolgreich eingelesen und als RDS gespeichert.")