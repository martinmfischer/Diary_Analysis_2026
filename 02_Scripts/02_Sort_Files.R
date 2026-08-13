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

rm(list = ls())

#===============================================================================
# Packages
#===============================================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  fs,
  tools
)

source(file.path("02_Scripts", "00_Helpers.R"))


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
# Build the shared screenshot index and copy files
#===============================================================================

# Studientag, Foto-Nummer und Zieldateiname stammen aus derselben Funktion wie
# im Coding-Sheet (03_Create_Coding_file.R). Dadurch stimmen die hier kopierten
# Dateien und die im Coding-Sheet referenzierten Pfade per Konstruktion überein.
index <- derive_screenshot_index(daily, participant_folder = target_folder)

dir_create(target_folder)

n_copied <- 0L
n_missing <- 0L

for (i in seq_len(nrow(index))) {
  source_file <- path(source_folder, index$original_filename[i])
  destination <- index$filepath[i]

  if (!file_exists(source_file)) {
    warning("Source file not found: ", source_file)
    n_missing <- n_missing + 1L
    next
  }

  dir_create(path_dir(destination))
  file_copy(source_file, destination, overwrite = TRUE)
  n_copied <- n_copied + 1L
}

message("Copied screenshots: ", n_copied)
message("Missing source files: ", n_missing)
message("Finished copying screenshots.")