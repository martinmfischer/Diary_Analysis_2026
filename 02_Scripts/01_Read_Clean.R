################################################################################
# Project: Tagebuchstudie – Datenimport
# File:    01_Read_Clean.R
#
#   - Liest die Rohdaten aus CSV-Dateien im Ordner "01_Data" ein.
#   - Entfernt technisch leere Einträge.
#   - Entfernt Personen, die die Screening-Einschlusskriterien nicht erfüllen.
#   - Entfernt Personen ohne vollständig abgeschlossene Outro-Befragung.
#   - Entfernt Personen mit weniger als 3 unterschiedlichen Screenshot-Tagen.
#   - Speichert die bereinigten Datensätze als RDS-Dateien im gleichen Ordner.
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
# Settings
# ==============================================================================

minimum_participation_days <- 3L


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
# Clean empty entries
# ==============================================================================

## Technisch leere Diary-/Outro-Einträge entfernen

empty_diary <- diary %>% filter(is.na(firstOpened))

n_diary_empty <- sum(is.na(diary$firstOpened))
n_outro_empty <- sum(is.na(outro$firstOpened))

diary <- diary %>% filter(!is.na(firstOpened))
outro <- outro %>% filter(!is.na(firstOpened))

message("Empty diary entries removed: ", n_diary_empty)
message("Empty outro entries removed: ", n_outro_empty)


# ==============================================================================
# Remove participants failing screening eligibility
# ==============================================================================

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

message(
  "Excluded participants due to screening eligibility: ",
  length(users_to_remove)
)


## Aus Diary und Outro entfernen

n_diary_before <- nrow(diary)
n_outro_before <- nrow(outro)

diary <- diary %>%
  filter(!personalParticipantCode %in% users_to_remove)

outro <- outro %>%
  filter(!personalParticipantCode %in% users_to_remove)

message(
  "Removed diary rows due to screening eligibility: ",
  n_diary_before - nrow(diary)
)

message(
  "Removed outro rows due to screening eligibility: ",
  n_outro_before - nrow(outro)
)


# ==============================================================================
# Remove incomplete outro participants
# ==============================================================================

## Nur vollständig abgeschlossene Outro-Befragungen behalten.
## `committed` wird wie bisher als Abschlussindikator verwendet.

n_outro_participants_before <- n_distinct(
  outro$personalParticipantCode,
  na.rm = TRUE
)

outro <- outro %>%
  filter(!is.na(committed))

outro_users <- unique(outro$personalParticipantCode)

n_removed_incomplete_outro <- n_outro_participants_before -
  n_distinct(outro$personalParticipantCode, na.rm = TRUE)

screening <- screening %>%
  filter(personalParticipantCode %in% outro_users)

diary <- diary %>%
  filter(personalParticipantCode %in% outro_users)

message(
  "Removed participants without completed outro: ",
  n_removed_incomplete_outro
)


# ==============================================================================
# Remove participants with fewer than 3 screenshot days
# ==============================================================================

## Ein Tag zählt als Teilnahmetag, wenn in der entsprechenden Diary-Zeile
## mindestens ein Screenshot-Feld tatsächlich befüllt ist.
##
## Mehrere Screenshots am selben Tag zählen weiterhin nur als EIN Teilnahmetag.

is_valid_screenshot <- function(x) {
  x <- as.character(x)
  
  !is.na(x) &
    stringr::str_squish(x) != "" &
    !stringr::str_squish(x) %in% c("-1", "NA")
}


## Screenshot-Variablen automatisch erkennen, z. B.
## daily_1_screenshot, daily_2_screenshot, ...

screenshot_variables <- names(diary)[
  stringr::str_detect(names(diary), "^daily_[0-9]+_screenshot$")
]

if (length(screenshot_variables) == 0) {
  stop(
    "Keine Screenshot-Variablen nach dem Muster ",
    "`daily_X_screenshot` im Diary-Datensatz gefunden."
  )
}


## Hilfsfunktion: Datum aus Zeit-/Datumsvariable gewinnen.
## Bevorzugt wird `scheduled`; falls dieses fehlt oder leer ist,
## wird auf `committed` und anschließend `firstOpened` zurückgegriffen.

date_from_column <- function(data, variable) {
  
  if (!variable %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }
  
  value <- as.character(data[[variable]])
  value <- substr(value, 1, 10)
  
  value[
    !stringr::str_detect(
      value,
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
    )
  ] <- NA_character_
  
  value
}


## Pro Diary-Zeile bestimmen, ob mindestens ein Screenshot vorhanden ist.

diary_with_participation <- diary %>%
  mutate(
    .diary_row = row_number(),
    screenshot_count_row = rowSums(
      across(
        all_of(screenshot_variables),
        ~ as.integer(is_valid_screenshot(.x))
      ),
      na.rm = TRUE
    ),
    has_screenshot = screenshot_count_row > 0
  )


scheduled_day <- date_from_column(
  diary_with_participation,
  "scheduled"
)

committed_day <- date_from_column(
  diary_with_participation,
  "committed"
)

opened_day <- date_from_column(
  diary_with_participation,
  "firstOpened"
)


## Falls ausnahmsweise kein Datum verfügbar ist, erhält die Zeile eine
## eindeutige Fallback-ID. Dadurch geht ein vorhandener Screenshot nicht verloren.

diary_with_participation <- diary_with_participation %>%
  mutate(
    participation_day = dplyr::coalesce(
      scheduled_day,
      committed_day,
      opened_day,
      paste0("row_", .diary_row)
    )
  )


## Screenshot-Tage je Person zählen.

participation_summary <- diary_with_participation %>%
  filter(
    !is.na(personalParticipantCode),
    has_screenshot
  ) %>%
  group_by(personalParticipantCode) %>%
  summarise(
    screenshot_days = n_distinct(participation_day),
    screenshots_total = sum(screenshot_count_row, na.rm = TRUE),
    .groups = "drop"
  )


## Personen identifizieren, die das Diary-Inklusionskriterium erfüllen.

eligible_diary_users <- participation_summary %>%
  filter(screenshot_days >= minimum_participation_days) %>%
  pull(personalParticipantCode)


## Für die Konsolenausgabe explizit bestimmen, wer wegen zu weniger
## Screenshot-Tage ausgeschlossen wird. Personen ohne einen einzigen Screenshot
## haben keinen Eintrag in `participation_summary` und werden ebenfalls erfasst.

users_before_diary_filter <- union(
  union(
    screening$personalParticipantCode,
    diary$personalParticipantCode
  ),
  outro$personalParticipantCode
) %>%
  unique() %>%
  na.omit()

users_removed_diary <- setdiff(
  users_before_diary_filter,
  eligible_diary_users
)

message(
  "Excluded participants with fewer than ",
  minimum_participation_days,
  " screenshot days: ",
  length(users_removed_diary)
)


## Alle drei Samples auf dieselben auswertbaren Personen beschränken.

screening <- screening %>%
  filter(personalParticipantCode %in% eligible_diary_users)

diary <- diary %>%
  filter(personalParticipantCode %in% eligible_diary_users)

outro <- outro %>%
  filter(personalParticipantCode %in% eligible_diary_users)


# ==============================================================================
# Final sample report
# ==============================================================================

message(
  "Remaining participants in screening: ",
  n_distinct(screening$personalParticipantCode)
)

message(
  "Remaining participants in daily: ",
  n_distinct(diary$personalParticipantCode)
)

message(
  "Remaining participants in outro: ",
  n_distinct(outro$personalParticipantCode)
)

message(
  "Number of diary entries: ",
  nrow(diary)
)

message(
  "Minimum required screenshot days: ",
  minimum_participation_days
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

message("CSV files successfully read, cleaned and saved as RDS.")
