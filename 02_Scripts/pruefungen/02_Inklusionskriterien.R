# Inhaltliche Prüfung der verbindlichen Auswahl und der Coding-Verknüpfung
# Nach 01_Daten_einlesen.R ausführen; prüft gespeicherte Daten gegen die Rohexporte
# Keine Dateiveränderung und keine erneute Codierung

library(dplyr)
library(readr)
library(openxlsx)
source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8")

audit <- read_csv("03_Output/Stichprobenpruefung.csv", show_col_types = FALSE)
read_raw <- function(path) {
  read_delim(path, delim = ";", col_types = cols(.default = "c"), show_col_types = FALSE)
}
intro_export <- read_raw("01_Data/screening-befragung tagebuchstudie.csv")
outro_export <- read_raw("01_Data/abschlussbefragung tagebuchstudie.csv")
diary_export <- read_raw("01_Data/täglicher fragebogen + screenshot-upload.csv")
completed_intro_ids <- intro_export$personalParticipantCode[!is.na(clean_text(intro_export$committed))]
completed_outro_ids <- outro_export$personalParticipantCode[!is.na(clean_text(outro_export$committed))]
screened_ids <- intro_export$personalParticipantCode[
  as_logical_safe(intro_export$intro_stop_age) %in% TRUE &
    as_logical_safe(intro_export$intro_stop_usage) %in% TRUE
]

# Die Uploadgrenze direkt aus den Rohfeldern zählen, vor allen Codierphasen
duplicate_rows <- read.xlsx("Backups/Coding/coding_sheet_backup_2026-10-13_140323.xlsx", sheet = "Coding")
duplicate_rows <- duplicate_rows %>% filter(is_marked_screenshot_duplicate(notes))
raw_uploads <- diary_export %>%
  select(personalParticipantCode, matches("^daily_[0-9]+_screenshot$")) %>%
  tidyr::pivot_longer(-personalParticipantCode, values_to = "filename") %>%
  transmute(participant = personalParticipantCode, filename = clean_text(filename)) %>%
  filter(!is.na(filename)) %>%
  distinct(participant, filename) %>%
  anti_join(duplicate_rows %>% transmute(participant, filename = original_filename),
    by = c("participant", "filename"))
exclusions <- read_csv("06_Coding/upload_exclusions.csv", show_col_types = FALSE)
stopifnot(nrow(exclusions) == 4L)
raw_uploads <- raw_uploads %>% anti_join(exclusions, by = c("participant", "filename"))
seven_upload_ids <- raw_uploads %>% count(participant) %>% filter(n >= 7L) %>% pull(participant)
expected_ids <- Reduce(intersect, list(completed_intro_ids, completed_outro_ids, screened_ids, seven_upload_ids))
actual_ids <- audit$participant[audit$included]
stopifnot(setequal(expected_ids, actual_ids))
stopifnot(sum(audit$n_uploads[audit$included]) ==
  nrow(filter(raw_uploads, participant %in% expected_ids)))

for (path in c("01_Data/screening-befragung_tagebuchstudie.rds",
  "01_Data/taeglicher_fragebogen_screenshot_upload.rds",
  "01_Data/abschlussbefragung_tagebuchstudie.rds")) {
  data <- readRDS(path)
  stopifnot(setequal(unique(data$participant), expected_ids))
  screenshot_columns <- grep("^daily_[0-9]+_screenshot$", names(data), value = TRUE)
  if (length(screenshot_columns)) {
    stopifnot(!any(unlist(data[screenshot_columns]) %in% exclusions$filename))
  }
}

coding <- read.xlsx("06_Coding/coding_sheet.xlsx", sheet = "Coding")
stopifnot(!anyDuplicated(coding$screenshot_id), !anyDuplicated(coding$original_filename))
selected_coding <- coding %>% filter(participant %in% expected_ids)
stopifnot(all(selected_coding$original_filename %in% raw_uploads$filename))
stopifnot(setequal(unique(coding$codierphase),
  c("hauptcodierung", "gemeinsame codierung", "reli pretest")))
stopifnot(nrow(duplicate_rows) == 23L)
stopifnot(isTRUE(is_marked_screenshot_duplicate("Screenshot Duplikat")),
  isFALSE(is_marked_screenshot_duplicate("TN hat offensichtlich doppelt teilgenommen")))

message("Verifiziert: ", length(expected_ids), " Personen; ",
  nrow(selected_coding), " Coding-Zeilen einschließlich Pretest")
