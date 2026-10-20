# 01 Gemeinsame Stichprobe -------------------------------------------------------
# Aufgerufen durch: Datenimport, deskriptive Auswertung und Exploration
# Benötigt: geladene Datenpakete und funktionen/Daten_und_Statistik.R
# Liest: rohe Fragebogen-CSV-Dateien und dokumentierte Duplikate im Coding-Backup
# Liefert: intro_raw, outro_raw, upload_raw, study_ids, sample_audit,
#          upload_records, outro_answered und raw_flow
# Schreibt: 03_Output/Stichprobenpruefung.csv
# Festgelegt am 16.10.2026: Intro und Outro abgeschlossen, mindestens sieben Uploads

# Gemeinsamer Import: Rohdaten statt vorgefilterter RDS-Dateien.
# Benötigt dplyr, tidyr, readr, openxlsx und funktionen/Daten_und_Statistik.R.

minimum_screenshots <- 7L

intro_file <- "01_Data/screening-befragung tagebuchstudie.csv"
outro_file <- "01_Data/abschlussbefragung tagebuchstudie.csv"
diary_file <- "01_Data/täglicher fragebogen + screenshot-upload.csv"
read_questionnaire <- function(path) {
  readr::read_delim(path,
    delim = ";", col_types = readr::cols(.default = "c"),
    trim_ws = TRUE, show_col_types = FALSE
  ) %>%
    mutate(participant = clean_text(personalParticipantCode))
}
intro_raw <- read_questionnaire(intro_file)
outro_raw <- read_questionnaire(outro_file)
upload_raw <- read_questionnaire(diary_file)
stopifnot(!anyDuplicated(intro_raw$participant), !anyDuplicated(outro_raw$participant))

# Vier historisch ungecodete Dateien wurden am 16.10.2026 ausdrücklich ausgeschlossen
# Roh-CSV bleibt unverändert; auch die gespeicherten Analyse-RDS enthalten diese
# Dateien nicht mehr als Uploads. Andere Zeilen ohne firstOpened bleiben erhalten
upload_exclusions <- readr::read_csv("06_Coding/upload_exclusions.csv", show_col_types = FALSE)
upload_raw <- upload_raw %>% mutate(across(matches("^daily_[0-9]+_screenshot$"),
  ~ if_else(paste(participant, .x) %in% paste(upload_exclusions$participant, upload_exclusions$filename),
    NA_character_, .x)))

# Dateinamen zählen Uploads, ohne Bilder zu öffnen. Bekannte Duplikate entfallen.
removed_coding <- openxlsx::read.xlsx(
  "06_Coding/coding_sheet_backup_2026-10-13_140323.xlsx",
  sheet = "Coding"
) %>%
  filter(is_marked_screenshot_duplicate(notes)) %>%
  transmute(participant = clean_text(participant), filename = clean_text(original_filename))
upload_records <- upload_raw %>%
  select(participant, scheduled, matches("^daily_[0-9]+_screenshot$")) %>%
  pivot_longer(matches("^daily_[0-9]+_screenshot$"), values_to = "filename") %>%
  mutate(filename = clean_text(filename)) %>%
  filter(!is.na(filename)) %>%
  distinct(participant, filename, .keep_all = TRUE) %>%
  anti_join(removed_coding, by = c("participant", "filename"))
upload_counts <- upload_records %>%
  count(participant, name = "n_uploads")
outro_completion <- outro_raw %>%
  transmute(participant, outro_complete = !is.na(clean_text(committed)))
sample_audit <- intro_raw %>%
  transmute(participant,
    intro_complete = !is.na(clean_text(committed)),
    eligible = as_logical_safe(intro_stop_age) %in% TRUE &
      as_logical_safe(intro_stop_usage) %in% TRUE
  ) %>%
  left_join(upload_counts, by = "participant") %>%
  left_join(outro_completion, by = "participant") %>%
  mutate(
    n_uploads = replace_na(n_uploads, 0L),
    outro_complete = replace_na(outro_complete, FALSE),
    questionnaires_complete = intro_complete & outro_complete,
    included_any_upload = eligible & questionnaires_complete & n_uploads >= 1L,
    included = eligible & questionnaires_complete & n_uploads >= minimum_screenshots,
    included_7 = included,
    exclusion_reason = case_when(
      !eligible ~ "Screening-Kriterien nicht erfüllt",
      !intro_complete ~ "Intro nicht abgeschlossen",
      !outro_complete ~ "Outro nicht abgeschlossen",
      n_uploads < minimum_screenshots ~ "Weniger als sieben unterschiedliche Uploads",
      TRUE ~ "Eingeschlossen"
    )
  )
study_ids <- sample_audit %>%
  filter(included) %>%
  pull(participant)

# Abschluss wird über committed bestimmt. Verfügbare Skalenantworten werden
# anschließend separat geprüft; einzelne fehlende Werte bleiben auswertungsbezogen
outro_answered <- outro_raw %>%
  filter(participant %in% study_ids) %>%
  filter(if_any(matches("^outro_(reactivity|ease)_"), ~ !is.na(clean_text(.x))))
raw_flow <- tibble(
  Stufe = c(
    "Registrierte Intro-Zeilen", "Screening-Kriterien erfüllt",
    "Screening erfüllt, Intro und Outro abgeschlossen",
    "Hauptstichprobe: zusätzlich mindestens sieben unterschiedliche Uploads",
    "Davon mit Outro-Skalenantworten"
  ),
  Personen = c(
    nrow(sample_audit), sum(sample_audit$eligible),
    sum(sample_audit$eligible & sample_audit$questionnaires_complete), length(study_ids),
    sum(outro_answered$participant %in% study_ids)
  )
)

stopifnot(all(sample_audit$intro_complete[sample_audit$included]),
  all(sample_audit$outro_complete[sample_audit$included]),
  all(sample_audit$n_uploads[sample_audit$included] >= 7L))

# Protokoll für die Kontrolle der Stichprobe; enthält keine Bilddaten.
dir.create("03_Output", showWarnings = FALSE)
readr::write_csv(sample_audit, "03_Output/Stichprobenpruefung.csv")
