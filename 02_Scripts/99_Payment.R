################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    99_Payment.R
# Purpose: Zahlungsdaten aus SoSci mit Screening, tatsächlichen Screenshot-
#          Uploads und Abschlussbefragung verknüpfen und eine auszahlungsfertige
#          Excel-Datei erzeugen.
#
# Auszahlungskriterien:
#   1) Screening vollständig abgeschlossen
#   2) mindestens 3 unterschiedliche Teilnahmetage mit >= 1 Screenshot
#   3) Abschlussbefragung (Outro) vollständig abgeschlossen
#   4) Ausschluss über intro_stop_age / intro_stop_usage greift weiterhin hart
#   5) Auszahlung: 25 Euro
#
# Inputs:
#   - `ds` wird oben direkt über die SoSci-API geladen (Zahlungsbefragung)
#   - 01_Data/screening-befragung tagebuchstudie.csv
#   - 01_Data/täglicher fragebogen + screenshot-upload.csv
#   - 01_Data/abschlussbefragung tagebuchstudie.csv
#
# Output:
#   - 03_Output/Auszahlung_Tagebuchstudie.xlsx
################################################################################
rm(list = ls())

# Zahlungsbefragung direkt aus SoSci laden
eval(parse(
  "https://survey.ifkw.lmu.de/mesm-inzentive/?act=vT203cD1JiYYOUfLn2Me78sZ&vQuality&rScript",
  encoding = "UTF-8"
))


#===============================================================================
# 01 Packages, helpers and settings
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  readr,
  tidyverse,
  openxlsx,
  fs
)

source(file.path("02_Scripts", "00_Helpers.R"))

if (!exists("as_logical_safe")) {
  stop("Die Helper-Funktion `as_logical_safe()` wurde nicht gefunden.")
}

minimum_participation_days <- 3L
payment_amount <- 25

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

output_folder <- "03_Output"
output_file <- file.path(
  output_folder,
  "Auszahlung_Tagebuchstudie.xlsx"
)

fs::dir_create(output_folder)


#===============================================================================
# 02 Basic checks and cleaning helpers
#===============================================================================

if (!exists("ds")) {
  stop("Die SoSci-API hat kein Objekt `ds` erzeugt.")
}

input_files <- c(screening_file, diary_file, outro_file)
missing_input_files <- input_files[!file.exists(input_files)]

if (length(missing_input_files) > 0) {
  stop(
    "Folgende RAW-Dateien wurden nicht gefunden: ",
    paste(missing_input_files, collapse = ", ")
  )
}

required_payment_variables <- c(
  "IN01_RV1", # Personal Participant Code
  "IN02_01",  # Vorname
  "IN02_02",  # Nachname
  "IN02_03",  # Straße + Hausnummer
  "IN02_04",  # PLZ
  "IN02_05",  # Ort
  "IN02_06",  # Land
  "IN02_07"   # IBAN
)

missing_payment_variables <- setdiff(
  required_payment_variables,
  names(ds)
)

if (length(missing_payment_variables) > 0) {
  stop(
    "Folgende Variablen fehlen in den SoSci-Zahlungsdaten: ",
    paste(missing_payment_variables, collapse = ", ")
  )
}

clean_code <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "-1")] <- NA_character_
  stringr::str_to_lower(x)
}

clean_text_payment <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "-1")] <- NA_character_
  x
}

clean_iban <- function(x) {
  x <- clean_text_payment(x)
  ifelse(
    is.na(x),
    NA_character_,
    stringr::str_to_upper(stringr::str_remove_all(x, "\\s+"))
  )
}

is_nonempty <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  !is.na(x) & !x %in% c("", "NA", "-1")
}

is_valid_screenshot <- function(x) {
  is_nonempty(x)
}

# Prüft, ob eine Befragungszeile als vollständig abgeschickt gelten kann.
# Priorität:
#   1) `committed` (bei den RAW-Dateien der Diary-Study)
#   2) FINISHED / finished (falls in einem Export vorhanden)
#
# Bei `committed` zählen TRUE bzw. ein Zeitstempel als abgeschlossen;
# FALSE/0/leere Werte zählen als nicht abgeschlossen.
survey_complete_vector <- function(data, survey_name) {
  if ("committed" %in% names(data)) {
    x <- stringr::str_to_lower(
      stringr::str_squish(as.character(data$committed))
    )
    
    return(
      !is.na(x) &
        !x %in% c("", "na", "-1", "false", "0", "nein", "no")
    )
  }
  
  completion_variable <- intersect(
    c("FINISHED", "finished", "Finished"),
    names(data)
  )
  
  if (length(completion_variable) > 0) {
    return(
      as_logical_safe(data[[completion_variable[[1]]]]) %in% TRUE
    )
  }
  
  stop(
    "Für `", survey_name,
    "` wurde weder `committed` noch FINISHED/finished gefunden. ",
    "Damit kann 'vollständig abgeschlossen' nicht sicher bestimmt werden."
  )
}

# Participant-Code in RAW-Dateien robust finden.
find_participant_variable <- function(data, data_name) {
  candidate <- intersect(
    c("personalParticipantCode", "personal_participant_code", "participant"),
    names(data)
  )
  
  if (length(candidate) == 0) {
    stop("Kein Personal Participant Code in `", data_name, "` gefunden.")
  }
  
  candidate[[1]]
}

# Datum aus einem möglichen Zeitstempel ziehen.
date_from_column <- function(data, variable) {
  if (!variable %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }
  
  value <- as.character(data[[variable]])
  value <- substr(value, 1, 10)
  value[!stringr::str_detect(value, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")] <- NA_character_
  value
}


#===============================================================================
# 03 Read RAW study files
#===============================================================================

screening_raw <- read_delim(
  screening_file,
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

diary_raw <- read_delim(
  diary_file,
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

outro_raw <- read_delim(
  outro_file,
  delim = ";",
  show_col_types = FALSE,
  trim_ws = TRUE
)

screening_participant_var <- find_participant_variable(
  screening_raw,
  "Screening"
)

diary_participant_var <- find_participant_variable(
  diary_raw,
  "Diary"
)

outro_participant_var <- find_participant_variable(
  outro_raw,
  "Outro"
)


#===============================================================================
# 04 Clean empty Diary / Outro entries
#===============================================================================
# Wie im bisherigen Cleaning: technisch leere Einträge ohne firstOpened entfernen.

if (!"firstOpened" %in% names(diary_raw)) {
  stop("Variable `firstOpened` fehlt in der Diary-RAW-Datei.")
}

if (!"firstOpened" %in% names(outro_raw)) {
  stop("Variable `firstOpened` fehlt in der Outro-RAW-Datei.")
}

n_diary_empty <- sum(!is_nonempty(diary_raw$firstOpened))
n_outro_empty <- sum(!is_nonempty(outro_raw$firstOpened))

diary <- diary_raw %>%
  filter(is_nonempty(firstOpened))

outro <- outro_raw %>%
  filter(is_nonempty(firstOpened))

message("Empty diary entries removed: ", n_diary_empty)
message("Empty outro entries removed: ", n_outro_empty)


#===============================================================================
# 05 Screening: exclusion status + completion status
#===============================================================================
# WICHTIG:
# Die bisherige Ausschlusslogik bleibt inhaltlich unverändert: Wenn intro_stop_age
# oder intro_stop_usage nicht TRUE ist (also auch NA), ist die Person NICHT
# teilnahme- bzw. auszahlungsberechtigt.
#
# Neu: Diese Personen werden nicht mehr aus den Daten bzw. der Excel-Tabelle
# entfernt. Stattdessen werden sie über ein eigenes Boolean-Flag kenntlich
# gemacht. So bleiben alle Fälle transparent sichtbar.

required_screening_variables <- c(
  screening_participant_var,
  "intro_stop_age",
  "intro_stop_usage"
)

missing_screening_variables <- setdiff(
  required_screening_variables,
  names(screening_raw)
)

if (length(missing_screening_variables) > 0) {
  stop(
    "Folgende Variablen fehlen in der Screening-Datei: ",
    paste(missing_screening_variables, collapse = ", ")
  )
}

screening <- screening_raw %>%
  mutate(
    participant = clean_code(.data[[screening_participant_var]]),
    eligible_age = as_logical_safe(intro_stop_age),
    eligible_usage = as_logical_safe(intro_stop_usage),
    .screening_complete = survey_complete_vector(screening_raw, "Screening")
  )

screening_eliminated <- screening %>%
  filter(
    !is.na(participant),
    !(eligible_age %in% TRUE) | !(eligible_usage %in% TRUE)
  )

# Wie bisher: Wenn ein Code in mindestens einer Screening-Zeile an einem
# Stop-Kriterium scheitert, gilt er als hart ausgeschlossen.
users_to_remove <- unique(screening_eliminated$participant)

message("Participants failing screening eligibility: ", length(users_to_remove))

screening_excluded_summary <- screening_eliminated %>%
  group_by(participant) %>%
  summarise(
    `Alle intro_stop_age TRUE` = all(eligible_age %in% TRUE),
    `Alle intro_stop_usage TRUE` = all(eligible_usage %in% TRUE),
    `N Screening-Zeilen mit Ausschluss` = n(),
    `Grund` = "intro_stop_age und/oder intro_stop_usage nicht TRUE",
    .groups = "drop"
  ) %>%
  arrange(participant)

# Diary und Outro werden NICHT mehr um diese Personen bereinigt. Die Codes
# bleiben für die Statusprüfung erhalten; der Ausschluss greift später über das
# Boolean-Flag `Erfüllt Screening-Einschlusskriterien`.
diary <- diary %>%
  mutate(participant = clean_code(.data[[diary_participant_var]]))

outro <- outro %>%
  mutate(participant = clean_code(.data[[outro_participant_var]]))

# Screeningstatus für alle Codes, die im Screening vorkommen.
# "Screening vollständig" und "Einschlusskriterien erfüllt" werden bewusst
# getrennt ausgewiesen: Auch eine hart ausgeschlossene Person kann einen
# vollständig abgeschickten Screening-Datensatz besitzen.
screening_status <- screening %>%
  filter(!is.na(participant)) %>%
  group_by(participant) %>%
  summarise(
    `Hat Screening vollständig ausgefüllt` = any(.screening_complete %in% TRUE),
    `Erfüllt Screening-Einschlusskriterien` =
      !first(participant) %in% users_to_remove,
    .groups = "drop"
  )


#===============================================================================
# 06 Prepare payment survey data
#===============================================================================
# Alle Codes aus der Zahlungsbefragung bleiben erhalten. Für Stammdaten wird
# bevorzugt der jüngste vollständig abgeschickte Datensatz genutzt; falls keiner
# vorliegt, der jüngste vorhandene Datensatz. Der Abschlussstatus wird als
# eigenes Boolean-Flag ausgewiesen.

payment_raw <- as_tibble(ds) %>%
  mutate(
    participant = clean_code(IN01_RV1),
    .payment_row = row_number(),
    .finished = if ("FINISHED" %in% names(.)) {
      as_logical_safe(FINISHED)
    } else {
      TRUE
    },
    .lastdata = if ("LASTDATA" %in% names(.)) {
      suppressWarnings(as.POSIXct(as.character(LASTDATA), tz = "Europe/Berlin"))
    } else {
      as.POSIXct(NA)
    }
  ) %>%
  filter(!is.na(participant))

payment_duplicates <- payment_raw %>%
  count(participant, name = "N_Zahlungsbefragungen") %>%
  filter(N_Zahlungsbefragungen > 1)

payment_status <- payment_raw %>%
  group_by(participant) %>%
  summarise(
    `Hat Zahlungsbefragung vollständig ausgefüllt` = any(.finished %in% TRUE),
    .groups = "drop"
  )

# Ein Datensatz pro Code für Name/Adresse/IBAN: vollständige Fälle zuerst,
# anschließend nach Aktualität.
payment <- payment_raw %>%
  arrange(
    participant,
    desc(.finished),
    desc(.lastdata),
    desc(.payment_row)
  ) %>%
  distinct(participant, .keep_all = TRUE)

payment_completed <- payment_raw %>%
  filter(.finished %in% TRUE) %>%
  arrange(
    participant,
    desc(.lastdata),
    desc(.payment_row)
  ) %>%
  distinct(participant, .keep_all = TRUE)


#===============================================================================
# 07 Count actual screenshot participation days from RAW Diary
#===============================================================================
# Gezählt wird ein Tag nur dann, wenn in der jeweiligen Daily-Zeile mindestens
# ein Screenshot-Feld tatsächlich befüllt ist. Mehrere Screenshots am selben Tag
# zählen weiterhin nur als EIN Teilnahmetag.

screenshot_variables <- names(diary)[
  stringr::str_detect(names(diary), "^daily_[0-9]+_screenshot$")
]

if (length(screenshot_variables) == 0) {
  stop("Keine Variablen nach dem Muster `daily_X_screenshot` gefunden.")
}

diary <- diary %>%
  mutate(
    .upload_row = row_number(),
    screenshot_count_row = rowSums(
      across(
        all_of(screenshot_variables),
        ~ as.integer(is_valid_screenshot(.x))
      ),
      na.rm = TRUE
    ),
    has_screenshot = screenshot_count_row > 0
  )

scheduled_day <- date_from_column(diary, "scheduled")
committed_day <- date_from_column(diary, "committed")
opened_day <- date_from_column(diary, "firstOpened")

diary <- diary %>%
  mutate(
    participation_day = dplyr::coalesce(
      scheduled_day,
      committed_day,
      opened_day,
      paste0("row_", .upload_row)
    )
  )

upload_summary <- diary %>%
  filter(!is.na(participant)) %>%
  group_by(participant) %>%
  summarise(
    Teilnahmetage = n_distinct(participation_day[has_screenshot]),
    Screenshots = sum(screenshot_count_row, na.rm = TRUE),
    `Hat genug Screenshots hochgeladen` =
      Teilnahmetage >= minimum_participation_days,
    .groups = "drop"
  )


#===============================================================================
# 08 Determine complete Outro participation
#===============================================================================

outro <- outro %>%
  mutate(
    .outro_complete = survey_complete_vector(outro, "Outro")
  )

outro_status <- outro %>%
  filter(!is.na(participant)) %>%
  group_by(participant) %>%
  summarise(
    `Hat Outro vollständig ausgefüllt` = any(.outro_complete %in% TRUE),
    .groups = "drop"
  )


#===============================================================================
# 09 Build study-status table
#===============================================================================
# Zentrale Personenliste = Union aller Participant Codes, die irgendwo in
# Screening, Diary, Outro oder Zahlungsbefragung vorkommen. Dadurch steht jeder
# bekannte Fall in der späteren Excel-Tabelle, auch wenn einzelne Befragungen
# fehlen oder die Screening-Einschlusskriterien nicht erfüllt sind.

participant_universe <- tibble(
  participant = unique(c(
    screening$participant,
    diary$participant,
    outro$participant,
    payment_raw$participant
  ))
) %>%
  filter(!is.na(participant))

study_status <- participant_universe %>%
  left_join(
    screening_status,
    by = "participant"
  ) %>%
  left_join(
    upload_summary,
    by = "participant"
  ) %>%
  left_join(
    outro_status,
    by = "participant"
  ) %>%
  left_join(
    payment_status,
    by = "participant"
  ) %>%
  mutate(
    `Hat Screening vollständig ausgefüllt` = replace_na(
      `Hat Screening vollständig ausgefüllt`,
      FALSE
    ),
    `Erfüllt Screening-Einschlusskriterien` = replace_na(
      `Erfüllt Screening-Einschlusskriterien`,
      FALSE
    ),
    Teilnahmetage = replace_na(Teilnahmetage, 0L),
    Screenshots = replace_na(Screenshots, 0L),
    `Hat genug Screenshots hochgeladen` = replace_na(
      `Hat genug Screenshots hochgeladen`,
      FALSE
    ),
    `Hat Outro vollständig ausgefüllt` = replace_na(
      `Hat Outro vollständig ausgefüllt`,
      FALSE
    ),
    `Hat Zahlungsbefragung vollständig ausgefüllt` = replace_na(
      `Hat Zahlungsbefragung vollständig ausgefüllt`,
      FALSE
    ),
    `Studienteilnahme vollständig` =
      `Erfüllt Screening-Einschlusskriterien` &
      `Hat Screening vollständig ausgefüllt` &
      `Hat genug Screenshots hochgeladen` &
      `Hat Outro vollständig ausgefüllt`
  )


#===============================================================================
# 10 Merge payment data and determine eligibility
#===============================================================================
# Die Auszahlungstabelle startet nun mit ALLEN bekannten Participant Codes.
# Personen, die am Screening scheitern oder einzelne Erhebungsbestandteile nicht
# abgeschlossen haben, bleiben sichtbar und erhalten entsprechend FALSE-Flags.
#
# Betrag = 25 Euro nur bei vollständiger Studienteilnahme UND vollständig
# abgeschickter Zahlungsbefragung. Fehlende Stammdaten werden weiterhin separat
# im Kontrollblatt markiert.

payment_details <- payment %>%
  transmute(
    participant,
    Name = clean_text_payment(IN02_02),
    Vorname = clean_text_payment(IN02_01),
    IBAN = clean_iban(IN02_07),
    `Straße Hausnr.` = clean_text_payment(IN02_03),
    PLZ = clean_text_payment(IN02_04),
    Ort = clean_text_payment(IN02_05),
    Land = clean_text_payment(IN02_06)
  )

payment_table <- study_status %>%
  left_join(
    payment_details,
    by = "participant"
  ) %>%
  mutate(
    `Auszahlungsbereit` =
      `Studienteilnahme vollständig` &
      `Hat Zahlungsbefragung vollständig ausgefüllt`,
    Betrag = if_else(
      `Auszahlungsbereit`,
      payment_amount,
      0
    )
  ) %>%
  select(
    `Personal Participant Code` = participant,
    Name,
    Vorname,
    IBAN,
    Betrag,
    `Auszahlungsbereit`,
    `Studienteilnahme vollständig`,
    `Erfüllt Screening-Einschlusskriterien`,
    `Hat Screening vollständig ausgefüllt`,
    `Hat genug Screenshots hochgeladen`,
    `Hat Outro vollständig ausgefüllt`,
    `Hat Zahlungsbefragung vollständig ausgefüllt`,
    Teilnahmetage,
    Screenshots,
    `Straße Hausnr.`,
    PLZ,
    Ort,
    Land
  ) %>%
  arrange(
    desc(`Auszahlungsbereit`),
    desc(`Studienteilnahme vollständig`),
    Name,
    Vorname,
    `Personal Participant Code`
  )


#===============================================================================
# 11 Payment/QC checks
#===============================================================================

missing_payment_details <- payment_table %>%
  filter(
    `Auszahlungsbereit` &
      (
        is.na(Name) |
          is.na(Vorname) |
          is.na(IBAN) |
          is.na(`Straße Hausnr.`) |
          is.na(PLZ) |
          is.na(Ort) |
          is.na(Land)
      )
  )

screening_ineligible <- payment_table %>%
  filter(!`Erfüllt Screening-Einschlusskriterien`) %>%
  select(
    `Personal Participant Code`,
    Name,
    Vorname,
    `Erfüllt Screening-Einschlusskriterien`,
    `Hat Screening vollständig ausgefüllt`
  )

participants_without_screening <- payment_table %>%
  filter(!`Hat Screening vollständig ausgefüllt`) %>%
  select(
    `Personal Participant Code`,
    Name,
    Vorname,
    `Erfüllt Screening-Einschlusskriterien`,
    `Hat Screening vollständig ausgefüllt`
  )

participants_without_enough_screenshots <- payment_table %>%
  filter(!`Hat genug Screenshots hochgeladen`) %>%
  select(
    `Personal Participant Code`,
    Name,
    Vorname,
    Teilnahmetage,
    Screenshots,
    `Hat genug Screenshots hochgeladen`
  )

participants_without_outro <- payment_table %>%
  filter(!`Hat Outro vollständig ausgefüllt`) %>%
  select(
    `Personal Participant Code`,
    Name,
    Vorname,
    `Hat Outro vollständig ausgefüllt`
  )

eligible_without_payment <- payment_table %>%
  filter(
    `Studienteilnahme vollständig`,
    !`Hat Zahlungsbefragung vollständig ausgefüllt`
  ) %>%
  select(
    `Personal Participant Code`,
    Name,
    Vorname,
    `Studienteilnahme vollständig`,
    `Hat Zahlungsbefragung vollständig ausgefüllt`
  ) %>%
  arrange(`Personal Participant Code`)

# Derselbe IBAN bei mehreren tatsächlichen Auszahlungsfällen ist nicht zwingend
# falsch, sollte vor der Überweisung aber geprüft werden.
duplicate_iban <- payment_table %>%
  filter(
    `Auszahlungsbereit`,
    !is.na(IBAN)
  ) %>%
  add_count(IBAN, name = "N_mit_dieser_IBAN") %>%
  filter(N_mit_dieser_IBAN > 1) %>%
  arrange(IBAN, Name, Vorname)


#===============================================================================
# 12 Create formatted Excel payment list
#===============================================================================

workbook <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  workbook,
  "Auszahlung",
  gridLines = FALSE
)

# Zeile 1: Gesamtsumme
openxlsx::writeData(
  workbook,
  "Auszahlung",
  x = "Gesamtsumme:",
  startCol = 1,
  startRow = 1
)

openxlsx::writeFormula(
  workbook,
  "Auszahlung",
  x = paste0("SUM(E4:E", nrow(payment_table) + 3, ")"),
  startCol = 5,
  startRow = 1
)

# Zeile 3: Header; Daten ab Zeile 4
excel_table <- payment_table

openxlsx::writeData(
  workbook,
  "Auszahlung",
  x = excel_table,
  startCol = 1,
  startRow = 3,
  headerStyle = openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9EAF0",
    border = "Bottom",
    valign = "center"
  ),
  withFilter = TRUE
)

currency_style <- openxlsx::createStyle(
  numFmt = '#,##0.00 [$€-407]'
)

openxlsx::addStyle(
  workbook,
  "Auszahlung",
  style = currency_style,
  rows = 1,
  cols = 5,
  stack = TRUE
)

if (nrow(excel_table) > 0) {
  openxlsx::addStyle(
    workbook,
    "Auszahlung",
    style = currency_style,
    rows = 4:(nrow(excel_table) + 3),
    cols = 5,
    gridExpand = TRUE,
    stack = TRUE
  )
}

# Participant Code, PLZ und IBAN als Text behandeln.
text_style <- openxlsx::createStyle(numFmt = "@")

if (nrow(excel_table) > 0) {
  participant_col <- which(names(excel_table) == "Personal Participant Code")
  iban_col <- which(names(excel_table) == "IBAN")
  plz_col <- which(names(excel_table) == "PLZ")
  
  openxlsx::addStyle(
    workbook,
    "Auszahlung",
    style = text_style,
    rows = 4:(nrow(excel_table) + 3),
    cols = c(participant_col, iban_col, plz_col),
    gridExpand = TRUE,
    stack = TRUE
  )
}

# TRUE/FALSE-Spalten farblich markieren.
status_columns <- c(
  "Auszahlungsbereit",
  "Studienteilnahme vollständig",
  "Erfüllt Screening-Einschlusskriterien",
  "Hat Screening vollständig ausgefüllt",
  "Hat genug Screenshots hochgeladen",
  "Hat Outro vollständig ausgefüllt",
  "Hat Zahlungsbefragung vollständig ausgefüllt"
)

status_cols <- which(names(excel_table) %in% status_columns)

if (nrow(excel_table) > 0 && length(status_cols) > 0) {
  for (status_col in status_cols) {
    openxlsx::conditionalFormatting(
      workbook,
      "Auszahlung",
      cols = status_col,
      rows = 4:(nrow(excel_table) + 3),
      rule = "TRUE",
      style = openxlsx::createStyle(
        fgFill = "#E2F0D9",
        fontColour = "#375623"
      )
    )
    
    openxlsx::conditionalFormatting(
      workbook,
      "Auszahlung",
      cols = status_col,
      rows = 4:(nrow(excel_table) + 3),
      rule = "FALSE",
      style = openxlsx::createStyle(
        fgFill = "#FCE4D6",
        fontColour = "#9C0006"
      )
    )
  }
}

openxlsx::setColWidths(
  workbook,
  "Auszahlung",
  cols = seq_len(ncol(excel_table)),
  widths = c(
    22, # Participant Code
    20, # Name
    18, # Vorname
    28, # IBAN
    13, # Betrag
    20, # Auszahlungsbereit
    25, # Studienteilnahme vollständig
    36, # Screening-Einschlusskriterien
    34, # Screening vollständig
    34, # Screenshots ausreichend
    31, # Outro vollständig
    38, # Zahlungsbefragung vollständig
    14, # Teilnahmetage
    12, # Screenshots
    30, # Straße
    10, # PLZ
    20, # Ort
    18  # Land
  )
)

openxlsx::freezePane(
  workbook,
  "Auszahlung",
  firstActiveRow = 4
)


# Zusätzliche kompakte Übersicht über alle bekannten Studienteilnehmenden.
# Auch hart ausgeschlossene bzw. unvollständige Fälle bleiben sichtbar.
openxlsx::addWorksheet(
  workbook,
  "Teilnahmestatus",
  gridLines = FALSE
)

status_export <- payment_table %>%
  select(
    `Personal Participant Code`,
    `Auszahlungsbereit`,
    `Studienteilnahme vollständig`,
    `Erfüllt Screening-Einschlusskriterien`,
    `Hat Screening vollständig ausgefüllt`,
    `Hat genug Screenshots hochgeladen`,
    `Hat Outro vollständig ausgefüllt`,
    `Hat Zahlungsbefragung vollständig ausgefüllt`,
    Teilnahmetage,
    Screenshots
  ) %>%
  arrange(
    desc(`Auszahlungsbereit`),
    desc(`Studienteilnahme vollständig`),
    `Personal Participant Code`
  )

openxlsx::writeData(
  workbook,
  "Teilnahmestatus",
  x = status_export,
  startRow = 1,
  startCol = 1,
  withFilter = TRUE,
  headerStyle = openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9EAF0",
    border = "Bottom"
  )
)

if (nrow(status_export) > 0) {
  status_export_cols <- which(names(status_export) %in% status_columns)
  
  for (status_col in status_export_cols) {
    openxlsx::conditionalFormatting(
      workbook,
      "Teilnahmestatus",
      cols = status_col,
      rows = 2:(nrow(status_export) + 1),
      rule = "TRUE",
      style = openxlsx::createStyle(
        fgFill = "#E2F0D9",
        fontColour = "#375623"
      )
    )
    
    openxlsx::conditionalFormatting(
      workbook,
      "Teilnahmestatus",
      cols = status_col,
      rows = 2:(nrow(status_export) + 1),
      rule = "FALSE",
      style = openxlsx::createStyle(
        fgFill = "#FCE4D6",
        fontColour = "#9C0006"
      )
    )
  }
}

openxlsx::setColWidths(
  workbook,
  "Teilnahmestatus",
  cols = 1:ncol(status_export),
  widths = c(22, 20, 25, 36, 34, 34, 31, 38, 14, 12)
)

openxlsx::freezePane(
  workbook,
  "Teilnahmestatus",
  firstActiveRow = 2
)


# Kontrollblatt
openxlsx::addWorksheet(
  workbook,
  "Kontrolle",
  gridLines = FALSE
)

control_row <- 1

write_control_block <- function(title, data) {
  openxlsx::writeData(
    workbook,
    "Kontrolle",
    title,
    startRow = control_row,
    startCol = 1
  )
  
  openxlsx::addStyle(
    workbook,
    "Kontrolle",
    openxlsx::createStyle(textDecoration = "bold"),
    rows = control_row,
    cols = 1,
    stack = TRUE
  )
  
  control_row <<- control_row + 1
  
  if (nrow(data) == 0) {
    openxlsx::writeData(
      workbook,
      "Kontrolle",
      "Keine Fälle",
      startRow = control_row,
      startCol = 1
    )
    control_row <<- control_row + 2
  } else {
    openxlsx::writeData(
      workbook,
      "Kontrolle",
      data,
      startRow = control_row,
      startCol = 1,
      withFilter = FALSE
    )
    control_row <<- control_row + nrow(data) + 2
  }
}

write_control_block(
  "Hart ausgeschlossen über intro_stop_age / intro_stop_usage",
  screening_excluded_summary
)

write_control_block(
  "Doppelte Participant Codes in der Zahlungsbefragung",
  payment_duplicates
)

write_control_block(
  "Screening-Einschlusskriterien nicht erfüllt",
  screening_ineligible
)

write_control_block(
  "Screening nicht vollständig",
  participants_without_screening
)

write_control_block(
  paste0(
    "Weniger als ",
    minimum_participation_days,
    " Screenshot-Tage"
  ),
  participants_without_enough_screenshots
)

write_control_block(
  "Outro nicht vollständig",
  participants_without_outro
)

write_control_block(
  "Studienteilnahme vollständig, aber unvollständige Zahlungsdaten",
  missing_payment_details
)

write_control_block(
  "Studienteilnahme vollständig, aber keine abgeschlossene Zahlungsbefragung",
  eligible_without_payment
)

write_control_block(
  "Mehrfach verwendete IBAN unter Auszahlungsberechtigten",
  duplicate_iban
)

openxlsx::setColWidths(
  workbook,
  "Kontrolle",
  cols = 1:15,
  widths = "auto"
)

openxlsx::saveWorkbook(
  workbook,
  file = output_file,
  overwrite = TRUE
)


#===============================================================================
# 13 Console report
#===============================================================================

total_payment <- sum(payment_table$Betrag, na.rm = TRUE)
n_study_complete <- sum(payment_table$`Studienteilnahme vollständig`, na.rm = TRUE)
n_payment_ready <- sum(payment_table$`Auszahlungsbereit`, na.rm = TRUE)

n_screening_eligible <- sum(
  payment_table$`Erfüllt Screening-Einschlusskriterien`,
  na.rm = TRUE
)

n_screening_complete <- sum(
  payment_table$`Hat Screening vollständig ausgefüllt`,
  na.rm = TRUE
)

n_screenshot_complete <- sum(
  payment_table$`Hat genug Screenshots hochgeladen`,
  na.rm = TRUE
)

n_outro_complete <- sum(
  payment_table$`Hat Outro vollständig ausgefüllt`,
  na.rm = TRUE
)

n_payment_complete <- sum(
  payment_table$`Hat Zahlungsbefragung vollständig ausgefüllt`,
  na.rm = TRUE
)

cat(
  "\n============================================================\n",
  "PAYMENT CHECK COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat("Alle gelisteten Participant Codes: ", nrow(payment_table), "\n", sep = "")
cat("Harte Screening-Ausschlüsse: ", length(users_to_remove), "\n", sep = "")
cat("Screening-Einschlusskriterien erfüllt: ", n_screening_eligible, "\n", sep = "")
cat("Screening vollständig: ", n_screening_complete, "\n", sep = "")
cat(
  "Genug Screenshot-Tage (>= ",
  minimum_participation_days,
  "): ",
  n_screenshot_complete,
  "\n",
  sep = ""
)
cat("Outro vollständig: ", n_outro_complete, "\n", sep = "")
cat("Zahlungsbefragung vollständig: ", n_payment_complete, "\n", sep = "")
cat("Studienteilnahme vollständig: ", n_study_complete, "\n", sep = "")
cat("Auszahlungsbereit: ", n_payment_ready, "\n", sep = "")
cat("Nicht auszahlungsbereit: ", nrow(payment_table) - n_payment_ready, "\n", sep = "")
cat(
  "Gesamtauszahlung: ",
  format(total_payment, nsmall = 2, decimal.mark = ","),
  " EUR\n",
  sep = ""
)
cat("Doppelte Zahlungs-Codes: ", nrow(payment_duplicates), "\n", sep = "")
cat(
  "Fehlende Zahlungsdetails bei Auszahlungsbereiten: ",
  nrow(missing_payment_details),
  "\n",
  sep = ""
)
cat(
  "Vollständige Studienteilnahme ohne Zahlungsbefragung: ",
  nrow(eligible_without_payment),
  "\n",
  sep = ""
)
cat("Mehrfach verwendete IBANs (Zeilen): ", nrow(duplicate_iban), "\n", sep = "")
cat("Excel: ", output_file, "\n", sep = "")
cat("============================================================\n")
