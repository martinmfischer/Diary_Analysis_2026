
################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    99_Payment.R
# Purpose: Zahlungsdaten aus SoSci mit den tatsächlichen Screenshot-Uploads
#          verknüpfen und eine auszahlungsfertige Excel-Datei erzeugen.
#
# Auszahlungskriterium:
#   - mindestens 3 unterschiedliche Teilnahmetage mit >= 1 Screenshot
#   - Auszahlung: 25 Euro
#
# Inputs:
#   - `ds` wird oben direkt über die SoSci-API geladen
#   - 01_Data/taeglicher_fragebogen_screenshot_upload.rds
#
# Output:
#   - 03_Output/Auszahlung_Tagebuchstudie.xlsx
################################################################################
rm(list = ls())

eval(parse("https://survey.ifkw.lmu.de/mesm-inzentive/?act=vT203cD1JiYYOUfLn2Me78sZ&vQuality&rScript", encoding="UTF-8"))

#===============================================================================
# 01 Packages and settings
#===============================================================================
# Zentrale Einstellungen für Auszahlungsschwelle, Betrag und Dateipfade.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  openxlsx,
  fs
)

minimum_participation_days <- 3L
payment_amount <- 25

upload_file <- file.path(
  "01_Data",
  "taeglicher_fragebogen_screenshot_upload.rds"
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
# Prüft die benötigten Variablen und vereinheitlicht Participant Codes / IBANs.

if (!exists("ds")) {
  stop("Die SoSci-API hat kein Objekt `ds` erzeugt.")
}

if (!file.exists(upload_file)) {
  stop("Upload-Datei nicht gefunden: ", upload_file)
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

is_valid_screenshot <- function(x) {
  x <- as.character(x)
  !is.na(x) &
    stringr::str_squish(x) != "" &
    !stringr::str_squish(x) %in% c("-1", "NA")
}


#===============================================================================
# 03 Prepare payment survey data
#===============================================================================
# Eine Person soll nur einmal in der Auszahlungsliste vorkommen. Falls jemand die
# Zahlungsbefragung mehrfach abgeschickt hat, wird der jüngste vollständige Fall
# verwendet; andernfalls der zuletzt vorliegende Datensatz.

payment_raw <- as_tibble(ds) %>%
  mutate(
    participant = clean_code(IN01_RV1),
    .payment_row = row_number(),
    .finished = if ("FINISHED" %in% names(.)) {
      as.logical(FINISHED)
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

payment <- payment_raw %>%
  filter(.finished %in% TRUE) %>%
  arrange(
    participant,
    desc(.lastdata),
    desc(.payment_row)
  ) %>%
  distinct(participant, .keep_all = TRUE)

if (nrow(payment) == 0) {
  stop("Keine abgeschlossenen Zahlungsbefragungen mit Participant Code gefunden.")
}


#===============================================================================
# 04 Count actual screenshot participation days
#===============================================================================
# Gezählt wird ein Tag nur dann, wenn in der jeweiligen Daily-Zeile mindestens
# ein Screenshot-Feld tatsächlich befüllt ist. Mehrere Screenshots am selben Tag
# zählen weiterhin nur als EIN Teilnahmetag.

uploads_raw <- readRDS(upload_file) %>%
  as_tibble()

participant_variable <- intersect(
  c("personalParticipantCode", "personal_participant_code", "participant"),
  names(uploads_raw)
)

if (length(participant_variable) == 0) {
  stop("Kein Personal Participant Code in der Upload-Datei gefunden.")
}

participant_variable <- participant_variable[[1]]

screenshot_variables <- names(uploads_raw)[
  stringr::str_detect(names(uploads_raw), "^daily_[0-9]+_screenshot$")
]

if (length(screenshot_variables) == 0) {
  stop("Keine Variablen nach dem Muster `daily_X_screenshot` gefunden.")
}

uploads <- uploads_raw %>%
  mutate(
    participant = clean_code(.data[[participant_variable]]),
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

# Der geplante Befragungstag (`scheduled`) ist die bevorzugte Tagesdefinition.
# Falls er fehlt, wird auf committed bzw. firstOpened zurückgegriffen. Als letzte
# Absicherung zählt eine Zeile mit Screenshot als eigener Tag.
date_from_column <- function(data, variable) {
  if (!variable %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }
  
  value <- as.character(data[[variable]])
  value <- substr(value, 1, 10)
  value[!stringr::str_detect(value, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")] <- NA_character_
  value
}

scheduled_day <- date_from_column(uploads, "scheduled")
committed_day <- date_from_column(uploads, "committed")
opened_day <- date_from_column(uploads, "firstOpened")

uploads <- uploads %>%
  mutate(
    participation_day = dplyr::coalesce(
      scheduled_day,
      committed_day,
      opened_day,
      paste0("row_", .upload_row)
    )
  )

upload_summary <- uploads %>%
  filter(!is.na(participant)) %>%
  group_by(participant) %>%
  summarise(
    Teilnahmetage = n_distinct(participation_day[has_screenshot]),
    Screenshots = sum(screenshot_count_row, na.rm = TRUE),
    .groups = "drop"
  )


#===============================================================================
# 05 Merge payment data and determine eligibility
#===============================================================================
# Alle Personen mit abgeschlossener Zahlungsbefragung bleiben in der Liste.
# Fehlender Diary-Match entspricht 0 beobachteten Teilnahmetagen und damit keiner
# automatischen Auszahlung.

payment_table <- payment %>%
  transmute(
    participant,
    Name = clean_text_payment(IN02_02),
    Vorname = clean_text_payment(IN02_01),
    IBAN = clean_iban(IN02_07),
    `Straße Hausnr.` = clean_text_payment(IN02_03),
    PLZ = clean_text_payment(IN02_04),
    Ort = clean_text_payment(IN02_05),
    Land = clean_text_payment(IN02_06)
  ) %>%
  left_join(
    upload_summary,
    by = "participant"
  ) %>%
  mutate(
    Teilnahmetage = replace_na(Teilnahmetage, 0L),
    Screenshots = replace_na(Screenshots, 0L),
    `Hat genug Screenshots hochgeladen` = Teilnahmetage >= minimum_participation_days,
    Betrag = if_else(
      `Hat genug Screenshots hochgeladen`,
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
    Teilnahmetage,
    Screenshots,
    `Hat genug Screenshots hochgeladen`,
    `Straße Hausnr.`,
    PLZ,
    Ort,
    Land
  ) %>%
  arrange(
    desc(`Hat genug Screenshots hochgeladen`),
    Name,
    Vorname
  )


#===============================================================================
# 06 Payment/QC checks
#===============================================================================
# Kritische Fälle werden nicht stillschweigend entfernt, sondern separat für die
# manuelle Kontrolle zusammengestellt.

missing_payment_details <- payment_table %>%
  filter(
    `Hat genug Screenshots hochgeladen` &
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

payment_without_diary_match <- payment_table %>%
  filter(Teilnahmetage == 0) %>%
  select(`Personal Participant Code`, Name, Vorname)

eligible_without_payment <- upload_summary %>%
  filter(Teilnahmetage >= minimum_participation_days) %>%
  anti_join(
    payment %>% select(participant),
    by = "participant"
  ) %>%
  arrange(desc(Teilnahmetage), participant)

# Derselbe IBAN bei mehreren Auszahlungsfällen ist nicht zwingend falsch, sollte
# aber vor einer Überweisung kurz geprüft werden.
duplicate_iban <- payment_table %>%
  filter(
    `Hat genug Screenshots hochgeladen`,
    !is.na(IBAN)
  ) %>%
  add_count(IBAN, name = "N_mit_dieser_IBAN") %>%
  filter(N_mit_dieser_IBAN > 1) %>%
  arrange(IBAN, Name, Vorname)


#===============================================================================
# 07 Create formatted Excel payment list
#===============================================================================
# Das erste Sheet ist direkt als Auszahlungsliste nutzbar. Personen ohne erfülltes
# Kriterium bleiben sichtbar, erhalten aber Betrag = 0 Euro. Ein zweites Sheet
# enthält ausschließlich technische Kontrollhinweise ohne zusätzliche Analysen.

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

# Participant Code, PLZ und IBAN zwingend als Text behandeln, damit führende Nullen erhalten bleiben.
text_style <- openxlsx::createStyle(numFmt = "@")

if (nrow(excel_table) > 0) {
  openxlsx::addStyle(
    workbook,
    "Auszahlung",
    style = text_style,
    rows = 4:(nrow(excel_table) + 3),
    cols = c(1, 4, 10),
    gridExpand = TRUE,
    stack = TRUE
  )
}

# Visuelle Markierung des Auszahlungskriteriums.
eligibility_col <- which(names(excel_table) == "Hat genug Screenshots hochgeladen")

if (nrow(excel_table) > 0) {
  openxlsx::conditionalFormatting(
    workbook,
    "Auszahlung",
    cols = eligibility_col,
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
    cols = eligibility_col,
    rows = 4:(nrow(excel_table) + 3),
    rule = "FALSE",
    style = openxlsx::createStyle(
      fgFill = "#FCE4D6",
      fontColour = "#9C0006"
    )
  )
}

openxlsx::setColWidths(
  workbook,
  "Auszahlung",
  cols = seq_len(ncol(excel_table)),
  widths = c(22, 20, 18, 28, 13, 14, 12, 30, 30, 10, 20, 18)
)

openxlsx::freezePane(
  workbook,
  "Auszahlung",
  firstActiveRow = 4
)


# Kontrollblatt: nur Fälle, die vor der Auszahlung geprüft werden sollten.
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
  "Doppelte Participant Codes in der Zahlungsbefragung",
  payment_duplicates
)

write_control_block(
  "Auszahlungsberechtigt, aber unvollständige Zahlungsdaten",
  missing_payment_details
)

write_control_block(
  "Zahlungsbefragung vorhanden, aber kein Screenshot-Tag gefunden",
  payment_without_diary_match
)

write_control_block(
  "Mindestens 3 Screenshot-Tage, aber keine abgeschlossene Zahlungsbefragung",
  eligible_without_payment
)

write_control_block(
  "Mehrfach verwendete IBAN unter Auszahlungsberechtigten",
  duplicate_iban
)

openxlsx::setColWidths(
  workbook,
  "Kontrolle",
  cols = 1:12,
  widths = "auto"
)

openxlsx::saveWorkbook(
  workbook,
  file = output_file,
  overwrite = TRUE
)


#===============================================================================
# 08 Console report
#===============================================================================
# Kurzer Workflow-Check für die Auszahlung, ohne sensible Stammdaten auszugeben.

total_payment <- sum(payment_table$Betrag, na.rm = TRUE)
n_eligible <- sum(payment_table$`Hat genug Screenshots hochgeladen`, na.rm = TRUE)

cat(
  "\n============================================================\n",
  "PAYMENT CHECK COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat("Abgeschlossene Zahlungsbefragungen: ", nrow(payment), "\n", sep = "")
cat("Auszahlungsberechtigt (>= ", minimum_participation_days, " Tage): ", n_eligible, "\n", sep = "")
cat("Nicht auszahlungsberechtigt: ", nrow(payment_table) - n_eligible, "\n", sep = "")
cat("Gesamtauszahlung: ", format(total_payment, nsmall = 2, decimal.mark = ","), " EUR\n", sep = "")
cat("Doppelte Zahlungs-Codes: ", nrow(payment_duplicates), "\n", sep = "")
cat("Fehlende Zahlungsdetails bei Berechtigten: ", nrow(missing_payment_details), "\n", sep = "")
cat("Berechtigte ohne Zahlungsbefragung: ", nrow(eligible_without_payment), "\n", sep = "")
cat("Mehrfach verwendete IBANs (Zeilen): ", nrow(duplicate_iban), "\n", sep = "")
cat("Excel: ", output_file, "\n", sep = "")
cat("============================================================\n")
