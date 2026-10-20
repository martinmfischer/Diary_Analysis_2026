# Gemeinsame Funktionen für Import, Analyse, Codiersheet und Vergütung
# Aufruf: source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8")
# Diese Datei definiert Funktionen, liest und schreibt aber keine Studiendaten
# clean_text() behandelt auch den Text "-1" als fehlende Angabe
# clean_numeric() konvertiert nur Zahlen; dort behandeln die Analysen -1 explizit


is_marked_screenshot_duplicate <- function(notes) {
  text <- stringr::str_to_lower(as.character(notes))
  # Hinweise auf doppelte Teilnahme sind keine markierten Bildduplikate
  stringr::str_detect(text, "duplikat|duplicate|doppelt") &
    !stringr::str_detect(text, "doppelt.*teilgenommen|doppelte.*teilnahme")
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\u00a0", " ")
  x <- stringr::str_squish(x)

  missing_tokens <- c(
    "",
    "-1",
    "NA",
    "N/A",
    "NULL"
  )

  x[
    is.na(x) |
      stringr::str_to_upper(x) %in% missing_tokens
  ] <- NA_character_

  x
}


clean_numeric <- function(x) {
  suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
}


markdown_table <- function(data) {
  as.character(knitr::kable(data, format = "pipe", digits = 2, row.names = FALSE))
}

# Präregistrierter Index: sechs Items, Item 5 umpolen, nur vollständige Werte.
incidentality_scores <- function(data) {
  items <- data[paste0("intro_incidentality_", 1:6)]
  items[] <- lapply(items, function(x) dplyr::na_if(clean_numeric(x), -1))
  items$intro_incidentality_5 <- 6 - items$intro_incidentality_5
  items
}

# Für alle explorativen Korrelationen dieselbe Regel für gültige Wertepaare.
safe_spearman <- function(x, y) {
  valid <- is.finite(x) & is.finite(y)
  if (sum(valid) < 3 || stats::sd(x[valid]) == 0 || stats::sd(y[valid]) == 0) {
    return(NA_real_)
  }
  stats::cor(x[valid], y[valid], method = "spearman")
}


as_logical_safe <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  x_character <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(x)
    )
  )

  dplyr::case_when(
    x_character %in% c(
      "true",
      "t",
      "1",
      "yes",
      "y",
      "ja",
      "j"
    ) ~ TRUE,
    x_character %in% c(
      "false",
      "f",
      "0",
      "no",
      "n",
      "nein"
    ) ~ FALSE,
    TRUE ~ NA
  )
}


clean_binary <- function(x) {
  raw <- stringr::str_to_lower(
    clean_text(x)
  )

  numeric_value <- clean_numeric(raw)

  dplyr::case_when(
    raw %in% c(
      "true",
      "t",
      "yes",
      "y",
      "ja",
      "j",
      "completed",
      "complete"
    ) ~ 1L,
    raw %in% c(
      "false",
      "f",
      "no",
      "n",
      "nein",
      "incomplete"
    ) ~ 0L,
    numeric_value == 1 ~ 1L,
    numeric_value == 0 ~ 0L,
    numeric_value == -1 ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}


recode_gender_numeric <- function(x) {
  x_character <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(x)
    )
  )

  numeric_candidate <- clean_numeric(x_character)

  dplyr::case_when(
    numeric_candidate %in% 1:3 ~ numeric_candidate,
    x_character %in% c("w", "weiblich", "female", "frau") ~ 1,
    x_character %in% c("m", "männlich", "maennlich", "male", "mann") ~ 2,
    x_character %in% c("d", "divers", "diverse", "other") ~ 3,
    TRUE ~ NA_real_
  )
}


# ===============================================================================
# 02 Safe numeric operations
# ===============================================================================

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}


safe_sd <- function(x) {
  if (sum(!is.na(x)) < 2) {
    return(NA_real_)
  }

  stats::sd(x, na.rm = TRUE)
}


safe_median <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  stats::median(x, na.rm = TRUE)
}


safe_min <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  min(x, na.rm = TRUE)
}


safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}


safe_divide <- function(numerator, denominator) {
  result <- numerator / denominator

  result[
    is.na(denominator) |
      denominator == 0
  ] <- NA_real_

  result
}


safe_date_min <- function(x) {
  x <- suppressWarnings(
    as.Date(x)
  )

  if (all(is.na(x))) {
    return(
      as.Date(NA)
    )
  }

  min(x, na.rm = TRUE)
}


#-------------------------------------------------------------------------------
# Screenshot index: single source of truth for study day, photo and filename
#-------------------------------------------------------------------------------
# Wird von vorbereitung/01_Codingsheet_erstellen.R verwendet
# Der gemeinsame Index stellt sicher, dass Tag, Foto-Nummer und Dateiname
# per Konstruktion übereinstimmen. Nimmt die breiten Daily-Daten (Spalten
# daily_<n>_<feld>, inkl. daily_<n>_screenshot) und liefert eine Zeile je
# hochgeladenem Screenshot.
derive_screenshot_index <- function(daily, participant_folder = "05_Participants") {
  if (!"committed" %in% names(daily)) daily$committed <- NA

  daily <- daily %>%
    dplyr::mutate(
      dplyr::across(dplyr::matches("^daily_[0-9]+_"), as.character),
      participant = clean_text(personalParticipantCode),
      submission_row = dplyr::row_number(),
      scheduled_date = suppressWarnings(as.Date(scheduled))
    ) %>%
    dplyr::arrange(participant, scheduled, committed, submission_row) %>%
    dplyr::group_by(participant) %>%
    dplyr::mutate(
      # Studientag = Kalenderabstand zum ersten Upload (1-basiert).
      first_scheduled_date = safe_date_min(scheduled_date),
      study_day = as.integer(scheduled_date - first_scheduled_date) + 1L
    ) %>%
    dplyr::ungroup()

  daily %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("^daily_[0-9]+_"),
      names_to = c("screenshot_slot", ".value"),
      names_pattern = "^daily_([0-9]+)_(.+)$"
    ) %>%
    dplyr::mutate(
      screenshot_slot = as.integer(screenshot_slot),
      original_filename = clean_text(screenshot)
    ) %>%
    dplyr::filter(!is.na(original_filename)) %>%
    # Foto-Nummer fortlaufend je Person und Studientag, stabil sortiert.
    dplyr::arrange(
      participant, study_day, scheduled, committed, submission_row, screenshot_slot
    ) %>%
    dplyr::group_by(participant, study_day) %>%
    dplyr::mutate(photo = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      ext = tools::file_ext(original_filename),
      ext = dplyr::if_else(is.na(ext) | ext == "", "", paste0(".", ext)),
      filename = paste0(participant, "_Tag_", study_day, "_Photo_", photo, ext),
      filepath = file.path(
        participant_folder, participant, paste0("Tag_", study_day), filename
      ),
      screenshot_id = paste0(participant, "_D", study_day, "_P", photo)
    )
}
