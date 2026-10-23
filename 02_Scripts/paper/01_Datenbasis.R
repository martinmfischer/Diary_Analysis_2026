# Datenbasis für die Paper-Analysen
# Benötigt: Pakete und gemeinsame Funktionen aus 02_Paper_Analysen.R.
# Liefert: intro, daily, daily_public, outro, Stichprobenfluss und Codebook.
# Die Bilder werden nicht geöffnet. Rohdaten und Coding Sheet bleiben unverändert.

daily_file <- "06_Coding/coding_sheet.xlsx"

# 02 Daten einlesen und Schlüssel prüfen ----------------------------------------

source("02_Scripts/analyseschritte/01_Stichprobe.R", encoding = "UTF-8", local = TRUE)
daily_raw <- read.xlsx(daily_file, sheet = "Coding")
codebook <- read.xlsx(daily_file, sheet = "Codebook")

intro_raw <- intro_raw %>%
  mutate(participant = clean_text(personalParticipantCode))

outro_raw <- outro_raw %>%
  mutate(participant = clean_text(personalParticipantCode))

daily_raw <- daily_raw %>%
  mutate(
    participant = clean_text(participant),
    screenshot_id = clean_text(screenshot_id),
    across(
      c(
        public_rel_coded, advertisement_coded, topic_coded,
        source_coded, media_format
      ),
      clean_numeric
    )
  )

# Intro und Outro: eine Zeile je Person. Daily: eine Zeile je Screenshot.
stopifnot(
  !anyNA(intro_raw$participant),
  !anyNA(outro_raw$participant),
  !anyNA(daily_raw$participant),
  !anyNA(daily_raw$screenshot_id),
  !anyDuplicated(intro_raw$participant),
  !anyDuplicated(outro_raw$participant),
  !anyDuplicated(daily_raw$screenshot_id)
)

marked_duplicates <- daily_raw %>%
  filter(is_marked_screenshot_duplicate(notes))

if (nrow(marked_duplicates) > 0) {
  stop("Das Coding Sheet enthält noch markierte Duplikate.")
}

# 03 Kategoriensystem prüfen ----------------------------------------------------

coding_categories <- codebook %>%
  transmute(
    variable = Variable,
    code = as.character(Code),
    category = str_trim(Category)
  ) %>%
  filter(str_detect(code, "^[0-9]+$"))

expected_codes <- list(
  public_rel_coded = c(1, 0, 99, 98, 97),
  advertisement_coded = c(1, 0, 99),
  topic_coded = c(1:14, 99),
  source_coded = c(1:10, 99),
  media_format = c(1:3, 99)
)

for (variable in names(expected_codes)) {
  categories <- coding_categories %>%
    filter(.data$variable == .env$variable)

  if (anyDuplicated(categories$code) || anyNA(categories$category) ||
    !setequal(categories$code, as.character(expected_codes[[variable]]))) {
    stop("Codebook prüfen: ", variable)
  }

  observed_codes <- na.omit(daily_raw[[variable]])

  if (any(!observed_codes %in% expected_codes[[variable]])) {
    stop("Ungültige Codes: ", variable)
  }
}

# 04 Gemeinsame Analysestichprobe -----------------------------------------------

participant_selection <- sample_audit %>%
  rename(n_screenshots = n_uploads) %>%
  mutate(intro_available = TRUE)

included_ids <- participant_selection %>%
  filter(included) %>%
  pull(participant)

if (length(included_ids) == 0) {
  stop("Leere Analysestichprobe.")
}

intro <- intro_raw %>% filter(participant %in% included_ids)
daily <- daily_raw %>% filter(participant %in% included_ids)
outro <- outro_answered %>% filter(participant %in% included_ids)
daily_public <- daily %>% filter(public_rel_coded == 1)

if (nrow(daily_public) == 0) {
  stop("Keine öffentlich relevanten Beiträge vorhanden.")
}

sample_flow <- tibble(
  Stufe = c(
    "Eingeschlossene Personen (Rohdaten)", "Intro vorhanden", "Daily: Personen mit Coding",
    "Daily: codierte Screenshots",
    "Outro: Personen mit Skalenantworten",
    "Öffentlich relevante Screenshots"
  ),
  N = c(
    length(included_ids),
    nrow(intro),
    n_distinct(daily$participant),
    nrow(daily),
    nrow(outro),
    nrow(daily_public)
  )
)
