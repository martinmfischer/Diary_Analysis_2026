################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    04c_Outro_Analysis.R
# Version: 2026-08-10
#
# Purpose:
#   Kompakte Auswertung der Abschlussbefragung. Im Mittelpunkt stehen die
#   methodisch relevanten Fragen: Hat die Teilnahme das Verhalten verändert
#   (Reaktivität)? Wie gut war die App nutzbar? Lassen sich diese Angaben in
#   ausgewählten Diary-Mustern wiederfinden?
#
# Outputs:
#   03_Output/Outro_Results.xlsx
#     - Scales              publication-ready Skalenübersicht
#     - Items               publication-ready Itemdeskriptiven
#     - Method_Associations wenige theoriegeleitete explorative Zusammenhänge
#     - Open_Text           Freitext für qualitative Sichtung
#   03_Output/outro_prepared.rds
#   03_Output/Outro_Open_Text_Report.md
#     - automatische, rein deskriptive Übersicht der Freitextantworten
#     - Themenhäufigkeiten über ein transparentes Schlagwort-Dictionary
#     - häufige Begriffe und illustrative Originalantworten
#   04_Figures/Outro_*.png
#
# Notes:
#   - Höhere Reactivity-Werte = stärkere studienbedingte Reaktivität.
#   - Items 2, 3 und 5 werden dafür invertiert.
#   - Höhere Ease-of-Use-Werte = höhere wahrgenommene Benutzerfreundlichkeit.
#   - Reliabilität wird mit Cronbachs Alpha und Omega total berichtet.
#   - Ein möglicher präregistrierter Einzelausschluss orientiert sich an Omega
#     total; Omega hierarchical wird für die Ein-Faktor-Lösung nicht verwendet.
################################################################################

rm(list = ls())


#===============================================================================
# 01 Settings and paths
#===============================================================================
# Nur wenige zentrale Schalter. Diagnostische Detailoutputs werden bewusst nicht
# geschrieben; Probleme erscheinen als stop()/warning() oder im Konsolenreport.

omega_cutoff <- 0.70
apply_single_item_exclusion <- TRUE
create_figures <- TRUE

# Freitext-Report: rein deskriptive Orientierung, kein Ersatz für qualitative
# Codierung. Die Originalantworten bleiben unverändert erhalten.
create_open_text_report <- TRUE
open_text_report_include_appendix <- TRUE
open_text_examples_per_theme <- 2
open_text_top_terms <- 12

helper_script <- file.path("02_Scripts", "00_Helpers.R")
data_folder <- "01_Data"
output_folder <- "03_Output"
figure_folder <- "04_Figures"
tables_folder <- file.path("03_Output", "Tables")

daily_participant_file <- file.path(
  output_folder,
  "daily_participant_level.rds"
)

output_excel <- file.path(
  output_folder,
  "Outro_Results.xlsx"
)

output_rds <- file.path(
  output_folder,
  "outro_prepared.rds"
)

open_text_report_file <- file.path(
  output_folder,
  "Outro_Open_Text_Report.md"
)


#===============================================================================
# 02 Packages and shared helpers
#===============================================================================
# Das gemeinsame Helper-Script liefert Cleaning, Reliabilität, Excel-Helfer und
# insbesondere das gemeinsame Grafiktheme von Screening, Daily und Outro.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  psych,
  janitor,
  openxlsx,
  fs
)

if (!file.exists(helper_script)) {
  stop("Helper-Script nicht gefunden: ", helper_script)
}

source(helper_script)

fs::dir_create(output_folder)
fs::dir_create(figure_folder)
fs::dir_create(tables_folder)


#===============================================================================
# 03 Locate and load Outro data
#===============================================================================
# Der Dateiname darf flexibel bleiben; bei mehreren Treffern wird nicht geraten,
# sondern abgebrochen, damit nicht versehentlich die falsche Welle analysiert wird.

outro_files <- fs::dir_ls(
  path = data_folder,
  type = "file",
  regexp = "(?i)(outro|abschluss|closing).*\\.rds$"
)

if (length(outro_files) == 0) {
  stop("No outro/closing RDS file found in 01_Data.")
}

if (length(outro_files) > 1) {
  stop(
    "Multiple possible outro files found:\n- ",
    paste(outro_files, collapse = "\n- ")
  )
}

outro_file <- outro_files[[1]]
outro_raw <- readRDS(outro_file) %>% janitor::clean_names()

participant_variable <- first_existing(
  outro_raw,
  c(
    "personal_participant_code",
    "personalparticipantcode",
    "participant",
    "participant_code"
  ),
  description = "Participant Code"
)

reactivity_raw_items <- paste0("outro_reactivity_", 1:5)
ease_items <- paste0("outro_ease_", 1:8)
closed_items <- c(reactivity_raw_items, ease_items)

required_variables <- c(participant_variable, closed_items)
missing_required <- setdiff(required_variables, names(outro_raw))

if (length(missing_required) > 0) {
  stop(
    "Required outro variables are missing:\n- ",
    paste(missing_required, collapse = "\n- ")
  )
}

# Freitext ist methodisch nützlich, aber kein Grund, die quantitative Analyse zu
# stoppen. Falls die Variablen fehlen, werden leere Spalten ergänzt.
if (!"outro_problems_free" %in% names(outro_raw)) {
  outro_raw$outro_problems_free <- NA_character_
}
if (!"outro_suggestions_free" %in% names(outro_raw)) {
  outro_raw$outro_suggestions_free <- NA_character_
}


#===============================================================================
# 04 Clean participants and item values
#===============================================================================
# Doppelte Abschlüsse werden nur dann eindeutig aufgelöst, wenn ein committed-
# Zeitpunkt vorliegt. Skalenwerte außerhalb 1–5 gelten als Datenfehler.

outro <- outro_raw %>%
  mutate(
    participant = clean_text(.data[[participant_variable]]),
    across(all_of(closed_items), ~ na_if(clean_numeric(.x), -1)),
    problems_free = clean_text(outro_problems_free),
    suggestions_free = clean_text(outro_suggestions_free)
  ) %>%
  filter(!is.na(participant))

duplicate_participants <- outro %>%
  count(participant, name = "N_Rows") %>%
  filter(N_Rows > 1)

if (nrow(duplicate_participants) > 0) {
  if (!"committed" %in% names(outro)) {
    stop(
      "Duplicate participant codes in the outro data, but no `committed` ",
      "variable to unambiguously select the most recent completion."
    )
  }
  
  warning(
    nrow(duplicate_participants),
    " duplicate participant codes: the most recent committed completion is used."
  )
  
  outro <- outro %>%
    arrange(participant, desc(committed)) %>%
    distinct(participant, .keep_all = TRUE)
}

range_issues <- purrr::map_dfr(
  closed_items,
  ~ outro %>%
    filter(!is.na(.data[[.x]]), .data[[.x]] < 1 | .data[[.x]] > 5) %>%
    transmute(participant, Item = .x, Value = .data[[.x]])
)

if (nrow(range_issues) > 0) {
  stop(
    "Outro contains ", nrow(range_issues),
    " values outside the intended 1-5 scale."
  )
}


#===============================================================================
# 05 Construct directionally aligned items
#===============================================================================
# Die Reaktivitätsitems werden so ausgerichtet, dass ein hoher Wert immer mehr
# studienbedingte Veränderung bedeutet. Die Originalitems bleiben erhalten.

outro <- outro %>%
  mutate(
    outro_reactivity_2_reversed = 6 - outro_reactivity_2,
    outro_reactivity_3_reversed = 6 - outro_reactivity_3,
    outro_reactivity_5_reversed = 6 - outro_reactivity_5,
    outro_complete = if_all(all_of(closed_items), ~ !is.na(.x))
  )

reactivity_items <- c(
  "outro_reactivity_1",
  "outro_reactivity_2_reversed",
  "outro_reactivity_3_reversed",
  "outro_reactivity_4",
  "outro_reactivity_5_reversed"
)

reactivity_labels <- c(
  outro_reactivity_1 =
    "Deliberately searched for uploadable posts",
  outro_reactivity_2_reversed =
    "Displayed content felt less like before (reverse-coded)",
  outro_reactivity_3_reversed =
    "Own use deviated more from the usual (reverse-coded)",
  outro_reactivity_4 =
    "More publicly relevant content shown",
  outro_reactivity_5_reversed =
    "Uploads reflected normal use less well (reverse-coded)"
)

ease_labels <- c(
  outro_ease_1 = "The app is user-friendly",
  outro_ease_2 = "Participation requires few steps",
  outro_ease_3 = "Using the app is effortless",
  outro_ease_4 = "Errors can be fixed quickly",
  outro_ease_5 = "The app can be used reliably every time",
  outro_ease_6 = "Downloading and installing was easy",
  outro_ease_7 = "Entering the login code was easy",
  outro_ease_8 = "Finding my way around the app was easy"
)


#===============================================================================
# 06 Restrict to the Diary analysis sample
#===============================================================================
# Das kompakte Daily-Script speichert im Participant-RDS bereits nur Personen,
# die das Diary-Inklusionskriterium erfüllen. Deshalb reicht hier ein ID-Match;
# ein zusätzlicher Abschlussindikator ist nicht mehr nötig.

daily_filter_applied <- FALSE
daily_participant <- NULL

if (file.exists(daily_participant_file)) {
  daily_participant <- readRDS(daily_participant_file) %>%
    janitor::clean_names()
  
  daily_participant_variable <- first_existing(
    daily_participant,
    c("participant", "personal_participant_code", "personalparticipantcode"),
    description = "Participant Code in Daily participant data"
  )
  
  daily_participant <- daily_participant %>%
    mutate(
      participant = clean_text(.data[[daily_participant_variable]])
    ) %>%
    filter(!is.na(participant)) %>%
    distinct(participant, .keep_all = TRUE)
  
  eligible_daily_ids <- daily_participant$participant
  daily_filter_applied <- TRUE
  
  outro_analysis <- outro %>%
    filter(outro_complete, participant %in% eligible_daily_ids)
  
} else {
  warning(
    "daily_participant_level.rds not found; outro is filtered only by ",
    "complete questionnaire."
  )
  
  outro_analysis <- outro %>%
    filter(outro_complete)
}

if (nrow(outro_analysis) == 0) {
  stop("No cases remain after applying the outro/daily criteria.")
}


#===============================================================================
# 07 Reliability and preregistered item-deletion check
#===============================================================================
# Reliabilität ist hier ein Skalencheck, nicht das Hauptergebnis. Berichtet werden
# Alpha und Omega total. Ein einzelnes Item wird nur ausgeschlossen, wenn Omega
# total zunächst < .70 liegt und nach genau einem Ausschluss mindestens .70 ist.

reactivity_reliability <- calculate_scale_reliability(
  outro_analysis,
  reactivity_items,
  "Reactivity"
)

ease_reliability <- calculate_scale_reliability(
  outro_analysis,
  ease_items,
  "Ease of Use"
)

select_by_omega_total <- function(reliability_result, items, cutoff, allow_exclusion) {
  full_omega <- reliability_result$summary$Omega_Total[[1]]
  selected_items <- items
  excluded_item <- NA_character_
  
  candidates <- reliability_result$leave_one_out %>%
    filter(!is.na(Omega_Total)) %>%
    arrange(desc(Omega_Total))
  
  if (
    allow_exclusion &&
    !is.na(full_omega) &&
    full_omega < cutoff &&
    nrow(candidates) > 0 &&
    candidates$Omega_Total[[1]] >= cutoff
  ) {
    excluded_item <- candidates$Item_Removed[[1]]
    selected_items <- setdiff(items, excluded_item)
  }
  
  list(
    selected_items = selected_items,
    excluded_item = excluded_item,
    full_omega = full_omega,
    best_omega_deleted = if (nrow(candidates) == 0) NA_real_ else candidates$Omega_Total[[1]]
  )
}

reactivity_selection <- select_by_omega_total(
  reactivity_reliability,
  reactivity_items,
  omega_cutoff,
  apply_single_item_exclusion
)

ease_selection <- select_by_omega_total(
  ease_reliability,
  ease_items,
  omega_cutoff,
  apply_single_item_exclusion
)

reactivity_final_items <- reactivity_selection$selected_items
ease_final_items <- ease_selection$selected_items

outro_analysis$reactivity_index <- complete_mean(
  outro_analysis,
  reactivity_final_items
)

outro_analysis$ease_index <- complete_mean(
  outro_analysis,
  ease_final_items
)

reactivity_reliability_final <- calculate_scale_reliability(
  outro_analysis,
  reactivity_final_items,
  "Reactivity"
)

ease_reliability_final <- calculate_scale_reliability(
  outro_analysis,
  ease_final_items,
  "Ease of Use"
)


#===============================================================================
# 08 Publication table: scales
#===============================================================================
# Eine kompakte Tabelle enthält alles, was für Methoden-/Ergebnistext typischerweise
# gebraucht wird: N, Itemzahl, Lage/Streuung, CI und Reliabilität.

reactivity_desc <- descriptive_summary(
  outro_analysis$reactivity_index,
  "Reaktivität"
)

ease_desc <- descriptive_summary(
  outro_analysis$ease_index,
  "Ease of Use"
)

scale_table <- bind_rows(
  reactivity_desc %>%
    transmute(
      Scale = "Reactivity",
      N = N_Valid,
      Items = length(reactivity_final_items),
      M = Mean,
      SD,
      CI95_Lower,
      CI95_Upper,
      Alpha = reactivity_reliability_final$summary$Cronbach_Alpha[[1]],
      Omega_Total = reactivity_reliability_final$summary$Omega_Total[[1]],
      Omega_Hierarchical = reactivity_reliability_final$summary$Omega_Hierarchical[[1]],
      Excluded_Item = reactivity_selection$excluded_item,
      Interpretation = "Higher = stronger study-induced reactivity"
    ),
  ease_desc %>%
    transmute(
      Scale = "Ease of Use",
      N = N_Valid,
      Items = length(ease_final_items),
      M = Mean,
      SD,
      CI95_Lower,
      CI95_Upper,
      Alpha = ease_reliability_final$summary$Cronbach_Alpha[[1]],
      Omega_Total = ease_reliability_final$summary$Omega_Total[[1]],
      Omega_Hierarchical = ease_reliability_final$summary$Omega_Hierarchical[[1]],
      Excluded_Item = ease_selection$excluded_item,
      Interpretation = "Higher = better perceived ease of use"
    )
) %>%
  mutate(
    across(c(M, SD, CI95_Lower, CI95_Upper), ~ round(.x, 2)),
    across(c(Alpha, Omega_Total, Omega_Hierarchical), ~ round(.x, 3))
  )


#===============================================================================
# 09 Publication table: items
#===============================================================================
# Itemwerte sind bei Reaktivität besonders wichtig, weil die fünf Fragen mehrere
# Formen möglicher Reaktivität abbilden. Deshalb bleiben sie neben dem Index sichtbar.

reactivity_item_table <- item_descriptives(
  outro_analysis,
  reactivity_items,
  reactivity_labels
) %>%
  mutate(
    Scale = "Reactivity",
    Item_Number = match(Item, reactivity_items),
    Coding = "Direction-aligned: higher = more reactivity"
  )

ease_item_table <- item_descriptives(
  outro_analysis,
  ease_items,
  ease_labels
) %>%
  mutate(
    Scale = "Ease of Use",
    Item_Number = match(Item, ease_items),
    Coding = if_else(
      Item_Number <= 5,
      "Core usability",
      "App-specific"
    )
  )

item_table <- bind_rows(
  reactivity_item_table,
  ease_item_table
) %>%
  arrange(factor(Scale, levels = c("Reactivity", "Ease of Use")), Item_Number) %>%
  transmute(
    Scale,
    Item = Item_Number,
    Statement = Variable,
    Coding,
    N = N_Valid,
    M = round(Mean, 2),
    SD = round(SD, 2),
    CI95_Lower = round(CI95_Lower, 2),
    CI95_Upper = round(CI95_Upper, 2)
  )


#===============================================================================
# 10 Join selected Diary indicators
#===============================================================================
# Für die methodische Validierung reichen wenige Diary-Marker. Die im Daily-Script
# vorberechneten Tagesslopes sind besonders relevant: Sie prüfen, ob subjektiv
# berichtete Reaktivität mit Veränderungen über die sieben Tage zusammenhängt.

if (!is.null(daily_participant)) {
  daily_keep <- c(
    "participant",
    "n_screenshots",
    "n_active_days",
    "share_publicly_relevant",
    "share_incidental_broad",
    "absolute_incidentality_gap_broad",
    "upload_count_day_slope",
    "targeted_post_day_slope",
    "thorough_reading_day_slope"
  )
  
  outro_analysis <- outro_analysis %>%
    left_join(
      daily_participant %>% select(any_of(daily_keep)),
      by = "participant"
    )
}


#===============================================================================
# 11 Exploratory method associations
#===============================================================================
# Nur theoriegeleitete Zusammenhänge werden geprüft. Sie dienen als methodischer
# Plausibilitätscheck und werden nicht als kausale Effekte der Studie interpretiert.

association_specs <- tribble(
  ~Family, ~X, ~Y, ~X_Label, ~Y_Label,
  "Scale relation", "reactivity_index", "ease_index",
  "Reactivity", "Ease of use",
  "Reactivity x diary", "reactivity_index", "upload_count_day_slope",
  "Reactivity", "Trend in upload count over days",
  "Reactivity x diary", "reactivity_index", "targeted_post_day_slope",
  "Reactivity", "Trend in targeted exposure over days",
  "Reactivity x diary", "reactivity_index", "thorough_reading_day_slope",
  "Reactivity", "Trend in thorough reading over days",
  "Reactivity x diary", "reactivity_index", "absolute_incidentality_gap_broad",
  "Reactivity", "Absolute screening-diary incidental-exposure gap",
  "Ease x participation", "ease_index", "n_screenshots",
  "Ease of use", "Number of uploaded screenshots",
  "Ease x participation", "ease_index", "n_active_days",
  "Ease of use", "Number of active diary days"
)

available_associations <- association_specs %>%
  filter(X %in% names(outro_analysis), Y %in% names(outro_analysis))

method_associations <- if (nrow(available_associations) > 0) {
  pmap_dfr(
    available_associations,
    function(Family, X, Y, X_Label, Y_Label) {
      spearman_test(
        outro_analysis,
        X,
        Y,
        X_Label,
        Y_Label
      ) %>%
        mutate(Family = Family, .before = 1)
    }
  ) %>%
    group_by(Family) %>%
    mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH")) %>%
    ungroup() %>%
    transmute(
      Family,
      Predictor = Variable_1,
      Outcome = Variable_2,
      N,
      Spearman_Rho = round(Spearman_Rho, 3),
      P_Value = round(P_Value, 4),
      P_Adjusted_BH = round(P_Adjusted_BH, 4),
      Note = "Exploratory; participant level"
    )
} else {
  tibble(
    Family = character(), Predictor = character(), Outcome = character(),
    N = integer(), Spearman_Rho = double(), P_Value = double(),
    P_Adjusted_BH = double(), Note = character()
  )
}


#===============================================================================
# 12 Open text: descriptive summary and readable report
#===============================================================================
# Die beiden Freitextfragen dienen primär der methodischen Einordnung der App und
# der Studienteilnahme. Deshalb wird hier KEINE automatisierte qualitative
# Inhaltsanalyse behauptet. Stattdessen erzeugt das Skript eine transparente,
# reproduzierbare Orientierung:
#   1. Antwortquote und Antwortlänge,
#   2. heuristische Themenmarker über ein explizites Schlagwort-Dictionary,
#   3. häufige Begriffe nach Entfernung deutscher Stoppwörter,
#   4. illustrative Originalantworten,
#   5. optional einen vollständigen Anhang aller Antworten.
# Die Themenmarker dürfen später eine manuelle Sichtung strukturieren, ersetzen
# aber keine induktive/deduktive qualitative Codierung.

open_text <- outro_analysis %>%
  select(participant, problems_free, suggestions_free) %>%
  pivot_longer(
    cols = c(problems_free, suggestions_free),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  mutate(
    Question = recode(
      Question,
      problems_free = "Problems / uncertainties",
      suggestions_free = "Suggestions for improvement"
    ),
    Response = str_squish(Response),
    Word_Count = str_count(Response, "\\S+")
  ) %>%
  filter(Response != "") %>%
  arrange(Question, participant)

open_text_question_levels <- c(
  "Problems / uncertainties",
  "Suggestions for improvement"
)

# Transparent, methodologically relevant theme markers. Multiple assignments are
# explicitly allowed: one response may address, e.g., both upload and navigation
# problems. Theme names are English; keyword patterns stay German to match the
# German responses.
open_text_theme_dictionary <- tribble(
  ~Scope, ~Theme, ~Pattern,
  "Problems / uncertainties",
  "No problems / uncertainties",
  "^\\s*(nein|nö|keine?( probleme?| schwierigkeiten?| unsicherheiten?)?|nichts|alles (gut|okay|ok)|problemlos|hat (gut|alles) funktioniert)[.! ]*$",
  "Suggestions for improvement",
  "No suggestions",
  "^\\s*(nein|nö|keine?( vorschläge?| anmerkungen?| verbesserungen?)?|nichts|alles (gut|okay|ok)|so (ist|passt) es gut)[.! ]*$",
  "Both",
  "Installation / login / code",
  "install|download|login|log-in|einlog|anmeld|registr|teilnehmer.?code|login.?code|code eing",
  "Both",
  "Upload / screenshot / media selection",
  "upload|hochlad|screenshot|screen.?shot|foto|bild|aufnahme|galerie|kamera|datei ausw",
  "Both",
  "Navigation / operation",
  "navig|orientier|bedien|menü|menu|button|schaltfläche|zurück|weiter|seite wechsel|finde? nicht|gefunden",
  "Both",
  "Technical stability / connection",
  "absturz|abgestürzt|häng|fehler|bug|funktioniert? nicht|ging nicht|laden|lädt|verbind|internet|netz|sync|synchron",
  "Both",
  "Reminders / timing",
  "erinner|benachr|notification|push|uhrzeit|zeitpunkt|morgens|abends|früh|spät",
  "Both",
  "Comprehensibility / task",
  "unklar|unverständlich|verständlich|unsicher|frage|formulierung|definition|öffentlich.? relevant|relevan.*inhalt|was.*hochlad|welche.*beitr",
  "Both",
  "Readability / visual design",
  "schrift|lesbar|schriftgröße|größe der schrift|design|layout|farbe|kontrast|darstellung|optik",
  "Both",
  "Effort / length / steps",
  "aufwand|zeitaufw|mühsam|umständ|zu lang|lange gedauert|viele schritte|weniger schritte|dauer",
  "Both",
  "Data protection / privacy",
  "datenschutz|privat|privacy|persönliche daten|personenbezogen|sicherheit|zugriff.*daten",
  "Both",
  "Device / platform / compatibility",
  "iphone|ipad|ios|android|smartphone|tablet|motorola|facebook|instagram|tiktok|twitter|\\bx\\b|plattform",
  "Both",
  "Positive user experience",
  "benutzerfreund|übersichtlich|intuitiv|einfach|problemlos|gut funktioniert|zufrieden|unkompliziert"
)

# Jede Antwort wird gegen alle für ihre Frage passenden Marker geprüft.
open_text_themes <- if (nrow(open_text) > 0) {
  tidyr::crossing(
    open_text,
    open_text_theme_dictionary
  ) %>%
    filter(Scope == "Both" | Scope == Question) %>%
    filter(
      str_detect(
        Response,
        regex(Pattern, ignore_case = TRUE)
      )
    ) %>%
    distinct(Question, participant, Response, Word_Count, Theme)
} else {
  tibble(
    Question = character(),
    participant = character(),
    Response = character(),
    Word_Count = integer(),
    Theme = character()
  )
}

# Fragebezogene Basiszahlen; zusätzlich wird sichtbar, wie viele Antworten durch
# das Dictionary überhaupt angesprochen wurden. Eine hohe Nichtzuordnungsquote ist
# ein Signal für manuelle Sichtung, nicht für "sonstige" Inhalte.
open_text_tagged <- open_text_themes %>%
  distinct(Question, participant)

open_text_question_summary <- tibble(
  Question = open_text_question_levels
) %>%
  left_join(
    open_text %>%
      group_by(Question) %>%
      summarise(
        N_Responses = n(),
        N_Participants = n_distinct(participant),
        Median_Words = median(Word_Count),
        Mean_Words = mean(Word_Count),
        .groups = "drop"
      ),
    by = "Question"
  ) %>%
  left_join(
    open_text_tagged %>%
      count(Question, name = "N_Tagged"),
    by = "Question"
  ) %>%
  mutate(
    across(c(N_Responses, N_Participants, N_Tagged), ~ replace_na(.x, 0L)),
    Response_Rate = 100 * N_Responses / nrow(outro_analysis),
    N_Untagged = N_Responses - N_Tagged,
    Percent_Untagged = if_else(
      N_Responses > 0,
      100 * N_Untagged / N_Responses,
      NA_real_
    ),
    Mean_Words = round(Mean_Words, 1),
    Response_Rate = round(Response_Rate, 1),
    Percent_Untagged = round(Percent_Untagged, 1)
  )

open_text_theme_summary <- open_text_themes %>%
  count(Question, Theme, name = "N_Responses") %>%
  left_join(
    open_text_question_summary %>%
      select(Question, N_Question_Responses = N_Responses),
    by = "Question"
  ) %>%
  mutate(
    Percent_of_Question_Responses = if_else(
      N_Question_Responses > 0,
      100 * N_Responses / N_Question_Responses,
      NA_real_
    ),
    Percent_of_Question_Responses = round(Percent_of_Question_Responses, 1)
  ) %>%
  arrange(Question, desc(N_Responses), Theme)

# Häufige Begriffe werden deskriptiv über die Zahl der Personen gezählt, die den
# Begriff mindestens einmal verwenden. Das verhindert, dass Wiederholungen in
# einer langen Einzelantwort das Ranking dominieren.
german_stopwords <- if (requireNamespace("stopwords", quietly = TRUE)) {
  stopwords::stopwords("de")
} else {
  c(
    "aber", "alle", "als", "also", "am", "an", "auch", "auf", "aus",
    "bei", "bin", "bis", "da", "das", "dass", "dem", "den", "der", "des",
    "die", "dies", "diese", "dieser", "doch", "ein", "eine", "einem",
    "einen", "einer", "es", "für", "hat", "habe", "haben", "hier", "ich",
    "im", "in", "ist", "ja", "kann", "kein", "keine", "mit", "mir", "nicht",
    "noch", "nur", "oder", "sehr", "sich", "sie", "sind", "so", "und", "von",
    "war", "was", "wie", "wir", "wurde", "zu", "zum", "zur"
  )
}

open_text_custom_stopwords <- c(
  "app", "studie", "gesis", "smart", "teilnahme", "tage", "tag", "fragebogen"
)

open_text_terms <- if (nrow(open_text) > 0) {
  open_text %>%
    transmute(
      Question,
      participant,
      Token = str_extract_all(
        str_to_lower(Response),
        "[a-zäöüß]{3,}"
      )
    ) %>%
    unnest(Token) %>%
    filter(
      !Token %in% german_stopwords,
      !Token %in% open_text_custom_stopwords
    ) %>%
    group_by(Question, Token) %>%
    summarise(
      Participants = n_distinct(participant),
      Occurrences = n(),
      .groups = "drop"
    ) %>%
    group_by(Question) %>%
    arrange(desc(Participants), desc(Occurrences), Token) %>%
    slice_head(n = open_text_top_terms) %>%
    ungroup()
} else {
  tibble(
    Question = character(),
    Token = character(),
    Participants = integer(),
    Occurrences = integer()
  )
}

# Für illustrative Beispiele werden keine "besten" oder besonders eindrucksvollen
# Antworten ausgesucht. Stattdessen nehmen wir Antworten nahe der medianen Länge
# innerhalb eines Themes; das ist reproduzierbar und reduziert Cherry-Picking.
open_text_theme_examples <- open_text_themes %>%
  group_by(Question, Theme) %>%
  mutate(
    Theme_Median_Words = median(Word_Count),
    Distance_to_Median = abs(Word_Count - Theme_Median_Words)
  ) %>%
  arrange(Distance_to_Median, participant) %>%
  slice_head(n = open_text_examples_per_theme) %>%
  ungroup()

# Nicht automatisch erfasste Antworten werden separat markiert. Sie sind für die
# manuelle Prüfung besonders interessant, weil das Dictionary dort offensichtlich
# nicht ausreicht.
open_text_unmatched <- open_text %>%
  anti_join(
    open_text_tagged,
    by = c("Question", "participant")
  )

#-------------------------------------------------------------------------------
# 12a Markdown report
#-------------------------------------------------------------------------------
# Markdown ist absichtlich gewählt: robust, ohne Word-/Pandoc-Abhängigkeit,
# versionskontrollierbar und in RStudio/VS Code/GitHub direkt gut lesbar.

md_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- str_replace_all(x, "\\|", "\\\\|")
  x <- str_replace_all(x, "[\\r\\n]+", " ")
  str_squish(x)
}

md_table <- function(data) {
  if (nrow(data) == 0) return("_No data._")
  
  x <- as.data.frame(
    lapply(data, md_escape),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  c(
    paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |"),
    apply(
      x,
      1,
      function(row) paste0("| ", paste(row, collapse = " | "), " |")
    )
  )
}

make_theme_sentence <- function(question) {
  top <- open_text_theme_summary %>%
    filter(Question == question) %>%
    slice_head(n = 3)
  
  if (nrow(top) == 0) {
    return("No responses were captured by the heuristic theme markers for this question.")
  }
  
  pieces <- paste0(
    top$Theme,
    " (n = ", top$N_Responses,
    "; ", top$Percent_of_Question_Responses, " %)"
  )
  
  paste0(
    "The most frequently marked themes were ",
    paste(pieces, collapse = "; "),
    "."
  )
}

if (create_open_text_report) {
  report_overview <- open_text_question_summary %>%
    transmute(
      Question = Question,
      Responses = N_Responses,
      `Response rate (%)` = Response_Rate,
      `Median words` = Median_Words,
      `Not auto-assigned` = N_Untagged
    )
  
  report_lines <- c(
    "# Open-text overview - closing survey",
    "",
    paste0("*Automatically generated on ", Sys.Date(), ".*"),
    "",
    paste0("Analysis sample: **N = ", nrow(outro_analysis), "** participants."),
    "",
    "> **Note:** This report is a descriptive orientation aid. ",
    "> Theme assignment is based on a transparent keyword dictionary, ",
    "> allows multiple assignments and is **not a qualitative content analysis**. ",
    "> For publication-relevant claims, the original responses should additionally ",
    "> be reviewed manually or coded systematically.",
    "",
    "## Overview",
    "",
    md_table(report_overview),
    ""
  )
  
  for (question in open_text_question_levels) {
    q_summary <- open_text_question_summary %>%
      filter(Question == question)
    
    q_themes <- open_text_theme_summary %>%
      filter(Question == question) %>%
      transmute(
        Theme = Theme,
        N = N_Responses,
        `Share of responses (%)` = Percent_of_Question_Responses
      )
    
    q_terms <- open_text_terms %>%
      filter(Question == question) %>%
      transmute(
        Term = Token,
        `Participants using term` = Participants,
        `Total occurrences` = Occurrences
      )
    
    report_lines <- c(
      report_lines,
      paste0("## ", question),
      "",
      if (nrow(q_summary) > 0) {
        paste0(
          "**", q_summary$N_Responses, " responses** (",
          q_summary$Response_Rate, " % of the analysis sample); ",
          "median = ", q_summary$Median_Words, " words."
        )
      } else {
        "No responses."
      },
      "",
      make_theme_sentence(question),
      "",
      "### Heuristically marked themes",
      "",
      md_table(q_themes),
      "",
      "### Frequent terms",
      "",
      "Primarily counts how many distinct participants use a term.",
      "",
      md_table(q_terms),
      ""
    )
    
    top_themes <- open_text_theme_summary %>%
      filter(Question == question) %>%
      slice_head(n = 6) %>%
      pull(Theme)
    
    if (length(top_themes) > 0) {
      report_lines <- c(
        report_lines,
        "### Illustrative responses for frequent themes",
        "",
        "Examples were selected reproducibly by proximity to the theme's median response length; they are not to be read as representative quotes.",
        ""
      )
      
      for (theme in top_themes) {
        examples <- open_text_theme_examples %>%
          filter(Question == question, Theme == theme)
        
        theme_n <- open_text_theme_summary %>%
          filter(Question == question, Theme == theme) %>%
          pull(N_Responses)
        
        report_lines <- c(
          report_lines,
          paste0("#### ", theme, " (n = ", theme_n, ")"),
          ""
        )
        
        for (i in seq_len(nrow(examples))) {
          report_lines <- c(
            report_lines,
            paste0(
              "> **", md_escape(examples$participant[[i]]), ":** ",
              md_escape(examples$Response[[i]])
            ),
            ""
          )
        }
      }
    }
    
    q_unmatched <- open_text_unmatched %>%
      filter(Question == question)
    
    report_lines <- c(
      report_lines,
      "### Responses not automatically assigned",
      "",
      paste0(
        "**n = ", nrow(q_unmatched), "**. These responses deserve particular ",
        "attention in a manual review because the dictionary does not cover them."
      ),
      ""
    )
  }
  
  if (open_text_report_include_appendix && nrow(open_text) > 0) {
    report_lines <- c(
      report_lines,
      "# Appendix: complete open-text responses",
      "",
      "The following responses are reproduced verbatim in content; only superfluous spaces and line breaks were normalized.",
      ""
    )
    
    for (question in open_text_question_levels) {
      q_raw <- open_text %>%
        filter(Question == question)
      
      report_lines <- c(
        report_lines,
        paste0("## ", question),
        ""
      )
      
      if (nrow(q_raw) == 0) {
        report_lines <- c(report_lines, "_No responses._", "")
      } else {
        for (i in seq_len(nrow(q_raw))) {
          report_lines <- c(
            report_lines,
            paste0(
              "- **", md_escape(q_raw$participant[[i]]), "** — ",
              md_escape(q_raw$Response[[i]])
            )
          )
        }
        report_lines <- c(report_lines, "")
      }
    }
  }
  
  writeLines(
    report_lines,
    con = open_text_report_file,
    useBytes = TRUE
  )
}


#===============================================================================
# 13 Save prepared participant-level data
#===============================================================================
# Das RDS enthält die vollständige finale Outro-Stichprobe inklusive der wenigen
# verbundenen Daily-Marker und ist der maschinenlesbare Input für weitere Analysen.

saveRDS(
  outro_analysis,
  output_rds
)


#===============================================================================
# 14 Publication-oriented Excel workbook
#===============================================================================
# Excel enthält bewusst nur direkt interpretierbare Tabellen. QC, Missingness und
# Item-Deletion-Details werden nicht in zahlreiche separate Arbeitsblätter zerlegt.

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = unname(project_colors["primary"]),
  halign = "center",
  valign = "center",
  border = "Bottom"
)

add_excel_sheet(workbook, "Scales", scale_table, header_style)
add_excel_sheet(workbook, "Items", item_table, header_style)
add_excel_sheet(workbook, "Method_Associations", method_associations, header_style)
add_excel_sheet(workbook, "Open_Text_Summary", open_text_theme_summary, header_style)
add_excel_sheet(workbook, "Open_Text", open_text, header_style)

# Lange Itemtexte und Freitexte bekommen feste Breiten und Zeilenumbruch.
openxlsx::setColWidths(
  workbook,
  "Open_Text_Summary",
  cols = which(names(open_text_theme_summary) %in% c("Question", "Theme")),
  widths = c(28, 42)
)

openxlsx::setColWidths(
  workbook,
  "Items",
  cols = which(names(item_table) %in% c("Statement", "Coding")),
  widths = c(48, 30)
)

openxlsx::setColWidths(
  workbook,
  "Open_Text",
  cols = which(names(open_text) == "Response"),
  widths = 70
)

if (nrow(open_text) > 0) {
  openxlsx::addStyle(
    workbook,
    "Open_Text",
    style = openxlsx::createStyle(wrapText = TRUE, valign = "top"),
    rows = 2:(nrow(open_text) + 1),
    cols = seq_len(ncol(open_text)),
    gridExpand = TRUE
  )
}

openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = TRUE
)

# Publication-ready tables (.docx), APA-style: scales (with omega total/
# hierarchical) and item-level descriptives.
save_pub_table(
  scale_table %>% transmute(
    Scale,
    k = Items,
    `M (SD)` = m_sd(M, SD),
    `95% CI` = fmt_ci(CI95_Lower, CI95_Upper),
    `Cronbach's alpha` = fmt_num(Alpha, 2),
    `Omega total` = fmt_num(Omega_Total, 2),
    `Omega hierarchical` = fmt_num(Omega_Hierarchical, 2),
    `Excluded item` = ifelse(is.na(Excluded_Item), "-", Excluded_Item)
  ),
  file.path(tables_folder, "Tab_Outro_Scales.docx"),
  table_number = 8,
  title = "Perceived reactivity and ease of use (scales)",
  note = paste0(
    "Items rated 1-5. Preregistration specified hierarchical omega; because both ",
    "scales are modeled unidimensionally, omega total is the more appropriate ",
    "estimate and the basis for the single-item-exclusion rule (deviation documented)."
  )
)
save_pub_table(
  item_table %>% transmute(
    Scale, Item, Statement, Coding, N,
    `M (SD)` = m_sd(M, SD),
    `95% CI` = fmt_ci(CI95_Lower, CI95_Upper)
  ),
  file.path(tables_folder, "Tab_Outro_Items.docx"),
  table_number = 9,
  title = "Item-level descriptives (reactivity and ease of use)",
  note = "M (SD) and 95% CI on a 1-5 scale. Reactivity items are direction-aligned (higher = more reactivity)."
)


#===============================================================================
# 15 Figure: Reactivity items
#===============================================================================
# Punkt + 95%-CI ist für Likert-Mittelwerte kompakter und lesbarer als Balken.
# Die neutrale Skalenmitte erleichtert die inhaltliche Einordnung.

if (create_figures) {
  reactivity_plot_data <- item_table %>%
    filter(Scale == "Reactivity") %>%
    mutate(
      Statement = forcats::fct_rev(factor(Statement, levels = unique(Statement)))
    )
  
  p_reactivity <- ggplot(
    reactivity_plot_data,
    aes(x = M, y = Statement)
  ) +
    geom_vline(
      xintercept = 3,
      linetype = "22",
      linewidth = 0.55,
      color = unname(project_colors["grid"])
    ) +
    geom_errorbar(
      aes(xmin = CI95_Lower, xmax = CI95_Upper),
      width = 0.16,
      linewidth = 0.7,
      color = unname(project_colors["accent"])
    ) +
    geom_point(
      size = 3,
      color = unname(project_colors["dark"])
    ) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Perceived reactivity",
      subtitle = "Direction-aligned item means with 95% confidence intervals",
      x = "Agreement / reactivity (1–5)",
      y = NULL,
      caption = "Higher values indicate stronger study-induced reactivity; dashed = scale midpoint."
    ) +
    theme_project(legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 10)
    )
  
  save_project_plot(
    p_reactivity,
    file.path(figure_folder, "Outro_Reactivity_Items.png"),
    width = 9.5,
    height = 5.4
  )
  
  
  #===============================================================================
  # 16 Figure: Ease-of-use items
  #===============================================================================
  # Die acht Usability-Items werden im selben Layout dargestellt, sodass Screening,
  # Daily und Outro visuell aus einem Guss bleiben.
  
  ease_plot_data <- item_table %>%
    filter(Scale == "Ease of Use") %>%
    mutate(
      Statement = forcats::fct_rev(factor(Statement, levels = unique(Statement)))
    )
  
  p_ease <- ggplot(
    ease_plot_data,
    aes(x = M, y = Statement)
  ) +
    geom_vline(
      xintercept = 3,
      linetype = "22",
      linewidth = 0.55,
      color = unname(project_colors["grid"])
    ) +
    geom_errorbar(
      aes(xmin = CI95_Lower, xmax = CI95_Upper),
      width = 0.16,
      linewidth = 0.7,
      color = unname(project_colors["primary"])
    ) +
    geom_point(
      size = 3,
      color = unname(project_colors["dark"])
    ) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Perceived ease of use",
      subtitle = "Item means with 95% confidence intervals",
      x = "Agreement / ease of use (1–5)",
      y = NULL,
      caption = "Higher values indicate better perceived ease of use; dashed = scale midpoint."
    ) +
    theme_project(legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 10)
    )
  
  save_project_plot(
    p_ease,
    file.path(figure_folder, "Outro_Ease_Items.png"),
    width = 9.5,
    height = 6.5
  )
  
  
  #===============================================================================
  # 17 Figure: method associations
  #===============================================================================
  # Die explorativen Korrelationen werden nur visualisiert, wenn tatsächlich
  # auswertbare Zusammenhänge vorliegen. Der Plot zeigt Effektgrößen, nicht Signifikanz.
  
  association_plot_data <- method_associations %>%
    filter(
      Family != "Scale relation",
      !is.na(Spearman_Rho)
    ) %>%
    mutate(
      Label = paste(Predictor, "~", Outcome),
      Label = forcats::fct_reorder(Label, Spearman_Rho)
    )
  
  if (nrow(association_plot_data) > 0) {
    p_associations <- ggplot(
      association_plot_data,
      aes(x = Spearman_Rho, y = Label, color = Family)
    ) +
      geom_vline(
        xintercept = 0,
        linewidth = 0.6,
        color = unname(project_colors["medium"])
      ) +
      geom_segment(
        aes(x = 0, xend = Spearman_Rho, yend = Label),
        linewidth = 0.9,
        alpha = 0.7
      ) +
      geom_point(size = 3.2) +
      scale_x_continuous(
        limits = c(-1, 1),
        breaks = seq(-1, 1, 0.25)
      ) +
      scale_color_project() +
      labs(
        title = "Methodological plausibility checks",
        subtitle = "Exploratory Spearman correlations at the participant level",
        x = "Spearman rho",
        y = NULL,
        caption = "Exploratory; effect sizes serve methodological orientation, not evidence of causal reactivity."
      ) +
      theme_project() +
      theme(panel.grid.major.y = element_blank())
    
    save_project_plot(
      p_associations,
      file.path(figure_folder, "Outro_Method_Associations.png"),
      width = 10,
      height = 5.8
    )
  }
}


#===============================================================================
# 18 Console report
#===============================================================================
# Alles, was primär QC oder Workflow-Dokumentation ist, landet in der Konsole
# statt in zusätzlichen Excel-Blättern.

cat(
  "\n============================================================\n",
  "OUTRO ANALYSIS COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat("Outro file: ", outro_file, "\n", sep = "")
cat("Raw participants: ", n_distinct(outro_raw[[participant_variable]], na.rm = TRUE), "\n", sep = "")
cat("Duplicate participant codes: ", nrow(duplicate_participants), "\n", sep = "")
cat("Complete Outro questionnaires: ", sum(outro$outro_complete, na.rm = TRUE), "\n", sep = "")
cat("Daily sample filter applied: ", daily_filter_applied, "\n", sep = "")
cat("Final analysis sample: ", nrow(outro_analysis), "\n\n", sep = "")

cat(
  "Reactivity: M = ", round(reactivity_desc$Mean, 2),
  ", SD = ", round(reactivity_desc$SD, 2),
  ", alpha = ", round(reactivity_reliability_final$summary$Cronbach_Alpha[[1]], 3),
  ", omega total = ", round(reactivity_reliability_final$summary$Omega_Total[[1]], 3),
  "\n",
  sep = ""
)

cat(
  "  Item excluded: ",
  ifelse(is.na(reactivity_selection$excluded_item), "none", reactivity_selection$excluded_item),
  "\n",
  sep = ""
)

cat(
  "Ease of Use: M = ", round(ease_desc$Mean, 2),
  ", SD = ", round(ease_desc$SD, 2),
  ", alpha = ", round(ease_reliability_final$summary$Cronbach_Alpha[[1]], 3),
  ", omega total = ", round(ease_reliability_final$summary$Omega_Total[[1]], 3),
  "\n",
  sep = ""
)

cat(
  "  Item excluded: ",
  ifelse(is.na(ease_selection$excluded_item), "none", ease_selection$excluded_item),
  "\n\n",
  sep = ""
)

cat("Open-text responses – problems: ", sum(!is.na(outro_analysis$problems_free)), "\n", sep = "")
cat("Open-text responses – suggestions: ", sum(!is.na(outro_analysis$suggestions_free)), "\n", sep = "")
cat("Open-text responses not captured by theme dictionary: ", nrow(open_text_unmatched), "\n", sep = "")

if (nrow(open_text_theme_summary) > 0) {
  cat("\nMost frequent open-text themes (heuristic):\n")
  open_text_theme_summary %>%
    group_by(Question) %>%
    slice_head(n = 5) %>%
    ungroup() %>%
    select(Question, Theme, N_Responses, Percent_of_Question_Responses) %>%
    print(n = Inf)
}

cat("Exploratory method associations: ", nrow(method_associations), "\n\n", sep = "")

cat("Excel: ", output_excel, "\n", sep = "")
cat("Prepared RDS: ", output_rds, "\n", sep = "")
if (create_open_text_report) cat("Open-text report: ", open_text_report_file, "\n", sep = "")
if (create_figures) cat("Figures: ", figure_folder, "/Outro_*.png\n", sep = "")
cat("============================================================\n")
