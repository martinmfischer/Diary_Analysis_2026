################################################################################
# Project: Tagebuchstudie
# Datei: vorbereitung/01_Codingsheet_erstellen.R
# Zusammengeführtes Skript: ersetzt die bisherige 02 UND 03.
# Separater Neuaufbau nach dem Datenimport, nicht Teil eines Berichtslaufs
# Voraussetzung: vorhandene 02_Scripts/funktionen/Daten_und_Statistik.R.
# Screenshots: 05_Participants/LA, /MF, /LG (flach).
# Verteilung: alphabetisch nach screenshot_id, zyklisch LA/MF/LG.
# Bei erneutem Aufbau alte Ausgaben in _Backups_Coding sichern.
# Explorer: Ansicht nach Name aufsteigend wählen; Ordner speichern keine Sortierung.
#
# Purpose:
#   Erstellt coding_sheet.xlsx für die manuelle Screenshot-Codierung.
#   Eine Zeile = ein Screenshot.
#
# Sichtbarer Aufbau des Coding-Sheets:
#   [INFO / DATEI] | [MANUELLES CODING] | [TEILNEHMER-INFO]
#
# Manuelles Coding:
#   public_rel_coded    1 = öffentlich relevant; 0 = nicht öffentlich relevant;
#                       99 = unklar; 98 = kein Einzelbeitrag sozialer Medien;
#                       97 = technisch fehlerhaft / unlesbar.
#                       Nur bei 1 weitere Inhaltscodierung.
#   advertisement_coded 1 = Werbung/Anzeige; 0 = keine Werbung/Anzeige;
#                       99 = sonstiges / nicht eindeutig.
#   topic_coded         1-14 = Hauptthema; 99 = sonstiges / nicht eindeutig
#   source_coded        1-10 = Quellentyp; 99 = sonstige / Account nicht erkennbar
#   source_name_coded   konkrete Quelle / Account
#   platform_coded      geprüfte Plattform, vorausgefüllt
#   media_format        1 Text/Link; 2 statisch visuell; 3 bewegt/audiovisuell;
#                       99 nicht bestimmbar
#
# Input:  01_Data/taeglicher_fragebogen_screenshot_upload.rds
# Output: 06_Coding/coding_sheet.xlsx
#
# ACHTUNG: Nach Beginn der manuellen Codierung ist die Excel-Datei maßgeblich.
################################################################################



# ===============================================================================
# 01 Pakete, Dateipfade und Schutz vor Überschreiben
# ===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, fs)

source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8")

overwrite_existing <- FALSE

data_file <- file.path("01_Data", "taeglicher_fragebogen_screenshot_upload.rds")
participant_folder <- "05_Participants"
source_folder <- file.path("01_Data", "files")
output_folder <- "06_Coding"
output_excel <- file.path(output_folder, "coding_sheet.xlsx")

fs::dir_create(output_folder)

if (!file.exists(data_file)) stop("Daily-RDS nicht gefunden: ", data_file)
if (file.exists(output_excel) && !overwrite_existing) {
  stop(
    "coding_sheet.xlsx existiert bereits und wird nicht überschrieben.\n",
    "Für einen bewussten Neuaufbau overwrite_existing <- TRUE setzen."
  )
}


# ===============================================================================
# 02 Hilfsfunktionen und Beschriftungen
# ===============================================================================

label_code <- function(x, labels) {
  dplyr::recode(as.character(x), !!!labels,
    .default = "Invalid code", .missing = NA_character_
  )
}

collapse_interactions <- function(read, research, engagement) {
  x <- c(
    if (!is.na(read) && read == 1) "Read/watched thoroughly",
    if (!is.na(research) && research == 1) "Sought further information",
    if (!is.na(engagement) && engagement == 1) "Engaged with the post"
  )
  if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
}

platform_labels <- c(`1` = "Facebook", `2` = "Instagram", `3` = "TikTok", `4` = "X")
incidentality_labels <- c(
  `1` = "Deliberately searched for this topic or this account's posts",
  `2` = "Follows the account, but did not specifically seek the post",
  `3` = "Came across the post by chance"
)
locality_labels <- c(`1` = "At home", `2` = "Out and about", `3` = "Don't know")
situation_labels <- c(
  `1` = "Used the platform alone",
  `2` = "Used the platform together with someone else",
  `3` = "Don't know"
)
interaction_labels <- c(`1` = "Yes", `0` = "No", `-1` = "No answer")


# ===============================================================================
# 03 Tagebuchdaten in eine Zeile je Screenshot überführen
# ===============================================================================

daily <- readRDS(data_file)

missing_base <- setdiff(c("personalParticipantCode", "scheduled"), names(daily))
if (length(missing_base) > 0) {
  stop("Benötigte Variablen fehlen: ", paste(missing_base, collapse = ", "))
}

if (!any(str_detect(names(daily), "^daily_[0-9]+_screenshot$"))) {
  stop("Keine Variablen nach dem Muster daily_[n]_screenshot gefunden.")
}

# Dieselbe Indexfunktion wie bisher verwenden, damit Pretest-IDs stabil bleiben.
coding <- derive_screenshot_index(daily, participant_folder = participant_folder)

# Historische Ausschlüsse werden über Originaldateinamen abgeglichen

canonical_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(as.character(x)))
  x <- sub("_Tag_([0-9]+)_Photo_([0-9]+)$", "_D\\1_P\\2", x)
  tolower(trimws(x))
}
# Originaldateien statt veränderlicher Tag-/Foto-IDs: Zuordnung aus altem Import
pretest_map_file <- "06_Coding/pretest_excluded_original_files.csv"
if (!file.exists(pretest_map_file)) stop("Gespeicherte Pretest-Dateizuordnung fehlt; aus Datensicherung wiederherstellen.")
pretest_map <- readr::read_csv(pretest_map_file, show_col_types = FALSE)
excluded_ids <- coding %>%
  semi_join(pretest_map, by = c("participant", "original_filename")) %>%
  pull(screenshot_id) %>%
  canonical_id() %>%
  unique()
# Bereits dokumentierte Duplikate auch bei einem Neuaufbau ausschließen
removed_files <- openxlsx::read.xlsx("06_Coding/coding_sheet_backup_2026-10-13_140323.xlsx",
  sheet = "Coding"
) %>%
  filter(is_marked_screenshot_duplicate(notes)) %>%
  select(participant, original_filename)
coding <- coding %>% anti_join(removed_files, by = c("participant", "original_filename"))
required_index <- c("screenshot_id", "filename", "original_filename", "filepath")
if (!all(required_index %in% names(coding))) stop("Screenshot-Index unvollständig.")
index_ids <- canonical_id(coding$screenshot_id)
if (anyNA(index_ids) || any(!grepl("^[a-z0-9]+_d[0-9]+_p[0-9]+$", index_ids))) {
  stop("Ungültige Screenshot-IDs im Index.")
}
if (anyDuplicated(index_ids)) stop("Doppelte Screenshot-IDs im Index.")
if (!identical(index_ids, canonical_id(coding$filename))) {
  stop("Screenshot-ID und Dateiname stimmen nicht überein; funktionen/Daten_und_Statistik.R prüfen.")
}
exclusion_report <- tibble(
  screenshot_id = excluded_ids,
  found_in_data = excluded_ids %in% index_ids
)
n_excluded <- sum(index_ids %in% excluded_ids)
coding <- coding[!index_ids %in% excluded_ids, , drop = FALSE]
# Echte alphabetische (lexikographische) Reihenfolge; P10 steht vor P2.
coding <- coding[order(tolower(coding$screenshot_id), method = "radix"), , drop = FALSE]
coders <- c("LA", "MF", "LG")
coding$coder <- rep(coders, length.out = nrow(coding))
coding$filepath <- file.path(participant_folder, coding$coder, coding$filename)
if (anyDuplicated(tolower(coding$filepath))) stop("Doppelte Zieldateinamen.")
if (anyNA(coding$filename) || any(basename(coding$filename) != coding$filename)) {
  stop("Ungültiger Zieldateiname.")
}
source_paths <- file.path(source_folder, coding$original_filename)
if (anyNA(source_paths) || any(!file.exists(source_paths))) {
  stop(
    "Quelldateien fehlen; keine Ausgaben verändert:\n",
    paste(source_paths[is.na(source_paths) | !file.exists(source_paths)], collapse = "\n")
  )
}
# Erst vollständig in einem temporären Verzeichnis erstellen und prüfen.
# Dieses liegt auf demselben Laufwerk wie die endgültigen Ausgaben.
stage_root <- tempfile(".coding_build_", tmpdir = ".")
stage_participants <- file.path(stage_root, "05_Participants")
fs::dir_create(file.path(stage_participants, coders))
staged_paths <- file.path(stage_participants, coding$coder, coding$filename)
for (i in seq_len(nrow(coding))) {
  fs::file_copy(source_paths[i], staged_paths[i], overwrite = FALSE)
}
if (!identical(
  unname(tools::md5sum(source_paths)),
  unname(tools::md5sum(staged_paths))
)) {
  stop("Prüfung der kopierten Dateien fehlgeschlagen: ", stage_root)
}
stopifnot(!any(canonical_id(coding$screenshot_id) %in% excluded_ids))
coder_counts <- table(factor(coding$coder, levels = coders))
stopifnot(diff(range(as.integer(coder_counts))) <= 1L)
message("Pretest-Ausschlüsse in den Daten: ", n_excluded, " / ", length(excluded_ids))
message("Nicht gefundene Ausschluss-IDs: ", sum(!exclusion_report$found_in_data))
print(coder_counts)

# Optionale Felder ergänzen, falls eine GESIS-Version sie nicht enthält.
expected_fields <- c(
  "screenshot", "topic", "account", "platform", "incidentality",
  "interaction_1", "interaction_2", "interaction_3",
  "locality", "situation", "startstop"
)
for (x in setdiff(expected_fields, names(coding))) coding[[x]] <- NA

if (any(is.na(coding$participant))) {
  stop("Mindestens ein Screenshot besitzt keinen gültigen Participant Code.")
}


# ===============================================================================
# 04 Variablen und Spalten für die manuelle Codierung vorbereiten
# ===============================================================================

coding <- coding %>%
  mutate(
    topic_participant = clean_text(topic),
    account_participant = clean_text(account),
    platform_code = na_if(clean_numeric(platform), -1),
    incidentality_code = na_if(clean_numeric(incidentality), -1),
    locality_code = na_if(clean_numeric(locality), -1),
    situation_code = na_if(clean_numeric(situation), -1),
    interaction_read_code = clean_numeric(interaction_1),
    interaction_research_code = clean_numeric(interaction_2),
    interaction_engagement_code = clean_numeric(interaction_3),
    platform_reported = label_code(platform_code, platform_labels),
    incidentality_label = label_code(incidentality_code, incidentality_labels),
    locality_label = label_code(locality_code, locality_labels),
    situation_label = label_code(situation_code, situation_labels),
    interaction_read = label_code(interaction_read_code, interaction_labels),
    interaction_research = label_code(interaction_research_code, interaction_labels),
    interaction_engagement = label_code(interaction_engagement_code, interaction_labels),
    interaction_summary = purrr::pmap_chr(
      list(interaction_read_code, interaction_research_code, interaction_engagement_code),
      collapse_interactions
    ),
    startstop_raw = startstop,
    startstop_label = case_when(
      str_to_lower(clean_text(startstop_raw)) %in% c("true", "t", "1") ~ "Weiter",
      str_to_lower(clean_text(startstop_raw)) %in% c("false", "f", "0") ~ "Stopp",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    file_exists = fs::file_exists(staged_paths),
    # Manual coding
    public_rel_coded = NA_integer_,
    advertisement_coded = NA_integer_,
    topic_coded = NA_character_,
    source_coded = NA_character_,
    source_name_coded = NA_character_,
    platform_coded = platform_reported,
    media_format = NA_integer_,
    notes = NA_character_,
    coding_completed = FALSE,
    coding_date = as.Date(NA)
  )


# ===============================================================================
# 05 Aufbau des Codiersheets
# ===============================================================================

id_cols <- c(
  "screenshot_id", "participant", "study_day", "photo",
  "filename", "filepath", "file_exists"
)

coding_cols <- c(
  "public_rel_coded", "advertisement_coded", "topic_coded", "source_coded", "source_name_coded",
  "platform_coded", "media_format", "notes", "coder",
  "coding_completed", "coding_date"
)

# Im Workbook sichtbar: nur unmittelbar hilfreiche Angaben der Teilnehmenden.
visible_info_cols <- c("topic_participant", "account_participant", "platform_reported")

# Für 04b_Daily_Analysis.R nötig, aber beim Codieren standardmäßig verborgen.
hidden_info_cols <- c(
  "incidentality_label", "interaction_read", "interaction_research",
  "interaction_engagement", "interaction_summary",
  "locality_label", "situation_label", "startstop_label"
)

technical_cols <- c(
  "platform_code", "incidentality_code",
  "interaction_read_code", "interaction_research_code", "interaction_engagement_code",
  "locality_code", "situation_code", "startstop_raw",
  "original_filename", "screenshot_slot", "submission_row",
  "scheduled", "committed"
)

coding_export <- coding %>%
  select(all_of(c(id_cols, coding_cols, visible_info_cols, hidden_info_cols, technical_cols)))


# ===============================================================================
# 06 Qualitätsprüfung und Kategoriensystem
# ===============================================================================

n_participant_days <- coding_export %>%
  filter(!is.na(participant), !is.na(study_day)) %>%
  distinct(participant, study_day) %>%
  nrow()

quality_summary <- tibble(
  Indicator = c(
    "Screenshots total", "Participants", "Participant-days",
    "Files found", "Files not found",
    "Missing platform values", "Missing incidental-exposure values",
    "Study day outside 1-7"
  ),
  Value = c(
    nrow(coding_export),
    n_distinct(coding_export$participant, na.rm = TRUE),
    n_participant_days,
    sum(coding_export$file_exists %in% TRUE),
    sum(coding_export$file_exists %in% FALSE),
    sum(is.na(coding_export$platform_reported)),
    sum(is.na(coding_export$incidentality_label)),
    sum(is.na(coding_export$study_day) |
      coding_export$study_day < 1 | coding_export$study_day > 7)
  )
)

missing_files <- coding_export %>%
  filter(file_exists %in% FALSE) %>%
  select(screenshot_id, participant, study_day, photo, filename, filepath)

codebook <- tribble(
  ~Variable, ~Code, ~Category, ~Rule,
  "general_rules",
  "",
  "Allgemeine Codierregel",
  "Das Codebuch definiert die manuell codierten Variablen Relevanz, Advertisement Topic, Source und Format. Pro Beitrag wird je Variable genau eine Hauptkategorie vergeben.",
  "general_rules",
  "",
  "Allgemeine Codierregel",
  "Nicht die Posts als solche werden codiert, sondern die Screenshots.",
  "general_rules",
  "",
  "Allgemeine Codierregel",
  "Vor der Codierung jeden Post mindestens einmal gründlich anschauen und durchlesen.",
  "general_rules",
  "",
  "Allgemeine Codierregel",
  "Alles auf dem Screenshot sichtbares kann zur Codierung verwendet werden, inkl. Caption, Bild und andere ggf. beiläufig sichtbare Postelemente",
  "general_rules",
  "",
  "Allgemeine Codierregel",
  "Codiert wird ausschließlich der sichtbare Beitrag einschließlich der Caption. Inferenzen auf thematische Hintergründe sind zu vermeiden.",
  "general_rules",
  "",
  "Allgemeine Codierregel",
  "Residual-Kategorien nur verwenden, wenn nach Prüfung des Codebuchs keine eindeutige Zuordnung möglich ist",
  "general_rules",
  "",
  "Allgemeine Codierregel",
  "Bei unvollständigen oder nicht zuzuordnenden Posts wird nach Codierung unter 1.99 nicht weiter verfahren.",
  "public_rel_coded",
  "",
  "Codierlogik",
  "Codierlogik: Die Teilnehmenden wurden bereits bei der Erhebung gebeten, Inhalte hochzuladen, die sie selbst als öffentlich relevant einschätzen. Ihre Auswahl bildet daher den Ausgangspunkt der Codierung. Diese Variable dient nicht dazu, öffentliche Relevanz für jeden Beitrag neu und möglichst trennscharf zu bestimmen, sondern ausschließlich dazu, offensichtliche Fehluploads auszusortieren. Entsprechend gilt: Im Zweifelsfall wird ein Beitrag als öffentlich relevant (1) codiert. Code 0 wird nur vergeben, wenn aus dem Beitrag klar hervorgeht, dass er ausschließlich privaten, persönlichen oder anderweitig offensichtlich nicht öffentlich relevanten Inhalt enthält. Entscheidend ist der Inhalt des einzelnen Posts. Der veröffentlichende Account, dessen Reichweite oder die öffentliche Sichtbarkeit eines Beitrags sind für sich genommen nicht ausschlaggebend.",
  "public_rel_coded",
  "1",
  "Öffentlich relevant",
  "Der Beitrag enthält eine erkennbare Information, Einordnung, Bewertung, Meinung, Warnung, Empfehlung oder Handlungsaufforderung zu einem überprivaten Sachverhalt, der für eine Orts-, Themen- oder Gruppenöffentlichkeit von Bedeutung sein kann. Dazu gehören beispielsweise Politik, gesellschaftliche Debatten, Gesundheit und Sicherheit mit überindividuellem Bezug, Wissenschaft, Umwelt, Wirtschaft und Arbeitswelt, Kultur, Geschichte, Sport, Religion, Wetter, Verkehr, öffentliche Infrastruktur sowie lokale Entwicklungen und Veranstaltungen. Entscheidend ist der konkrete Sachbezug des Beitrags; das Themenfeld allein genügt nicht.",
  "public_rel_coded",
  "0",
  "Nicht öffentlich relevant",
  "Der Beitrag ist im Wesentlichen privat, persönlich oder selbstdarstellerisch und enthält keinen eigenständigen öffentlich relevanten Sachbezug. Beispiele: private Urlaubs- oder Familienfotos, persönliche Statusmeldungen, Geburtstagsgrüße, reine Selbstdarstellung, Unterhaltung ohne öffentlichen Sachbezug.",
  "public_rel_coded",
  "99",
  "Unklar",
  "Der Beitrag ist grundsätzlich lesbar, es fehlen aber notwendige Informationen oder Kontext, sodass der Inhalt nicht zuverlässig beurteilt werden kann. 99 ist nicht für normale Grenzfälle vorgesehen; diese werden mit 1 codiert.",
  "public_rel_coded",
  "98",
  "Bild enthält keinen Einzelbeitrag von sozialen Medien",
  "Im Bild ist kein einzelner Social-Media-Beitrag als Codiereinheit erkennbar.",
  "public_rel_coded",
  "97",
  "Technisch fehlerhaft oder anderweitig unlesbar",
  "Der Beitrag kann aufgrund technischer Fehler oder mangelnder Lesbarkeit nicht beurteilt werden.",
  "advertisement_coded",
  "",
  "Codierlogik",
  "Codierlogik: Ist der Beitrag als Anzeige oder Werbung gekennzeichnet? Primär ausschlaggebend ist die Frage, ob der Beitrag finanziert wurde, beispielsweise als Anzeige auf einer Plattform (bspw. Meta-Ads), oder deutlich sichtbar als sponsored content (bspw. eine Kooperation von Rewe mit einem Influencer)",
  "advertisement_coded",
  "1",
  "Werbung oder Anzeige",
  "Es handelt sich um einen klar erkennbaren Anzeigenbeitrag. Im Beitrag ist klar erkennbar, dass es sich um eine Plattformwerbung handelt, beispielsweise durch ein klar erkennbares Wort “Anzeige” o.ä.",
  "advertisement_coded",
  "0",
  "Keine Werbung oder Anzeige",
  "Es handelt sich nicht um einen im obigen Sinne kommerziellen Beitrag.",
  "advertisement_coded", "99", "Sonstiges / nicht eindeutig", "",
  "topic_coded",
  "",
  "Codierlogik",
  "Codierlogik: Zentrales Thema des Beitrags. Politische Entscheidungen und Konflikte werden unter Politik codiert, auch wenn sie ein Sachgebiet wie Gesundheit oder Migration betreffen. Sachbezogene Information ohne primären politischen Entscheidungsbezug wird dem jeweiligen Fachgebiet zugeordnet. Es ist nur zu codieren, was explizit aus dem Screenshot hervorgeht - Inferenz ist so gut es geht zu vermeiden (nicht die Posts werden codiert, sondern die Screenshots).",
  "topic_coded",
  "1",
  "Politik, Staat & Wahlen",
  "Parteien, Regierungen, Parlamente, politische Entscheidungen, Wahlkampf, Demokratie, politische Proteste. Fokus auf Innenpolitik, Rest unter 2 codieren. Fokus auf Politiker /Akteure sowie Strukturen ergänzen bei Maßnahmen weiter unten",
  "topic_coded",
  "2",
  "Internationales, Krieg & Sicherheit",
  "Internationale Beziehungen, Kriege, Militär, Terrorismus, geopolitische Konflikte, äußere Sicherheit.",
  "topic_coded",
  "3",
  "Wirtschaft, Arbeit, Finanzen & Verbraucher",
  "Konjunktur, Unternehmen, Arbeitsmarkt, Preise, Geldanlage, Renten, Steuern, Verbraucherthemen.",
  "topic_coded",
  "4",
  "Gesellschaft, Soziales, Migration & Religion",
  "Zusammenleben, Ungleichheit, soziale Gruppen, Migration/Integration inkl. Flucht, Familie, Religion, gesellschaftliche Debatten und Engagement.",
  "topic_coded",
  "5",
  "Bildung, Wissenschaft & Technologie",
  "Schule, Hochschule, Forschung, Digitalisierung, KI, technische Entwicklungen",
  "topic_coded",
  "6",
  "Gesundheit & Pflege",
  "Krankheiten, Prävention, Medizin, Versorgung, Pflege, Gesundheitssystem ohne dominanten politischen Fokus, Ernährung",
  "topic_coded",
  "7",
  "Klima, Umwelt & Energie",
  "Klimawandel (bei Waldbränden etc. nur bei deutlicher Benennung codieren), Naturschutz, Umwelt, Energieversorgung und Energiewende, Tierschutz, Flora und Fauna",
  "topic_coded",
  "8",
  "Kriminalität, Unglücke & Justiz",
  "Straftaten, Polizei, Gerichtsverfahren, Strafverfolgung, Urteile. Auch politisch motivierte Kriminalität. Auch Unfälle und Blaulichtberichterstattung.",
  "topic_coded",
  "9",
  "Verkehr, Infrastruktur & Wohnen",
  "ÖPNV, Straßen, Bahn, Mobilität, Bau, digitale Infrastruktur, Mieten und Wohnraum.",
  "topic_coded",
  "10",
  "Wetter & Naturereignisse",
  "Wetterberichte, Warnungen, Unwetter, Hochwasser, Erdbeben und andere Naturereignisse.",
  "topic_coded",
  "11",
  "Kultur, Medien & Unterhaltung",
  "Kunst, Literatur, Film, Musik, Fernsehen, Prominenz, Freizeit- und Unterhaltungsangebote ohne konkreten Eventcharakter oder ohne spezifischem Datum, Reisen.",
  "topic_coded",
  "12",
  "Geschichte & Erinnerung",
  "Historische Ereignisse, Jahrestage, Erinnerungskultur, historische Einordnung.",
  "topic_coded",
  "13",
  "Sport",
  "Sportereignisse, Ergebnisse, Persönlichkeiten, Sportpolitik nur bei klarem Sportfokus.",
  "topic_coded",
  "14",
  "Veranstaltungen & öffentlicher Service",
  "Lokale Termine, Öffnungszeiten, Warn- und Servicehinweise, kommunale Angebote, Veranstaltungshinweise mit klarem Datum.",
  "topic_coded",
  "99",
  "Sonstiges / Themenmix / nicht eindeutig",
  "Nur wenn kein Schwerpunkt sicher bestimmbar ist oder keine andere Kategorie zutrifft.",
  "source_coded",
  "",
  "Codierlogik",
  "Codierlogik: Codiert wird der Account, der den Beitrag veröffentlicht hat. Bei Reposts zählt der repostende Account. Es zählt der veröffentlichende Account, nicht eine lediglich erwähnte oder abgebildete Person. Der konkrete Accountname wird zusätzlich in source_name_coded einheitlich ausgeschrieben. Bei Nichtkenntnis darf der Account über Suchmaschinen gesucht werden - zulässig sind hier primär allgemein anerkannte Quellen wie Wikipedia und die Biographie der Seite selbst.",
  "source_coded",
  "1",
  "Journalistisches Medium",
  "Redaktionell arbeitende generalistische Nachrichtenmedien, Lokalmedien, öffentlich-rechtliche und private journalistische Angebote.",
  "source_coded",
  "2",
  "Alternatives oder parteiisches Medienangebot",
  "Medienähnliche Angebote mit generalistischen Nachrichtenfokus, mit ausgeprägter politischer/ideologischer Positionierung, mit Anti-Mainstream-Positionierung und/oder ohne professionell-journalistischen Hintergrund.",
  "source_coded",
  "3",
  "Partei oder Politiker:in",
  "Parteien, Fraktionen, Mandatsträger:innen, Kandidierende und deren offizielle Accounts.",
  "source_coded",
  "4",
  "Staatliche oder öffentliche Institution",
  "Behörden, Ministerien, Kommunen, Polizei, Gerichte, öffentliche Einrichtungen, auch Museen, Konzertsäle etc., sofern nicht unter einer spezifischeren Kategorie zu subsumieren. Wissenschaftliche Einrichtungen sind unter 6 zu codieren.",
  "source_coded",
  "5",
  "NGO, Verband, Verein, Initiative oder Bewegung",
  "Zivilgesellschaftliche Organisationen, Interessenverbände, Kampagnen, Vereine, Gruppen und soziale Bewegungen.",
  "source_coded",
  "6",
  "Wissenschaft, Expert:in oder Faktencheck",
  "Forschungseinrichtungen, Hochschulen, Fachleute in Expertenrolle sowie Faktencheck-Organisationen, die nicht nur auf sozialen Medien auftreten.",
  "source_coded",
  "7",
  "Unternehmen oder Marke",
  "Kommerzielle Organisationen, Marken, Händler, Arbeitgeber und Produktaccounts.",
  "source_coded",
  "8",
  "Journalist:in",
  "Persönlicher Account eines/einer Journalist:in, kein Post unter eigenem Namen auf Account eines Mediums, einer Zeitung o.ä.",
  "source_coded",
  "9",
  "Content Creator:in, Influencer:in, oder sonstige Person der Öffentlichkeit",
  "Personenaccounts mit öffentlicher Reichweite, die auf sozialen Medien auftreten, soweit nicht als Politiker:in oder institutionelle Expert:in zu codieren. Im Falle von Content Creator muss keine Einzelperson sichtbar oder im Vordergrund stehen, aber es sollte sich um einen kleinen dahinterstehenden Personenkreis handeln und kein journalistisches Medium o.ä.",
  "source_coded",
  "10",
  "Private Person",
  "Erkennbar persönlicher Account ohne institutionelle oder öffentliche Sprecherrolle und mit nur sehr geringer Reichweite.",
  "source_coded",
  "99",
  "Sonstige / Account nicht erkennbar",
  "Account mit den obigen Kategorien nicht erfassbar oder nicht erkennbar (z.B. abgeschnitten)",
  "source_name_coded",
  "",
  "Konkreter Accountname",
  "Der sichtbare Accountname wird als standardisierter Freitext erfasst, z. B. „Frankfurter Rundschau“, „Bundesregierung“ oder „kein Bock auf Nazis“. Zusätze wie @-Handle, Emojis oder wechselnde Groß-/Kleinschreibung werden entfernt, sofern sie nicht elementarer Bestandteil des Handles sind oder zur eindeutigen Identifikation nötig sind.",
  "media_format",
  "",
  "Codierlogik",
  "Codierziel Codiert wird die formale Darstellungsform des Posts, nicht das Dateiformat des hochgeladenen Screenshots und nicht die Länge des begleitenden Beschreibungstextes (Caption). Maßgeblich ist der Medienkörper des Posts, also das, was im Feed als eigentlicher Beitrag präsentiert beziehungsweise abgespielt wird. Berücksichtigte Elemente Berücksichtigt werden: - Facebook und X: kompletter Post, außer Accountname und Engagement-Metriken/Kommentare - Instagram und TikTok: kompletter visueller Teil des Posts (d.h. Video/Image), nicht: Caption, Accountname, Engagement-Metriken/Kommentare",
  "media_format",
  "1",
  "Text-/linkbasiert",
  "Der Post besteht aus nativem Plattformtext und ggf. einer standardisierten Link-/Artikelvorschau. Es liegt kein eigenständiges statisches oder bewegtes visuelles Medium als primärer Postkörper vor.",
  "media_format",
  "2",
  "Statisches visuelles Format",
  "Der Post besteht ausschließlich aus einem oder mehreren unbewegten visuellen Elementen, etwa Foto, Illustration, Grafik, Meme, Infografik, Texttafel oder ausschließlich statischem Carousel.",
  "media_format",
  "3",
  "Bewegtes audiovisuelles Format",
  "Der Post enthält ausschließlich zeitlich ablaufenden beziehungsweise bewegten Inhalt, etwa Video, Reel, TikTok, GIF oder Animation. Wird auch bei umfangreichen Texteinblendungen oder Untertiteln codiert.",
  "media_format",
  "99",
  "Nicht bestimmbar",
  "Anhand des Screenshots ist nicht erkennbar, welches Format der Post hat.",
  "public_rel_coded",
  "",
  "Öffentliche Relevanz – im Detail",
  "Das Verständnis öffentlicher Relevanz orientiert sich am Konzept der Public Connection in Verbindung mit einem Audience Centered Approach (Swart et al., 2017; Hasebrink et al., 2023). Im Mittelpunkt steht dabei nicht ausschließlich eine normative Vorstellung davon, welche Themen gesellschaftlich relevant sein sollten, sondern auch, über welche Inhalte Menschen Verbindungen zu unterschiedlichen Öffentlichkeiten herstellen. Dieser Audience Centered Approach entspricht dem Erhebungsdesign der Studie: Die Teilnehmenden wurden gebeten, selbst diejenigen Social-Media-Beiträge hochzuladen, die sie als öffentlich relevant wahrnehmen. Die Variable public_rel_coded dient daher nicht dazu, diese Einschätzung anhand einer engen Definition nachträglich zu überprüfen, sondern lediglich dazu, offensichtliche Fehluploads auszusortieren.",
  "public_rel_coded", "", "Öffentliche Relevanz – im Detail", "Grundregel",
  "public_rel_coded",
  "",
  "Öffentliche Relevanz – im Detail",
  "Ein Beitrag wird mit 1 codiert, solange er nicht eindeutig als nicht öffentlich relevanter Fehlupload erkennbar ist. Im Zweifel gilt 1.",
  "public_rel_coded",
  "",
  "Öffentliche Relevanz – im Detail",
  "Öffentliche Relevanz wird entsprechend breit verstanden. Sie kann sich neben Politik und gesellschaftlichen Debatten beispielsweise auch auf Gesundheit, Wissenschaft, Umwelt, Wirtschaft, Kultur, Medien, Geschichte, Sport, Religion, lokale Themen oder andere Themen- und Gruppenöffentlichkeiten beziehen. Codierende sollen nicht beurteilen, wie gesellschaftlich wichtig ein Thema ist oder theoretische Grenzfälle von Öffentlichkeit entscheiden. Auch Werbung und andere kommerzielle Inhalte werden über diese Variable nicht ausgeschlossen.",
  "public_rel_coded", "", "Öffentliche Relevanz – im Detail", "Wann wird 0 codiert?",
  "public_rel_coded",
  "",
  "Öffentliche Relevanz – im Detail",
  "0 wird nur vergeben, wenn der Beitrag eindeutig nicht der Erhebungsinstruktion entspricht und ausschließlich privaten oder persönlichen Inhalt ohne erkennbaren weitergehenden Themenbezug enthält, beispielsweise:",
  "public_rel_coded",
  "",
  "Öffentliche Relevanz – im Detail",
  "- private Familien-, Urlaubs- oder Alltagsfotos, - Geburtstagsgrüße oder rein zwischenmenschliche Kommunikation, - Selfies oder persönliche Statusmeldungen ohne weiteren Sachbezug, - offensichtlich versehentlich hochgeladene Inhalte, - reine Unterhaltung ohne erkennbaren Themenbezug.",
  "public_rel_coded",
  "",
  "Öffentliche Relevanz – im Detail",
  "Persönliche Elemente allein sind kein Ausschlussgrund. Sobald ein plausibler Bezug zu einem öffentlichen, lokalen, thematischen oder gruppenbezogenen Gegenstand erkennbar ist, wird 1 codiert.",
  "public_rel_coded", "", "Öffentliche Relevanz – im Detail", "Entscheidungsregel",
  "public_rel_coded",
  "",
  "Öffentliche Relevanz – im Detail",
  "Ist eindeutig erkennbar, dass es sich um einen Fehlupload ohne plausiblen Bezug zu einem öffentlichen, lokalen, thematischen oder gruppenbezogenen Gegenstand handelt?",
  "public_rel_coded",
  "",
  "Öffentliche Relevanz – im Detail",
  "Ja → 0 Nein / im Zweifel → 1 99 wird nur verwendet, wenn der Inhalt aufgrund fehlender Informationen oder fehlenden Kontextes tatsächlich nicht beurteilt werden kann.",
  "platform_coded", "", "Geprüfte Plattform", "Vorausgefüllt; bei Bedarf korrigieren.",
  "notes", "", "Notizen", "Nur für Grenzfälle oder Besonderheiten.",
  "coder", "", "Coder", "Automatisch gleichmäßig zugewiesen: LA, MF oder LG.",
  "coding_completed",
  "TRUE/FALSE",
  "Codierung abgeschlossen",
  "TRUE erst nach finaler Prüfung. Nur bei public_rel_coded = 1 weitere Inhaltscodierung.",
  "coding_date", "", "Codierdatum", "Datum der finalen Codierung."
)


# ===============================================================================
# 07 Excel-Arbeitsmappe erstellen
# ===============================================================================

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Coding", gridLines = FALSE)
openxlsx::writeData(wb, "Coding", coding_export, withFilter = FALSE)

id_idx <- match(id_cols, names(coding_export))
coding_idx <- match(coding_cols, names(coding_export))
visible_idx <- match(visible_info_cols, names(coding_export))
hidden_idx <- match(hidden_info_cols, names(coding_export))
technical_idx <- match(technical_cols, names(coding_export))

header_style <- function(fill) {
  openxlsx::createStyle(
    fgFill = fill, textDecoration = "bold", halign = "center", valign = "center",
    wrapText = TRUE, border = "Bottom"
  )
}

openxlsx::addStyle(wb, "Coding", header_style("#DCE7EA"), 1, id_idx, gridExpand = TRUE)
openxlsx::addStyle(wb, "Coding", header_style("#D5B47A"), 1, coding_idx, gridExpand = TRUE)
openxlsx::addStyle(wb, "Coding", header_style("#E7EEEE"), 1,
  c(visible_idx, hidden_idx),
  gridExpand = TRUE
)
openxlsx::addStyle(wb, "Coding", header_style("#E1E1E1"), 1,
  technical_idx,
  gridExpand = TRUE
)

if (nrow(coding_export) > 0) {
  rows <- 2:(nrow(coding_export) + 1)

  # Coding block visually distinct.
  openxlsx::addStyle(
    wb, "Coding",
    openxlsx::createStyle(fgFill = "#FFF7E8", valign = "top", wrapText = TRUE),
    rows, coding_idx,
    gridExpand = TRUE, stack = TRUE
  )

  # Thick separators before Coding and visible Daily info.
  openxlsx::addStyle(
    wb, "Coding",
    openxlsx::createStyle(border = "Left", borderStyle = "thick", borderColour = "#8A8A8A"),
    1:(nrow(coding_export) + 1), c(min(coding_idx), min(visible_idx)),
    gridExpand = TRUE, stack = TRUE
  )

  openxlsx::addFilter(wb, "Coding", rows = 1, cols = seq_len(ncol(coding_export)))

  validation <- c(
    public_rel_coded = '"1,0,99,98,97"',
    advertisement_coded = '"1,0,99"',
    topic_coded = '"1,2,3,4,5,6,7,8,9,10,11,12,13,14,99"',
    source_coded = '"1,2,3,4,5,6,7,8,9,10,99"',
    platform_coded = '"Facebook,Instagram,TikTok,X"',
    media_format = '"1,2,3,99"',
    coder = '"LA,MF,LG"',
    coding_completed = '"FALSE,TRUE"'
  )

  purrr::iwalk(validation, ~ openxlsx::dataValidation(
    wb, "Coding",
    cols = match(.y, names(coding_export)), rows = rows,
    type = "list", value = .x
  ))

  # public_rel_coded: nur bei Code 1 weitere Inhaltscodierung.
  rel_col <- match("public_rel_coded", names(coding_export))
  rel_chr <- openxlsx::int2col(rel_col)

  for (rule in list(
    list(value = 1, fill = "#DDEBDD"),
    list(value = 0, fill = "#E6E6E6"),
    list(value = 99, fill = "#F4E0C7"),
    list(value = 98, fill = "#E8DFF0"),
    list(value = 97, fill = "#F4CCCC")
  )) {
    openxlsx::conditionalFormatting(
      wb, "Coding",
      cols = rel_col, rows = rows, type = "expression",
      rule = paste0("$", rel_chr, "2=", rule$value),
      style = openxlsx::createStyle(fgFill = rule$fill)
    )
  }

  # advertisement_coded: 1 = Werbung, 0 = keine Werbung, 99 = unklar/sonstiges.
  ad_col <- match("advertisement_coded", names(coding_export))
  ad_chr <- openxlsx::int2col(ad_col)

  for (rule in list(
    list(value = 1, fill = "#FCE4D6"),
    list(value = 0, fill = "#E6E6E6"),
    list(value = 99, fill = "#F4E0C7")
  )) {
    openxlsx::conditionalFormatting(
      wb, "Coding",
      cols = ad_col, rows = rows, type = "expression",
      rule = paste0("$", ad_chr, "2=", rule$value),
      style = openxlsx::createStyle(fgFill = rule$fill)
    )
  }

  gated_cols <- match(
    c(
      "advertisement_coded", "topic_coded", "source_coded", "source_name_coded",
      "platform_coded", "media_format"
    ),
    names(coding_export)
  )
  openxlsx::conditionalFormatting(
    wb, "Coding",
    cols = gated_cols, rows = rows, type = "expression",
    rule = paste0("$", rel_chr, "2<>1"),
    style = openxlsx::createStyle(fgFill = "#EFEFEF", fontColour = "#999999")
  )

  done_col <- match("coding_completed", names(coding_export))
  done_chr <- openxlsx::int2col(done_col)
  openxlsx::conditionalFormatting(
    wb, "Coding",
    cols = done_col, rows = rows, type = "expression",
    rule = paste0("$", done_chr, "2=TRUE"),
    style = openxlsx::createStyle(fgFill = "#DDEBDD")
  )
}

# Header-Hinweise direkt aus dem aktuellen Codebook erzeugen.
# So bleiben Kategorien, Codes und Hinweise bei Anpassungen synchron.
for (variable in c(
  "public_rel_coded", "advertisement_coded", "topic_coded",
  "source_coded", "source_name_coded", "media_format"
)) {
  entries <- codebook[codebook$Variable == variable, ]
  categories <- entries[entries$Code != "", ]
  logic <- entries$Rule[entries$Category == "Codierlogik"]
  hint <- paste(c(logic, paste0(categories$Code, " = ", categories$Category)),
    collapse = "\n"
  )
  if (variable == "public_rel_coded") {
    hint <- paste(hint, "Im Zweifel 1. Werbung allein ist kein Ausschlussgrund.",
      "Nur bei 1 weitere Inhaltscodierung.",
      sep = "\n\n"
    )
  }
  if (variable == "source_name_coded") hint <- paste(entries$Rule, collapse = "\n")
  openxlsx::writeComment(
    wb, "Coding", match(variable, names(coding_export)), 1,
    openxlsx::createComment(hint, author = "Codebook")
  )
}

openxlsx::freezePane(wb, "Coding",
  firstActiveRow = 2,
  firstActiveCol = length(id_cols) + 1
)

widths <- c(
  screenshot_id = 18, participant = 14, study_day = 9, photo = 8,
  filename = 28, filepath = 42, file_exists = 10,
  public_rel_coded = 14, advertisement_coded = 18, topic_coded = 25, source_coded = 27,
  source_name_coded = 27, platform_coded = 14, media_format = 13,
  notes = 32, coder = 12, coding_completed = 15, coding_date = 12,
  topic_participant = 28, account_participant = 28, platform_reported = 14
)
purrr::iwalk(widths, ~ openxlsx::setColWidths(
  wb, "Coding", match(.y, names(coding_export)), .x
))

# Analysis information stays in the same sheet for 04b, but out of the coder's way.
openxlsx::setColWidths(
  wb, "Coding",
  cols = c(hidden_idx, technical_idx), widths = 12, hidden = TRUE
)
openxlsx::setRowHeights(wb, "Coding", rows = 1, heights = 34)


# Codebook
openxlsx::addWorksheet(wb, "Codebook", gridLines = FALSE)
openxlsx::writeDataTable(wb, "Codebook", codebook, tableStyle = "TableStyleMedium2")
openxlsx::freezePane(wb, "Codebook", firstRow = TRUE)
openxlsx::setColWidths(wb, "Codebook", 1:3, "auto")
openxlsx::setColWidths(wb, "Codebook", 4, 70)
openxlsx::addStyle(
  wb, "Codebook", openxlsx::createStyle(wrapText = TRUE, valign = "top"),
  rows = 2:(nrow(codebook) + 1), cols = 1:4, gridExpand = TRUE
)

# Nachvollziehbarer Abgleich aller Pretest-Ausschlüsse.
openxlsx::addWorksheet(wb, "Pretest_Exclusions")
openxlsx::writeDataTable(wb, "Pretest_Exclusions", exclusion_report)
openxlsx::setColWidths(wb, "Pretest_Exclusions", 1:2, "auto")
quality_summary <- bind_rows(quality_summary, tibble(
  Indicator = c(
    "Pretest IDs listed", "Pretest cases excluded", "Pretest IDs not found",
    paste("Assigned to", coders)
  ),
  Value = c(
    length(excluded_ids), n_excluded, sum(!exclusion_report$found_in_data),
    as.integer(coder_counts)
  )
))
# Quality: compact summary plus file list only when needed.
openxlsx::addWorksheet(wb, "Quality", gridLines = FALSE)
openxlsx::writeDataTable(wb, "Quality", quality_summary, tableStyle = "TableStyleMedium2")
if (nrow(missing_files) > 0) {
  start <- nrow(quality_summary) + 4
  openxlsx::writeData(wb, "Quality", "Files not found", startRow = start)
  openxlsx::writeDataTable(
    wb, "Quality", missing_files,
    startRow = start + 1,
    tableStyle = "TableStyleMedium2"
  )
}
openxlsx::setColWidths(wb, "Quality", 1:10, "auto")


# ===============================================================================
# 08 Ergebnisse prüfen und speichern
# ===============================================================================

staged_excel <- file.path(stage_root, "coding_sheet.xlsx")
openxlsx::saveWorkbook(wb, staged_excel, overwrite = FALSE)
check <- openxlsx::read.xlsx(staged_excel, sheet = "Coding")
stopifnot(nrow(check) == nrow(coding_export))
if (nrow(check) > 0) {
  stopifnot(
    identical(check$screenshot_id, coding_export$screenshot_id),
    identical(check$coder, coding_export$coder),
    identical(check$filepath, coding_export$filepath)
  )
}

# Alte Ordner außerhalb von 05_Participants sichern (auch alte TN-Unterordner).
# Bereits codierte Excel-Dateien nur mit overwrite_existing = TRUE neu aufbauen.
backup_folder <- tempfile(paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_"),
  tmpdir = "_Backups_Coding"
)
fs::dir_create(backup_folder)
backup_participants <- file.path(backup_folder, "05_Participants")
backup_excel <- file.path(backup_folder, "coding_sheet.xlsx")
# file.rename statt fs::dir_move: funktioniert auch mit älteren fs-Versionen.
move_checked <- function(from, to) {
  if (!file.rename(from, to)) stop("Verschieben fehlgeschlagen: ", from, " -> ", to)
}
had_participants <- dir.exists(participant_folder)
had_excel <- file.exists(output_excel)
tryCatch(
  {
    if (had_participants) move_checked(participant_folder, backup_participants)
    if (had_excel) move_checked(output_excel, backup_excel)
    move_checked(stage_participants, participant_folder)
    move_checked(staged_excel, output_excel)
  },
  error = function(e) {
    # Bei Fehler alte Ausgaben wiederherstellen; neue Daten im Stage behalten.
    if (dir.exists(backup_participants)) {
      if (dir.exists(participant_folder)) move_checked(participant_folder, stage_participants)
      move_checked(backup_participants, participant_folder)
    } else if (!had_participants && dir.exists(participant_folder)) {
      move_checked(participant_folder, stage_participants)
    }
    if (file.exists(backup_excel)) {
      if (file.exists(output_excel)) move_checked(output_excel, staged_excel)
      move_checked(backup_excel, output_excel)
    }
    stop(conditionMessage(e), "\nNeuaufbau abgebrochen. Stage: ", stage_root)
  }
)
unlink(stage_root, recursive = TRUE)
message("Alte Ausgaben gesichert unter: ", backup_folder)

cat(
  "\nCODING SHEET CREATED\n",
  "Screenshots:   ", nrow(coding_export), "\n",
  "Participants:  ", n_distinct(coding_export$participant, na.rm = TRUE), "\n",
  "Files missing: ", nrow(missing_files), "\n",
  "Excel:         ", output_excel, "\n",
  sep = ""
)
