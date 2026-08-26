################################################################################
# Project: Tagebuchstudie
# File:    03_Create_Coding_File.R
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
#   advertisement_coded 1 = Werbung/Anzeige; 2 = keine Werbung/Anzeige;
#                       99 = sonstiges / nicht eindeutig.
#   topic_coded         1-14 = Hauptthema; 99 = sonstiges / nicht eindeutig
#   source_coded        1-11 = Quellentyp; 99 = sonstige / Account nicht erkennbar
#   source_name_coded   konkrete Quelle / Account
#   platform_coded      geprüfte Plattform, vorausgefüllt
#   media_format        1 Text/Link; 2 statisch visuell; 3 bewegt/audiovisuell;
#                       4 gemischt; 99 nicht bestimmbar
#
# Input:  01_Data/taeglicher_fragebogen_screenshot_upload.rds
# Output: 06_Coding/coding_sheet.xlsx
#
# ACHTUNG: Nach Beginn der manuellen Codierung ist die Excel-Datei maßgeblich.
################################################################################

rm(list = ls())


#===============================================================================
# 01 Setup
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, fs)

source(file.path("02_Scripts", "00_Helpers.R"))

overwrite_existing <- FALSE

data_file          <- file.path("01_Data", "taeglicher_fragebogen_screenshot_upload.rds")
participant_folder <- "05_Participants"
output_folder      <- "06_Coding"
output_excel       <- file.path(output_folder, "coding_sheet.xlsx")

fs::dir_create(output_folder)

if (!file.exists(data_file)) stop("Daily-RDS nicht gefunden: ", data_file)
if (file.exists(output_excel) && !overwrite_existing) {
  stop(
    "coding_sheet.xlsx existiert bereits und wird nicht überschrieben.\n",
    "Für einen bewussten Neuaufbau overwrite_existing <- TRUE setzen."
  )
}


#===============================================================================
# 02 Small local helpers and labels
#===============================================================================

label_code <- function(x, labels) {
  dplyr::recode(as.character(x), !!!labels,
                .default = "Invalid code", .missing = NA_character_)
}

collapse_interactions <- function(read, research, engagement) {
  x <- c(
    if (!is.na(read)       && read       == 1) "Read/watched thoroughly",
    if (!is.na(research)   && research   == 1) "Sought further information",
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


#===============================================================================
# 03 Daily data -> one row per screenshot
#===============================================================================

daily <- readRDS(data_file)

missing_base <- setdiff(c("personalParticipantCode", "scheduled"), names(daily))
if (length(missing_base) > 0) {
  stop("Benötigte Variablen fehlen: ", paste(missing_base, collapse = ", "))
}

if (!any(str_detect(names(daily), "^daily_[0-9]+_screenshot$"))) {
  stop("Keine Variablen nach dem Muster daily_[n]_screenshot gefunden.")
}

# Studientag, Foto-Nummer, Dateiname, Pfad und screenshot_id stammen aus der
# gemeinsamen Funktion (00_Helpers.R), die auch 02_Sort_Files.R nutzt. So passen
# Coding-Sheet und kopierte Dateien per Konstruktion zusammen.
coding <- derive_screenshot_index(daily, participant_folder = participant_folder)

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


#===============================================================================
# 04 Prepare variables and manual coding columns
#===============================================================================

coding <- coding %>%
  mutate(
    topic_participant   = clean_text(topic),
    account_participant = clean_text(account),
    
    platform_code      = na_if(clean_numeric(platform), -1),
    incidentality_code = na_if(clean_numeric(incidentality), -1),
    locality_code      = na_if(clean_numeric(locality), -1),
    situation_code     = na_if(clean_numeric(situation), -1),
    
    interaction_read_code       = clean_numeric(interaction_1),
    interaction_research_code   = clean_numeric(interaction_2),
    interaction_engagement_code = clean_numeric(interaction_3),
    
    platform_reported  = label_code(platform_code, platform_labels),
    incidentality_label = label_code(incidentality_code, incidentality_labels),
    locality_label      = label_code(locality_code, locality_labels),
    situation_label     = label_code(situation_code, situation_labels),
    interaction_read       = label_code(interaction_read_code, interaction_labels),
    interaction_research   = label_code(interaction_research_code, interaction_labels),
    interaction_engagement = label_code(interaction_engagement_code, interaction_labels),
    interaction_summary = purrr::pmap_chr(
      list(interaction_read_code, interaction_research_code, interaction_engagement_code),
      collapse_interactions
    ),
    startstop_raw = startstop,
    startstop_label = case_when(
      str_to_lower(clean_text(startstop_raw)) %in% c("true", "t", "1")  ~ "Weiter",
      str_to_lower(clean_text(startstop_raw)) %in% c("false", "f", "0") ~ "Stopp",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    file_exists = fs::file_exists(filepath),
    # Manual coding
    public_rel_coded  = NA_integer_,
    advertisement_coded = NA_integer_,
    topic_coded       = NA_character_,
    source_coded      = NA_character_,
    source_name_coded = NA_character_,
    platform_coded    = platform_reported,
    media_format      = NA_integer_,
    notes             = NA_character_,
    coder             = NA_character_,
    coding_completed  = FALSE,
    coding_date       = as.Date(NA)
  )


#===============================================================================
# 05 Coding-sheet layout
#===============================================================================

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


#===============================================================================
# 06 Quality and codebook
#===============================================================================

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
  
  # ---------------------------------------------------------------------------
  # 1. Öffentliche Relevanz
  # ---------------------------------------------------------------------------
  "public_rel_coded", "", "Codierlogik",
  "Es werden qua Studieninteresse nur Posts analysiert, in denen es um öffentlich relevante Themen geht. Diese Variable dient der Identifikation dieser Posts bzw. dem Ausschluss von Posts, die keine öffentlich relevanten Themen behandeln. Entscheidend ist der Inhalt des einzelnen Posts, nicht der veröffentlichende Account. Eine ausführliche Definition sowie eine detaillierte Auflistung von möglichen Themenfeldern finden sich im Anhang.",
  "public_rel_coded", "1", "Öffentlich relevant",
  "Der Beitrag enthält ausschließlich oder überwiegend Informationen, Meinungen oder Handlungsorientierungen zu gesellschaftlich, politisch, wirtschaftlich, kulturell, wissenschaftlich, gesundheitlich oder lokal relevanten Themen. Der Inhalt geht über rein private Interessen oder Beziehungen hinaus. Beispiele: aktuelle Informationen und Berichterstattung über Ereignisse und Entwicklungen von öffentlichem Interesse, Politik, gesellschaftliche Debatten, Gesundheitsthemen mit nicht nur persönlichem Bezug, Wissenschaft, Umwelt, Wirtschaft, Kultur, Unterhaltung (mit hinreichender Rezeption in der breiteren Gesellschaft), Geschichte, Sport, Wetter, Verkehr, öffentliche Veranstaltungen und lokale Informationen.",
  "public_rel_coded", "0", "Nicht öffentlich relevant",
  "Der Beitrag enthält ausschließlich oder überwiegend private, persönliche, selbstdarstellerische oder rein kommerzielle Inhalte ohne erkennbaren Bezug zu einem öffentlich relevanten Thema. Beispiele: Private Urlaubs- oder Familienfotos, persönliche Statusmeldungen, Geburtstagsgrüße, reine Selbstdarstellung, Unterhaltung ohne öffentlichen Bezug, Produktwerbung, Rabattaktionen oder private Verkaufsangebote.",
  "public_rel_coded", "99", "Unklar",
  "Thema des Beitrags ist nicht erkennbar",
  "public_rel_coded", "98", "Bild enthält keinen Einzelbeitrag von sozialen Medien",
  "",
  "public_rel_coded", "97", "Technisch fehlerhaft oder anderweitig unlesbar",
  "",
  
  # ---------------------------------------------------------------------------
  # 2. Advertisement
  # ---------------------------------------------------------------------------
  "advertisement_coded", "", "Codierlogik",
  "Ist der Beitrag als Anzeige oder Werbung gekennzeichnet? Primär ausschlaggebend ist die Frage, ob der Beitrag finanziert wurde, beispielsweise als Anzeige auf einer Plattform (bspw. Meta-Ads), oder deutlich sichtbar als sponsored content (bspw. eine Kooperation von Rewe mit einem Influencer).",
  "advertisement_coded", "1", "Werbung oder Anzeige",
  "Es handelt sich um einen klar erkennbaren Anzeigenbeitrag. Im Beitrag ist klar erkennbar, dass es sich um eine Plattformwerbung handelt, beispielsweise durch ein klar erkennbares Wort 'Anzeige' o.ä.",
  "advertisement_coded", "2", "Keine Werbung oder Anzeige",
  "Es handelt sich nicht um einen im obigen Sinne kommerziellen Beitrag.",
  "advertisement_coded", "99", "Sonstiges / nicht eindeutig",
  "",
  
  # ---------------------------------------------------------------------------
  # 3. Topic
  # ---------------------------------------------------------------------------
  "topic_coded", "", "Codierlogik",
  "Zentrales Thema des Beitrags. Politische Entscheidungen und Konflikte werden unter Politik codiert, auch wenn sie ein Sachgebiet wie Gesundheit oder Migration betreffen. Sachbezogene Information ohne primären politischen Entscheidungsbezug wird dem jeweiligen Fachgebiet zugeordnet. Bei mehreren gleichwertigen Themen zählt das erstgenannte Thema. Falls ein Thema klar ersichtlich dominant ist, wird dieses auch bei späterer Nennung codiert. Wenn das Thema nicht erkennbar ist, wird der Themenvorschlag der Teilnehmenden in das Codierraster eingeordnet.",
  "topic_coded", "1", "Politik, Staat & Wahlen",
  "Parteien, Regierungen, Parlamente, politische Entscheidungen, Wahlkampf, Demokratie, politische Proteste. Fokus auf Innenpolitik, Rest unter 2 codieren. Fokus auf Politiker /Akteure sowie Strukturen ergänzen bei Maßnahmen weiter unten",
  "topic_coded", "2", "Internationales, Krieg & Sicherheit",
  "Internationale Beziehungen, Kriege, Militär, Terrorismus, geopolitische Konflikte, äußere Sicherheit.",
  "topic_coded", "3", "Wirtschaft, Arbeit, Finanzen & Verbraucher",
  "Konjunktur, Unternehmen, Arbeitsmarkt, Preise, Geldanlage, Renten, Steuern, Verbraucherthemen.",
  "topic_coded", "4", "Gesellschaft, Soziales, Migration & Religion",
  "Zusammenleben, Ungleichheit, soziale Gruppen, Migration/Integration, Familie, Religion, gesellschaftliche Debatten.",
  "topic_coded", "5", "Bildung, Wissenschaft & Technologie",
  "Schule, Hochschule, Forschung, Digitalisierung, KI, technische Entwicklungen.",
  "topic_coded", "6", "Gesundheit & Pflege",
  "Krankheiten, Prävention, Medizin, Versorgung, Pflege, Gesundheitssystem ohne dominanten politischen Fokus.",
  "topic_coded", "7", "Klima, Umwelt & Energie",
  "Klimawandel, Naturschutz, Umweltbelastung, Energieversorgung und Energiewende.",
  "topic_coded", "8", "Kriminalität & Justiz",
  "Straftaten, Polizei, Gerichtsverfahren, Strafverfolgung, Urteile. Auch politisch motivierte Kriminalität. Auch Unfälle und Blaulichtberichterstattung.",
  "topic_coded", "9", "Verkehr, Infrastruktur & Wohnen",
  "ÖPNV, Straßen, Bahn, Mobilität, Bau, digitale Infrastruktur, Mieten und Wohnraum.",
  "topic_coded", "10", "Wetter & Naturereignisse",
  "Wetterberichte, Warnungen, Unwetter, Hochwasser, Erdbeben und andere Naturereignisse.",
  "topic_coded", "11", "Kultur, Medien & Unterhaltung",
  "Kunst, Literatur, Film, Musik, Fernsehen, Prominenz, Freizeit- und Unterhaltungsangebote.",
  "topic_coded", "12", "Geschichte & Erinnerung",
  "Historische Ereignisse, Jahrestage, Erinnerungskultur, historische Einordnung.",
  "topic_coded", "13", "Sport",
  "Sportereignisse, Ergebnisse, Persönlichkeiten, Sportpolitik nur bei klarem Sportfokus.",
  "topic_coded", "14", "Veranstaltungen & öffentlicher Service",
  "Lokale Termine, Öffnungszeiten, Warn- und Servicehinweise, kommunale Angebote, Veranstaltungshinweise.",
  "topic_coded", "99", "Sonstiges / nicht eindeutig",
  "Nur wenn kein Schwerpunkt sicher bestimmbar ist oder keine andere Kategorie zutrifft.",
  
  # ---------------------------------------------------------------------------
  # 4. Source
  # ---------------------------------------------------------------------------
  "source_coded", "", "Codierlogik",
  "Codiert wird der Account, der den Beitrag veröffentlicht hat. Bei Reposts zählt der ursprüngliche Account. Es zählt der veröffentlichende Account, nicht eine lediglich erwähnte oder abgebildete Person. Der konkrete Accountname wird zusätzlich in source_name_coded einheitlich ausgeschrieben. Bei Nichtkenntnis darf der Account über Suchmaschinen gesucht werden - zulässig sind hier primär allgemein anerkannte Quellen wie Wikipedia und die Biographie der Seite selbst.",
  "source_coded", "1", "Journalistisches Medium",
  "Redaktionell arbeitende Nachrichtenmedien, Lokalmedien, öffentlich-rechtliche und private journalistische Angebote.",
  "source_coded", "2", "Alternatives oder parteiisches Medienangebot",
  "Medienähnliche Angebote mit ausgeprägter politischer/ideologischer Positionierung oder ohne professionell-journalistischen Hintergrund.",
  "source_coded", "3", "Partei oder Politiker:in",
  "Parteien, Fraktionen, Mandatsträger:innen, Kandidierende und deren offizielle Accounts.",
  "source_coded", "4", "Staatliche oder öffentliche Institution",
  "Behörden, Ministerien, Kommunen, Polizei, Gerichte, öffentliche Einrichtungen.",
  "source_coded", "5", "NGO, Verband, Verein, Initiative oder Bewegung",
  "Zivilgesellschaftliche Organisationen, Interessenverbände, Kampagnen, Vereine und soziale Bewegungen.",
  "source_coded", "6", "Wissenschaft, Expert:in oder Faktencheck",
  "Forschungseinrichtungen, Hochschulen, Fachleute in Expertenrolle sowie Faktencheck-Organisationen.",
  "source_coded", "7", "Unternehmen oder Marke",
  "Kommerzielle Organisationen, Marken, Händler, Arbeitgeber und Produktaccounts.",
  "source_coded", "8", "Journalist:in",
  "Persönlicher Account eines/einer Journalist:in, kein Post unter eigenem Namen auf Account eines Mediums, einer Zeitung o.ä.",
  "source_coded", "9", "Creator:in, Influencer:in, oder sonstige Person der Öffentlichkeit",
  "Personenaccounts mit öffentlicher Reichweite, soweit nicht als Politiker:in oder institutionelle Expert:in zu codieren.",
  "source_coded", "10", "Private Person",
  "Erkennbar persönlicher Account ohne institutionelle oder öffentliche Sprecherrolle und mit nur sehr geringer Reichweite.",
  "source_coded", "11", "Kollektiv, Meme-, Satire- oder Aggregator-Seite",
  "Nicht eindeutig personalisierte Sammel-, Meme-, Satire- oder Repost-Accounts.",
  "source_coded", "99", "Sonstige / Account nicht erkennbar",
  "Account mit den obigen Kategorien nicht erfassbar oder nicht erkennbar (z.B. abgeschnitten)",
  "source_name_coded", "", "Zusatzfeld source_name_coded",
  "Der sichtbare Accountname wird als standardisierter Freitext erfasst, z. B. 'Frankfurter Rundschau', 'Bundesregierung' oder 'kein Bock auf Nazis'. Zusätze wie @-Handle, Emojis oder wechselnde Groß-/Kleinschreibung werden entfernt, sofern sie nicht elementarer Bestandteil des Handles sind oder zur eindeutigen Identifikation nötig sind.",
  "platform_coded", "", "Geprüfte Plattform",
  "Vorausgefüllt; bei Bedarf korrigieren.",
  
  # ---------------------------------------------------------------------------
  # 5. Format
  # ---------------------------------------------------------------------------
  "media_format", "", "Codierziel / Berücksichtigte Elemente",
  "Codiert wird die formale Darstellungsform des Posts, nicht das Dateiformat des hochgeladenen Screenshots und nicht die Länge des begleitenden Beschreibungstextes (Caption). Maßgeblich ist der Medienkörper des Posts, also das, was im Feed als eigentlicher Beitrag präsentiert beziehungsweise abgespielt wird. Facebook und X: kompletter Post, außer Accountname und Engagement-Metriken/Kommentare. Instagram und TikTok: kompletter visueller Teil des Posts (d.h. Video/Image), nicht: Caption, Accountname, Engagement-Metriken/Kommentare.",
  "media_format", "1", "Text-/linkbasiert",
  "Der Post besteht aus nativem Plattformtext und ggf. einer standardisierten Link-/Artikelvorschau. Es liegt kein eigenständiges statisches oder bewegtes visuelles Medium als primärer Postkörper vor.",
  "media_format", "2", "Statisches visuelles Format",
  "Der Post besteht ausschließlich aus einem oder mehreren unbewegten visuellen Elementen, etwa Foto, Illustration, Grafik, Meme, Infografik, Texttafel oder ausschließlich statischem Carousel.",
  "media_format", "3", "Bewegtes audiovisuelles Format",
  "Der Post enthält ausschließlich zeitlich ablaufenden beziehungsweise bewegten Inhalt, etwa Video, Reel, TikTok, GIF oder Animation. Wird auch bei umfangreichen Texteinblendungen oder Untertiteln codiert.",
  "media_format", "4", "Gemischtes Format (Text mit Bild oder Video, ggf. Link)",
  "Ein einzelner Post beziehungsweise ein Carousel enthält statische und bewegte Medienbestandteile sowie ggf. Text.",
  "media_format", "99", "Nicht bestimmbar",
  "Anhand des Screenshots ist nicht erkennbar, welches Format der Post hat.",
  
  # ---------------------------------------------------------------------------
  # Administrative Felder
  # ---------------------------------------------------------------------------
  "notes", "", "Notizen", "Nur für Grenzfälle oder Besonderheiten.",
  "coder", "", "Coder", "Initialen oder Name.",
  "coding_completed", "TRUE/FALSE", "Codierung abgeschlossen",
  "TRUE erst nach finaler Prüfung. Nur bei public_rel_coded = 1 werden Topic, Source, source_name_coded und media_format codiert.",
  "coding_date", "", "Codierdatum", "Datum der finalen Codierung."
)


#===============================================================================
# 07 Excel workbook
#===============================================================================

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Coding", gridLines = FALSE)
openxlsx::writeData(wb, "Coding", coding_export, withFilter = FALSE)

id_idx        <- match(id_cols, names(coding_export))
coding_idx    <- match(coding_cols, names(coding_export))
visible_idx   <- match(visible_info_cols, names(coding_export))
hidden_idx    <- match(hidden_info_cols, names(coding_export))
technical_idx <- match(technical_cols, names(coding_export))

header_style <- function(fill) openxlsx::createStyle(
  fgFill = fill, textDecoration = "bold", halign = "center", valign = "center",
  wrapText = TRUE, border = "Bottom"
)

openxlsx::addStyle(wb, "Coding", header_style("#DCE7EA"), 1, id_idx, gridExpand = TRUE)
openxlsx::addStyle(wb, "Coding", header_style("#D5B47A"), 1, coding_idx, gridExpand = TRUE)
openxlsx::addStyle(wb, "Coding", header_style("#E7EEEE"), 1,
                   c(visible_idx, hidden_idx), gridExpand = TRUE)
openxlsx::addStyle(wb, "Coding", header_style("#E1E1E1"), 1,
                   technical_idx, gridExpand = TRUE)

if (nrow(coding_export) > 0) {
  rows <- 2:(nrow(coding_export) + 1)
  
  # Coding block visually distinct.
  openxlsx::addStyle(
    wb, "Coding",
    openxlsx::createStyle(fgFill = "#FFF7E8", valign = "top", wrapText = TRUE),
    rows, coding_idx, gridExpand = TRUE, stack = TRUE
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
    public_rel_coded     = '"1,0,99,98,97"',
    advertisement_coded = '"1,2,99"',
    topic_coded         = '"1,2,3,4,5,6,7,8,9,10,11,12,13,14,99"',
    source_coded        = '"1,2,3,4,5,6,7,8,9,10,11,99"',
    platform_coded      = '"Facebook,Instagram,TikTok,X"',
    media_format        = '"1,2,3,4,99"',
    coding_completed    = '"FALSE,TRUE"'
  )
  
  purrr::iwalk(validation, ~ openxlsx::dataValidation(
    wb, "Coding", cols = match(.y, names(coding_export)), rows = rows,
    type = "list", value = .x
  ))
  
  # public_rel_coded as gate: only code 1 receives further content coding.
  rel_col <- match("public_rel_coded", names(coding_export))
  rel_chr <- openxlsx::int2col(rel_col)
  
  for (rule in list(
    list(value = 1,  fill = "#DDEBDD"),
    list(value = 0,  fill = "#E6E6E6"),
    list(value = 99, fill = "#F4E0C7"),
    list(value = 98, fill = "#E8DFF0"),
    list(value = 97, fill = "#F4CCCC")
  )) {
    openxlsx::conditionalFormatting(
      wb, "Coding", cols = rel_col, rows = rows, type = "expression",
      rule = paste0("$", rel_chr, "2=", rule$value),
      style = openxlsx::createStyle(fgFill = rule$fill)
    )
  }
  
  # advertisement_coded: 1 = Werbung, 2 = keine Werbung, 99 = unklar/sonstiges.
  ad_col <- match("advertisement_coded", names(coding_export))
  ad_chr <- openxlsx::int2col(ad_col)
  
  for (rule in list(
    list(value = 1,  fill = "#FCE4D6"),
    list(value = 2,  fill = "#E6E6E6"),
    list(value = 99, fill = "#F4E0C7")
  )) {
    openxlsx::conditionalFormatting(
      wb, "Coding", cols = ad_col, rows = rows, type = "expression",
      rule = paste0("$", ad_chr, "2=", rule$value),
      style = openxlsx::createStyle(fgFill = rule$fill)
    )
  }
  
  gated_cols <- match(
    c("topic_coded", "source_coded", "source_name_coded", "media_format"),
    names(coding_export)
  )
  openxlsx::conditionalFormatting(
    wb, "Coding", cols = gated_cols, rows = rows, type = "expression",
    rule = paste0("$", rel_chr, "2<>1"),
    style = openxlsx::createStyle(fgFill = "#EFEFEF", fontColour = "#999999")
  )
  
  done_col <- match("coding_completed", names(coding_export))
  done_chr <- openxlsx::int2col(done_col)
  openxlsx::conditionalFormatting(
    wb, "Coding", cols = done_col, rows = rows, type = "expression",
    rule = paste0("$", done_chr, "2=TRUE"),
    style = openxlsx::createStyle(fgFill = "#DDEBDD")
  )
}

# Concise instructions directly in the relevant headers.
openxlsx::writeComment(
  wb, "Coding", match("public_rel_coded", names(coding_export)), 1,
  openxlsx::createComment(
    "1 = öffentlich relevant\n0 = nicht öffentlich relevant\n99 = unklar\n98 = kein Einzelbeitrag sozialer Medien\n97 = technisch fehlerhaft / unlesbar\n\nNur bei 1 Topic, Source, Source Name und Format codieren.",
    author = "Codebook"
  )
)
openxlsx::writeComment(
  wb, "Coding", match("advertisement_coded", names(coding_export)), 1,
  openxlsx::createComment(
    "1 = Werbung / Anzeige\n2 = keine Werbung / Anzeige\n99 = sonstiges / nicht eindeutig",
    author = "Codebook"
  )
)

openxlsx::writeComment(
  wb, "Coding", match("topic_coded", names(coding_export)), 1,
  openxlsx::createComment(
    paste(
      "1 Politik, Staat & Wahlen",
      "2 Internationales, Krieg & Sicherheit",
      "3 Wirtschaft, Arbeit, Finanzen & Verbraucher",
      "4 Gesellschaft, Soziales, Migration & Religion",
      "5 Bildung, Wissenschaft & Technologie",
      "6 Gesundheit & Pflege",
      "7 Klima, Umwelt & Energie",
      "8 Kriminalität & Justiz",
      "9 Verkehr, Infrastruktur & Wohnen",
      "10 Wetter & Naturereignisse",
      "11 Kultur, Medien & Unterhaltung",
      "12 Geschichte & Erinnerung",
      "13 Sport",
      "14 Veranstaltungen & öffentlicher Service",
      "99 Sonstiges / nicht eindeutig",
      sep = "\n"
    ),
    author = "Codebook"
  )
)

openxlsx::writeComment(
  wb, "Coding", match("source_coded", names(coding_export)), 1,
  openxlsx::createComment(
    paste(
      "1 Journalistisches Medium",
      "2 Alternatives/parteiisches Medienangebot",
      "3 Partei oder Politiker:in",
      "4 Staatliche/öffentliche Institution",
      "5 NGO, Verband, Verein, Initiative/Bewegung",
      "6 Wissenschaft, Expert:in oder Faktencheck",
      "7 Unternehmen oder Marke",
      "8 Journalist:in",
      "9 Creator:in/Influencer:in/öffentliche Person",
      "10 Private Person",
      "11 Kollektiv/Meme-/Satire-/Aggregator-Seite",
      "99 Sonstige / Account nicht erkennbar",
      sep = "\n"
    ),
    author = "Codebook"
  )
)

openxlsx::writeComment(
  wb, "Coding", match("media_format", names(coding_export)), 1,
  openxlsx::createComment(
    "1 = text-/linkbasiert\n2 = statisches visuelles Format\n3 = bewegtes audiovisuelles Format\n4 = gemischtes Format\n99 = nicht bestimmbar\n\nInstagram/TikTok: Caption nicht berücksichtigen.",
    author = "Codebook"
  )
)

openxlsx::freezePane(wb, "Coding", firstActiveRow = 2,
                     firstActiveCol = length(id_cols) + 1)

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
  wb, "Coding", cols = c(hidden_idx, technical_idx), widths = 12, hidden = TRUE
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

# Quality: compact summary plus file list only when needed.
openxlsx::addWorksheet(wb, "Quality", gridLines = FALSE)
openxlsx::writeDataTable(wb, "Quality", quality_summary, tableStyle = "TableStyleMedium2")
if (nrow(missing_files) > 0) {
  start <- nrow(quality_summary) + 4
  openxlsx::writeData(wb, "Quality", "Files not found", startRow = start)
  openxlsx::writeDataTable(
    wb, "Quality", missing_files, startRow = start + 1,
    tableStyle = "TableStyleMedium2"
  )
}
openxlsx::setColWidths(wb, "Quality", 1:10, "auto")


#===============================================================================
# 08 Save
#===============================================================================

openxlsx::saveWorkbook(wb, output_excel, overwrite = overwrite_existing)

cat(
  "\nCODING SHEET CREATED\n",
  "Screenshots:   ", nrow(coding_export), "\n",
  "Participants:  ", n_distinct(coding_export$participant, na.rm = TRUE), "\n",
  "Files missing: ", nrow(missing_files), "\n",
  "Excel:         ", output_excel, "\n",
  sep = ""
)
