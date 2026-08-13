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
#   public_rel_coded  1 = öffentlich relevant; 0 = nicht relevant;
#                     -1 = nicht beurteilbar. Bei 0/-1 keine weitere
#                     Inhaltscodierung.
#   topic_coded       Hauptthema
#   source_coded      Quellentyp
#   source_name_coded konkrete Quelle / Account
#   platform_coded    geprüfte Plattform, vorausgefüllt
#   media_format      1 Text/Link; 2 statisch visuell; 3 bewegt/audiovisuell;
#                     4 statisch + bewegt; -1 nicht bestimmbar
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
  "public_rel_coded", "topic_coded", "source_coded", "source_name_coded",
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
  "public_rel_coded", "1", "Publicly relevant",
  "Information, opinion, evaluation, contextualization or action orientation with meaning beyond the private circle.",
  "public_rel_coded", "0", "Not publicly relevant",
  "Purely private, personal, self-presentational, purely entertaining or purely commercial function without public reference.",
  "public_rel_coded", "-1", "Not assessable",
  "Screenshot technically unusable or content not reliably assessable.",
  "topic_coded", "", "Main topic", "Only if public_rel_coded = 1; per the topic codebook.",
  "source_coded", "", "Source type", "Only if public_rel_coded = 1; per the source codebook.",
  "source_name_coded", "", "Concrete source", "Only if public_rel_coded = 1; visible account/source name.",
  "platform_coded", "", "Verified platform", "Pre-filled; correct if needed.",
  "media_format", "1", "Text/link-based",
  "Native text post or standardized link/article preview; ignore the caption of an image/video post.",
  "media_format", "2", "Static visual format",
  "Photo, illustration, graphic, meme, text card, infographic or purely static carousel.",
  "media_format", "3", "Moving audiovisual format",
  "Video, reel, TikTok, GIF or animation; also with text overlays or subtitles.",
  "media_format", "4", "Mixed media format",
  "Actual combination of static and moving media elements within the same post.",
  "media_format", "-1", "Not determinable", "Post format cannot be reliably identified from the screenshot.",
  "notes", "", "Notes", "Only for borderline cases or particularities.",
  "coder", "", "Coder", "Initials or name.",
  "coding_completed", "TRUE/FALSE", "Coding completed",
  "TRUE only after final review; for public_rel_coded = 0/-1 leave topic, source and format empty.",
  "coding_date", "", "Coding date", "Date of the final coding."
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
    public_rel_coded = '"1,0,-1"',
    platform_coded   = '"Facebook,Instagram,TikTok,X"',
    media_format     = '"1,2,3,4,-1"',
    coding_completed = '"FALSE,TRUE"'
  )
  
  purrr::iwalk(validation, ~ openxlsx::dataValidation(
    wb, "Coding", cols = match(.y, names(coding_export)), rows = rows,
    type = "list", value = .x
  ))
  
  # public_rel_coded as gate: 0/-1 grays out Topic, Source and Format.
  rel_col <- match("public_rel_coded", names(coding_export))
  rel_chr <- openxlsx::int2col(rel_col)
  
  for (rule in list(
    list(value = 1,  fill = "#DDEBDD"),
    list(value = 0,  fill = "#E6E6E6"),
    list(value = -1, fill = "#F4E0C7")
  )) {
    openxlsx::conditionalFormatting(
      wb, "Coding", cols = rel_col, rows = rows, type = "expression",
      rule = paste0("$", rel_chr, "2=", rule$value),
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
    "1 = publicly relevant\n0 = not publicly relevant\n-1 = not assessable\n\nFor 0/-1 leave topic, source and format empty.",
    author = "Codebook"
  )
)
openxlsx::writeComment(
  wb, "Coding", match("media_format", names(coding_export)), 1,
  openxlsx::createComment(
    "1 = text/link\n2 = static visual\n3 = moving/audiovisual\n4 = static + moving\n-1 = not determinable\n\nIgnore the caption.",
    author = "Codebook"
  )
)

openxlsx::freezePane(wb, "Coding", firstActiveRow = 2,
                     firstActiveCol = length(id_cols) + 1)

widths <- c(
  screenshot_id = 18, participant = 14, study_day = 9, photo = 8,
  filename = 28, filepath = 42, file_exists = 10,
  public_rel_coded = 14, topic_coded = 25, source_coded = 27,
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
