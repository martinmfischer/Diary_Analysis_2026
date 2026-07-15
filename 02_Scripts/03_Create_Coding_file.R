################################################################################
# Project: Tagebuchstudie
# File:    03_Create_Coding_File.R
#
# Purpose:
#   Erstellt eine Excel-Datei für die manuelle Inhaltscodierung der
#   hochgeladenen Screenshots.
#
# Struktur:
#   - Eine Zeile entspricht einem Screenshot.
#   - Automatisch vorhandene Daily-Angaben werden übernommen.
#   - Numerische Antwortcodes werden zusätzlich in Klartext übersetzt.
#   - Leere Spalten für die manuelle Codierung werden angelegt.
#
# Manuelle Codierung:
#   - topic_coded       = Thema des Beitrags
#   - source_coded      = Kategorie der Quelle bzw. des Accounts
#   - source_name_coded = konkreter Name der Quelle bzw. des Accounts
#   - platform_coded    = Plattform, vorausgefüllt und ggf. korrigierbar
#   - media_format      = Text, Bild, Video oder Mischform/unklar
#
# Input:
#   01_Data/taeglicher_fragebogen_screenshot_upload.rds
#
# Output:
#   06_Coding/coding_sheet.xlsx
#   06_Coding/coding_sheet_initial.rds
#
# Wichtiger Hinweis:
#   Die Excel-Datei ist nach Beginn der manuellen Codierung die maßgebliche
#   Coding-Datei. Das RDS bildet nur den automatisch erzeugten Ausgangsstand ab.
################################################################################


#===============================================================================
# 01 Packages
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  openxlsx,
  fs,
  tools
)


#===============================================================================
# 02 Settings
#===============================================================================

# FALSE verhindert, dass eine bereits bearbeitete Coding-Datei versehentlich
# überschrieben wird.
overwrite_existing <- FALSE

# Maximale Anzahl möglicher Screenshots pro Daily-Befragung
maximum_screenshots <- 10


#===============================================================================
# 03 Paths
#===============================================================================

data_file <- file.path(
  "01_Data",
  "taeglicher_fragebogen_screenshot_upload.rds"
)

participant_folder <- "05_Participants"
output_folder <- "06_Coding"

output_excel <- file.path(
  output_folder,
  "coding_sheet.xlsx"
)

output_rds <- file.path(
  output_folder,
  "coding_sheet_initial.rds"
)

fs::dir_create(output_folder)


#===============================================================================
# 04 Protect existing coding
#===============================================================================

if (
  file.exists(output_excel) &&
  !overwrite_existing
) {
  
  stop(
    paste0(
      "Die Datei '",
      output_excel,
      "' existiert bereits.\n\n",
      "Sie wird nicht überschrieben, damit keine manuelle Codierung ",
      "verloren geht.\n",
      "Zum bewussten Überschreiben muss am Anfang des Skripts ",
      "'overwrite_existing <- TRUE' gesetzt werden."
    )
  )
}


#===============================================================================
# 05 Helper functions
#===============================================================================

# Textwerte vereinheitlichen und technische Missing-Codes entfernen
clean_text <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_squish(x)
  
  x[
    is.na(x) |
      stringr::str_to_upper(x) %in% c(
        "",
        "-1",
        "NA",
        "N/A",
        "NULL"
      )
  ] <- NA_character_
  
  x
}


# Numerische Werte robust einlesen
clean_numeric <- function(x) {
  
  x <- suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
  
  x[x == -1] <- NA_real_
  
  x
}


# Einzelwert aus einer Tabellenzeile auslesen
get_row_value <- function(
    row,
    variable
) {
  
  if (!variable %in% names(row)) {
    return(NA)
  }
  
  row[[variable]][[1]]
}


# Plattformcodes übersetzen
label_platform <- function(x) {
  
  x <- clean_numeric(x)
  
  dplyr::case_when(
    x == 1 ~ "Facebook",
    x == 2 ~ "Instagram",
    x == 3 ~ "TikTok",
    x == 4 ~ "X",
    is.na(x) ~ NA_character_,
    TRUE ~ "Ungültiger Code"
  )
}


# Incidentality-Codes übersetzen
label_incidentality <- function(x) {
  
  x <- clean_numeric(x)
  
  dplyr::case_when(
    x == 1 ~
      "Gezielt nach Thema oder Beiträgen dieses Accounts gesucht",
    
    x == 2 ~
      "Account gefolgt, Beitrag aber nicht gezielt gesucht",
    
    x == 3 ~
      "Zufällig auf den Beitrag gestoßen",
    
    is.na(x) ~ NA_character_,
    
    TRUE ~ "Ungültiger Code"
  )
}


# Räumlichen Nutzungskontext übersetzen
label_locality <- function(x) {
  
  x <- clean_numeric(x)
  
  dplyr::case_when(
    x == 1 ~ "Zu Hause",
    x == 2 ~ "Unterwegs",
    x == 3 ~ "Weiß nicht mehr",
    is.na(x) ~ NA_character_,
    TRUE ~ "Ungültiger Code"
  )
}


# Sozialen Nutzungskontext übersetzen
label_situation <- function(x) {
  
  x <- clean_numeric(x)
  
  dplyr::case_when(
    x == 1 ~ "Plattform allein genutzt",
    x == 2 ~ "Plattform gemeinsam mit jemand anderem genutzt",
    x == 3 ~ "Weiß nicht mehr",
    is.na(x) ~ NA_character_,
    TRUE ~ "Ungültiger Code"
  )
}


# Binäre Mehrfachauswahl übersetzen
#
# 1  = Option ausgewählt
# 0  = Option nicht ausgewählt
# -1 = keine verwertbare Angabe
label_interaction <- function(x) {
  
  x <- suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
  
  dplyr::case_when(
    x == 1 ~ "Ja",
    x == 0 ~ "Nein",
    x == -1 ~ "Keine Angabe",
    is.na(x) ~ NA_character_,
    TRUE ~ "Ungültiger Code"
  )
}


# Logischen Start-/Stoppwert lesbar machen
label_startstop <- function(x) {
  
  x <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(x)
    )
  )
  
  dplyr::case_when(
    x %in% c("true", "t", "1") ~ "Weiter",
    x %in% c("false", "f", "0") ~ "Stopp",
    is.na(x) | x %in% c("", "na", "n/a") ~ NA_character_,
    TRUE ~ "Ungültiger Code"
  )
}


# Dateinamen mit Endung erzeugen
create_filename <- function(
    participant,
    study_day,
    photo,
    original_filename
) {
  
  extension <- tools::file_ext(
    original_filename
  )
  
  extension_part <- if (
    is.na(extension) ||
    extension == ""
  ) {
    ""
  } else {
    paste0(
      ".",
      extension
    )
  }
  
  paste0(
    participant,
    "_Tag_",
    study_day,
    "_Photo_",
    photo,
    extension_part
  )
}


# Excel-Blatt einheitlich formatieren
format_excel_sheet <- function(
    workbook,
    sheet,
    data
) {
  
  header_style <- openxlsx::createStyle(
    fontColour = "#FFFFFF",
    fgFill = "#315F6B",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "Bottom"
  )
  
  openxlsx::addStyle(
    workbook,
    sheet = sheet,
    style = header_style,
    rows = 1,
    cols = seq_len(
      ncol(data)
    ),
    gridExpand = TRUE
  )
  
  openxlsx::freezePane(
    workbook,
    sheet = sheet,
    firstRow = TRUE,
    firstCol = TRUE
  )
  
  openxlsx::setColWidths(
    workbook,
    sheet = sheet,
    cols = seq_len(
      ncol(data)
    ),
    widths = "auto"
  )
}


#===============================================================================
# 06 Load data
#===============================================================================

if (!file.exists(data_file)) {
  
  stop(
    "Die Daily-RDS-Datei wurde nicht gefunden: ",
    data_file
  )
}

daily <- readRDS(
  data_file
)


#===============================================================================
# 07 Check required variables
#===============================================================================

required_base_variables <- c(
  "personalParticipantCode",
  "scheduled"
)

required_screenshot_variables <- paste0(
  "daily_",
  1:maximum_screenshots,
  "_screenshot"
)

required_variables <- c(
  required_base_variables,
  required_screenshot_variables
)

missing_variables <- setdiff(
  required_variables,
  names(daily)
)

if (length(missing_variables) > 0) {
  
  stop(
    paste0(
      "Folgende benötigte Variablen fehlen:\n- ",
      paste(
        missing_variables,
        collapse = "\n- "
      )
    )
  )
}


#===============================================================================
# 08 Harmonize variable types
#===============================================================================

# Diese Variablen können je Screenshot-Slot aufgrund der CSV-Erkennung
# unterschiedliche Datentypen besitzen. Deshalb werden sie vereinheitlicht.

daily <- daily %>%
  mutate(
    across(
      matches(
        "^daily_\\d+_(screenshot|topic|account)$"
      ),
      as.character
    ),
    
    personalParticipantCode = as.character(
      personalParticipantCode
    )
  )


#===============================================================================
# 09 Determine study day
#===============================================================================

daily <- daily %>%
  mutate(
    submission_row = row_number(),
    
    scheduled_date = as.Date(
      scheduled
    )
  ) %>%
  
  arrange(
    personalParticipantCode,
    scheduled,
    committed,
    submission_row
  ) %>%
  
  group_by(
    personalParticipantCode
  ) %>%
  
  mutate(
    study_day = dense_rank(
      scheduled_date
    )
  ) %>%
  
  ungroup() %>%
  
  group_by(
    personalParticipantCode,
    scheduled_date
  ) %>%
  
  mutate(
    submission_within_day = row_number()
  ) %>%
  
  ungroup()


#===============================================================================
# 10 Transform to screenshot-level long format
#===============================================================================

coding_long <- purrr::map_dfr(
  seq_len(
    nrow(daily)
  ),
  function(row_number_daily) {
    
    row <- daily[
      row_number_daily,
      ,
      drop = FALSE
    ]
    
    participant <- clean_text(
      get_row_value(
        row,
        "personalParticipantCode"
      )
    )
    
    study_day <- clean_numeric(
      get_row_value(
        row,
        "study_day"
      )
    )
    
    scheduled_value <- get_row_value(
      row,
      "scheduled"
    )
    
    committed_value <- get_row_value(
      row,
      "committed"
    )
    
    submission_row <- clean_numeric(
      get_row_value(
        row,
        "submission_row"
      )
    )
    
    submission_within_day <- clean_numeric(
      get_row_value(
        row,
        "submission_within_day"
      )
    )
    
    
    purrr::map_dfr(
      seq_len(
        maximum_screenshots
      ),
      function(screenshot_slot) {
        
        prefix <- paste0(
          "daily_",
          screenshot_slot,
          "_"
        )
        
        original_filename <- clean_text(
          get_row_value(
            row,
            paste0(
              prefix,
              "screenshot"
            )
          )
        )
        
        # Slots ohne hochgeladene Datei werden vollständig übersprungen.
        if (is.na(original_filename)) {
          return(NULL)
        }
        
        
        platform_code <- clean_numeric(
          get_row_value(
            row,
            paste0(
              prefix,
              "platform"
            )
          )
        )
        
        incidentality_code <- clean_numeric(
          get_row_value(
            row,
            paste0(
              prefix,
              "incidentality"
            )
          )
        )
        
        interaction_read_code <- suppressWarnings(
          as.numeric(
            as.character(
              get_row_value(
                row,
                paste0(
                  prefix,
                  "interaction_1"
                )
              )
            )
          )
        )
        
        interaction_research_code <- suppressWarnings(
          as.numeric(
            as.character(
              get_row_value(
                row,
                paste0(
                  prefix,
                  "interaction_2"
                )
              )
            )
          )
        )
        
        interaction_engagement_code <- suppressWarnings(
          as.numeric(
            as.character(
              get_row_value(
                row,
                paste0(
                  prefix,
                  "interaction_3"
                )
              )
            )
          )
        )
        
        locality_code <- clean_numeric(
          get_row_value(
            row,
            paste0(
              prefix,
              "locality"
            )
          )
        )
        
        situation_code <- clean_numeric(
          get_row_value(
            row,
            paste0(
              prefix,
              "situation"
            )
          )
        )
        
        startstop_raw <- get_row_value(
          row,
          paste0(
            prefix,
            "startstop"
          )
        )
        
        
        interaction_selected <- c(
          if (
            !is.na(interaction_read_code) &&
            interaction_read_code == 1
          ) {
            "Gründlich gelesen/angeschaut"
          } else {
            NULL
          },
          
          if (
            !is.na(interaction_research_code) &&
            interaction_research_code == 1
          ) {
            "Weiter zum Thema informiert"
          } else {
            NULL
          },
          
          if (
            !is.na(interaction_engagement_code) &&
            interaction_engagement_code == 1
          ) {
            "Mit dem Beitrag interagiert"
          } else {
            NULL
          }
        )
        
        interaction_summary <- if (
          length(interaction_selected) == 0
        ) {
          NA_character_
        } else {
          paste(
            interaction_selected,
            collapse = "; "
          )
        }
        
        
        tibble(
          participant = participant,
          study_day = study_day,
          
          submission_row = submission_row,
          submission_within_day = submission_within_day,
          screenshot_slot = screenshot_slot,
          
          scheduled = scheduled_value,
          committed = committed_value,
          
          original_filename = original_filename,
          
          topic_participant = clean_text(
            get_row_value(
              row,
              paste0(
                prefix,
                "topic"
              )
            )
          ),
          
          account_participant = clean_text(
            get_row_value(
              row,
              paste0(
                prefix,
                "account"
              )
            )
          ),
          
          platform_code = platform_code,
          platform_reported = label_platform(
            platform_code
          ),
          
          incidentality_code = incidentality_code,
          incidentality_label = label_incidentality(
            incidentality_code
          ),
          
          interaction_read_code =
            interaction_read_code,
          
          interaction_read =
            label_interaction(
              interaction_read_code
            ),
          
          interaction_research_code =
            interaction_research_code,
          
          interaction_research =
            label_interaction(
              interaction_research_code
            ),
          
          interaction_engagement_code =
            interaction_engagement_code,
          
          interaction_engagement =
            label_interaction(
              interaction_engagement_code
            ),
          
          interaction_summary =
            interaction_summary,
          
          locality_code = locality_code,
          locality_label = label_locality(
            locality_code
          ),
          
          situation_code = situation_code,
          situation_label = label_situation(
            situation_code
          ),
          
          startstop_raw = startstop_raw,
          startstop_label = label_startstop(
            startstop_raw
          )
        )
      }
    )
  }
)


#===============================================================================
# 11 Assign unique photo numbers within each participant and study day
#===============================================================================

# Falls eine Person am selben Studientag mehrere Daily-Fragebögen abschließt,
# werden die Screenshots trotzdem eindeutig und fortlaufend nummeriert.

coding_long <- coding_long %>%
  arrange(
    participant,
    study_day,
    scheduled,
    committed,
    submission_row,
    screenshot_slot
  ) %>%
  
  group_by(
    participant,
    study_day
  ) %>%
  
  mutate(
    photo = row_number()
  ) %>%
  
  ungroup()


#===============================================================================
# 12 Create final filenames and paths
#===============================================================================

coding_long <- coding_long %>%
  rowwise() %>%
  
  mutate(
    filename = create_filename(
      participant = participant,
      study_day = study_day,
      photo = photo,
      original_filename = original_filename
    ),
    
    filepath = file.path(
      participant_folder,
      participant,
      paste0(
        "Tag_",
        study_day
      ),
      filename
    ),
    
    file_exists = fs::file_exists(
      filepath
    ),
    
    screenshot_id = paste0(
      participant,
      "_D",
      study_day,
      "_P",
      photo
    )
  ) %>%
  
  ungroup()


#===============================================================================
# 13 Add manual coding columns
#===============================================================================

coding <- coding_long %>%
  mutate(
    
    # Manuell zu codierende Variablen
    topic_coded = NA_character_,
    
    source_coded = NA_character_,
    
    source_name_coded = NA_character_,
    
    # Plattform wird vorausgefüllt, kann im Coding aber korrigiert werden.
    platform_coded = platform_reported,
    
    media_format = NA_character_,
    
    notes = NA_character_,
    
    coder = NA_character_,
    
    coding_completed = FALSE,
    
    coding_date = as.Date(NA)
  )


#===============================================================================
# 14 Arrange columns for coding
#===============================================================================

coding_export <- coding %>%
  select(
    
    # Identifikation und Datei
    screenshot_id,
    participant,
    study_day,
    photo,
    filename,
    filepath,
    file_exists,
    
    # Manuelle Coding-Spalten
    topic_coded,
    source_coded,
    source_name_coded,
    platform_coded,
    media_format,
    notes,
    coder,
    coding_completed,
    coding_date,
    
    # Angaben der Teilnehmenden
    topic_participant,
    account_participant,
    
    # Lesbare Daily-Angaben
    platform_reported,
    incidentality_label,
    interaction_read,
    interaction_research,
    interaction_engagement,
    interaction_summary,
    locality_label,
    situation_label,
    startstop_label,
    
    # Technische und numerische Angaben
    platform_code,
    incidentality_code,
    interaction_read_code,
    interaction_research_code,
    interaction_engagement_code,
    locality_code,
    situation_code,
    startstop_raw,
    
    original_filename,
    screenshot_slot,
    submission_row,
    submission_within_day,
    scheduled,
    committed
  )


#===============================================================================
# 15 Data-quality overview
#===============================================================================

quality_summary <- tibble(
  Indicator = c(
    "Screenshots insgesamt",
    "Teilnehmende",
    "Teilnehmertage",
    "Dateien am erwarteten Speicherort gefunden",
    "Dateien am erwarteten Speicherort nicht gefunden",
    "Fehlende Plattformangaben",
    "Fehlende Incidentality-Angaben",
    "Fehlende freiwillige Themenangaben",
    "Fehlende freiwillige Accountangaben"
  ),
  
  Value = c(
    nrow(coding_export),
    
    n_distinct(
      coding_export$participant
    ),
    
    n_distinct(
      paste(
        coding_export$participant,
        coding_export$study_day,
        sep = "_"
      )
    ),
    
    sum(
      coding_export$file_exists,
      na.rm = TRUE
    ),
    
    sum(
      !coding_export$file_exists,
      na.rm = TRUE
    ),
    
    sum(
      is.na(
        coding_export$platform_reported
      )
    ),
    
    sum(
      is.na(
        coding_export$incidentality_label
      )
    ),
    
    sum(
      is.na(
        coding_export$topic_participant
      )
    ),
    
    sum(
      is.na(
        coding_export$account_participant
      )
    )
  )
)


missing_files <- coding_export %>%
  filter(
    !file_exists
  ) %>%
  select(
    screenshot_id,
    participant,
    study_day,
    photo,
    filename,
    filepath,
    original_filename
  )


#===============================================================================
# 16 Codebook
#===============================================================================

codebook <- tribble(
  ~Variable, ~Type, ~Description, ~Values,
  
  "screenshot_id",
  "Automatisch",
  "Eindeutige ID des Screenshots",
  "Participant Code + Studientag + Fotonummer",
  
  "participant",
  "Automatisch",
  "Pseudonymer Participant Code",
  "Freitext-ID",
  
  "study_day",
  "Automatisch",
  "Chronologisch bestimmter Studientag der jeweiligen Person",
  "1 bis 7; bei weiteren Datumswerten ggf. höher",
  
  "photo",
  "Automatisch",
  "Fortlaufende Nummer des Fotos innerhalb von Person und Studientag",
  "1 bis n",
  
  "topic_coded",
  "Manuelles Coding",
  "Inhaltliches Hauptthema des Beitrags",
  "Gemäß finalem Topic-Codebuch",
  
  "source_coded",
  "Manuelles Coding",
  "Kategorie der Quelle bzw. des Accounts",
  "Gemäß Source-Codebuch bzw. DBÖS",
  
  "source_name_coded",
  "Manuelles Coding",
  "Konkreter Name des Accounts, Mediums, Akteurs oder der Organisation",
  "Freitext",
  
  "platform_coded",
  "Manuelles Coding",
  "Geprüfte Plattform des Beitrags; mit Angabe der Person vorausgefüllt",
  "Facebook; Instagram; TikTok; X",
  
  "media_format",
  "Manuelles Coding",
  "Dominantes Format des dargestellten Beitrags",
  "Text; Bild; Video; Mischform/unklar",
  
  "coding_completed",
  "Coding-Workflow",
  "Kennzeichnet eine vollständig codierte Zeile",
  "FALSE; TRUE",
  
  "topic_participant",
  "Daily-Befragung",
  "Freiwillige Themenangabe der teilnehmenden Person",
  "Freitext; NA bei erkennbarer Darstellung oder fehlender Angabe",
  
  "account_participant",
  "Daily-Befragung",
  "Freiwillige Accountangabe der teilnehmenden Person",
  "Freitext; NA bei erkennbarer Darstellung oder fehlender Angabe",
  
  "platform_reported",
  "Daily-Befragung",
  "Von der teilnehmenden Person angegebene Plattform",
  "Facebook; Instagram; TikTok; X",
  
  "incidentality_label",
  "Daily-Befragung",
  "Art, wie die Person auf den Beitrag gestoßen ist",
  paste(
    "Gezielt gesucht;",
    "Account gefolgt, aber nicht gezielt gesucht;",
    "zufällig begegnet"
  ),
  
  "interaction_read",
  "Daily-Befragung",
  "Beitrag gründlich gelesen oder angeschaut",
  "Ja; Nein; Keine Angabe",
  
  "interaction_research",
  "Daily-Befragung",
  "Weiterführende Informationen gesucht",
  "Ja; Nein; Keine Angabe",
  
  "interaction_engagement",
  "Daily-Befragung",
  "Mit dem Beitrag sichtbar interagiert",
  "Ja; Nein; Keine Angabe",
  
  "locality_label",
  "Daily-Befragung",
  "Räumlicher Nutzungskontext",
  "Zu Hause; unterwegs; weiß nicht mehr",
  
  "situation_label",
  "Daily-Befragung",
  "Sozialer Nutzungskontext",
  "Allein; gemeinsam mit anderen; weiß nicht mehr",
  
  "platform_code",
  "Technisch",
  "Numerischer Rohcode der Plattform",
  "1 Facebook; 2 Instagram; 3 TikTok; 4 X",
  
  "incidentality_code",
  "Technisch",
  "Numerischer Rohcode der Incidentality-Antwort",
  "1 gezielt; 2 gefolgt, nicht gezielt; 3 zufällig",
  
  "interaction_read_code",
  "Technisch",
  "Rohcode für gründliche Rezeption",
  "1 ausgewählt; 0 nicht ausgewählt; -1 keine Angabe",
  
  "interaction_research_code",
  "Technisch",
  "Rohcode für weiterführende Informationssuche",
  "1 ausgewählt; 0 nicht ausgewählt; -1 keine Angabe",
  
  "interaction_engagement_code",
  "Technisch",
  "Rohcode für sichtbare Interaktion",
  "1 ausgewählt; 0 nicht ausgewählt; -1 keine Angabe"
)


#===============================================================================
# 17 Save initial RDS
#===============================================================================

saveRDS(
  coding_export,
  output_rds
)


#===============================================================================
# 18 Create Excel workbook
#===============================================================================

workbook <- openxlsx::createWorkbook()


#-------------------------------------------------------------------------------
# Coding sheet
#-------------------------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  sheetName = "Coding"
)

openxlsx::writeDataTable(
  workbook,
  sheet = "Coding",
  x = coding_export,
  tableStyle = "TableStyleMedium2",
  withFilter = TRUE
)

format_excel_sheet(
  workbook,
  sheet = "Coding",
  data = coding_export
)


#-------------------------------------------------------------------------------
# Quality summary
#-------------------------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  sheetName = "Quality_Summary"
)

openxlsx::writeDataTable(
  workbook,
  sheet = "Quality_Summary",
  x = quality_summary,
  tableStyle = "TableStyleMedium2"
)

format_excel_sheet(
  workbook,
  sheet = "Quality_Summary",
  data = quality_summary
)


#-------------------------------------------------------------------------------
# Missing files
#-------------------------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  sheetName = "Missing_Files"
)

openxlsx::writeDataTable(
  workbook,
  sheet = "Missing_Files",
  x = missing_files,
  tableStyle = "TableStyleMedium2"
)

format_excel_sheet(
  workbook,
  sheet = "Missing_Files",
  data = missing_files
)


#-------------------------------------------------------------------------------
# Codebook
#-------------------------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  sheetName = "Codebook"
)

openxlsx::writeDataTable(
  workbook,
  sheet = "Codebook",
  x = codebook,
  tableStyle = "TableStyleMedium2"
)

format_excel_sheet(
  workbook,
  sheet = "Codebook",
  data = codebook
)


#===============================================================================
# 19 Add dropdown menus
#===============================================================================

if (nrow(coding_export) > 0) {
  
  coding_rows <- 2:(
    nrow(coding_export) + 1
  )
  
  
  platform_column <- which(
    names(coding_export) ==
      "platform_coded"
  )
  
  format_column <- which(
    names(coding_export) ==
      "media_format"
  )
  
  completed_column <- which(
    names(coding_export) ==
      "coding_completed"
  )
  
  
  openxlsx::dataValidation(
    workbook,
    sheet = "Coding",
    cols = platform_column,
    rows = coding_rows,
    type = "list",
    value = '"Facebook,Instagram,TikTok,X"'
  )
  
  
  openxlsx::dataValidation(
    workbook,
    sheet = "Coding",
    cols = format_column,
    rows = coding_rows,
    type = "list",
    value = '"Text,Bild,Video,Mischform/unklar"'
  )
  
  
  openxlsx::dataValidation(
    workbook,
    sheet = "Coding",
    cols = completed_column,
    rows = coding_rows,
    type = "list",
    value = '"FALSE,TRUE"'
  )
}


#===============================================================================
# 20 Improve column widths
#===============================================================================

wide_columns <- which(
  names(coding_export) %in% c(
    "topic_coded",
    "source_coded",
    "source_name_coded",
    "notes",
    "topic_participant",
    "account_participant",
    "incidentality_label",
    "interaction_summary",
    "filepath",
    "original_filename"
  )
)

openxlsx::setColWidths(
  workbook,
  sheet = "Coding",
  cols = wide_columns,
  widths = 35
)


narrow_columns <- which(
  names(coding_export) %in% c(
    "study_day",
    "photo",
    "file_exists",
    "platform_code",
    "incidentality_code",
    "interaction_read_code",
    "interaction_research_code",
    "interaction_engagement_code",
    "locality_code",
    "situation_code",
    "submission_row",
    "submission_within_day",
    "screenshot_slot"
  )
)

openxlsx::setColWidths(
  workbook,
  sheet = "Coding",
  cols = narrow_columns,
  widths = 12
)


#===============================================================================
# 21 Save workbook
#===============================================================================

openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = overwrite_existing
)


#===============================================================================
# 22 Console report
#===============================================================================

cat(
  "\n",
  "============================================================\n",
  "CODING SHEET CREATED\n",
  "============================================================\n",
  sep = ""
)

cat(
  "Screenshots: ",
  nrow(coding_export),
  "\n",
  sep = ""
)

cat(
  "Participants: ",
  n_distinct(
    coding_export$participant
  ),
  "\n",
  sep = ""
)

cat(
  "Participant days: ",
  n_distinct(
    paste(
      coding_export$participant,
      coding_export$study_day,
      sep = "_"
    )
  ),
  "\n",
  sep = ""
)

cat(
  "Files found: ",
  sum(
    coding_export$file_exists,
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Files not found: ",
  sum(
    !coding_export$file_exists,
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "\nExcel file:\n",
  output_excel,
  "\n",
  sep = ""
)

cat(
  "\nInitial RDS:\n",
  output_rds,
  "\n",
  sep = ""
)

cat(
  "============================================================\n"
)