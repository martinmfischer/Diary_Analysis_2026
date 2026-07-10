################################################################################
# Project: Tagebuchstudie
# File:    04b_Daily_Analysis.R
#
# Purpose:
#   Aufbereitung und Auswertung der Daily-Befragung nach Abschluss der
#   manuellen Inhaltscodierung.
#
# Analyseeinheit:
#   Eine Zeile entspricht einem hochgeladenen Screenshot.
#
# Datenquellen:
#   06_Coding/coding_sheet.xlsx
#
# Die Coding-Datei enthält:
#   - automatisch übernommene Angaben aus der Daily-Befragung
#   - manuell codiertes Topic
#   - manuell codierten Account bzw. Source
#   - manuell geprüfte oder codierte Plattform
#   - optional manuell codiertes Medienformat
#
# Analyseschritte:
#   1. Coding-Datei einlesen
#   2. Spaltennamen und Datentypen vereinheitlichen
#   3. fehlende Werte und Antwortcodes bereinigen
#   4. abgeschlossene Codierungen auswählen
#   5. Datenqualität prüfen
#   6. Screenshot-Ebene deskriptiv auswerten
#   7. Daily-Daten auf Personenebene aggregieren
#   8. codierte Inhalte mit Nutzungssituation und Praktiken verknüpfen
#   9. Tabellen, Analysedatensätze und Abbildungen exportieren
#
# Output:
#   03_Output/Daily_Results.xlsx
#   03_Output/daily_screenshot_level.rds
#   03_Output/daily_participant_level.rds
#
# Figures:
#   04_Figures/Daily_*.png
################################################################################


#===============================================================================
# 01 Packages
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  readxl,
  openxlsx,
  janitor,
  fs,
  scales
)


#===============================================================================
# 02 Paths
#===============================================================================

coding_file <- file.path(
  "06_Coding",
  "coding_sheet.xlsx"
)

output_folder <- "03_Output"
figure_folder <- "04_Figures"

output_excel <- file.path(
  output_folder,
  "Daily_Results.xlsx"
)

screenshot_rds <- file.path(
  output_folder,
  "daily_screenshot_level.rds"
)

participant_rds <- file.path(
  output_folder,
  "daily_participant_level.rds"
)

fs::dir_create(output_folder)
fs::dir_create(figure_folder)


#===============================================================================
# 03 Helper functions
#===============================================================================

# Erste vorhandene Variable aus mehreren möglichen Variablennamen auswählen
first_existing <- function(
    data,
    candidates,
    required = TRUE,
    variable_description = "Variable"
) {
  
  found <- candidates[
    candidates %in% names(data)
  ]
  
  if (length(found) == 0) {
    
    if (required) {
      stop(
        variable_description,
        " wurde nicht gefunden. Erwartet wurde eine der folgenden Variablen: ",
        paste(
          candidates,
          collapse = ", "
        )
      )
    }
    
    return(NA_character_)
  }
  
  found[[1]]
}


# Optionale Spalte auslesen; falls nicht vorhanden, NA-Vektor zurückgeben
column_or_na <- function(data, variable) {
  
  if (
    length(variable) == 0 ||
    is.na(variable) ||
    !variable %in% names(data)
  ) {
    return(
      rep(
        NA,
        nrow(data)
      )
    )
  }
  
  data[[variable]]
}


# Textwerte bereinigen
clean_text <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_squish(x)
  
  x[
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


# Numerische Werte bereinigen
clean_numeric <- function(x) {
  
  x <- suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
  
  x[x == -1] <- NA_real_
  
  x
}


# Logische und binäre Werte vereinheitlichen
clean_binary <- function(x) {
  
  x_character <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(x)
    )
  )
  
  dplyr::case_when(
    x_character %in% c(
      "1",
      "true",
      "t",
      "yes",
      "ja"
    ) ~ TRUE,
    
    x_character %in% c(
      "0",
      "false",
      "f",
      "no",
      "nein"
    ) ~ FALSE,
    
    x_character %in% c(
      "-1",
      "",
      "na",
      "n/a",
      "null"
    ) ~ NA,
    
    TRUE ~ NA
  )
}


# Plattformcodes und Plattformnamen vereinheitlichen
normalize_platform <- function(x) {
  
  x_clean <- stringr::str_to_lower(
    clean_text(x)
  )
  
  dplyr::case_when(
    x_clean %in% c(
      "1",
      "facebook",
      "fb"
    ) ~ "Facebook",
    
    x_clean %in% c(
      "2",
      "instagram",
      "insta"
    ) ~ "Instagram",
    
    x_clean %in% c(
      "3",
      "tiktok",
      "tik tok"
    ) ~ "TikTok",
    
    x_clean %in% c(
      "4",
      "x",
      "twitter",
      "x (vormals twitter)"
    ) ~ "X",
    
    is.na(x_clean) ~ NA_character_,
    
    TRUE ~ "Sonstige/unklar"
  )
}


# Incidentality-Antworten labeln
normalize_incidentality <- function(x) {
  
  x_clean <- clean_numeric(x)
  
  dplyr::case_when(
    x_clean == 1 ~ "Gezielt gesucht",
    x_clean == 2 ~ "Account gefolgt, Beitrag nicht gezielt gesucht",
    x_clean == 3 ~ "Zufällig begegnet",
    is.na(x_clean) ~ NA_character_,
    TRUE ~ "Unklar"
  )
}


# Räumlichen Nutzungskontext labeln
normalize_locality <- function(x) {
  
  x_clean <- clean_numeric(x)
  
  dplyr::case_when(
    x_clean == 1 ~ "Zu Hause",
    x_clean == 2 ~ "Unterwegs",
    x_clean == 3 ~ "Weiß nicht mehr",
    is.na(x_clean) ~ NA_character_,
    TRUE ~ "Unklar"
  )
}


# Sozialen Nutzungskontext labeln
normalize_situation <- function(x) {
  
  x_clean <- clean_numeric(x)
  
  dplyr::case_when(
    x_clean == 1 ~ "Allein",
    x_clean == 2 ~ "Gemeinsam mit anderen",
    x_clean == 3 ~ "Weiß nicht mehr",
    is.na(x_clean) ~ NA_character_,
    TRUE ~ "Unklar"
  )
}


# Medienformat vereinheitlichen
normalize_format <- function(x) {
  
  x_original <- clean_text(x)
  
  x_clean <- stringr::str_to_lower(
    x_original
  )
  
  dplyr::case_when(
    x_clean %in% c(
      "text",
      "textbasiert",
      "text-based",
      "text based"
    ) ~ "Text",
    
    x_clean %in% c(
      "bild",
      "image",
      "foto",
      "photo",
      "bildbasiert",
      "image-based"
    ) ~ "Bild",
    
    x_clean %in% c(
      "video",
      "videobasiert",
      "video-based"
    ) ~ "Video",
    
    is.na(x_clean) ~ NA_character_,
    
    TRUE ~ x_original
  )
}


# Sicherer Mittelwert für logische Indikatoren
safe_proportion <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(
    x,
    na.rm = TRUE
  )
}


# Häufigkeitstabelle für kategoriale Variablen
frequency_summary <- function(
    data,
    variable,
    variable_label
) {
  
  values <- as.character(
    data[[variable]]
  )
  
  values <- tidyr::replace_na(
    values,
    "Missing"
  )
  
  tibble(
    Level = values
  ) %>%
    count(
      Level,
      name = "N"
    ) %>%
    mutate(
      Percent_Total = 100 * N / sum(N),
      Variable = variable_label,
      Variable_Name = variable
    ) %>%
    select(
      Variable,
      Variable_Name,
      Level,
      N,
      Percent_Total
    )
}


# Kreuztabelle mit Zeilenprozenten
cross_table <- function(
    data,
    row_variable,
    column_variable
) {
  
  data %>%
    filter(
      !is.na(.data[[row_variable]]),
      !is.na(.data[[column_variable]])
    ) %>%
    count(
      .data[[row_variable]],
      .data[[column_variable]],
      name = "N"
    ) %>%
    group_by(
      .data[[row_variable]]
    ) %>%
    mutate(
      Row_Percent = 100 * N / sum(N)
    ) %>%
    ungroup()
}


# Excel-Blatt formatiert anlegen
add_excel_sheet <- function(
    workbook,
    sheet_name,
    data,
    header_style
) {
  
  sheet_name <- stringr::str_sub(
    sheet_name,
    1,
    31
  )
  
  openxlsx::addWorksheet(
    workbook,
    sheetName = sheet_name
  )
  
  openxlsx::writeData(
    workbook,
    sheet = sheet_name,
    x = data,
    withFilter = TRUE
  )
  
  if (ncol(data) > 0) {
    
    openxlsx::addStyle(
      workbook,
      sheet = sheet_name,
      style = header_style,
      rows = 1,
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
    )
    
    openxlsx::freezePane(
      workbook,
      sheet = sheet_name,
      firstRow = TRUE
    )
    
    openxlsx::setColWidths(
      workbook,
      sheet = sheet_name,
      cols = seq_len(ncol(data)),
      widths = "auto"
    )
  }
}


#===============================================================================
# 04 Visual design
#===============================================================================

project_colors <- c(
  primary = "#315F6B",
  secondary = "#7D9DA3",
  accent = "#C49A5A",
  dark = "#26383F",
  medium = "#66777D",
  light = "#E8EFF1",
  grid = "#DCE4E6",
  white = "#FFFFFF",
  missing = "#B8C2C5"
)

platform_colors <- c(
  Facebook = "#315F6B",
  Instagram = "#4F7E82",
  TikTok = "#7D9DA3",
  X = "#65747B",
  `Sonstige/unklar` = "#B8C2C5"
)

incidentality_colors <- c(
  `Gezielt gesucht` = "#315F6B",
  `Account gefolgt, Beitrag nicht gezielt gesucht` = "#7D9DA3",
  `Zufällig begegnet` = "#C49A5A",
  Unklar = "#B8C2C5"
)

interaction_colors <- c(
  `Gründlich gelesen/angeschaut` = "#315F6B",
  `Weiter informiert` = "#7D9DA3",
  `Sichtbar interagiert` = "#C49A5A"
)

format_colors <- c(
  Text = "#315F6B",
  Bild = "#7D9DA3",
  Video = "#C49A5A"
)


theme_project <- function(base_size = 12) {
  
  theme_minimal(
    base_size = base_size
  ) +
    theme(
      plot.background = element_rect(
        fill = project_colors["white"],
        color = NA
      ),
      
      panel.background = element_rect(
        fill = project_colors["white"],
        color = NA
      ),
      
      plot.title = element_text(
        color = project_colors["dark"],
        face = "bold",
        size = rel(1.25),
        margin = margin(
          b = 5
        )
      ),
      
      plot.subtitle = element_text(
        color = project_colors["medium"],
        size = rel(0.95),
        margin = margin(
          b = 12
        )
      ),
      
      plot.caption = element_text(
        color = project_colors["medium"],
        size = rel(0.8),
        hjust = 0,
        margin = margin(
          t = 10
        )
      ),
      
      axis.title = element_text(
        color = project_colors["dark"],
        face = "bold"
      ),
      
      axis.text = element_text(
        color = project_colors["dark"]
      ),
      
      axis.ticks = element_blank(),
      
      panel.grid.major.x = element_blank(),
      
      panel.grid.major.y = element_line(
        color = project_colors["grid"],
        linewidth = 0.4
      ),
      
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_blank(),
      
      strip.background = element_rect(
        fill = project_colors["light"],
        color = NA
      ),
      
      strip.text = element_text(
        color = project_colors["dark"],
        face = "bold"
      ),
      
      plot.margin = margin(
        15,
        20,
        15,
        15
      )
    )
}


theme_set(
  theme_project()
)


percent_labels <- scales::label_number(
  accuracy = 1,
  suffix = " %",
  decimal.mark = ","
)


save_project_plot <- function(
    plot,
    filename,
    width = 7,
    height = 5
) {
  
  ggsave(
    filename = file.path(
      figure_folder,
      filename
    ),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = project_colors["white"]
  )
}


#===============================================================================
# 05 Load coding data
#===============================================================================

if (!file.exists(coding_file)) {
  stop(
    "Die Coding-Datei wurde nicht gefunden: ",
    coding_file
  )
}

coding_raw <- readxl::read_excel(
  coding_file
) %>%
  janitor::clean_names()


#===============================================================================
# 06 Identify relevant columns
#===============================================================================

participant_variable <- first_existing(
  coding_raw,
  c(
    "participant",
    "personal_participant_code",
    "personalparticipantcode"
  ),
  variable_description = "Participant Code"
)

study_day_variable <- first_existing(
  coding_raw,
  c(
    "study_day",
    "day",
    "diary_day"
  ),
  variable_description = "Studientag"
)

photo_variable <- first_existing(
  coding_raw,
  c(
    "photo",
    "photo_number",
    "screenshot_number"
  ),
  required = FALSE
)

filename_variable <- first_existing(
  coding_raw,
  c(
    "filename",
    "new_filename",
    "file_name"
  ),
  variable_description = "Dateiname"
)

filepath_variable <- first_existing(
  coding_raw,
  c(
    "filepath",
    "file_path",
    "path"
  ),
  required = FALSE
)


# Manuell codiertes Topic
topic_coded_variable <- first_existing(
  coding_raw,
  c(
    "topic_main",
    "topic_coded",
    "coded_topic",
    "topic"
  ),
  variable_description = "manuell codiertes Topic"
)


# Optionales Subtopic
topic_sub_variable <- first_existing(
  coding_raw,
  c(
    "topic_sub",
    "subtopic",
    "topic_secondary"
  ),
  required = FALSE
)


# Account-/Source-Kategorie
account_type_variable <- first_existing(
  coding_raw,
  c(
    "account_type",
    "source",
    "source_type",
    "account_category"
  ),
  required = FALSE
)


# Name des Accounts bzw. der Source
account_name_variable <- first_existing(
  coding_raw,
  c(
    "account_name",
    "source_name",
    "account_coded",
    "coded_account",
    "account"
  ),
  required = FALSE
)


# Manuell codierte oder geprüfte Plattform
platform_coded_variable <- first_existing(
  coding_raw,
  c(
    "platform_coded",
    "coded_platform",
    "platform_manual",
    "platform"
  ),
  variable_description = "codierte Plattform"
)


# Optionales Format
format_variable <- first_existing(
  coding_raw,
  c(
    "media_format",
    "format",
    "content_format"
  ),
  required = FALSE
)


# Ursprüngliche freiwillige Angaben der Teilnehmenden
topic_participant_variable <- first_existing(
  coding_raw,
  c(
    "topic_participant",
    "participant_topic"
  ),
  required = FALSE
)

account_participant_variable <- first_existing(
  coding_raw,
  c(
    "account_participant",
    "participant_account",
    "account"
  ),
  required = FALSE
)


# Workflow-Variablen
coding_completed_variable <- first_existing(
  coding_raw,
  c(
    "coding_completed",
    "coded",
    "coding_complete"
  ),
  required = FALSE
)

coding_date_variable <- first_existing(
  coding_raw,
  c(
    "coding_date",
    "date_coded"
  ),
  required = FALSE
)

coder_variable <- first_existing(
  coding_raw,
  c(
    "coder",
    "coded_by"
  ),
  required = FALSE
)

notes_variable <- first_existing(
  coding_raw,
  c(
    "notes",
    "coding_notes",
    "remarks"
  ),
  required = FALSE
)


# Automatische Daily-Variablen
incidentality_variable <- first_existing(
  coding_raw,
  c(
    "incidentality",
    "daily_incidentality"
  ),
  variable_description = "Incidentality"
)

interaction_read_variable <- first_existing(
  coding_raw,
  c(
    "interaction_read",
    "interaction_1"
  ),
  variable_description = "gründliche Rezeption"
)

interaction_research_variable <- first_existing(
  coding_raw,
  c(
    "interaction_research",
    "interaction_2"
  ),
  variable_description = "weiterführende Informationssuche"
)

interaction_engagement_variable <- first_existing(
  coding_raw,
  c(
    "interaction_engagement",
    "interaction_3"
  ),
  variable_description = "sichtbare Interaktion"
)

locality_variable <- first_existing(
  coding_raw,
  c(
    "locality",
    "daily_locality"
  ),
  variable_description = "Lokalität"
)

situation_variable <- first_existing(
  coding_raw,
  c(
    "situation",
    "daily_situation"
  ),
  variable_description = "soziale Situation"
)


#===============================================================================
# 07 Construct screenshot-level dataset
#===============================================================================

daily <- tibble(
  row_id = seq_len(
    nrow(coding_raw)
  ),
  
  participant = clean_text(
    column_or_na(
      coding_raw,
      participant_variable
    )
  ),
  
  study_day = clean_numeric(
    column_or_na(
      coding_raw,
      study_day_variable
    )
  ),
  
  photo = clean_numeric(
    column_or_na(
      coding_raw,
      photo_variable
    )
  ),
  
  filename = clean_text(
    column_or_na(
      coding_raw,
      filename_variable
    )
  ),
  
  filepath = clean_text(
    column_or_na(
      coding_raw,
      filepath_variable
    )
  ),
  
  topic_participant = clean_text(
    column_or_na(
      coding_raw,
      topic_participant_variable
    )
  ),
  
  account_participant = clean_text(
    column_or_na(
      coding_raw,
      account_participant_variable
    )
  ),
  
  topic_coded = clean_text(
    column_or_na(
      coding_raw,
      topic_coded_variable
    )
  ),
  
  topic_sub_coded = clean_text(
    column_or_na(
      coding_raw,
      topic_sub_variable
    )
  ),
  
  account_type_coded = clean_text(
    column_or_na(
      coding_raw,
      account_type_variable
    )
  ),
  
  account_name_coded = clean_text(
    column_or_na(
      coding_raw,
      account_name_variable
    )
  ),
  
  platform_coded = normalize_platform(
    column_or_na(
      coding_raw,
      platform_coded_variable
    )
  ),
  
  media_format = normalize_format(
    column_or_na(
      coding_raw,
      format_variable
    )
  ),
  
  incidentality = normalize_incidentality(
    column_or_na(
      coding_raw,
      incidentality_variable
    )
  ),
  
  interaction_read = clean_binary(
    column_or_na(
      coding_raw,
      interaction_read_variable
    )
  ),
  
  interaction_research = clean_binary(
    column_or_na(
      coding_raw,
      interaction_research_variable
    )
  ),
  
  interaction_engagement = clean_binary(
    column_or_na(
      coding_raw,
      interaction_engagement_variable
    )
  ),
  
  locality = normalize_locality(
    column_or_na(
      coding_raw,
      locality_variable
    )
  ),
  
  situation = normalize_situation(
    column_or_na(
      coding_raw,
      situation_variable
    )
  ),
  
  coding_completed = clean_binary(
    column_or_na(
      coding_raw,
      coding_completed_variable
    )
  ),
  
  coding_date = column_or_na(
    coding_raw,
    coding_date_variable
  ),
  
  coder = clean_text(
    column_or_na(
      coding_raw,
      coder_variable
    )
  ),
  
  notes = clean_text(
    column_or_na(
      coding_raw,
      notes_variable
    )
  )
)


#===============================================================================
# 08 File checks
#===============================================================================

daily <- daily %>%
  mutate(
    file_exists = case_when(
      is.na(filepath) ~ NA,
      TRUE ~ fs::file_exists(filepath)
    )
  )


#===============================================================================
# 09 Determine whether coding is complete
#===============================================================================

# Für Account genügt entweder eine Account-/Source-Kategorie oder ein Name.
daily <- daily %>%
  mutate(
    required_codes_complete =
      !is.na(topic_coded) &
      !is.na(platform_coded) &
      (
        !is.na(account_type_coded) |
          !is.na(account_name_coded)
      )
  )


# coding_completed wird nur verwendet, wenn mindestens eine Zeile ausdrücklich
# als abgeschlossen markiert wurde. Andernfalls erfolgt die Auswahl anhand der
# tatsächlich ausgefüllten Pflichtcodes.
use_completion_flag <- any(
  daily$coding_completed %in% TRUE,
  na.rm = TRUE
)

if (use_completion_flag) {
  
  daily <- daily %>%
    mutate(
      analysis_ready = coding_completed %in% TRUE
    )
  
  message(
    "Für die Analysestichprobe wird die Variable 'coding_completed' verwendet."
  )
  
} else {
  
  daily <- daily %>%
    mutate(
      analysis_ready = required_codes_complete
    )
  
  message(
    paste0(
      "Keine abgeschlossenen Coding-Flags gefunden. ",
      "Die Analysestichprobe wird anhand ausgefüllter Topic-, Account- ",
      "und Plattformcodes bestimmt."
    )
  )
}


#===============================================================================
# 10 Data-quality checks
#===============================================================================

duplicate_filenames <- daily %>%
  filter(
    !is.na(filename)
  ) %>%
  count(
    filename,
    name = "Number_of_Rows"
  ) %>%
  filter(
    Number_of_Rows > 1
  )


duplicate_screenshot_ids <- daily %>%
  filter(
    !is.na(participant),
    !is.na(study_day),
    !is.na(photo)
  ) %>%
  count(
    participant,
    study_day,
    photo,
    name = "Number_of_Rows"
  ) %>%
  filter(
    Number_of_Rows > 1
  )


incomplete_coding <- daily %>%
  filter(
    !required_codes_complete
  ) %>%
  select(
    participant,
    study_day,
    photo,
    filename,
    topic_coded,
    account_type_coded,
    account_name_coded,
    platform_coded,
    coding_completed
  )


missing_files <- daily %>%
  filter(
    file_exists %in% FALSE
  ) %>%
  select(
    participant,
    study_day,
    photo,
    filename,
    filepath
  )


coding_status_summary <- daily %>%
  summarise(
    N_Rows = n(),
    
    N_Analysis_Ready = sum(
      analysis_ready,
      na.rm = TRUE
    ),
    
    N_Not_Analysis_Ready = sum(
      !analysis_ready,
      na.rm = TRUE
    ),
    
    N_With_Complete_Required_Codes = sum(
      required_codes_complete,
      na.rm = TRUE
    ),
    
    N_Missing_Topic = sum(
      is.na(topic_coded)
    ),
    
    N_Missing_Account = sum(
      is.na(account_type_coded) &
        is.na(account_name_coded)
    ),
    
    N_Missing_Platform = sum(
      is.na(platform_coded)
    ),
    
    N_Missing_Format = sum(
      is.na(media_format)
    ),
    
    N_Missing_Files = sum(
      file_exists %in% FALSE,
      na.rm = TRUE
    )
  )


#===============================================================================
# 11 Select final coded screenshot dataset
#===============================================================================

daily_analysis <- daily %>%
  filter(
    analysis_ready
  ) %>%
  arrange(
    participant,
    study_day,
    photo
  )


if (nrow(daily_analysis) == 0) {
  stop(
    paste0(
      "Keine vollständig codierten Screenshots gefunden. ",
      "Bitte Coding-Datei und Coding-Spalten prüfen."
    )
  )
}


#===============================================================================
# 12 Overview
#===============================================================================

daily_overview <- tibble(
  Indicator = c(
    "Codierte Screenshots",
    "Teilnehmende mit codierten Screenshots",
    "Teilnehmertage mit mindestens einem Screenshot",
    "Mittlere Screenshots pro Person",
    "Median Screenshots pro Person",
    "Minimum Screenshots pro Person",
    "Maximum Screenshots pro Person"
  ),
  
  Value = c(
    nrow(daily_analysis),
    
    n_distinct(
      daily_analysis$participant
    ),
    
    n_distinct(
      paste(
        daily_analysis$participant,
        daily_analysis$study_day,
        sep = "_"
      )
    ),
    
    mean(
      as.numeric(
        table(
          daily_analysis$participant
        )
      )
    ),
    
    median(
      as.numeric(
        table(
          daily_analysis$participant
        )
      )
    ),
    
    min(
      as.numeric(
        table(
          daily_analysis$participant
        )
      )
    ),
    
    max(
      as.numeric(
        table(
          daily_analysis$participant
        )
      )
    )
  )
)


#===============================================================================
# 13 Screenshots per participant
#===============================================================================

screenshots_per_participant <- daily_analysis %>%
  group_by(
    participant
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    N_Study_Days = n_distinct(
      study_day
    ),
    
    First_Study_Day = min(
      study_day,
      na.rm = TRUE
    ),
    
    Last_Study_Day = max(
      study_day,
      na.rm = TRUE
    ),
    
    Mean_Screenshots_per_Active_Day =
      N_Screenshots / N_Study_Days,
    
    At_Least_Seven_Screenshots =
      N_Screenshots >= 7,
    
    .groups = "drop"
  )


#===============================================================================
# 14 Screenshots per study day
#===============================================================================

screenshots_per_day <- daily_analysis %>%
  group_by(
    study_day
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    N_Participants = n_distinct(
      participant
    ),
    
    Mean_Screenshots_per_Participant =
      N_Screenshots / N_Participants,
    
    .groups = "drop"
  )


#===============================================================================
# 15 Content coding: topics
#===============================================================================

topic_summary <- frequency_summary(
  daily_analysis,
  "topic_coded",
  "Topic"
)

topic_sub_summary <- frequency_summary(
  daily_analysis,
  "topic_sub_coded",
  "Subtopic"
)


#===============================================================================
# 16 Content coding: accounts and sources
#===============================================================================

account_type_summary <- frequency_summary(
  daily_analysis,
  "account_type_coded",
  "Account-/Source-Kategorie"
)

account_name_summary <- frequency_summary(
  daily_analysis,
  "account_name_coded",
  "Account-/Source-Name"
)


#===============================================================================
# 17 Content coding: platform and format
#===============================================================================

platform_summary <- frequency_summary(
  daily_analysis,
  "platform_coded",
  "Plattform"
)

format_summary <- frequency_summary(
  daily_analysis,
  "media_format",
  "Medienformat"
)


#===============================================================================
# 18 Incidentality
#===============================================================================

incidentality_summary <- frequency_summary(
  daily_analysis,
  "incidentality",
  "Incidentality"
)


daily_analysis <- daily_analysis %>%
  mutate(
    
    # Enge Definition:
    # Nur vollständig zufällige Begegnungen gelten als inzidentell.
    incidentality_strict =
      incidentality == "Zufällig begegnet",
    
    # Breite Definition:
    # Sowohl nicht gezielt erwartete Beiträge gefolgter Accounts als auch
    # vollständig zufällige Begegnungen gelten als inzidentell.
    incidentality_broad =
      incidentality %in% c(
        "Account gefolgt, Beitrag nicht gezielt gesucht",
        "Zufällig begegnet"
      )
  )


incidentality_binary_summary <- tibble(
  Definition = c(
    "Eng: nur zufällige Begegnung",
    "Breit: nicht gezielt oder zufällig"
  ),
  
  N_Valid = c(
    sum(
      !is.na(
        daily_analysis$incidentality_strict
      )
    ),
    
    sum(
      !is.na(
        daily_analysis$incidentality_broad
      )
    )
  ),
  
  N_Incidental = c(
    sum(
      daily_analysis$incidentality_strict,
      na.rm = TRUE
    ),
    
    sum(
      daily_analysis$incidentality_broad,
      na.rm = TRUE
    )
  )
) %>%
  mutate(
    Percent_Incidental =
      100 * N_Incidental / N_Valid
  )


#===============================================================================
# 19 Selection and engagement practices
#===============================================================================

interaction_long <- daily_analysis %>%
  select(
    participant,
    study_day,
    filename,
    `Gründlich gelesen/angeschaut` = interaction_read,
    `Weiter informiert` = interaction_research,
    `Sichtbar interagiert` = interaction_engagement
  ) %>%
  pivot_longer(
    cols = c(
      `Gründlich gelesen/angeschaut`,
      `Weiter informiert`,
      `Sichtbar interagiert`
    ),
    names_to = "Interaction",
    values_to = "Selected"
  )


interaction_summary <- interaction_long %>%
  group_by(
    Interaction
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(Selected)
    ),
    
    N_Selected = sum(
      Selected,
      na.rm = TRUE
    ),
    
    Percent_Selected =
      100 * N_Selected / N_Valid,
    
    .groups = "drop"
  )


daily_analysis <- daily_analysis %>%
  mutate(
    number_of_practices = rowSums(
      cbind(
        interaction_read,
        interaction_research,
        interaction_engagement
      ),
      na.rm = TRUE
    ),
    
    all_interactions_missing =
      is.na(interaction_read) &
      is.na(interaction_research) &
      is.na(interaction_engagement),
    
    number_of_practices = if_else(
      all_interactions_missing,
      NA_real_,
      number_of_practices
    )
  )


practice_count_summary <- frequency_summary(
  daily_analysis,
  "number_of_practices",
  "Anzahl ausgewählter Praktiken"
)


interaction_combinations <- daily_analysis %>%
  mutate(
    Interaction_Combination = case_when(
      
      interaction_read %in% FALSE &
        interaction_research %in% FALSE &
        interaction_engagement %in% FALSE ~
        "Keine der drei Praktiken",
      
      interaction_read %in% TRUE &
        interaction_research %in% FALSE &
        interaction_engagement %in% FALSE ~
        "Nur gründlich rezipiert",
      
      interaction_read %in% FALSE &
        interaction_research %in% TRUE &
        interaction_engagement %in% FALSE ~
        "Nur weiter informiert",
      
      interaction_read %in% FALSE &
        interaction_research %in% FALSE &
        interaction_engagement %in% TRUE ~
        "Nur sichtbar interagiert",
      
      interaction_read %in% TRUE &
        interaction_research %in% TRUE &
        interaction_engagement %in% FALSE ~
        "Gründlich rezipiert und weiter informiert",
      
      interaction_read %in% TRUE &
        interaction_research %in% FALSE &
        interaction_engagement %in% TRUE ~
        "Gründlich rezipiert und sichtbar interagiert",
      
      interaction_read %in% FALSE &
        interaction_research %in% TRUE &
        interaction_engagement %in% TRUE ~
        "Weiter informiert und sichtbar interagiert",
      
      interaction_read %in% TRUE &
        interaction_research %in% TRUE &
        interaction_engagement %in% TRUE ~
        "Alle drei Praktiken",
      
      TRUE ~ NA_character_
    )
  ) %>%
  frequency_summary(
    "Interaction_Combination",
    "Kombination der Praktiken"
  )


#===============================================================================
# 20 Situational contexts
#===============================================================================

locality_summary <- frequency_summary(
  daily_analysis,
  "locality",
  "Räumlicher Kontext"
)

situation_summary <- frequency_summary(
  daily_analysis,
  "situation",
  "Sozialer Kontext"
)


#===============================================================================
# 21 Participant-level aggregation
#===============================================================================

daily_participant <- daily_analysis %>%
  group_by(
    participant
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    N_Study_Days = n_distinct(
      study_day
    ),
    
    Mean_Screenshots_per_Active_Day =
      N_Screenshots / N_Study_Days,
    
    At_Least_Seven_Screenshots =
      N_Screenshots >= 7,
    
    Proportion_Incidental_Strict =
      safe_proportion(
        incidentality_strict
      ),
    
    Proportion_Incidental_Broad =
      safe_proportion(
        incidentality_broad
      ),
    
    Proportion_Read_Thoroughly =
      safe_proportion(
        interaction_read
      ),
    
    Proportion_Researched_Further =
      safe_proportion(
        interaction_research
      ),
    
    Proportion_Engaged =
      safe_proportion(
        interaction_engagement
      ),
    
    Proportion_At_Home =
      safe_proportion(
        locality == "Zu Hause"
      ),
    
    Proportion_On_The_Go =
      safe_proportion(
        locality == "Unterwegs"
      ),
    
    Proportion_Alone =
      safe_proportion(
        situation == "Allein"
      ),
    
    Proportion_With_Others =
      safe_proportion(
        situation == "Gemeinsam mit anderen"
      ),
    
    Proportion_Facebook =
      safe_proportion(
        platform_coded == "Facebook"
      ),
    
    Proportion_Instagram =
      safe_proportion(
        platform_coded == "Instagram"
      ),
    
    Proportion_TikTok =
      safe_proportion(
        platform_coded == "TikTok"
      ),
    
    Proportion_X =
      safe_proportion(
        platform_coded == "X"
      ),
    
    Proportion_Text =
      safe_proportion(
        media_format == "Text"
      ),
    
    Proportion_Image =
      safe_proportion(
        media_format == "Bild"
      ),
    
    Proportion_Video =
      safe_proportion(
        media_format == "Video"
      ),
    
    .groups = "drop"
  )


#===============================================================================
# 22 Participant-level descriptive statistics
#===============================================================================

participant_level_summary <- daily_participant %>%
  select(
    where(is.numeric)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(
    Variable
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(Value)
    ),
    
    Mean = mean(
      Value,
      na.rm = TRUE
    ),
    
    SD = sd(
      Value,
      na.rm = TRUE
    ),
    
    Median = median(
      Value,
      na.rm = TRUE
    ),
    
    Minimum = min(
      Value,
      na.rm = TRUE
    ),
    
    Maximum = max(
      Value,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


#===============================================================================
# 23 Participant-level platform distribution
#===============================================================================

participant_platform <- daily_analysis %>%
  filter(
    !is.na(platform_coded)
  ) %>%
  count(
    participant,
    platform_coded,
    name = "N"
  ) %>%
  group_by(
    participant
  ) %>%
  mutate(
    Participant_Total = sum(N),
    Participant_Percent =
      100 * N / Participant_Total
  ) %>%
  ungroup()


#===============================================================================
# 24 Topic × platform
#===============================================================================

topic_by_platform <- cross_table(
  daily_analysis,
  "topic_coded",
  "platform_coded"
)


#===============================================================================
# 25 Account/source × platform
#===============================================================================

account_by_platform <- cross_table(
  daily_analysis,
  "account_type_coded",
  "platform_coded"
)


#===============================================================================
# 26 Format × platform
#===============================================================================

format_by_platform <- cross_table(
  daily_analysis,
  "media_format",
  "platform_coded"
)


#===============================================================================
# 27 Topic × incidentality
#===============================================================================

topic_by_incidentality <- cross_table(
  daily_analysis,
  "topic_coded",
  "incidentality"
)


#===============================================================================
# 28 Account/source × incidentality
#===============================================================================

account_by_incidentality <- cross_table(
  daily_analysis,
  "account_type_coded",
  "incidentality"
)


#===============================================================================
# 29 Format × incidentality
#===============================================================================

format_by_incidentality <- cross_table(
  daily_analysis,
  "media_format",
  "incidentality"
)


#===============================================================================
# 30 Topic × selection and engagement
#===============================================================================

topic_interactions <- daily_analysis %>%
  filter(
    !is.na(topic_coded)
  ) %>%
  group_by(
    topic_coded
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    N_Valid_Read = sum(
      !is.na(interaction_read)
    ),
    
    Percent_Read_Thoroughly =
      100 * safe_proportion(
        interaction_read
      ),
    
    N_Valid_Research = sum(
      !is.na(interaction_research)
    ),
    
    Percent_Researched_Further =
      100 * safe_proportion(
        interaction_research
      ),
    
    N_Valid_Engagement = sum(
      !is.na(interaction_engagement)
    ),
    
    Percent_Engaged =
      100 * safe_proportion(
        interaction_engagement
      ),
    
    .groups = "drop"
  )


#===============================================================================
# 31 Account/source × selection and engagement
#===============================================================================

account_interactions <- daily_analysis %>%
  filter(
    !is.na(account_type_coded)
  ) %>%
  group_by(
    account_type_coded
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    Percent_Read_Thoroughly =
      100 * safe_proportion(
        interaction_read
      ),
    
    Percent_Researched_Further =
      100 * safe_proportion(
        interaction_research
      ),
    
    Percent_Engaged =
      100 * safe_proportion(
        interaction_engagement
      ),
    
    .groups = "drop"
  )


#===============================================================================
# 32 Format × selection and engagement
#===============================================================================

format_interactions <- daily_analysis %>%
  filter(
    !is.na(media_format)
  ) %>%
  group_by(
    media_format
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    Percent_Read_Thoroughly =
      100 * safe_proportion(
        interaction_read
      ),
    
    Percent_Researched_Further =
      100 * safe_proportion(
        interaction_research
      ),
    
    Percent_Engaged =
      100 * safe_proportion(
        interaction_engagement
      ),
    
    .groups = "drop"
  )


#===============================================================================
# 33 Context × selection and engagement
#===============================================================================

locality_interactions <- daily_analysis %>%
  filter(
    !is.na(locality)
  ) %>%
  group_by(
    locality
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    Percent_Read_Thoroughly =
      100 * safe_proportion(
        interaction_read
      ),
    
    Percent_Researched_Further =
      100 * safe_proportion(
        interaction_research
      ),
    
    Percent_Engaged =
      100 * safe_proportion(
        interaction_engagement
      ),
    
    .groups = "drop"
  )


situation_interactions <- daily_analysis %>%
  filter(
    !is.na(situation)
  ) %>%
  group_by(
    situation
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    Percent_Read_Thoroughly =
      100 * safe_proportion(
        interaction_read
      ),
    
    Percent_Researched_Further =
      100 * safe_proportion(
        interaction_research
      ),
    
    Percent_Engaged =
      100 * safe_proportion(
        interaction_engagement
      ),
    
    .groups = "drop"
  )


#===============================================================================
# 34 Save analysis datasets
#===============================================================================

saveRDS(
  daily_analysis,
  screenshot_rds
)

saveRDS(
  daily_participant,
  participant_rds
)


#===============================================================================
# 35 Export Excel workbook
#===============================================================================

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)


add_excel_sheet(
  workbook,
  "Overview",
  daily_overview,
  header_style
)

add_excel_sheet(
  workbook,
  "Coding_Status",
  coding_status_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Incomplete_Coding",
  incomplete_coding,
  header_style
)

add_excel_sheet(
  workbook,
  "Duplicate_Filenames",
  duplicate_filenames,
  header_style
)

add_excel_sheet(
  workbook,
  "Duplicate_IDs",
  duplicate_screenshot_ids,
  header_style
)

add_excel_sheet(
  workbook,
  "Missing_Files",
  missing_files,
  header_style
)

add_excel_sheet(
  workbook,
  "Per_Participant",
  screenshots_per_participant,
  header_style
)

add_excel_sheet(
  workbook,
  "Per_Day",
  screenshots_per_day,
  header_style
)

add_excel_sheet(
  workbook,
  "Topics",
  topic_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Subtopics",
  topic_sub_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Account_Types",
  account_type_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Account_Names",
  account_name_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Platforms",
  platform_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Formats",
  format_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality",
  incidentality_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Binary",
  incidentality_binary_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions",
  interaction_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Interaction_Count",
  practice_count_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Interaction_Combinations",
  interaction_combinations,
  header_style
)

add_excel_sheet(
  workbook,
  "Locality",
  locality_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Situation",
  situation_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Participant_Level",
  daily_participant,
  header_style
)

add_excel_sheet(
  workbook,
  "Participant_Summary",
  participant_level_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Participant_Platform",
  participant_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_x_Platform",
  topic_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Account_x_Platform",
  account_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_x_Platform",
  format_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_x_Incidentality",
  topic_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Account_x_Incidentality",
  account_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_x_Incidentality",
  format_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_Interactions",
  topic_interactions,
  header_style
)

add_excel_sheet(
  workbook,
  "Account_Interactions",
  account_interactions,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_Interactions",
  format_interactions,
  header_style
)

add_excel_sheet(
  workbook,
  "Locality_Interactions",
  locality_interactions,
  header_style
)

add_excel_sheet(
  workbook,
  "Situation_Interactions",
  situation_interactions,
  header_style
)


openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = TRUE
)


#===============================================================================
# 36 Figure: Screenshots per participant
#===============================================================================

figure_screenshots_participant <- ggplot(
  screenshots_per_participant,
  aes(
    x = N_Screenshots
  )
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    fill = project_colors["primary"],
    color = project_colors["white"],
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 7,
    color = project_colors["accent"],
    linewidth = 0.9,
    linetype = "22"
  ) +
  labs(
    title = "Screenshots pro Person",
    subtitle = "Die gestrichelte Linie markiert die Mindestzahl von sieben Screenshots",
    x = "Anzahl codierter Screenshots",
    y = "Anzahl der Teilnehmenden"
  )


save_project_plot(
  figure_screenshots_participant,
  "Daily_Screenshots_per_Participant.png"
)


#===============================================================================
# 37 Figure: Screenshots per study day
#===============================================================================

figure_screenshots_day <- ggplot(
  screenshots_per_day,
  aes(
    x = factor(study_day),
    y = N_Screenshots
  )
) +
  geom_col(
    width = 0.65,
    fill = project_colors["primary"]
  ) +
  geom_text(
    aes(
      label = N_Screenshots
    ),
    vjust = -0.45,
    fontface = "bold",
    color = project_colors["dark"]
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +
  labs(
    title = "Screenshots nach Studientag",
    x = "Studientag",
    y = "Anzahl der Screenshots"
  )


save_project_plot(
  figure_screenshots_day,
  "Daily_Screenshots_per_Day.png"
)


#===============================================================================
# 38 Figure: Platform distribution
#===============================================================================

platform_plot_data <- daily_analysis %>%
  filter(
    !is.na(platform_coded)
  ) %>%
  count(
    platform_coded,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  )


figure_platform <- ggplot(
  platform_plot_data,
  aes(
    x = platform_coded,
    y = Percent,
    fill = platform_coded
  )
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = scales::number(
        Percent,
        accuracy = 0.1,
        decimal.mark = ",",
        suffix = " %"
      )
    ),
    vjust = -0.45,
    fontface = "bold",
    color = project_colors["dark"]
  ) +
  scale_fill_manual(
    values = platform_colors
  ) +
  scale_y_continuous(
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Plattformverteilung",
    subtitle = "Basis: vollständig codierte Screenshots",
    x = NULL,
    y = "Anteil der Screenshots"
  )


save_project_plot(
  figure_platform,
  "Daily_Platforms.png"
)


#===============================================================================
# 39 Figure: Top topics
#===============================================================================

top_topics_plot_data <- daily_analysis %>%
  filter(
    !is.na(topic_coded)
  ) %>%
  count(
    topic_coded,
    sort = TRUE,
    name = "N"
  ) %>%
  slice_head(
    n = 15
  ) %>%
  mutate(
    Percent = 100 * N / nrow(daily_analysis),
    topic_coded = forcats::fct_reorder(
      topic_coded,
      N
    )
  )


figure_topics <- ggplot(
  top_topics_plot_data,
  aes(
    x = topic_coded,
    y = Percent
  )
) +
  geom_col(
    width = 0.65,
    fill = project_colors["primary"]
  ) +
  geom_text(
    aes(
      label = scales::number(
        Percent,
        accuracy = 0.1,
        decimal.mark = ",",
        suffix = " %"
      )
    ),
    hjust = -0.15,
    fontface = "bold",
    color = project_colors["dark"]
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_y_continuous(
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.17
      )
    )
  ) +
  labs(
    title = "Häufigste Themen",
    subtitle = "Bis zu 15 häufigste codierte Topic-Kategorien",
    x = NULL,
    y = "Anteil aller Screenshots"
  )


save_project_plot(
  figure_topics,
  "Daily_Topics.png",
  width = 9,
  height = 7
)


#===============================================================================
# 40 Figure: Account/source categories
#===============================================================================

if (
  any(
    !is.na(
      daily_analysis$account_type_coded
    )
  )
) {
  
  account_plot_data <- daily_analysis %>%
    filter(
      !is.na(account_type_coded)
    ) %>%
    count(
      account_type_coded,
      sort = TRUE,
      name = "N"
    ) %>%
    slice_head(
      n = 15
    ) %>%
    mutate(
      Percent = 100 * N / sum(N),
      account_type_coded = forcats::fct_reorder(
        account_type_coded,
        N
      )
    )
  
  
  figure_accounts <- ggplot(
    account_plot_data,
    aes(
      x = account_type_coded,
      y = Percent
    )
  ) +
    geom_col(
      width = 0.65,
      fill = project_colors["secondary"]
    ) +
    geom_text(
      aes(
        label = scales::number(
          Percent,
          accuracy = 0.1,
          decimal.mark = ",",
          suffix = " %"
        )
      ),
      hjust = -0.15,
      fontface = "bold",
      color = project_colors["dark"]
    ) +
    coord_flip(
      clip = "off"
    ) +
    scale_y_continuous(
      labels = percent_labels,
      expand = expansion(
        mult = c(
          0,
          0.17
        )
      )
    ) +
    labs(
      title = "Account- und Source-Kategorien",
      subtitle = "Bis zu 15 häufigste codierte Kategorien",
      x = NULL,
      y = "Anteil der codierten Screenshots"
    )
  
  
  save_project_plot(
    figure_accounts,
    "Daily_Account_Types.png",
    width = 9,
    height = 7
  )
}


#===============================================================================
# 41 Figure: Formats
#===============================================================================

if (
  any(
    !is.na(
      daily_analysis$media_format
    )
  )
) {
  
  format_plot_data <- daily_analysis %>%
    filter(
      !is.na(media_format)
    ) %>%
    count(
      media_format,
      name = "N"
    ) %>%
    mutate(
      Percent = 100 * N / sum(N)
    )
  
  
  figure_formats <- ggplot(
    format_plot_data,
    aes(
      x = media_format,
      y = Percent,
      fill = media_format
    )
  ) +
    geom_col(
      width = 0.65
    ) +
    geom_text(
      aes(
        label = scales::number(
          Percent,
          accuracy = 0.1,
          decimal.mark = ",",
          suffix = " %"
        )
      ),
      vjust = -0.45,
      fontface = "bold",
      color = project_colors["dark"]
    ) +
    scale_fill_manual(
      values = format_colors,
      na.value = project_colors["missing"]
    ) +
    scale_y_continuous(
      labels = percent_labels,
      expand = expansion(
        mult = c(
          0,
          0.12
        )
      )
    ) +
    guides(
      fill = "none"
    ) +
    labs(
      title = "Medienformate",
      x = NULL,
      y = "Anteil der Screenshots"
    )
  
  
  save_project_plot(
    figure_formats,
    "Daily_Formats.png"
  )
}


#===============================================================================
# 42 Figure: Incidentality
#===============================================================================

incidentality_plot_data <- daily_analysis %>%
  filter(
    !is.na(incidentality)
  ) %>%
  count(
    incidentality,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 * N / sum(N),
    
    incidentality = factor(
      incidentality,
      levels = c(
        "Gezielt gesucht",
        "Account gefolgt, Beitrag nicht gezielt gesucht",
        "Zufällig begegnet",
        "Unklar"
      )
    )
  )


figure_incidentality <- ggplot(
  incidentality_plot_data,
  aes(
    x = incidentality,
    y = Percent,
    fill = incidentality
  )
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = scales::number(
        Percent,
        accuracy = 0.1,
        decimal.mark = ",",
        suffix = " %"
      )
    ),
    vjust = -0.45,
    fontface = "bold",
    color = project_colors["dark"]
  ) +
  scale_fill_manual(
    values = incidentality_colors
  ) +
  scale_y_continuous(
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.14
      )
    )
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Art der Informationsbegegnung",
    x = NULL,
    y = "Anteil der Screenshots"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    )
  )


save_project_plot(
  figure_incidentality,
  "Daily_Incidentality.png",
  width = 9
)


#===============================================================================
# 43 Figure: Selection and engagement practices
#===============================================================================

interaction_plot_data <- interaction_summary %>%
  mutate(
    Interaction = factor(
      Interaction,
      levels = c(
        "Gründlich gelesen/angeschaut",
        "Weiter informiert",
        "Sichtbar interagiert"
      )
    )
  )


figure_interactions <- ggplot(
  interaction_plot_data,
  aes(
    x = Interaction,
    y = Percent_Selected,
    fill = Interaction
  )
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = scales::number(
        Percent_Selected,
        accuracy = 0.1,
        decimal.mark = ",",
        suffix = " %"
      )
    ),
    vjust = -0.45,
    fontface = "bold",
    color = project_colors["dark"]
  ) +
  scale_fill_manual(
    values = interaction_colors
  ) +
  scale_y_continuous(
    limits = c(
      0,
      100
    ),
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.08
      )
    )
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Auswahl- und Engagementpraktiken",
    subtitle = "Mehrfachnennungen waren möglich",
    x = NULL,
    y = "Anteil der gültigen Antworten"
  )


save_project_plot(
  figure_interactions,
  "Daily_Interactions.png",
  width = 8
)


#===============================================================================
# 44 Figure: Situational contexts
#===============================================================================

context_plot_data <- daily_analysis %>%
  transmute(
    Räumlich = locality,
    Sozial = situation
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Context_Dimension",
    values_to = "Context"
  ) %>%
  filter(
    !is.na(Context)
  ) %>%
  count(
    Context_Dimension,
    Context,
    name = "N"
  ) %>%
  group_by(
    Context_Dimension
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  ungroup()


context_colors <- c(
  Räumlich = project_colors["primary"],
  Sozial = project_colors["accent"]
)


figure_contexts <- ggplot(
  context_plot_data,
  aes(
    x = forcats::fct_reorder(
      Context,
      Percent
    ),
    y = Percent,
    fill = Context_Dimension
  )
) +
  geom_col(
    width = 0.65
  ) +
  geom_text(
    aes(
      label = scales::number(
        Percent,
        accuracy = 0.1,
        decimal.mark = ",",
        suffix = " %"
      )
    ),
    hjust = -0.15,
    fontface = "bold",
    color = project_colors["dark"]
  ) +
  coord_flip(
    clip = "off"
  ) +
  facet_wrap(
    ~ Context_Dimension,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = context_colors
  ) +
  scale_y_continuous(
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.17
      )
    )
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Situative Nutzungskontexte",
    x = NULL,
    y = "Anteil der Screenshots"
  )


save_project_plot(
  figure_contexts,
  "Daily_Contexts.png",
  width = 10,
  height = 6
)


#===============================================================================
# 45 Console report
#===============================================================================

cat(
  "\n",
  "============================================================\n",
  "DAILY ANALYSIS COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat(
  "Rows in coding file: ",
  nrow(daily),
  "\n",
  sep = ""
)

cat(
  "Screenshots included in analysis: ",
  nrow(daily_analysis),
  "\n",
  sep = ""
)

cat(
  "Participants: ",
  n_distinct(
    daily_analysis$participant
  ),
  "\n",
  sep = ""
)

cat(
  "Participants with at least seven screenshots: ",
  sum(
    screenshots_per_participant$At_Least_Seven_Screenshots,
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Strict incidental exposure: ",
  round(
    100 * safe_proportion(
      daily_analysis$incidentality_strict
    ),
    1
  ),
  "%\n",
  sep = ""
)

cat(
  "Broad incidental exposure: ",
  round(
    100 * safe_proportion(
      daily_analysis$incidentality_broad
    ),
    1
  ),
  "%\n",
  sep = ""
)

cat(
  "\nExcel output:\n",
  output_excel,
  "\n",
  sep = ""
)

cat(
  "\nScreenshot-level RDS:\n",
  screenshot_rds,
  "\n",
  sep = ""
)

cat(
  "\nParticipant-level RDS:\n",
  participant_rds,
  "\n",
  sep = ""
)

cat(
  "\nFigures:\n",
  figure_folder,
  "\n",
  sep = ""
)

cat(
  "============================================================\n"
)