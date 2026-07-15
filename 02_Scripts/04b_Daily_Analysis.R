################################################################################
# Project: Tagebuchstudie
# File:    04b_Daily_Analysis.R
#
# Purpose:
#   Auswertung der vollständig ausgefüllten Coding-Datei und der darin
#   enthaltenen Angaben aus der Daily-Befragung.
#
# Analyseeinheit:
#   Eine Zeile entspricht einem vollständig codierten Screenshot.
#
# Primäre Inhalte:
#   - Topics
#   - Quellen-/Account-Kategorien
#   - konkrete Quellen-/Account-Namen
#   - Plattformen
#   - Medienformate
#   - Incidentality
#   - Selektions- und Engagementpraktiken
#   - räumliche und soziale Nutzungskontexte
#
# Auswertungsperspektiven:
#   1. Screenshot-gewichtet:
#      Jeder Screenshot geht gleich stark in die Auswertung ein.
#
#   2. Personen-gewichtet:
#      Zunächst werden Anteile pro Person berechnet. Anschließend werden diese
#      Personenanteile deskriptiv zusammengefasst. Personen mit sehr vielen
#      Screenshots dominieren dadurch nicht die Ergebnisse.
#
# Präregistriertes Einschlusskriterium:
#   Es werden nur Personen berücksichtigt, die mindestens sieben Screenshots
#   hochgeladen haben. Die Screenshots müssen nicht gleichmäßig über die sieben
#   Tage verteilt sein.
#
# Explorative Analysen:
#   - Topic, Quelle und Format nach Plattform
#   - Incidentality nach Plattform, Topic, Quelle und Format
#   - Interaktionspraktiken nach Content- und Nutzungseigenschaften
#   - individuelle Vielfalt von Topics, Quellen, Plattformen und Formaten
#   - optionale logistische Mehrebenenmodelle mit Random Intercept für Personen
#
# Input:
#   06_Coding/coding_sheet.xlsx
#
# Optionaler Input:
#   03_Output/screening_prepared.rds
#
# Output:
#   03_Output/Daily_Results.xlsx
#   03_Output/daily_screenshot_level.rds
#   03_Output/daily_participant_level.rds
#   03_Output/daily_exploratory_models.rds
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
  scales,
  lme4,
  broom.mixed
)


#===============================================================================
# 02 Settings
#===============================================================================

# Mindestzahl von Screenshots entsprechend der Präregistrierung
minimum_screenshots <- 7

# Bei TRUE bricht der Script ab, wenn Pflichtcodes fehlen oder das Coding nicht
# als abgeschlossen markiert wurde.
strict_coding_check <- TRUE

# Bei TRUE werden Personen auf die im aufbereiteten Screening-Datensatz
# enthaltenen Personen beschränkt, sofern die Datei existiert.
apply_screening_filter <- TRUE

# Nur Gruppen mit mindestens dieser Fallzahl werden in bestimmten explorativen
# Diagrammen angezeigt. Die vollständigen Tabellen werden dennoch exportiert.
minimum_group_n_for_figures <- 5

# Anzahl der Topics und konkreten Quellen, die in Ranglisten gezeigt werden
number_top_topics <- 15
number_top_sources <- 15

# Explorative Mehrebenenmodelle ausführen
run_exploratory_models <- TRUE


#===============================================================================
# 03 Paths
#===============================================================================

coding_file <- file.path(
  "06_Coding",
  "coding_sheet.xlsx"
)

screening_file <- file.path(
  "03_Output",
  "screening_prepared.rds"
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

model_rds <- file.path(
  output_folder,
  "daily_exploratory_models.rds"
)

fs::dir_create(output_folder)
fs::dir_create(figure_folder)


#===============================================================================
# 04 Helper functions: data cleaning
#===============================================================================

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


clean_numeric <- function(x) {
  
  x <- suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
  
  x[x == -1] <- NA_real_
  
  x
}


clean_binary <- function(x) {
  
  x_clean <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(x)
    )
  )
  
  dplyr::case_when(
    x_clean %in% c(
      "true",
      "t",
      "1",
      "yes",
      "ja",
      "ausgewählt"
    ) ~ TRUE,
    
    x_clean %in% c(
      "false",
      "f",
      "0",
      "no",
      "nein",
      "nicht ausgewählt"
    ) ~ FALSE,
    
    x_clean %in% c(
      "",
      "-1",
      "na",
      "n/a",
      "null",
      "keine angabe"
    ) ~ NA,
    
    TRUE ~ NA
  )
}


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
    
    TRUE ~ x
  )
}


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
    
    x_clean %in% c(
      "mischform",
      "gemischt",
      "mixed",
      "mischform/unklar"
    ) ~ "Mischform/unklar",
    
    is.na(x_clean) ~ NA_character_,
    
    TRUE ~ x_original
  )
}


normalize_incidentality <- function(
    label,
    code
) {
  
  code_clean <- clean_numeric(code)
  
  label_clean <- stringr::str_to_lower(
    clean_text(label)
  )
  
  dplyr::case_when(
    code_clean == 1 ~ "Gezielt gesucht",
    
    code_clean == 2 ~
      "Account gefolgt, Beitrag nicht gezielt gesucht",
    
    code_clean == 3 ~ "Zufällig begegnet",
    
    stringr::str_detect(
      label_clean,
      "gezielt"
    ) &
      !stringr::str_detect(
        label_clean,
        "nicht gezielt"
      ) ~ "Gezielt gesucht",
    
    stringr::str_detect(
      label_clean,
      "account gefolgt|folge dem account"
    ) ~ "Account gefolgt, Beitrag nicht gezielt gesucht",
    
    stringr::str_detect(
      label_clean,
      "zufällig"
    ) ~ "Zufällig begegnet",
    
    TRUE ~ NA_character_
  )
}


normalize_locality <- function(
    label,
    code
) {
  
  code_clean <- clean_numeric(code)
  
  label_clean <- stringr::str_to_lower(
    clean_text(label)
  )
  
  dplyr::case_when(
    code_clean == 1 ~ "Zu Hause",
    code_clean == 2 ~ "Unterwegs",
    code_clean == 3 ~ "Weiß nicht mehr",
    
    stringr::str_detect(
      label_clean,
      "zu hause"
    ) ~ "Zu Hause",
    
    stringr::str_detect(
      label_clean,
      "unterwegs"
    ) ~ "Unterwegs",
    
    stringr::str_detect(
      label_clean,
      "weiß nicht"
    ) ~ "Weiß nicht mehr",
    
    TRUE ~ NA_character_
  )
}


normalize_situation <- function(
    label,
    code
) {
  
  code_clean <- clean_numeric(code)
  
  label_clean <- stringr::str_to_lower(
    clean_text(label)
  )
  
  dplyr::case_when(
    code_clean == 1 ~ "Allein",
    code_clean == 2 ~ "Gemeinsam mit anderen",
    code_clean == 3 ~ "Weiß nicht mehr",
    
    stringr::str_detect(
      label_clean,
      "allein"
    ) ~ "Allein",
    
    stringr::str_detect(
      label_clean,
      "gemeinsam"
    ) ~ "Gemeinsam mit anderen",
    
    stringr::str_detect(
      label_clean,
      "weiß nicht"
    ) ~ "Weiß nicht mehr",
    
    TRUE ~ NA_character_
  )
}


#===============================================================================
# 05 Helper functions: descriptive analyses
#===============================================================================

safe_mean <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(
    x,
    na.rm = TRUE
  )
}


safe_sd <- function(x) {
  
  if (sum(!is.na(x)) <= 1) {
    return(NA_real_)
  }
  
  sd(
    x,
    na.rm = TRUE
  )
}


frequency_summary <- function(
    data,
    variable,
    variable_label
) {
  
  data %>%
    transmute(
      Level = as.character(
        .data[[variable]]
      )
    ) %>%
    mutate(
      Level = replace_na(
        Level,
        "Missing"
      )
    ) %>%
    count(
      Level,
      name = "N"
    ) %>%
    mutate(
      Percent = 100 * N / sum(N),
      Variable = variable_label,
      Variable_Name = variable
    ) %>%
    select(
      Variable,
      Variable_Name,
      Level,
      N,
      Percent
    )
}


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
      Row = .data[[row_variable]],
      Column = .data[[column_variable]],
      name = "N"
    ) %>%
    group_by(
      Row
    ) %>%
    mutate(
      Row_Total = sum(N),
      Row_Percent = 100 * N / Row_Total
    ) %>%
    ungroup() %>%
    group_by(
      Column
    ) %>%
    mutate(
      Column_Total = sum(N),
      Column_Percent = 100 * N / Column_Total
    ) %>%
    ungroup() %>%
    mutate(
      Total_Percent = 100 * N / sum(N),
      Row_Variable = row_variable,
      Column_Variable = column_variable,
      .before = 1
    )
}


participant_weighted_category <- function(
    data,
    variable,
    variable_label
) {
  
  participant_ids <- sort(
    unique(
      data$participant
    )
  )
  
  categories <- sort(
    unique(
      na.omit(
        as.character(
          data[[variable]]
        )
      )
    )
  )
  
  if (length(categories) == 0) {
    
    return(
      tibble(
        Variable = variable_label,
        Category = character(),
        N_Participants = integer(),
        Mean_Participant_Proportion = numeric(),
        SD_Participant_Proportion = numeric(),
        Median_Participant_Proportion = numeric(),
        Minimum_Participant_Proportion = numeric(),
        Maximum_Participant_Proportion = numeric()
      )
    )
  }
  
  profile <- data %>%
    transmute(
      participant,
      Category = as.character(
        .data[[variable]]
      )
    ) %>%
    filter(
      !is.na(Category)
    ) %>%
    count(
      participant,
      Category,
      name = "N"
    ) %>%
    tidyr::complete(
      participant = participant_ids,
      Category = categories,
      fill = list(
        N = 0
      )
    ) %>%
    group_by(
      participant
    ) %>%
    mutate(
      Participant_Total = sum(N),
      
      Proportion = if_else(
        Participant_Total > 0,
        N / Participant_Total,
        NA_real_
      )
    ) %>%
    ungroup()
  
  profile %>%
    group_by(
      Category
    ) %>%
    summarise(
      N_Participants = sum(
        !is.na(Proportion)
      ),
      
      Mean_Participant_Proportion =
        mean(
          Proportion,
          na.rm = TRUE
        ),
      
      SD_Participant_Proportion =
        sd(
          Proportion,
          na.rm = TRUE
        ),
      
      Median_Participant_Proportion =
        median(
          Proportion,
          na.rm = TRUE
        ),
      
      Minimum_Participant_Proportion =
        min(
          Proportion,
          na.rm = TRUE
        ),
      
      Maximum_Participant_Proportion =
        max(
          Proportion,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) %>%
    mutate(
      Variable = variable_label,
      .before = 1
    )
}


participant_weighted_binary <- function(
    data,
    variable,
    variable_label
) {
  
  person_proportions <- data %>%
    group_by(
      participant
    ) %>%
    summarise(
      N_Valid = sum(
        !is.na(
          .data[[variable]]
        )
      ),
      
      Proportion = safe_mean(
        .data[[variable]]
      ),
      
      .groups = "drop"
    )
  
  tibble(
    Variable = variable_label,
    
    N_Participants = sum(
      !is.na(
        person_proportions$Proportion
      )
    ),
    
    Mean_Participant_Proportion =
      mean(
        person_proportions$Proportion,
        na.rm = TRUE
      ),
    
    SD_Participant_Proportion =
      sd(
        person_proportions$Proportion,
        na.rm = TRUE
      ),
    
    Median_Participant_Proportion =
      median(
        person_proportions$Proportion,
        na.rm = TRUE
      ),
    
    Minimum_Participant_Proportion =
      min(
        person_proportions$Proportion,
        na.rm = TRUE
      ),
    
    Maximum_Participant_Proportion =
      max(
        person_proportions$Proportion,
        na.rm = TRUE
      )
  )
}


group_interaction_rates <- function(
    data,
    group_variable
) {
  
  data %>%
    transmute(
      Group = .data[[group_variable]],
      
      `Gründlich gelesen/angeschaut` =
        interaction_read,
      
      `Weiter informiert` =
        interaction_research,
      
      `Sichtbar interagiert` =
        interaction_engagement
    ) %>%
    filter(
      !is.na(Group)
    ) %>%
    pivot_longer(
      cols = c(
        `Gründlich gelesen/angeschaut`,
        `Weiter informiert`,
        `Sichtbar interagiert`
      ),
      names_to = "Practice",
      values_to = "Selected"
    ) %>%
    group_by(
      Group,
      Practice
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
    ) %>%
    mutate(
      Group_Variable = group_variable,
      .before = 1
    )
}


#===============================================================================
# 06 Helper functions: diversity
#===============================================================================

calculate_diversity <- function(x) {
  
  x <- x[
    !is.na(x)
  ]
  
  if (length(x) == 0) {
    
    return(
      tibble(
        Richness = NA_real_,
        Shannon = NA_real_,
        Evenness = NA_real_,
        Dominant_Category_Share = NA_real_
      )
    )
  }
  
  probabilities <- prop.table(
    table(x)
  )
  
  richness <- length(
    probabilities
  )
  
  shannon <- -sum(
    probabilities *
      log(probabilities)
  )
  
  evenness <- if (
    richness > 1
  ) {
    shannon / log(richness)
  } else {
    0
  }
  
  tibble(
    Richness = richness,
    Shannon = shannon,
    Evenness = evenness,
    Dominant_Category_Share = max(
      probabilities
    )
  )
}


diversity_by_participant <- function(
    data,
    variable,
    prefix
) {
  
  result <- data %>%
    group_by(
      participant
    ) %>%
    group_modify(
      ~ calculate_diversity(
        .x[[variable]]
      )
    ) %>%
    ungroup()
  
  names(result)[
    names(result) != "participant"
  ] <- paste0(
    prefix,
    "_",
    names(result)[
      names(result) != "participant"
    ]
  )
  
  result
}


#===============================================================================
# 07 Helper functions: quality checks
#===============================================================================

category_variant_check <- function(
    data,
    variable
) {
  
  data %>%
    transmute(
      Original = clean_text(
        .data[[variable]]
      )
    ) %>%
    filter(
      !is.na(Original)
    ) %>%
    mutate(
      Normalized = stringr::str_to_lower(
        stringr::str_squish(
          Original
        )
      )
    ) %>%
    group_by(
      Normalized
    ) %>%
    summarise(
      N = n(),
      
      Number_of_Variants = n_distinct(
        Original
      ),
      
      Variants = paste(
        sort(
          unique(Original)
        ),
        collapse = " | "
      ),
      
      .groups = "drop"
    ) %>%
    filter(
      Number_of_Variants > 1
    ) %>%
    arrange(
      desc(N)
    )
}


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
      cols = seq_len(
        ncol(data)
      ),
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
      cols = seq_len(
        ncol(data)
      ),
      widths = "auto"
    )
  }
}


#===============================================================================
# 08 Visual design
#===============================================================================

project_colors <- c(
  primary = "#315F6B",
  secondary = "#78999E",
  accent = "#C49A5A",
  accent_light = "#DEC79F",
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
  TikTok = "#78999E",
  X = "#65747B"
)


incidentality_colors <- c(
  `Gezielt gesucht` = "#315F6B",
  `Account gefolgt, Beitrag nicht gezielt gesucht` = "#78999E",
  `Zufällig begegnet` = "#C49A5A"
)


interaction_colors <- c(
  `Gründlich gelesen/angeschaut` = "#315F6B",
  `Weiter informiert` = "#78999E",
  `Sichtbar interagiert` = "#C49A5A"
)


format_colors <- c(
  Text = "#315F6B",
  Bild = "#78999E",
  Video = "#C49A5A",
  `Mischform/unklar` = "#B8C2C5"
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
      
      strip.background = element_rect(
        fill = project_colors["light"],
        color = NA
      ),
      
      strip.text = element_text(
        color = project_colors["dark"],
        face = "bold",
        margin = margin(
          7,
          7,
          7,
          7
        )
      ),
      
      legend.position = "bottom",
      
      legend.title = element_blank(),
      
      legend.text = element_text(
        color = project_colors["dark"]
      ),
      
      plot.margin = margin(
        15,
        22,
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
    width = 8,
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
# 09 Load coding data
#===============================================================================

if (!file.exists(coding_file)) {
  
  stop(
    "Die Coding-Datei wurde nicht gefunden: ",
    coding_file
  )
}


coding_raw <- readxl::read_excel(
  coding_file,
  sheet = "Coding"
) %>%
  janitor::clean_names()


#===============================================================================
# 10 Check required variables
#===============================================================================

required_variables <- c(
  "screenshot_id",
  "participant",
  "study_day",
  "photo",
  "filename",
  "topic_coded",
  "source_coded",
  "source_name_coded",
  "platform_coded",
  "media_format",
  "coding_completed",
  "incidentality_label",
  "interaction_read",
  "interaction_research",
  "interaction_engagement",
  "locality_label",
  "situation_label"
)

missing_variables <- setdiff(
  required_variables,
  names(coding_raw)
)

if (length(missing_variables) > 0) {
  
  stop(
    paste0(
      "Folgende Variablen fehlen in der Coding-Datei:\n- ",
      paste(
        missing_variables,
        collapse = "\n- "
      )
    )
  )
}


#===============================================================================
# 11 Prepare coding data
#===============================================================================

daily_all <- coding_raw %>%
  transmute(
    screenshot_id = clean_text(
      screenshot_id
    ),
    
    participant = clean_text(
      participant
    ),
    
    study_day = clean_numeric(
      study_day
    ),
    
    photo = clean_numeric(
      photo
    ),
    
    filename = clean_text(
      filename
    ),
    
    filepath = if (
      "filepath" %in% names(coding_raw)
    ) {
      clean_text(filepath)
    } else {
      NA_character_
    },
    
    file_exists = if (
      "file_exists" %in% names(coding_raw)
    ) {
      clean_binary(file_exists)
    } else {
      NA
    },
    
    topic = clean_text(
      topic_coded
    ),
    
    source_type = clean_text(
      source_coded
    ),
    
    source_name = clean_text(
      source_name_coded
    ),
    
    platform = normalize_platform(
      platform_coded
    ),
    
    platform_reported = if (
      "platform_reported" %in%
      names(coding_raw)
    ) {
      normalize_platform(
        platform_reported
      )
    } else {
      NA_character_
    },
    
    media_format = normalize_format(
      media_format
    ),
    
    incidentality = normalize_incidentality(
      label = incidentality_label,
      code = if (
        "incidentality_code" %in%
        names(coding_raw)
      ) {
        incidentality_code
      } else {
        NA
      }
    ),
    
    interaction_read = clean_binary(
      interaction_read
    ),
    
    interaction_research = clean_binary(
      interaction_research
    ),
    
    interaction_engagement = clean_binary(
      interaction_engagement
    ),
    
    locality = normalize_locality(
      label = locality_label,
      code = if (
        "locality_code" %in%
        names(coding_raw)
      ) {
        locality_code
      } else {
        NA
      }
    ),
    
    situation = normalize_situation(
      label = situation_label,
      code = if (
        "situation_code" %in%
        names(coding_raw)
      ) {
        situation_code
      } else {
        NA
      }
    ),
    
    coding_completed = clean_binary(
      coding_completed
    ),
    
    coder = if (
      "coder" %in% names(coding_raw)
    ) {
      clean_text(coder)
    } else {
      NA_character_
    },
    
    coding_date = if (
      "coding_date" %in% names(coding_raw)
    ) {
      coding_date
    } else {
      NA
    },
    
    notes = if (
      "notes" %in% names(coding_raw)
    ) {
      clean_text(notes)
    } else {
      NA_character_
    },
    
    scheduled = if (
      "scheduled" %in% names(coding_raw)
    ) {
      scheduled
    } else {
      NA
    }
  )


#===============================================================================
# 12 Coding quality checks
#===============================================================================

daily_all <- daily_all %>%
  mutate(
    required_coding_complete =
      !is.na(topic) &
      !is.na(source_type) &
      !is.na(source_name) &
      !is.na(platform) &
      !is.na(media_format),
    
    platform_mismatch =
      !is.na(platform) &
      !is.na(platform_reported) &
      platform != platform_reported
  )


duplicate_screenshot_ids <- daily_all %>%
  filter(
    !is.na(screenshot_id)
  ) %>%
  count(
    screenshot_id,
    name = "Number_of_Rows"
  ) %>%
  filter(
    Number_of_Rows > 1
  )


duplicate_filenames <- daily_all %>%
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


incomplete_coding <- daily_all %>%
  filter(
    !coding_completed |
      !required_coding_complete |
      is.na(coding_completed)
  )


platform_mismatches <- daily_all %>%
  filter(
    platform_mismatch
  ) %>%
  select(
    screenshot_id,
    participant,
    filename,
    platform_reported,
    platform
  )


topic_variants <- category_variant_check(
  daily_all,
  "topic"
)

source_type_variants <- category_variant_check(
  daily_all,
  "source_type"
)


coding_quality_summary <- tibble(
  Indicator = c(
    "Zeilen in Coding-Datei",
    "Eindeutige Screenshot-IDs",
    "Doppelte Screenshot-IDs",
    "Doppelte Dateinamen",
    "Coding als abgeschlossen markiert",
    "Pflichtcodes vollständig",
    "Unvollständige oder nicht abgeschlossene Zeilen",
    "Abweichungen zwischen berichteter und codierter Plattform",
    "Nicht gefundene Dateien"
  ),
  
  Value = c(
    nrow(daily_all),
    
    n_distinct(
      daily_all$screenshot_id
    ),
    
    nrow(
      duplicate_screenshot_ids
    ),
    
    nrow(
      duplicate_filenames
    ),
    
    sum(
      daily_all$coding_completed,
      na.rm = TRUE
    ),
    
    sum(
      daily_all$required_coding_complete,
      na.rm = TRUE
    ),
    
    nrow(
      incomplete_coding
    ),
    
    nrow(
      platform_mismatches
    ),
    
    sum(
      daily_all$file_exists %in% FALSE,
      na.rm = TRUE
    )
  )
)


if (
  strict_coding_check &&
  nrow(incomplete_coding) > 0
) {
  
  stop(
    paste0(
      nrow(incomplete_coding),
      " Zeilen sind nicht vollständig codiert oder nicht als abgeschlossen ",
      "markiert. Siehe Objekt 'incomplete_coding'."
    )
  )
}


if (nrow(duplicate_screenshot_ids) > 0) {
  
  stop(
    "Die Coding-Datei enthält doppelte Screenshot-IDs."
  )
}


#===============================================================================
# 13 Select completely coded rows
#===============================================================================

daily_coded <- daily_all %>%
  filter(
    coding_completed,
    required_coding_complete
  )


#===============================================================================
# 14 Apply optional screening filter
#===============================================================================

screening_filter_applied <- FALSE
participants_not_in_screening <- tibble()

if (
  apply_screening_filter &&
  file.exists(screening_file)
) {
  
  screening <- readRDS(
    screening_file
  ) %>%
    janitor::clean_names()
  
  screening_participant_variable <- c(
    "personal_participant_code",
    "personalparticipantcode",
    "participant"
  )
  
  screening_participant_variable <-
    screening_participant_variable[
      screening_participant_variable %in%
        names(screening)
    ][1]
  
  if (
    length(screening_participant_variable) == 1 &&
    !is.na(screening_participant_variable)
  ) {
    
    valid_screening_codes <- screening %>%
      transmute(
        participant = clean_text(
          .data[[
            screening_participant_variable
          ]]
        )
      ) %>%
      filter(
        !is.na(participant)
      ) %>%
      distinct(
        participant
      )
    
    participants_not_in_screening <- daily_coded %>%
      distinct(
        participant
      ) %>%
      anti_join(
        valid_screening_codes,
        by = "participant"
      )
    
    daily_coded <- daily_coded %>%
      semi_join(
        valid_screening_codes,
        by = "participant"
      )
    
    screening_filter_applied <- TRUE
  }
}


#===============================================================================
# 15 Apply preregistered screenshot criterion
#===============================================================================

participant_counts_before_filter <- daily_coded %>%
  count(
    participant,
    name = "N_Screenshots"
  ) %>%
  mutate(
    Included =
      N_Screenshots >=
      minimum_screenshots
  )


included_participants <- participant_counts_before_filter %>%
  filter(
    Included
  ) %>%
  pull(
    participant
  )


excluded_participants <- participant_counts_before_filter %>%
  filter(
    !Included
  )


daily <- daily_coded %>%
  filter(
    participant %in%
      included_participants
  )


if (nrow(daily) == 0) {
  
  stop(
    "Nach Anwendung der Einschlusskriterien verbleiben keine Screenshots."
  )
}


#===============================================================================
# 16 Create derived variables
#===============================================================================

daily <- daily %>%
  mutate(
    participant = factor(
      participant
    ),
    
    platform = factor(
      platform,
      levels = c(
        "Facebook",
        "Instagram",
        "TikTok",
        "X"
      )
    ),
    
    incidentality = factor(
      incidentality,
      levels = c(
        "Gezielt gesucht",
        "Account gefolgt, Beitrag nicht gezielt gesucht",
        "Zufällig begegnet"
      )
    ),
    
    locality = factor(
      locality,
      levels = c(
        "Zu Hause",
        "Unterwegs",
        "Weiß nicht mehr"
      )
    ),
    
    situation = factor(
      situation,
      levels = c(
        "Allein",
        "Gemeinsam mit anderen",
        "Weiß nicht mehr"
      )
    ),
    
    media_format = factor(
      media_format,
      levels = c(
        "Text",
        "Bild",
        "Video",
        "Mischform/unklar"
      )
    ),
    
    incidentality_strict = case_when(
      is.na(incidentality) ~ NA,
      incidentality ==
        "Zufällig begegnet" ~ TRUE,
      TRUE ~ FALSE
    ),
    
    incidentality_broad = case_when(
      is.na(incidentality) ~ NA,
      
      incidentality %in% c(
        "Account gefolgt, Beitrag nicht gezielt gesucht",
        "Zufällig begegnet"
      ) ~ TRUE,
      
      TRUE ~ FALSE
    ),
    
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


#===============================================================================
# 17 Sample and participation overview
#===============================================================================

screenshots_per_participant <- daily %>%
  group_by(
    participant
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    N_Active_Days = n_distinct(
      study_day
    ),
    
    Mean_Screenshots_per_Active_Day =
      N_Screenshots /
      N_Active_Days,
    
    First_Study_Day = min(
      study_day,
      na.rm = TRUE
    ),
    
    Last_Study_Day = max(
      study_day,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


screenshots_per_day <- daily %>%
  group_by(
    study_day
  ) %>%
  summarise(
    N_Screenshots = n(),
    
    N_Participants = n_distinct(
      participant
    ),
    
    Mean_Screenshots_per_Active_Participant =
      N_Screenshots /
      N_Participants,
    
    .groups = "drop"
  )


analysis_overview <- tibble(
  Indicator = c(
    "Screenshots vor Mindestmengenfilter",
    "Personen vor Mindestmengenfilter",
    "Ausgeschlossene Personen mit weniger als sieben Screenshots",
    "Screenshots in finaler Analysestichprobe",
    "Personen in finaler Analysestichprobe",
    "Teilnehmertage mit mindestens einem Screenshot",
    "Mittlere Screenshots pro Person",
    "Median Screenshots pro Person",
    "Mittlere aktive Tage pro Person",
    "Screening-Filter angewendet"
  ),
  
  Value = c(
    nrow(daily_coded),
    
    n_distinct(
      daily_coded$participant
    ),
    
    nrow(
      excluded_participants
    ),
    
    nrow(daily),
    
    n_distinct(
      daily$participant
    ),
    
    n_distinct(
      paste(
        daily$participant,
        daily$study_day,
        sep = "_"
      )
    ),
    
    mean(
      screenshots_per_participant$
        N_Screenshots
    ),
    
    median(
      screenshots_per_participant$
        N_Screenshots
    ),
    
    mean(
      screenshots_per_participant$
        N_Active_Days
    ),
    
    as.character(
      screening_filter_applied
    )
  )
)


#===============================================================================
# 18 Screenshot-weighted descriptive distributions
#===============================================================================

topic_summary <- frequency_summary(
  daily,
  "topic",
  "Topic"
)

source_type_summary <- frequency_summary(
  daily,
  "source_type",
  "Source-/Account-Kategorie"
)

source_name_summary <- frequency_summary(
  daily,
  "source_name",
  "Source-/Account-Name"
)

platform_summary <- frequency_summary(
  daily,
  "platform",
  "Plattform"
)

format_summary <- frequency_summary(
  daily,
  "media_format",
  "Medienformat"
)

incidentality_summary <- frequency_summary(
  daily,
  "incidentality",
  "Incidentality"
)

locality_summary <- frequency_summary(
  daily,
  "locality",
  "Räumlicher Kontext"
)

situation_summary <- frequency_summary(
  daily,
  "situation",
  "Sozialer Kontext"
)

practice_count_summary <- frequency_summary(
  daily,
  "number_of_practices",
  "Anzahl der Praktiken"
)


#===============================================================================
# 19 Screenshot-weighted interaction summaries
#===============================================================================

interaction_long <- daily %>%
  select(
    participant,
    screenshot_id,
    
    `Gründlich gelesen/angeschaut` =
      interaction_read,
    
    `Weiter informiert` =
      interaction_research,
    
    `Sichtbar interagiert` =
      interaction_engagement
  ) %>%
  pivot_longer(
    cols = c(
      `Gründlich gelesen/angeschaut`,
      `Weiter informiert`,
      `Sichtbar interagiert`
    ),
    names_to = "Practice",
    values_to = "Selected"
  )


interaction_summary <- interaction_long %>%
  group_by(
    Practice
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
      100 * N_Selected /
      N_Valid,
    
    .groups = "drop"
  )


incidentality_binary_summary <- tibble(
  Definition = c(
    "Eng: nur zufällig begegnet",
    "Breit: nicht gezielt gesucht oder zufällig begegnet"
  ),
  
  N_Valid = c(
    sum(
      !is.na(
        daily$incidentality_strict
      )
    ),
    
    sum(
      !is.na(
        daily$incidentality_broad
      )
    )
  ),
  
  N_Incidental = c(
    sum(
      daily$incidentality_strict,
      na.rm = TRUE
    ),
    
    sum(
      daily$incidentality_broad,
      na.rm = TRUE
    )
  )
) %>%
  mutate(
    Percent_Incidental =
      100 * N_Incidental /
      N_Valid
  )


#===============================================================================
# 20 Participant-weighted descriptive distributions
#===============================================================================

participant_weighted_topics <-
  participant_weighted_category(
    daily,
    "topic",
    "Topic"
  )

participant_weighted_sources <-
  participant_weighted_category(
    daily,
    "source_type",
    "Source-/Account-Kategorie"
  )

participant_weighted_platforms <-
  participant_weighted_category(
    daily,
    "platform",
    "Plattform"
  )

participant_weighted_formats <-
  participant_weighted_category(
    daily,
    "media_format",
    "Medienformat"
  )

participant_weighted_incidentality <-
  participant_weighted_category(
    daily,
    "incidentality",
    "Incidentality"
  )

participant_weighted_locality <-
  participant_weighted_category(
    daily,
    "locality",
    "Räumlicher Kontext"
  )

participant_weighted_situation <-
  participant_weighted_category(
    daily,
    "situation",
    "Sozialer Kontext"
  )


participant_weighted_practices <- bind_rows(
  participant_weighted_binary(
    daily,
    "interaction_read",
    "Gründlich gelesen/angeschaut"
  ),
  
  participant_weighted_binary(
    daily,
    "interaction_research",
    "Weiter informiert"
  ),
  
  participant_weighted_binary(
    daily,
    "interaction_engagement",
    "Sichtbar interagiert"
  ),
  
  participant_weighted_binary(
    daily,
    "incidentality_strict",
    "Incidentality, eng"
  ),
  
  participant_weighted_binary(
    daily,
    "incidentality_broad",
    "Incidentality, breit"
  )
)


#===============================================================================
# 21 Individual diversity measures
#===============================================================================

topic_diversity <- diversity_by_participant(
  daily,
  "topic",
  "Topic"
)

source_diversity <- diversity_by_participant(
  daily,
  "source_type",
  "Source"
)

platform_diversity <- diversity_by_participant(
  daily,
  "platform",
  "Platform"
)

format_diversity <- diversity_by_participant(
  daily,
  "media_format",
  "Format"
)


daily_participant <- screenshots_per_participant %>%
  left_join(
    topic_diversity,
    by = "participant"
  ) %>%
  left_join(
    source_diversity,
    by = "participant"
  ) %>%
  left_join(
    platform_diversity,
    by = "participant"
  ) %>%
  left_join(
    format_diversity,
    by = "participant"
  ) %>%
  left_join(
    daily %>%
      group_by(
        participant
      ) %>%
      summarise(
        Proportion_Incidental_Strict =
          safe_mean(
            incidentality_strict
          ),
        
        Proportion_Incidental_Broad =
          safe_mean(
            incidentality_broad
          ),
        
        Proportion_Read_Thoroughly =
          safe_mean(
            interaction_read
          ),
        
        Proportion_Researched_Further =
          safe_mean(
            interaction_research
          ),
        
        Proportion_Engaged =
          safe_mean(
            interaction_engagement
          ),
        
        Proportion_At_Home =
          safe_mean(
            locality == "Zu Hause"
          ),
        
        Proportion_On_The_Go =
          safe_mean(
            locality == "Unterwegs"
          ),
        
        Proportion_Alone =
          safe_mean(
            situation == "Allein"
          ),
        
        Proportion_With_Others =
          safe_mean(
            situation ==
              "Gemeinsam mit anderen"
          ),
        
        Proportion_Facebook =
          safe_mean(
            platform == "Facebook"
          ),
        
        Proportion_Instagram =
          safe_mean(
            platform == "Instagram"
          ),
        
        Proportion_TikTok =
          safe_mean(
            platform == "TikTok"
          ),
        
        Proportion_X =
          safe_mean(
            platform == "X"
          ),
        
        Proportion_Text =
          safe_mean(
            media_format == "Text"
          ),
        
        Proportion_Image =
          safe_mean(
            media_format == "Bild"
          ),
        
        Proportion_Video =
          safe_mean(
            media_format == "Video"
          ),
        
        .groups = "drop"
      ),
    by = "participant"
  )


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
# 22 Preregistered and exploratory cross tables
#===============================================================================

topic_by_platform <- cross_table(
  daily,
  "topic",
  "platform"
)

source_by_platform <- cross_table(
  daily,
  "source_type",
  "platform"
)

format_by_platform <- cross_table(
  daily,
  "media_format",
  "platform"
)

incidentality_by_platform <- cross_table(
  daily,
  "platform",
  "incidentality"
)

topic_by_incidentality <- cross_table(
  daily,
  "topic",
  "incidentality"
)

source_by_incidentality <- cross_table(
  daily,
  "source_type",
  "incidentality"
)

format_by_incidentality <- cross_table(
  daily,
  "media_format",
  "incidentality"
)

locality_by_incidentality <- cross_table(
  daily,
  "locality",
  "incidentality"
)

situation_by_incidentality <- cross_table(
  daily,
  "situation",
  "incidentality"
)


#===============================================================================
# 23 Interaction rates by content and context
#===============================================================================

interactions_by_topic <- group_interaction_rates(
  daily,
  "topic"
)

interactions_by_source <- group_interaction_rates(
  daily,
  "source_type"
)

interactions_by_platform <- group_interaction_rates(
  daily,
  "platform"
)

interactions_by_format <- group_interaction_rates(
  daily,
  "media_format"
)

interactions_by_incidentality <- group_interaction_rates(
  daily,
  "incidentality"
)

interactions_by_locality <- group_interaction_rates(
  daily,
  "locality"
)

interactions_by_situation <- group_interaction_rates(
  daily,
  "situation"
)


#===============================================================================
# 24 Optional exploratory multilevel models
#===============================================================================

prepare_model_data <- function(data) {
  
  data %>%
    mutate(
      incidentality_model = forcats::fct_relevel(
        incidentality,
        "Gezielt gesucht"
      ),
      
      locality_model = case_when(
        locality == "Zu Hause" ~ "Zu Hause",
        locality == "Unterwegs" ~ "Unterwegs",
        TRUE ~ NA_character_
      ),
      
      locality_model = factor(
        locality_model,
        levels = c(
          "Zu Hause",
          "Unterwegs"
        )
      ),
      
      situation_model = case_when(
        situation == "Allein" ~ "Allein",
        
        situation ==
          "Gemeinsam mit anderen" ~
          "Gemeinsam mit anderen",
        
        TRUE ~ NA_character_
      ),
      
      situation_model = factor(
        situation_model,
        levels = c(
          "Allein",
          "Gemeinsam mit anderen"
        )
      ),
      
      platform_model = forcats::fct_relevel(
        platform,
        "Facebook"
      ),
      
      format_model = forcats::fct_relevel(
        media_format,
        "Text"
      )
    )
}


fit_exploratory_model <- function(
    data,
    outcome
) {
  
  candidate_predictors <- c(
    "incidentality_model",
    "locality_model",
    "situation_model",
    "platform_model",
    "format_model"
  )
  
  valid_predictors <- candidate_predictors[
    purrr::map_lgl(
      candidate_predictors,
      function(variable) {
        
        dplyr::n_distinct(
          na.omit(
            data[[variable]]
          )
        ) >= 2
      }
    )
  ]
  
  required_model_variables <- c(
    outcome,
    "participant",
    valid_predictors
  )
  
  model_data <- data %>%
    select(
      all_of(
        required_model_variables
      )
    ) %>%
    drop_na()
  
  if (
    nrow(model_data) < 30 ||
    n_distinct(
      model_data[[outcome]]
    ) < 2 ||
    n_distinct(
      model_data$participant
    ) < 3 ||
    length(valid_predictors) == 0
  ) {
    
    return(
      list(
        model = NULL,
        
        results = tibble(
          Outcome = outcome,
          Term = NA_character_,
          Odds_Ratio = NA_real_,
          CI95_Lower = NA_real_,
          CI95_Upper = NA_real_,
          P_Value = NA_real_,
          N = nrow(model_data),
          Note =
            "Modell wegen unzureichender Daten oder Varianz nicht geschätzt."
        )
      )
    )
  }
  
  formula_text <- paste0(
    outcome,
    " ~ ",
    paste(
      valid_predictors,
      collapse = " + "
    ),
    " + (1 | participant)"
  )
  
  model <- tryCatch(
    lme4::glmer(
      formula = as.formula(
        formula_text
      ),
      
      data = model_data,
      
      family = binomial(),
      
      control = lme4::glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(
          maxfun = 200000
        )
      )
    ),
    
    error = function(e) {
      warning(
        "Modell für ",
        outcome,
        " konnte nicht geschätzt werden: ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  if (is.null(model)) {
    
    return(
      list(
        model = NULL,
        
        results = tibble(
          Outcome = outcome,
          Term = NA_character_,
          Odds_Ratio = NA_real_,
          CI95_Lower = NA_real_,
          CI95_Upper = NA_real_,
          P_Value = NA_real_,
          N = nrow(model_data),
          Note =
            "Modell konnte nicht geschätzt werden."
        )
      )
    )
  }
  
  results <- broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = TRUE,
    exponentiate = TRUE
  ) %>%
    transmute(
      Outcome = outcome,
      Term = term,
      Odds_Ratio = estimate,
      CI95_Lower = conf.low,
      CI95_Upper = conf.high,
      P_Value = p.value,
      N = nrow(model_data),
      Note = "Exploratives logistisches Mehrebenenmodell"
    )
  
  list(
    model = model,
    results = results
  )
}


exploratory_model_results <- tibble(
  Note = "Explorative Modelle wurden deaktiviert."
)

exploratory_models <- list()


if (run_exploratory_models) {
  
  model_data <- prepare_model_data(
    daily
  )
  
  model_read <- fit_exploratory_model(
    model_data,
    "interaction_read"
  )
  
  model_research <- fit_exploratory_model(
    model_data,
    "interaction_research"
  )
  
  model_engagement <- fit_exploratory_model(
    model_data,
    "interaction_engagement"
  )
  
  exploratory_models <- list(
    interaction_read =
      model_read$model,
    
    interaction_research =
      model_research$model,
    
    interaction_engagement =
      model_engagement$model
  )
  
  exploratory_model_results <- bind_rows(
    model_read$results,
    model_research$results,
    model_engagement$results
  )
  
  saveRDS(
    exploratory_models,
    model_rds
  )
}


#===============================================================================
# 25 Save prepared analysis datasets
#===============================================================================

saveRDS(
  daily,
  screenshot_rds
)

saveRDS(
  daily_participant,
  participant_rds
)


#===============================================================================
# 26 Export Excel workbook
#===============================================================================

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#315F6B",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)


add_excel_sheet(
  workbook,
  "Analysis_Overview",
  analysis_overview,
  header_style
)

add_excel_sheet(
  workbook,
  "Coding_Quality",
  coding_quality_summary,
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
  "Duplicate_IDs",
  duplicate_screenshot_ids,
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
  "Platform_Mismatches",
  platform_mismatches,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_Variants",
  topic_variants,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_Variants",
  source_type_variants,
  header_style
)

add_excel_sheet(
  workbook,
  "Excluded_Participants",
  excluded_participants,
  header_style
)

add_excel_sheet(
  workbook,
  "Not_In_Screening",
  participants_not_in_screening,
  header_style
)

add_excel_sheet(
  workbook,
  "Screenshots_per_Person",
  screenshots_per_participant,
  header_style
)

add_excel_sheet(
  workbook,
  "Screenshots_per_Day",
  screenshots_per_day,
  header_style
)

add_excel_sheet(
  workbook,
  "Topics_Screenshot",
  topic_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Sources_Screenshot",
  source_type_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_Names",
  source_name_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Platforms_Screenshot",
  platform_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Formats_Screenshot",
  format_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Screenshot",
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
  "Interactions_Screenshot",
  interaction_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Practice_Count",
  practice_count_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Locality_Screenshot",
  locality_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Situation_Screenshot",
  situation_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Topics_PersonWeighted",
  participant_weighted_topics,
  header_style
)

add_excel_sheet(
  workbook,
  "Sources_PersonWeighted",
  participant_weighted_sources,
  header_style
)

add_excel_sheet(
  workbook,
  "Platforms_PersonWeighted",
  participant_weighted_platforms,
  header_style
)

add_excel_sheet(
  workbook,
  "Formats_PersonWeighted",
  participant_weighted_formats,
  header_style
)

add_excel_sheet(
  workbook,
  "Incident_PersonWeighted",
  participant_weighted_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Contexts_PersonWeighted",
  bind_rows(
    participant_weighted_locality,
    participant_weighted_situation
  ),
  header_style
)

add_excel_sheet(
  workbook,
  "Practices_PersonWeighted",
  participant_weighted_practices,
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
  "Topic_x_Platform",
  topic_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_x_Platform",
  source_by_platform,
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
  "Incident_x_Platform",
  incidentality_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_x_Incident",
  topic_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_x_Incident",
  source_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_x_Incident",
  format_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Locality_x_Incident",
  locality_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Situation_x_Incident",
  situation_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions_by_Topic",
  interactions_by_topic,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions_by_Source",
  interactions_by_source,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions_by_Platform",
  interactions_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions_by_Format",
  interactions_by_format,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions_by_Incident",
  interactions_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions_by_Locality",
  interactions_by_locality,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions_by_Situation",
  interactions_by_situation,
  header_style
)

add_excel_sheet(
  workbook,
  "Exploratory_Models",
  exploratory_model_results,
  header_style
)


openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = TRUE
)


#===============================================================================
# 27 Figure: Screenshots per participant
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
    xintercept = minimum_screenshots,
    color = project_colors["accent"],
    linewidth = 0.9,
    linetype = "22"
  ) +
  labs(
    title = "Screenshots pro Person",
    subtitle = paste0(
      "Die gestrichelte Linie markiert das Einschlusskriterium von ",
      minimum_screenshots,
      " Screenshots."
    ),
    x = "Anzahl der Screenshots",
    y = "Anzahl der Teilnehmenden"
  )


save_project_plot(
  figure_screenshots_participant,
  "Daily_01_Screenshots_per_Person.png"
)


#===============================================================================
# 28 Figure: Screenshots per study day
#===============================================================================

figure_screenshots_day <- ggplot(
  screenshots_per_day,
  aes(
    x = factor(
      study_day
    ),
    y = N_Screenshots
  )
) +
  geom_col(
    width = 0.64,
    fill = project_colors["primary"]
  ) +
  geom_text(
    aes(
      label = N_Screenshots
    ),
    vjust = -0.45,
    color = project_colors["dark"],
    fontface = "bold"
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
    subtitle = "Absolute Anzahl der codierten Screenshots",
    x = "Studientag",
    y = "Anzahl der Screenshots"
  )


save_project_plot(
  figure_screenshots_day,
  "Daily_02_Screenshots_per_Day.png"
)


#===============================================================================
# 29 Figure: Platforms
#===============================================================================

platform_plot_data <- daily %>%
  count(
    platform,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  )


figure_platform <- ggplot(
  platform_plot_data,
  aes(
    x = platform,
    y = Percent,
    fill = platform
  )
) +
  geom_col(
    width = 0.64
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
    color = project_colors["dark"],
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = platform_colors
  ) +
  scale_y_continuous(
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.13
      )
    )
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Plattformverteilung",
    subtitle = "Screenshot-gewichtete Verteilung",
    x = NULL,
    y = "Anteil der Screenshots"
  )


save_project_plot(
  figure_platform,
  "Daily_03_Platforms.png"
)


#===============================================================================
# 30 Figure: Top topics
#===============================================================================

top_topics_plot_data <- daily %>%
  count(
    topic,
    sort = TRUE,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  slice_head(
    n = number_top_topics
  ) %>%
  mutate(
    topic = forcats::fct_reorder(
      topic,
      N
    )
  )


figure_topics <- ggplot(
  top_topics_plot_data,
  aes(
    x = topic,
    y = Percent
  )
) +
  geom_col(
    width = 0.64,
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
    color = project_colors["dark"],
    fontface = "bold"
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_y_continuous(
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.18
      )
    )
  ) +
  labs(
    title = "Häufigste Themen",
    subtitle = paste0(
      "Die ",
      number_top_topics,
      " häufigsten codierten Topic-Kategorien"
    ),
    x = NULL,
    y = "Anteil der Screenshots"
  )


save_project_plot(
  figure_topics,
  "Daily_04_Topics.png",
  width = 9,
  height = 7
)


#===============================================================================
# 31 Figure: Top source categories
#===============================================================================

top_sources_plot_data <- daily %>%
  count(
    source_type,
    sort = TRUE,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  slice_head(
    n = number_top_sources
  ) %>%
  mutate(
    source_type = forcats::fct_reorder(
      source_type,
      N
    )
  )


figure_sources <- ggplot(
  top_sources_plot_data,
  aes(
    x = source_type,
    y = Percent
  )
) +
  geom_col(
    width = 0.64,
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
    color = project_colors["dark"],
    fontface = "bold"
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_y_continuous(
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.18
      )
    )
  ) +
  labs(
    title = "Häufigste Quellen- und Accountkategorien",
    subtitle = paste0(
      "Die ",
      number_top_sources,
      " häufigsten codierten Kategorien"
    ),
    x = NULL,
    y = "Anteil der Screenshots"
  )


save_project_plot(
  figure_sources,
  "Daily_05_Source_Types.png",
  width = 9,
  height = 7
)


#===============================================================================
# 32 Figure: Formats
#===============================================================================

format_plot_data <- daily %>%
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
    width = 0.64
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
    color = project_colors["dark"],
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = format_colors
  ) +
  scale_y_continuous(
    labels = percent_labels,
    expand = expansion(
      mult = c(
        0,
        0.13
      )
    )
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Medienformate",
    subtitle = "Screenshot-gewichtete Verteilung",
    x = NULL,
    y = "Anteil der Screenshots"
  )


save_project_plot(
  figure_formats,
  "Daily_06_Formats.png"
)


#===============================================================================
# 33 Figure: Incidentality
#===============================================================================

incidentality_plot_data <- daily %>%
  count(
    incidentality,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
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
    width = 0.64
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
    color = project_colors["dark"],
    fontface = "bold"
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
    subtitle = "Screenshot-gewichtete Verteilung",
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
  "Daily_07_Incidentality.png",
  width = 9
)


#===============================================================================
# 34 Figure: Selection and engagement
#===============================================================================

figure_interactions <- ggplot(
  interaction_summary,
  aes(
    x = Practice,
    y = Percent_Selected,
    fill = Practice
  )
) +
  geom_col(
    width = 0.64
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
    color = project_colors["dark"],
    fontface = "bold"
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
    title = "Selektions- und Engagementpraktiken",
    subtitle = "Mehrfachnennungen waren möglich",
    x = NULL,
    y = "Anteil der gültigen Antworten"
  )


save_project_plot(
  figure_interactions,
  "Daily_08_Interactions.png",
  width = 8
)


#===============================================================================
# 35 Figure: Contexts
#===============================================================================

context_plot_data <- daily %>%
  transmute(
    Räumlich = as.character(
      locality
    ),
    
    Sozial = as.character(
      situation
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Context_Dimension",
    values_to = "Context"
  ) %>%
  filter(
    !is.na(Context),
    Context != "Weiß nicht mehr"
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
    width = 0.64
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
    color = project_colors["dark"],
    fontface = "bold"
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
        0.18
      )
    )
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Situative Nutzungskontexte",
    subtitle = "Antworten „Weiß nicht mehr“ sind nicht dargestellt",
    x = NULL,
    y = "Anteil der Screenshots"
  )


save_project_plot(
  figure_contexts,
  "Daily_09_Contexts.png",
  width = 10,
  height = 6
)


#===============================================================================
# 36 Figure: Incidentality by platform
#===============================================================================

incidentality_platform_plot_data <- daily %>%
  count(
    platform,
    incidentality,
    name = "N"
  ) %>%
  group_by(
    platform
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  ungroup()


figure_incidentality_platform <- ggplot(
  incidentality_platform_plot_data,
  aes(
    x = platform,
    y = Percent,
    fill = incidentality
  )
) +
  geom_col(
    width = 0.68
  ) +
  scale_fill_manual(
    values = incidentality_colors
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
        0.02
      )
    )
  ) +
  labs(
    title = "Informationsbegegnung nach Plattform",
    subtitle = "Prozentuale Verteilung innerhalb der jeweiligen Plattform",
    x = NULL,
    y = "Anteil innerhalb der Plattform"
  )


save_project_plot(
  figure_incidentality_platform,
  "Daily_10_Incidentality_by_Platform.png",
  width = 9,
  height = 6
)


#===============================================================================
# 37 Figure: Practices by incidentality
#===============================================================================

figure_interactions_incidentality <- ggplot(
  interactions_by_incidentality,
  aes(
    x = Group,
    y = Percent_Selected,
    fill = Practice
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.72
    ),
    width = 0.66
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
        0.04
      )
    )
  ) +
  labs(
    title = "Praktiken nach Art der Informationsbegegnung",
    subtitle = "Explorative, screenshot-gewichtete Gegenüberstellung",
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
  figure_interactions_incidentality,
  "Daily_11_Interactions_by_Incidentality.png",
  width = 10,
  height = 6
)


#===============================================================================
# 38 Figure: Format by platform
#===============================================================================

format_platform_plot_data <- daily %>%
  count(
    platform,
    media_format,
    name = "N"
  ) %>%
  group_by(
    platform
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  ungroup()


figure_format_platform <- ggplot(
  format_platform_plot_data,
  aes(
    x = platform,
    y = Percent,
    fill = media_format
  )
) +
  geom_col(
    width = 0.68
  ) +
  scale_fill_manual(
    values = format_colors
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
        0.02
      )
    )
  ) +
  labs(
    title = "Medienformate nach Plattform",
    subtitle = "Prozentuale Verteilung innerhalb der jeweiligen Plattform",
    x = NULL,
    y = "Anteil innerhalb der Plattform"
  )


save_project_plot(
  figure_format_platform,
  "Daily_12_Formats_by_Platform.png",
  width = 9,
  height = 6
)


#===============================================================================
# 39 Figure: Topic × platform heatmap
#===============================================================================

heatmap_topics <- daily %>%
  count(
    topic,
    platform,
    name = "N"
  ) %>%
  group_by(
    topic
  ) %>%
  mutate(
    Topic_Total = sum(N)
  ) %>%
  ungroup() %>%
  filter(
    Topic_Total >=
      minimum_group_n_for_figures
  ) %>%
  group_by(
    topic
  ) %>%
  mutate(
    Percent_within_Topic =
      100 * N / sum(N)
  ) %>%
  ungroup() %>%
  mutate(
    topic = forcats::fct_reorder(
      topic,
      Topic_Total
    )
  )


figure_topic_platform_heatmap <- ggplot(
  heatmap_topics,
  aes(
    x = platform,
    y = topic,
    fill = Percent_within_Topic
  )
) +
  geom_tile(
    color = project_colors["white"],
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = scales::number(
        Percent_within_Topic,
        accuracy = 1,
        decimal.mark = ",",
        suffix = " %"
      )
    ),
    size = 3
  ) +
  scale_fill_gradient(
    low = project_colors["light"],
    high = project_colors["primary"],
    labels = percent_labels
  ) +
  labs(
    title = "Themenverteilung über Plattformen",
    subtitle = paste0(
      "Nur Topics mit mindestens ",
      minimum_group_n_for_figures,
      " Screenshots; Prozentwerte innerhalb eines Topics"
    ),
    x = NULL,
    y = NULL,
    fill = "Anteil"
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )


save_project_plot(
  figure_topic_platform_heatmap,
  "Daily_13_Topic_Platform_Heatmap.png",
  width = 9,
  height = 8
)


#===============================================================================
# 40 Figure: Participant diversity
#===============================================================================

diversity_plot_data <- daily_participant %>%
  select(
    participant,
    Topic_Richness,
    Source_Richness,
    Platform_Richness,
    Format_Richness
  ) %>%
  pivot_longer(
    cols = -participant,
    names_to = "Dimension",
    values_to = "Richness"
  ) %>%
  mutate(
    Dimension = recode(
      Dimension,
      Topic_Richness = "Topics",
      Source_Richness = "Quellenkategorien",
      Platform_Richness = "Plattformen",
      Format_Richness = "Formate"
    )
  )


figure_diversity <- ggplot(
  diversity_plot_data,
  aes(
    x = Dimension,
    y = Richness
  )
) +
  geom_boxplot(
    width = 0.55,
    fill = project_colors["light"],
    color = project_colors["primary"],
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.10,
    alpha = 0.55,
    size = 1.8,
    color = project_colors["primary"]
  ) +
  labs(
    title = "Individuelle Vielfalt der hochgeladenen Inhalte",
    subtitle = "Anzahl unterschiedlicher Kategorien pro Person",
    x = NULL,
    y = "Anzahl unterschiedlicher Kategorien"
  )


save_project_plot(
  figure_diversity,
  "Daily_14_Participant_Diversity.png",
  width = 9,
  height = 6
)


#===============================================================================
# 41 Console report
#===============================================================================

cat(
  "\n",
  "============================================================\n",
  "DAILY ANALYSIS COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat(
  "Rows in coding sheet: ",
  nrow(coding_raw),
  "\n",
  sep = ""
)

cat(
  "Completely coded screenshots: ",
  nrow(daily_coded),
  "\n",
  sep = ""
)

cat(
  "Participants before minimum screenshot filter: ",
  n_distinct(
    daily_coded$participant
  ),
  "\n",
  sep = ""
)

cat(
  "Excluded participants with fewer than ",
  minimum_screenshots,
  " screenshots: ",
  nrow(excluded_participants),
  "\n",
  sep = ""
)

cat(
  "Final screenshots: ",
  nrow(daily),
  "\n",
  sep = ""
)

cat(
  "Final participants: ",
  n_distinct(
    daily$participant
  ),
  "\n",
  sep = ""
)

cat(
  "Strict incidental exposure: ",
  round(
    100 * safe_mean(
      daily$incidentality_strict
    ),
    1
  ),
  "%\n",
  sep = ""
)

cat(
  "Broad incidental exposure: ",
  round(
    100 * safe_mean(
      daily$incidentality_broad
    ),
    1
  ),
  "%\n",
  sep = ""
)

cat(
  "Platform mismatches: ",
  nrow(
    platform_mismatches
  ),
  "\n",
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

if (run_exploratory_models) {
  
  cat(
    "\nExploratory model objects:\n",
    model_rds,
    "\n",
    sep = ""
  )
}

cat(
  "\nFigures:\n",
  figure_folder,
  "\n",
  sep = ""
)

cat(
  "============================================================\n"
)