################################################################################
# Project: Tagebuchstudie
# File:    04c_Outro_analysis.R
#
# Purpose:
#   Aufbereitung und deskriptive Auswertung der Abschlussbefragung.
#
# Analyseschritte:
#   1. Outro-Daten automatisch im Ordner 01_Data identifizieren
#   2. Variablen und fehlende Werte aufbereiten
#   3. Analysestichprobe anhand der Daily-Daten bestimmen
#   4. Reaktivitätsitems richtungsbereinigen
#   5. Reaktivitäts- und Ease-of-Use-Index bilden
#   6. Hierarchisches Omega, Omega total und Cronbachs Alpha berechnen
#   7. Präregistrierte Prüfung eines möglichen Einzelausschlusses durchführen
#   8. Items und Indizes deskriptiv auswerten
#   9. Freitextantworten exportieren
#  10. Tabellen und Abbildungen speichern
#
# Richtung der Skalen:
#   Reactivity:
#     Höhere Werte = stärkere durch die Studie verursachte Reaktivität
#
#     Deshalb werden folgende Items invertiert:
#       Item 2: Inhalte sind gleich geblieben
#       Item 3: Nutzung ist gleich geblieben
#       Item 5: Uploads entsprechen der normalen Nutzung
#
#   Ease of Use:
#     Höhere Werte = höhere wahrgenommene Benutzerfreundlichkeit
#
# Input:
#   Eine RDS-Datei in 01_Data, deren Dateiname "outro", "abschluss"
#   oder "closing" enthält.
#
# Optionaler Input:
#   03_Output/daily_participant_level.rds
#
# Output:
#   03_Output/Outro_Results.xlsx
#   03_Output/outro_prepared.rds
#   03_Output/outro_reliability_objects.rds
#
# Figures:
#   04_Figures/Outro_*.png
################################################################################


#===============================================================================
# 01 Packages
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  psych,
  janitor,
  openxlsx,
  fs,
  scales
)


#===============================================================================
# 02 Settings
#===============================================================================

omega_cutoff <- 0.70

# Entspricht der Präregistrierung:
# Ein einzelnes Item darf ausgeschlossen werden, wenn hierdurch Omega
# von unter .70 auf mindestens .70 steigt.
apply_single_item_exclusion <- TRUE


#===============================================================================
# 03 Paths
#===============================================================================

data_folder <- "01_Data"
output_folder <- "03_Output"
figure_folder <- "04_Figures"

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

reliability_rds <- file.path(
  output_folder,
  "outro_reliability_objects.rds"
)

fs::dir_create(output_folder)
fs::dir_create(figure_folder)


#===============================================================================
# 04 Identify Outro data file
#===============================================================================

outro_files <- fs::dir_ls(
  path = data_folder,
  type = "file",
  regexp = "(?i)(outro|abschluss|closing).*\\.rds$"
)

if (length(outro_files) == 0) {
  
  stop(
    paste0(
      "Im Ordner 01_Data wurde keine Outro-RDS-Datei gefunden.\n",
      "Der Dateiname muss 'outro', 'abschluss' oder 'closing' enthalten."
    )
  )
}

if (length(outro_files) > 1) {
  
  stop(
    paste0(
      "Mehrere mögliche Outro-Dateien wurden gefunden:\n- ",
      paste(
        outro_files,
        collapse = "\n- "
      ),
      "\nBitte Dateinamen eindeutiger gestalten."
    )
  )
}

outro_file <- outro_files[[1]]


#===============================================================================
# 05 Helper functions
#===============================================================================

first_existing <- function(
    data,
    candidates,
    required = TRUE,
    description = "Variable"
) {
  
  found <- candidates[
    candidates %in% names(data)
  ]
  
  if (length(found) == 0) {
    
    if (required) {
      
      stop(
        description,
        " wurde nicht gefunden. Erwartet wurde eine der Variablen: ",
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


clean_numeric <- function(x) {
  
  x <- suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
  
  x[x == -1] <- NA_real_
  
  x
}


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
      "ja"
    ) ~ TRUE,
    
    x_clean %in% c(
      "false",
      "f",
      "0",
      "no",
      "nein"
    ) ~ FALSE,
    
    TRUE ~ NA
  )
}


complete_mean <- function(
    data,
    items
) {
  
  item_matrix <- as.matrix(
    data[
      ,
      items,
      drop = FALSE
    ]
  )
  
  n_answered <- rowSums(
    !is.na(item_matrix)
  )
  
  index <- rowMeans(
    item_matrix,
    na.rm = FALSE
  )
  
  index[
    n_answered < length(items)
  ] <- NA_real_
  
  index
}


descriptive_summary <- function(
    x,
    label
) {
  
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  n_valid <- sum(
    !is.na(x)
  )
  
  n_missing <- sum(
    is.na(x)
  )
  
  if (n_valid == 0) {
    
    return(
      tibble(
        Variable = label,
        N_Valid = 0L,
        N_Missing = n_missing,
        Mean = NA_real_,
        SD = NA_real_,
        Median = NA_real_,
        Minimum = NA_real_,
        Maximum = NA_real_,
        CI95_Lower = NA_real_,
        CI95_Upper = NA_real_
      )
    )
  }
  
  mean_value <- mean(
    x,
    na.rm = TRUE
  )
  
  sd_value <- if (
    n_valid > 1
  ) {
    sd(
      x,
      na.rm = TRUE
    )
  } else {
    NA_real_
  }
  
  if (
    n_valid > 1 &&
    !is.na(sd_value)
  ) {
    
    margin_error <- qt(
      0.975,
      df = n_valid - 1
    ) * sd_value / sqrt(n_valid)
    
    ci_lower <- mean_value - margin_error
    ci_upper <- mean_value + margin_error
    
  } else {
    
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }
  
  tibble(
    Variable = label,
    N_Valid = n_valid,
    N_Missing = n_missing,
    Mean = mean_value,
    SD = sd_value,
    Median = median(
      x,
      na.rm = TRUE
    ),
    Minimum = min(
      x,
      na.rm = TRUE
    ),
    Maximum = max(
      x,
      na.rm = TRUE
    ),
    CI95_Lower = ci_lower,
    CI95_Upper = ci_upper
  )
}


item_descriptives <- function(
    data,
    items,
    labels
) {
  
  purrr::map_dfr(
    items,
    function(item) {
      
      result <- descriptive_summary(
        data[[item]],
        labels[[item]]
      )
      
      result %>%
        mutate(
          Item = item,
          .before = 1
        )
    }
  )
}


item_distributions <- function(
    data,
    items,
    labels
) {
  
  purrr::map_dfr(
    items,
    function(item) {
      
      tibble(
        Response = data[[item]]
      ) %>%
        mutate(
          Response_Label = case_when(
            Response == 1 ~ "1 – Stimme überhaupt nicht zu",
            Response == 2 ~ "2",
            Response == 3 ~ "3",
            Response == 4 ~ "4",
            Response == 5 ~ "5 – Stimme voll und ganz zu",
            is.na(Response) ~ "Missing",
            TRUE ~ "Ungültiger Wert"
          )
        ) %>%
        count(
          Response,
          Response_Label,
          name = "N"
        ) %>%
        mutate(
          Percent = 100 * N / sum(N),
          Item = item,
          Item_Label = labels[[item]],
          .before = 1
        ) %>%
        arrange(
          is.na(Response),
          Response
        )
    }
  )
}


calculate_scale_reliability <- function(
    data,
    items,
    scale_name
) {
  
  item_data <- data %>%
    select(
      all_of(items)
    )
  
  complete_item_data <- item_data %>%
    filter(
      if_all(
        everything(),
        ~ !is.na(.x)
      )
    )
  
  n_complete <- nrow(
    complete_item_data
  )
  
  zero_variance_items <- names(
    complete_item_data
  )[
    purrr::map_lgl(
      complete_item_data,
      ~ dplyr::n_distinct(.x) <= 1
    )
  ]
  
  if (
    n_complete < 3 ||
    length(zero_variance_items) > 0
  ) {
    
    warning(
      scale_name,
      ": Reliabilität kann möglicherweise nicht stabil berechnet werden. ",
      "Vollständige Fälle: ",
      n_complete,
      "; Items ohne Varianz: ",
      paste(
        zero_variance_items,
        collapse = ", "
      )
    )
  }
  
  alpha_object <- tryCatch(
    psych::alpha(
      complete_item_data,
      check.keys = FALSE,
      warnings = FALSE
    ),
    error = function(e) {
      
      warning(
        scale_name,
        ": Alpha konnte nicht berechnet werden: ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  omega_object <- tryCatch(
    psych::omega(
      complete_item_data,
      nfactors = 1,
      plot = FALSE
    ),
    error = function(e) {
      
      warning(
        scale_name,
        ": Omega konnte nicht berechnet werden: ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  summary <- tibble(
    Scale = scale_name,
    Number_of_Items = length(items),
    N_Complete = n_complete,
    
    Cronbach_Alpha = if (
      is.null(alpha_object)
    ) {
      NA_real_
    } else {
      unname(
        alpha_object$total$raw_alpha
      )
    },
    
    Standardized_Alpha = if (
      is.null(alpha_object)
    ) {
      NA_real_
    } else {
      unname(
        alpha_object$total$std.alpha
      )
    },
    
    Omega_Hierarchical = if (
      is.null(omega_object)
    ) {
      NA_real_
    } else {
      unname(
        omega_object$omega_h
      )
    },
    
    Omega_Total = if (
      is.null(omega_object)
    ) {
      NA_real_
    } else {
      unname(
        omega_object$omega.tot
      )
    }
  )
  
  leave_one_out <- purrr::map_dfr(
    items,
    function(removed_item) {
      
      reduced_items <- setdiff(
        items,
        removed_item
      )
      
      reduced_data <- data %>%
        select(
          all_of(reduced_items)
        ) %>%
        filter(
          if_all(
            everything(),
            ~ !is.na(.x)
          )
        )
      
      reduced_alpha <- tryCatch(
        psych::alpha(
          reduced_data,
          check.keys = FALSE,
          warnings = FALSE
        ),
        error = function(e) NULL
      )
      
      reduced_omega <- tryCatch(
        psych::omega(
          reduced_data,
          nfactors = 1,
          plot = FALSE
        ),
        error = function(e) NULL
      )
      
      tibble(
        Scale = scale_name,
        Item_Removed = removed_item,
        Number_of_Items = length(reduced_items),
        N_Complete = nrow(reduced_data),
        
        Cronbach_Alpha = if (
          is.null(reduced_alpha)
        ) {
          NA_real_
        } else {
          unname(
            reduced_alpha$total$raw_alpha
          )
        },
        
        Omega_Hierarchical = if (
          is.null(reduced_omega)
        ) {
          NA_real_
        } else {
          unname(
            reduced_omega$omega_h
          )
        },
        
        Omega_Total = if (
          is.null(reduced_omega)
        ) {
          NA_real_
        } else {
          unname(
            reduced_omega$omega.tot
          )
        }
      )
    }
  )
  
  alpha_item_statistics <- if (
    is.null(alpha_object)
  ) {
    
    tibble(
      Scale = scale_name,
      Note = "Alpha konnte nicht berechnet werden."
    )
    
  } else {
    
    alpha_object$item.stats %>%
      as.data.frame() %>%
      tibble::rownames_to_column(
        "Item"
      ) %>%
      as_tibble() %>%
      mutate(
        Scale = scale_name,
        .before = 1
      )
  }
  
  alpha_if_deleted <- if (
    is.null(alpha_object)
  ) {
    
    tibble(
      Scale = scale_name,
      Note = "Alpha konnte nicht berechnet werden."
    )
    
  } else {
    
    alpha_object$alpha.drop %>%
      as.data.frame() %>%
      tibble::rownames_to_column(
        "Item_Removed"
      ) %>%
      as_tibble() %>%
      mutate(
        Scale = scale_name,
        .before = 1
      )
  }
  
  list(
    summary = summary,
    leave_one_out = leave_one_out,
    alpha_object = alpha_object,
    omega_object = omega_object,
    alpha_item_statistics = alpha_item_statistics,
    alpha_if_deleted = alpha_if_deleted
  )
}


select_scale_items <- function(
    reliability_result,
    original_items,
    cutoff = 0.70,
    allow_exclusion = TRUE
) {
  
  full_omega <- reliability_result$summary$
    Omega_Hierarchical[[1]]
  
  selected_items <- original_items
  excluded_item <- NA_character_
  decision <- "Alle Items beibehalten"
  
  if (
    allow_exclusion &&
    !is.na(full_omega) &&
    full_omega < cutoff
  ) {
    
    candidates <- reliability_result$leave_one_out %>%
      filter(
        !is.na(Omega_Hierarchical),
        Omega_Hierarchical >= cutoff
      ) %>%
      arrange(
        desc(Omega_Hierarchical),
        desc(Omega_Total)
      )
    
    if (nrow(candidates) > 0) {
      
      excluded_item <- candidates$
        Item_Removed[[1]]
      
      selected_items <- setdiff(
        original_items,
        excluded_item
      )
      
      decision <- paste0(
        "Item ausgeschlossen: ",
        excluded_item
      )
    }
  }
  
  tibble(
    Full_Omega_Hierarchical = full_omega,
    Cutoff = cutoff,
    Item_Excluded = excluded_item,
    Number_of_Final_Items = length(selected_items),
    Decision = decision,
    Selected_Items = paste(
      selected_items,
      collapse = "; "
    )
  ) %>%
    list(
      decision = .,
      selected_items = selected_items,
      excluded_item = excluded_item
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
# 06 Visual design
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

scale_colors <- c(
  Reactivity = project_colors["accent"],
  `Ease of Use` = project_colors["primary"]
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
      
      panel.grid.major.x = element_line(
        color = project_colors["grid"],
        linewidth = 0.4
      ),
      
      panel.grid.major.y = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_blank(),
      
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
# 07 Load Outro data
#===============================================================================

outro_raw <- readRDS(
  outro_file
) %>%
  janitor::clean_names()


#===============================================================================
# 08 Identify participant variable
#===============================================================================

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


#===============================================================================
# 09 Required variables
#===============================================================================

reactivity_raw_items <- paste0(
  "outro_reactivity_",
  1:5
)

ease_items <- paste0(
  "outro_ease_",
  1:8
)

required_variables <- c(
  participant_variable,
  reactivity_raw_items,
  ease_items,
  "outro_problems_free",
  "outro_suggestions_free"
)

missing_variables <- setdiff(
  required_variables,
  names(outro_raw)
)

if (length(missing_variables) > 0) {
  
  stop(
    paste0(
      "Folgende benötigte Outro-Variablen fehlen:\n- ",
      paste(
        missing_variables,
        collapse = "\n- "
      )
    )
  )
}


#===============================================================================
# 10 Prepare participant code and handle duplicate Outro rows
#===============================================================================

outro_raw <- outro_raw %>%
  mutate(
    participant = clean_text(
      .data[[participant_variable]]
    )
  )


duplicate_participants <- outro_raw %>%
  count(
    participant,
    name = "Number_of_Rows"
  ) %>%
  filter(
    !is.na(participant),
    Number_of_Rows > 1
  )


if (nrow(duplicate_participants) > 0) {
  
  if ("committed" %in% names(outro_raw)) {
    
    warning(
      nrow(duplicate_participants),
      " Participant Codes kommen mehrfach vor. ",
      "Je Person wird die zuletzt abgeschlossene Outro-Befragung verwendet."
    )
    
    outro_raw <- outro_raw %>%
      arrange(
        participant,
        desc(committed)
      ) %>%
      distinct(
        participant,
        .keep_all = TRUE
      )
    
  } else {
    
    stop(
      paste0(
        "Doppelte Participant Codes wurden gefunden, aber es existiert ",
        "keine Variable 'committed', anhand derer die neueste Befragung ",
        "bestimmt werden könnte."
      )
    )
  }
}


#===============================================================================
# 11 Prepare item values
#===============================================================================

outro <- outro_raw %>%
  mutate(
    across(
      all_of(
        c(
          reactivity_raw_items,
          ease_items
        )
      ),
      clean_numeric
    ),
    
    problems_free = clean_text(
      outro_problems_free
    ),
    
    suggestions_free = clean_text(
      outro_suggestions_free
    )
  )


#===============================================================================
# 12 Check expected response ranges
#===============================================================================

range_check <- purrr::map_dfr(
  c(
    reactivity_raw_items,
    ease_items
  ),
  function(item) {
    
    values <- outro[[item]]
    
    tibble(
      Item = item,
      
      Observed_Minimum = if (
        all(is.na(values))
      ) {
        NA_real_
      } else {
        min(
          values,
          na.rm = TRUE
        )
      },
      
      Observed_Maximum = if (
        all(is.na(values))
      ) {
        NA_real_
      } else {
        max(
          values,
          na.rm = TRUE
        )
      },
      
      N_Outside_1_to_5 = sum(
        values < 1 |
          values > 5,
        na.rm = TRUE
      )
    )
  }
)

if (any(range_check$N_Outside_1_to_5 > 0)) {
  
  warning(
    "Mindestens ein Outro-Item enthält Werte außerhalb des Bereichs 1 bis 5."
  )
}


#===============================================================================
# 13 Reverse-code Reactivity items
#===============================================================================

# Die Originalitems bleiben unverändert erhalten.
#
# Nach der Invertierung bedeuten höhere Werte für alle Items:
# stärkere Reaktivität durch die Studienteilnahme.

outro <- outro %>%
  mutate(
    outro_reactivity_2_reversed =
      6 - outro_reactivity_2,
    
    outro_reactivity_3_reversed =
      6 - outro_reactivity_3,
    
    outro_reactivity_5_reversed =
      6 - outro_reactivity_5
  )


reactivity_items <- c(
  "outro_reactivity_1",
  "outro_reactivity_2_reversed",
  "outro_reactivity_3_reversed",
  "outro_reactivity_4",
  "outro_reactivity_5_reversed"
)


#===============================================================================
# 14 Item labels
#===============================================================================

reactivity_labels <- c(
  outro_reactivity_1 =
    "Gezielt nach hochladbaren Beiträgen gesucht",
  
  outro_reactivity_2_reversed =
    "Wahrgenommene Veränderung der angezeigten Inhalte",
  
  outro_reactivity_3_reversed =
    "Veränderung der eigenen Social-Media-Nutzung",
  
  outro_reactivity_4 =
    "Mehr öffentlich relevante Inhalte angezeigt",
  
  outro_reactivity_5_reversed =
    "Uploads bilden normale Nutzung weniger gut ab"
)


ease_labels <- c(
  outro_ease_1 =
    "App ist benutzerfreundlich",
  
  outro_ease_2 =
    "Teilnahme erfordert wenige Schritte",
  
  outro_ease_3 =
    "Nutzung der App ist mühelos",
  
  outro_ease_4 =
    "Fehler lassen sich schnell beheben",
  
  outro_ease_5 =
    "App kann zuverlässig genutzt werden",
  
  outro_ease_6 =
    "Download und Installation waren einfach",
  
  outro_ease_7 =
    "Eingabe des Login-Codes war einfach",
  
  outro_ease_8 =
    "Orientierung in der App war einfach"
)


#===============================================================================
# 15 Determine completed Outro questionnaires
#===============================================================================

all_closed_items <- c(
  reactivity_raw_items,
  ease_items
)

outro <- outro %>%
  mutate(
    N_Closed_Items_Answered = rowSums(
      !is.na(
        across(
          all_of(all_closed_items)
        )
      )
    ),
    
    Outro_Complete =
      N_Closed_Items_Answered ==
      length(all_closed_items)
  )


#===============================================================================
# 16 Restrict sample using Daily completion criterion
#===============================================================================

daily_filter_applied <- FALSE

if (file.exists(daily_participant_file)) {
  
  daily_participant <- readRDS(
    daily_participant_file
  ) %>%
    janitor::clean_names()
  
  daily_participant_variable <- first_existing(
    daily_participant,
    c(
      "participant",
      "personal_participant_code",
      "personalparticipantcode"
    ),
    description = "Participant Code in den Daily-Daten"
  )
  
  daily_completion_variable <- first_existing(
    daily_participant,
    c(
      "at_least_seven_screenshots",
      "at_least_7_screenshots"
    ),
    description = "Daily-Abschlussindikator"
  )
  
  eligible_daily_codes <- daily_participant %>%
    mutate(
      participant = clean_text(
        .data[[daily_participant_variable]]
      ),
      
      eligible_daily = clean_binary(
        .data[[daily_completion_variable]]
      )
    ) %>%
    filter(
      eligible_daily %in% TRUE
    ) %>%
    pull(
      participant
    ) %>%
    unique()
  
  outro <- outro %>%
    mutate(
      Daily_Criterion_Met =
        participant %in%
        eligible_daily_codes
    )
  
  daily_filter_applied <- TRUE
  
} else {
  
  warning(
    paste0(
      "Die Datei '",
      daily_participant_file,
      "' wurde nicht gefunden. ",
      "Das Kriterium von mindestens sieben Screenshots wird daher ",
      "noch nicht angewendet."
    )
  )
  
  outro <- outro %>%
    mutate(
      Daily_Criterion_Met = NA
    )
}


#===============================================================================
# 17 Final analysis sample
#===============================================================================

if (daily_filter_applied) {
  
  outro_analysis <- outro %>%
    filter(
      Outro_Complete,
      Daily_Criterion_Met
    )
  
} else {
  
  outro_analysis <- outro %>%
    filter(
      Outro_Complete
    )
}


if (nrow(outro_analysis) == 0) {
  
  stop(
    paste0(
      "Nach Anwendung der Abschlusskriterien verbleiben keine Fälle. ",
      "Bitte Outro- und Daily-Daten prüfen."
    )
  )
}


#===============================================================================
# 18 Reliability analyses: full scales
#===============================================================================

reactivity_reliability <- calculate_scale_reliability(
  data = outro_analysis,
  items = reactivity_items,
  scale_name = "Reactivity"
)

ease_reliability <- calculate_scale_reliability(
  data = outro_analysis,
  items = ease_items,
  scale_name = "Ease of Use"
)


reliability_summary_full <- bind_rows(
  reactivity_reliability$summary,
  ease_reliability$summary
)

reliability_leave_one_out <- bind_rows(
  reactivity_reliability$leave_one_out,
  ease_reliability$leave_one_out
)


#===============================================================================
# 19 Apply preregistered item-selection rule
#===============================================================================

reactivity_selection <- select_scale_items(
  reliability_result = reactivity_reliability,
  original_items = reactivity_items,
  cutoff = omega_cutoff,
  allow_exclusion = apply_single_item_exclusion
)

ease_selection <- select_scale_items(
  reliability_result = ease_reliability,
  original_items = ease_items,
  cutoff = omega_cutoff,
  allow_exclusion = apply_single_item_exclusion
)


reactivity_final_items <-
  reactivity_selection$selected_items

ease_final_items <-
  ease_selection$selected_items


scale_decisions <- bind_rows(
  reactivity_selection$decision %>%
    mutate(
      Scale = "Reactivity",
      .before = 1
    ),
  
  ease_selection$decision %>%
    mutate(
      Scale = "Ease of Use",
      .before = 1
    )
)


#===============================================================================
# 20 Construct final scale indices
#===============================================================================

outro_analysis <- outro_analysis %>%
  mutate(
    reactivity_index = complete_mean(
      data = outro_analysis,
      items = reactivity_final_items
    ),
    
    ease_index = complete_mean(
      data = outro_analysis,
      items = ease_final_items
    )
  )


#===============================================================================
# 21 Recalculate reliability for final scales
#===============================================================================

reactivity_reliability_final <- calculate_scale_reliability(
  data = outro_analysis,
  items = reactivity_final_items,
  scale_name = "Reactivity – final"
)

ease_reliability_final <- calculate_scale_reliability(
  data = outro_analysis,
  items = ease_final_items,
  scale_name = "Ease of Use – final"
)


reliability_summary_final <- bind_rows(
  reactivity_reliability_final$summary,
  ease_reliability_final$summary
)


#===============================================================================
# 22 Sample overview
#===============================================================================

sample_overview <- tibble(
  Indicator = c(
    "Zeilen in ursprünglicher Outro-Datei",
    "Eindeutige Personen in ursprünglicher Outro-Datei",
    "Vollständig beantwortete Outro-Fragebögen",
    "Daily-Kriterium angewendet",
    "Personen in finaler Outro-Analysestichprobe",
    "Problembeschreibungen vorhanden",
    "Verbesserungsvorschläge vorhanden"
  ),
  
  Value = c(
    nrow(outro_raw),
    
    n_distinct(
      outro_raw$participant
    ),
    
    sum(
      outro$Outro_Complete,
      na.rm = TRUE
    ),
    
    as.character(
      daily_filter_applied
    ),
    
    n_distinct(
      outro_analysis$participant
    ),
    
    sum(
      !is.na(outro_analysis$problems_free)
    ),
    
    sum(
      !is.na(outro_analysis$suggestions_free)
    )
  )
)


#===============================================================================
# 23 Missing-data summary
#===============================================================================

missing_data_summary <- purrr::map_dfr(
  c(
    all_closed_items,
    "problems_free",
    "suggestions_free"
  ),
  function(variable) {
    
    tibble(
      Variable = variable,
      N_Total = nrow(outro),
      N_Valid = sum(
        !is.na(outro[[variable]])
      ),
      N_Missing = sum(
        is.na(outro[[variable]])
      ),
      Percent_Missing =
        100 * N_Missing / N_Total
    )
  }
)


#===============================================================================
# 24 Item descriptives
#===============================================================================

reactivity_item_descriptives <- item_descriptives(
  data = outro_analysis,
  items = reactivity_items,
  labels = reactivity_labels
)

ease_item_descriptives <- item_descriptives(
  data = outro_analysis,
  items = ease_items,
  labels = ease_labels
)


reactivity_item_distributions <- item_distributions(
  data = outro_analysis,
  items = reactivity_items,
  labels = reactivity_labels
)

ease_item_distributions <- item_distributions(
  data = outro_analysis,
  items = ease_items,
  labels = ease_labels
)


#===============================================================================
# 25 Scale descriptives
#===============================================================================

reactivity_index_summary <- descriptive_summary(
  outro_analysis$reactivity_index,
  "Reactivity-Index"
)

ease_index_summary <- descriptive_summary(
  outro_analysis$ease_index,
  "Ease-of-Use-Index"
)

scale_descriptives <- bind_rows(
  reactivity_index_summary,
  ease_index_summary
)


#===============================================================================
# 26 Open-text responses
#===============================================================================

problems_free <- outro_analysis %>%
  filter(
    !is.na(problems_free)
  ) %>%
  select(
    participant,
    problems_free
  ) %>%
  arrange(
    participant
  )


suggestions_free <- outro_analysis %>%
  filter(
    !is.na(suggestions_free)
  ) %>%
  select(
    participant,
    suggestions_free
  ) %>%
  arrange(
    participant
  )


open_text_summary <- tibble(
  Question = c(
    "Probleme oder Unsicherheiten bei der App-Nutzung",
    "Vorschläge zur Gestaltung der App"
  ),
  
  N_Responses = c(
    nrow(problems_free),
    nrow(suggestions_free)
  ),
  
  Percent_of_Analysis_Sample = c(
    100 * nrow(problems_free) /
      nrow(outro_analysis),
    
    100 * nrow(suggestions_free) /
      nrow(outro_analysis)
  )
)


#===============================================================================
# 27 Optional descriptive association between the indices
#===============================================================================

index_correlation <- if (
  sum(
    complete.cases(
      outro_analysis[
        ,
        c(
          "reactivity_index",
          "ease_index"
        )
      ]
    )
  ) >= 3
) {
  
  correlation_test <- cor.test(
    outro_analysis$reactivity_index,
    outro_analysis$ease_index,
    method = "spearman",
    exact = FALSE
  )
  
  tibble(
    Variables =
      "Reactivity-Index × Ease-of-Use-Index",
    
    Method = "Spearman",
    
    N = sum(
      complete.cases(
        outro_analysis[
          ,
          c(
            "reactivity_index",
            "ease_index"
          )
        ]
      )
    ),
    
    Correlation = unname(
      correlation_test$estimate
    ),
    
    P_Value = correlation_test$p.value,
    
    CI95_Lower = if (
      is.null(correlation_test$conf.int)
    ) {
      NA_real_
    } else {
      correlation_test$conf.int[[1]]
    },
    
    CI95_Upper = if (
      is.null(correlation_test$conf.int)
    ) {
      NA_real_
    } else {
      correlation_test$conf.int[[2]]
    },
    
    Analysis_Type = "Explorativ"
  )
  
} else {
  
  tibble(
    Variables =
      "Reactivity-Index × Ease-of-Use-Index",
    
    Method = "Spearman",
    
    N = sum(
      complete.cases(
        outro_analysis[
          ,
          c(
            "reactivity_index",
            "ease_index"
          )
        ]
      )
    ),
    
    Correlation = NA_real_,
    P_Value = NA_real_,
    CI95_Lower = NA_real_,
    CI95_Upper = NA_real_,
    Analysis_Type = "Explorativ"
  )
}


#===============================================================================
# 28 Save prepared data and reliability objects
#===============================================================================

saveRDS(
  outro_analysis,
  output_rds
)


saveRDS(
  list(
    reactivity_full =
      reactivity_reliability,
    
    ease_full =
      ease_reliability,
    
    reactivity_final =
      reactivity_reliability_final,
    
    ease_final =
      ease_reliability_final,
    
    scale_decisions =
      scale_decisions
  ),
  reliability_rds
)


#===============================================================================
# 29 Create Excel workbook
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
  "Sample_Overview",
  sample_overview,
  header_style
)

add_excel_sheet(
  workbook,
  "Duplicate_Codes",
  duplicate_participants,
  header_style
)

add_excel_sheet(
  workbook,
  "Range_Check",
  range_check,
  header_style
)

add_excel_sheet(
  workbook,
  "Missing_Data",
  missing_data_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Scale_Decisions",
  scale_decisions,
  header_style
)

add_excel_sheet(
  workbook,
  "Reliability_Full",
  reliability_summary_full,
  header_style
)

add_excel_sheet(
  workbook,
  "Reliability_Final",
  reliability_summary_final,
  header_style
)

add_excel_sheet(
  workbook,
  "Leave_One_Out",
  reliability_leave_one_out,
  header_style
)

add_excel_sheet(
  workbook,
  "React_Alpha_Items",
  reactivity_reliability$
    alpha_item_statistics,
  header_style
)

add_excel_sheet(
  workbook,
  "React_Alpha_Deleted",
  reactivity_reliability$
    alpha_if_deleted,
  header_style
)

add_excel_sheet(
  workbook,
  "Ease_Alpha_Items",
  ease_reliability$
    alpha_item_statistics,
  header_style
)

add_excel_sheet(
  workbook,
  "Ease_Alpha_Deleted",
  ease_reliability$
    alpha_if_deleted,
  header_style
)

add_excel_sheet(
  workbook,
  "Scale_Descriptives",
  scale_descriptives,
  header_style
)

add_excel_sheet(
  workbook,
  "Reactivity_Items",
  reactivity_item_descriptives,
  header_style
)

add_excel_sheet(
  workbook,
  "Reactivity_Distribution",
  reactivity_item_distributions,
  header_style
)

add_excel_sheet(
  workbook,
  "Ease_Items",
  ease_item_descriptives,
  header_style
)

add_excel_sheet(
  workbook,
  "Ease_Distribution",
  ease_item_distributions,
  header_style
)

add_excel_sheet(
  workbook,
  "Open_Text_Summary",
  open_text_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Problems_Free",
  problems_free,
  header_style
)

add_excel_sheet(
  workbook,
  "Suggestions_Free",
  suggestions_free,
  header_style
)

add_excel_sheet(
  workbook,
  "Index_Correlation",
  index_correlation,
  header_style
)


openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = TRUE
)


#===============================================================================
# 30 Figure: Reactivity items
#===============================================================================

reactivity_plot_data <- reactivity_item_descriptives %>%
  mutate(
    Variable = forcats::fct_reorder(
      Variable,
      Mean
    )
  )


figure_reactivity_items <- ggplot(
  reactivity_plot_data,
  aes(
    x = Mean,
    y = Variable
  )
) +
  geom_col(
    width = 0.62,
    fill = project_colors["accent"]
  ) +
  geom_errorbar(
    aes(
      xmin = CI95_Lower,
      xmax = CI95_Upper
    ),
    width = 0.18,
    color = project_colors["dark"],
    linewidth = 0.65
  ) +
  geom_point(
    color = project_colors["dark"],
    size = 2.3
  ) +
  scale_x_continuous(
    limits = c(
      1,
      5
    ),
    breaks = 1:5
  ) +
  labs(
    title = "Wahrgenommene Reaktivität",
    subtitle = "Mittelwerte und 95%-Konfidenzintervalle",
    x = "Zustimmung (1–5)",
    y = NULL,
    caption = paste0(
      "Alle Items sind so ausgerichtet, dass höhere Werte ",
      "stärkere Reaktivität anzeigen."
    )
  )


save_project_plot(
  figure_reactivity_items,
  "Outro_Reactivity_Items.png",
  width = 10,
  height = 6
)


#===============================================================================
# 31 Figure: Ease-of-use items
#===============================================================================

ease_plot_data <- ease_item_descriptives %>%
  mutate(
    Variable = forcats::fct_reorder(
      Variable,
      Mean
    )
  )


figure_ease_items <- ggplot(
  ease_plot_data,
  aes(
    x = Mean,
    y = Variable
  )
) +
  geom_col(
    width = 0.62,
    fill = project_colors["primary"]
  ) +
  geom_errorbar(
    aes(
      xmin = CI95_Lower,
      xmax = CI95_Upper
    ),
    width = 0.18,
    color = project_colors["dark"],
    linewidth = 0.65
  ) +
  geom_point(
    color = project_colors["white"],
    size = 2.3
  ) +
  scale_x_continuous(
    limits = c(
      1,
      5
    ),
    breaks = 1:5
  ) +
  labs(
    title = "Wahrgenommene Einfachheit der App-Nutzung",
    subtitle = "Mittelwerte und 95%-Konfidenzintervalle",
    x = "Zustimmung (1–5)",
    y = NULL,
    caption = "Höhere Werte stehen für eine höhere wahrgenommene Benutzerfreundlichkeit."
  )


save_project_plot(
  figure_ease_items,
  "Outro_Ease_Items.png",
  width = 10,
  height = 7
)


#===============================================================================
# 32 Figure: Reactivity index
#===============================================================================

figure_reactivity_index <- ggplot(
  outro_analysis,
  aes(
    x = reactivity_index
  )
) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 1,
    fill = project_colors["accent"],
    color = project_colors["white"],
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept =
      reactivity_index_summary$Mean,
    
    color = project_colors["dark"],
    linewidth = 0.9,
    linetype = "22"
  ) +
  scale_x_continuous(
    limits = c(
      1,
      5
    ),
    breaks = 1:5
  ) +
  labs(
    title = "Reaktivitätsindex",
    subtitle = paste0(
      "M = ",
      format(
        round(
          reactivity_index_summary$Mean,
          2
        ),
        decimal.mark = ","
      ),
      "; SD = ",
      format(
        round(
          reactivity_index_summary$SD,
          2
        ),
        decimal.mark = ","
      )
    ),
    x = "Reaktivität (1–5)",
    y = "Anzahl der Teilnehmenden",
    caption = "Höhere Werte stehen für eine stärkere Reaktivität durch die Studienteilnahme."
  )


save_project_plot(
  figure_reactivity_index,
  "Outro_Reactivity_Index.png"
)


#===============================================================================
# 33 Figure: Ease-of-use index
#===============================================================================

figure_ease_index <- ggplot(
  outro_analysis,
  aes(
    x = ease_index
  )
) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 1,
    fill = project_colors["primary"],
    color = project_colors["white"],
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept =
      ease_index_summary$Mean,
    
    color = project_colors["accent"],
    linewidth = 0.9,
    linetype = "22"
  ) +
  scale_x_continuous(
    limits = c(
      1,
      5
    ),
    breaks = 1:5
  ) +
  labs(
    title = "Ease-of-Use-Index",
    subtitle = paste0(
      "M = ",
      format(
        round(
          ease_index_summary$Mean,
          2
        ),
        decimal.mark = ","
      ),
      "; SD = ",
      format(
        round(
          ease_index_summary$SD,
          2
        ),
        decimal.mark = ","
      )
    ),
    x = "Wahrgenommene Einfachheit (1–5)",
    y = "Anzahl der Teilnehmenden",
    caption = "Höhere Werte stehen für eine höhere wahrgenommene Benutzerfreundlichkeit."
  )


save_project_plot(
  figure_ease_index,
  "Outro_Ease_Index.png"
)


#===============================================================================
# 34 Figure: Comparison of scale means
#===============================================================================

scale_plot_data <- scale_descriptives %>%
  mutate(
    Scale = recode(
      Variable,
      `Reactivity-Index` = "Reactivity",
      `Ease-of-Use-Index` = "Ease of Use"
    )
  )


figure_scale_comparison <- ggplot(
  scale_plot_data,
  aes(
    x = Scale,
    y = Mean,
    fill = Scale
  )
) +
  geom_col(
    width = 0.58
  ) +
  geom_errorbar(
    aes(
      ymin = CI95_Lower,
      ymax = CI95_Upper
    ),
    width = 0.15,
    color = project_colors["dark"],
    linewidth = 0.7
  ) +
  geom_point(
    color = project_colors["white"],
    size = 2.4
  ) +
  scale_fill_manual(
    values = scale_colors
  ) +
  scale_y_continuous(
    limits = c(
      1,
      5
    ),
    breaks = 1:5
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Bewertung der Studienteilnahme und App",
    subtitle = "Mittelwerte und 95%-Konfidenzintervalle",
    x = NULL,
    y = "Indexwert (1–5)"
  )


save_project_plot(
  figure_scale_comparison,
  "Outro_Scale_Comparison.png",
  width = 7,
  height = 5
)


#===============================================================================
# 35 Console report
#===============================================================================

cat(
  "\n",
  "============================================================\n",
  "OUTRO ANALYSIS COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat(
  "Outro file: ",
  outro_file,
  "\n",
  sep = ""
)

cat(
  "Rows in raw Outro data: ",
  nrow(outro_raw),
  "\n",
  sep = ""
)

cat(
  "Complete Outro questionnaires: ",
  sum(
    outro$Outro_Complete,
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Daily completion criterion applied: ",
  daily_filter_applied,
  "\n",
  sep = ""
)

cat(
  "Final analysis sample: ",
  nrow(outro_analysis),
  "\n",
  sep = ""
)

cat(
  "\nReactivity:\n",
  "  Items used: ",
  paste(
    reactivity_final_items,
    collapse = ", "
  ),
  "\n",
  "  Mean: ",
  round(
    reactivity_index_summary$Mean,
    2
  ),
  "\n",
  "  SD: ",
  round(
    reactivity_index_summary$SD,
    2
  ),
  "\n",
  "  Hierarchical omega: ",
  round(
    reliability_summary_final %>%
      filter(
        Scale == "Reactivity – final"
      ) %>%
      pull(
        Omega_Hierarchical
      ),
    3
  ),
  "\n",
  sep = ""
)

cat(
  "\nEase of Use:\n",
  "  Items used: ",
  paste(
    ease_final_items,
    collapse = ", "
  ),
  "\n",
  "  Mean: ",
  round(
    ease_index_summary$Mean,
    2
  ),
  "\n",
  "  SD: ",
  round(
    ease_index_summary$SD,
    2
  ),
  "\n",
  "  Hierarchical omega: ",
  round(
    reliability_summary_final %>%
      filter(
        Scale == "Ease of Use – final"
      ) %>%
      pull(
        Omega_Hierarchical
      ),
    3
  ),
  "\n",
  sep = ""
)

cat(
  "\nOpen responses:\n",
  "  Problems: ",
  nrow(problems_free),
  "\n",
  "  Suggestions: ",
  nrow(suggestions_free),
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
  "\nPrepared RDS:\n",
  output_rds,
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