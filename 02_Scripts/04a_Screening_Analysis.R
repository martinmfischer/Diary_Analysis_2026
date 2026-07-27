################################################################################
# Project: Tagebuchstudie
# File:    04a_Screening_Analysis.R
#
#
# Input:
#   01_Data/screening-befragung_tagebuchstudie.rds
#
# Output:
#   03_Output/Screening_Results.xlsx
#   03_Output/screening_prepared.rds
#   03_Output/screening_reliability_objects.rds
#
# Figures:
#   04_Figures/Screening_*.png
################################################################################

rm(list = ls())

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
# 02 Paths
#===============================================================================

helper_script <- file.path(
  "02_Scripts",
  "00_Helpers.R"
)

data_file <- file.path(
  "01_Data",
  "screening-befragung_tagebuchstudie.rds"
)

output_folder <- "03_Output"
figure_folder <- "04_Figures"

output_excel <- file.path(
  output_folder,
  "Screening_Results.xlsx"
)

output_rds <- file.path(
  output_folder,
  "screening_prepared.rds"
)

reliability_rds <- file.path(
  output_folder,
  "screening_reliability_objects.rds"
)

fs::dir_create(output_folder)
fs::dir_create(figure_folder)


#===============================================================================
# 03 Helper functions
#===============================================================================

source(helper_script)


#===============================================================================
# 04 Load data
#===============================================================================

if (!file.exists(data_file)) {
  stop(
    "Die Screening-Datei wurde nicht gefunden: ",
    data_file
  )
}

screening_raw <- readRDS(data_file)


#===============================================================================
# 05 Check required variables
#===============================================================================

required_variables <- c(
  "personalParticipantCode",
  "intro_stop_age",
  "intro_stop_usage",
  "intro_age_num",
  "intro_gender",
  "intro_education",
  "intro_freq_facebook",
  "intro_freq_instagram",
  "intro_freq_tiktok",
  "intro_freq_x",
  "intro_intensity",
  "intro_ib_undirected",
  "intro_ib_thematic",
  "intro_ib_social",
  "intro_ib_problem",
  "intro_incidentality_1",
  "intro_incidentality_2",
  "intro_incidentality_3",
  "intro_incidentality_4",
  "intro_incidentality_5",
  "intro_incidentality_6",
  "intro_context_local",
  "intro_context_situation"
)

missing_variables <- setdiff(
  required_variables,
  names(screening_raw)
)

if (length(missing_variables) > 0) {
  
  stop(
    paste0(
      "Folgende benötigte Variablen fehlen im Screening-Datensatz:\n- ",
      paste(
        missing_variables,
        collapse = "\n- "
      )
    )
  )
}


#===============================================================================
# 06 Check participant codes and duplicate rows
#===============================================================================

duplicate_participants <- screening_raw %>%
  count(
    personalParticipantCode,
    name = "Number_of_Rows"
  ) %>%
  filter(
    Number_of_Rows > 1
  )

if (nrow(duplicate_participants) > 0) {
  
  warning(
    nrow(duplicate_participants),
    " Participant Codes kommen mehrfach im Screening-Datensatz vor. ",
    "Die doppelten Fälle werden nicht automatisch entfernt."
  )
}


#===============================================================================
# 07 Prepare filter variables
#===============================================================================

screening_all <- screening_raw %>%
  mutate(
    intro_stop_age_logical = as_logical_safe(intro_stop_age),
    intro_stop_usage_logical = as_logical_safe(intro_stop_usage),

    eligible_screening = (
      intro_stop_age_logical %in% TRUE &
        intro_stop_usage_logical %in% TRUE
    )
  )


# Dokumentation der Screening-Filter
eligibility_summary <- screening_all %>%
  summarise(
    N_Rows_Total = n(),
    N_Participants_Total = n_distinct(personalParticipantCode),
    N_Age_Filter_Passed = sum(
      intro_stop_age_logical %in% TRUE,
      na.rm = TRUE
    ),
    N_Usage_Filter_Passed = sum(
      intro_stop_usage_logical %in% TRUE,
      na.rm = TRUE
    ),
    N_Both_Filters_Passed = sum(
      eligible_screening %in% TRUE,
      na.rm = TRUE
    ),
    N_Both_Filters_Not_Passed = sum(
      eligible_screening %in% FALSE,
      na.rm = TRUE
    ),
    N_Filter_Status_Missing = sum(
      is.na(intro_stop_age_logical) |
        is.na(intro_stop_usage_logical)
    )
  )


# Nur teilnahmeberechtigte Personen für die vorläufige Screening-Analyse
screening <- screening_all %>%
  filter(
    eligible_screening %in% TRUE
  )


#===============================================================================
# 08 Recode numeric missing values and ensure numeric types
#===============================================================================

numeric_screening_variables <- c(
  "intro_age_num",
  "intro_gender",
  "intro_education",
  "intro_freq_facebook",
  "intro_freq_instagram",
  "intro_freq_tiktok",
  "intro_freq_x",
  "intro_intensity",
  "intro_ib_undirected",
  "intro_ib_thematic",
  "intro_ib_social",
  "intro_ib_problem",
  "intro_incidentality_1",
  "intro_incidentality_2",
  "intro_incidentality_3",
  "intro_incidentality_4",
  "intro_incidentality_5",
  "intro_incidentality_6",
  "intro_context_local",
  "intro_context_situation"
)

screening <- screening %>%
  mutate(
    across(
      all_of(numeric_screening_variables),
      ~ suppressWarnings(as.numeric(.x))
    ),
    across(
      all_of(numeric_screening_variables),
      ~ na_if(.x, -1)
    )
  )


#===============================================================================
# 09 Validate scale ranges
#===============================================================================

expected_ranges <- list(
  intro_age_num = c(60, Inf),
  intro_gender = c(1, 3),
  intro_education = c(1, 8),
  intro_freq_facebook = c(1, 8),
  intro_freq_instagram = c(1, 8),
  intro_freq_tiktok = c(1, 8),
  intro_freq_x = c(1, 8),
  intro_intensity = c(1, 7),
  intro_ib_undirected = c(1, 5),
  intro_ib_thematic = c(1, 5),
  intro_ib_social = c(1, 5),
  intro_ib_problem = c(1, 5),
  intro_incidentality_1 = c(1, 5),
  intro_incidentality_2 = c(1, 5),
  intro_incidentality_3 = c(1, 5),
  intro_incidentality_4 = c(1, 5),
  intro_incidentality_5 = c(1, 5),
  intro_incidentality_6 = c(1, 5),
  intro_context_local = c(1, 3),
  intro_context_situation = c(1, 3)
)

range_check <- purrr::imap_dfr(
  expected_ranges,
  function(expected_range, variable_name) {
    
    x <- screening[[variable_name]]
    
    tibble(
      Variable = variable_name,
      Expected_Minimum = expected_range[1],
      Expected_Maximum = expected_range[2],
      Observed_Minimum = if (
        all(is.na(x))
      ) {
        NA_real_
      } else {
        min(x, na.rm = TRUE)
      },
      Observed_Maximum = if (
        all(is.na(x))
      ) {
        NA_real_
      } else {
        max(x, na.rm = TRUE)
      },
      N_Outside_Expected_Range = sum(
        x < expected_range[1] |
          x > expected_range[2],
        na.rm = TRUE
      )
    )
  }
)

if (any(range_check$N_Outside_Expected_Range > 0)) {
  
  warning(
    "Mindestens eine Screening-Variable enthält Werte außerhalb ",
    "des erwarteten Wertebereichs. Siehe Tabellenblatt 'Range_Check'."
  )
}


#===============================================================================
# 10 Label categorical variables
#===============================================================================

screening <- screening %>%
  mutate(
    
    gender = factor(
      intro_gender,
      levels = c(1, 2, 3),
      labels = c(
        "Weiblich",
        "Männlich",
        "Divers"
      )
    ),
    
    education = factor(
      intro_education,
      levels = 1:8,
      labels = c(
        "Schule ohne Abschluss beendet",
        "Haupt-/Volksschulabschluss",
        "Realschulabschluss/Mittlere Reife",
        "Polytechnische Oberschule",
        "Fachhochschulreife",
        "Abitur/Hochschulreife",
        "Hochschulabschluss",
        "Anderer Abschluss"
      )
    ),
    
    context_local = factor(
      intro_context_local,
      levels = c(1, 2, 3),
      labels = c(
        "Zu Hause",
        "Unterwegs",
        "An beiden Orten ähnlich häufig"
      )
    ),
    
    context_social = factor(
      intro_context_situation,
      levels = c(1, 2, 3),
      labels = c(
        "Überwiegend allein",
        "Überwiegend gemeinsam mit anderen",
        "Allein und gemeinsam ähnlich häufig"
      )
    )
  )


#===============================================================================
# 11 Recode education
#===============================================================================

# Dreistufige Rekodierung:
#   1–2 = niedrig
#   3–4 = mittel
#   5–7 = hoch
#
# Hochschulabschlüsse werden der hohen Bildungsgruppe zugeordnet, da sie
# oberhalb der in der Präregistrierung genannten Hochschulzugangsberechtigungen
# liegen. "Anderer Abschluss" kann ohne zusätzliche Angaben nicht eindeutig
# klassifiziert werden und wird deshalb als fehlend behandelt.

screening <- screening %>%
  mutate(
    education_three_level = case_when(
      intro_education %in% c(1, 2) ~ "Niedrig",
      intro_education %in% c(3, 4) ~ "Mittel",
      intro_education %in% c(5, 6, 7) ~ "Hoch",
      intro_education == 8 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    education_three_level = factor(
      education_three_level,
      levels = c(
        "Niedrig",
        "Mittel",
        "Hoch"
      )
    ),
    
    education_other_unclassified = intro_education == 8
  )


#===============================================================================
# 12 Label platform frequency variables
#===============================================================================

frequency_levels <- c(
  "Nie",
  "Seltener als einmal im Monat",
  "Einmal im Monat",
  "Zwei- bis dreimal im Monat",
  "Einmal pro Woche",
  "Mehrmals pro Woche",
  "Einmal täglich",
  "Mehrmals täglich"
)

screening <- screening %>%
  mutate(
    freq_facebook_label = factor(
      intro_freq_facebook,
      levels = 1:8,
      labels = frequency_levels,
      ordered = TRUE
    ),
    
    freq_instagram_label = factor(
      intro_freq_instagram,
      levels = 1:8,
      labels = frequency_levels,
      ordered = TRUE
    ),
    
    freq_tiktok_label = factor(
      intro_freq_tiktok,
      levels = 1:8,
      labels = frequency_levels,
      ordered = TRUE
    ),
    
    freq_x_label = factor(
      intro_freq_x,
      levels = 1:8,
      labels = frequency_levels,
      ordered = TRUE
    )
  )


#===============================================================================
# 13 Weekly platform-use indicators
#===============================================================================

# Antwortcodes 5–8 entsprechen mindestens wöchentlicher Nutzung.

screening <- screening %>%
  mutate(
    facebook_weekly = case_when(
      is.na(intro_freq_facebook) ~ NA,
      intro_freq_facebook >= 5 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    instagram_weekly = case_when(
      is.na(intro_freq_instagram) ~ NA,
      intro_freq_instagram >= 5 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    tiktok_weekly = case_when(
      is.na(intro_freq_tiktok) ~ NA,
      intro_freq_tiktok >= 5 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    x_weekly = case_when(
      is.na(intro_freq_x) ~ NA,
      intro_freq_x >= 5 ~ TRUE,
      TRUE ~ FALSE
    )
  )


#===============================================================================
# 14 Reverse coding and scale construction
#===============================================================================

# Das ursprüngliche Item bleibt erhalten.
# Auf der Skala von 1 bis 5 wird das Item mit 6 - x invertiert.

screening <- screening %>%
  mutate(
    intro_incidentality_5_reversed = case_when(
      is.na(intro_incidentality_5) ~ NA_real_,
      TRUE ~ 6 - intro_incidentality_5
    )
  )

incidentality_index_items <- c(
  "intro_incidentality_1",
  "intro_incidentality_2",
  "intro_incidentality_3",
  "intro_incidentality_4",
  "intro_incidentality_5_reversed",
  "intro_incidentality_6"
)

# Der Index wird nur gebildet, wenn alle sechs Items beantwortet wurden.
# Dies entspricht der Annahme verpflichtender Fragen und vermeidet Indizes,
# die auf unterschiedlich vielen Items beruhen.

screening <- screening %>%
  mutate(
    incidentality_items_answered = rowSums(
      !is.na(
        across(
          all_of(incidentality_index_items)
        )
      )
    ),
    
    incidentality_index = if_else(
      incidentality_items_answered == length(incidentality_index_items),
      rowMeans(
        across(
          all_of(incidentality_index_items)
        ),
        na.rm = FALSE
      ),
      NA_real_
    )
  )


#===============================================================================
# 15 Reliability analysis
#===============================================================================

incidentality_items <- screening %>%
  select(
    all_of(incidentality_index_items)
  )


incidentality_alpha <- tryCatch(
  psych::alpha(
    incidentality_items,
    check.keys = FALSE,
    warnings = FALSE
  ),
  error = function(e) {
    
    warning(
      "Cronbachs Alpha konnte nicht berechnet werden: ",
      conditionMessage(e)
    )
    
    NULL
  }
)


# Bei einem eindimensionalen Modell ist Omega total relevant.
# Omega hierarchical setzt eine hierarchische bzw. bifaktorielle Struktur
# mit mehreren Faktoren voraus und wird daher hier nicht berichtet.

incidentality_omega <- tryCatch(
  suppressWarnings(
    psych::omega(
      incidentality_items,
      nfactors = 1,
      plot = FALSE
    )
  ),
  error = function(e) {
    
    warning(
      "Omega total konnte nicht berechnet werden: ",
      conditionMessage(e)
    )
    
    NULL
  }
)


reliability_summary <- tibble(
  Scale = "Incidentality",
  
  Number_of_Items =
    length(incidentality_index_items),
  
  N_Complete = sum(
    complete.cases(
      incidentality_items
    )
  ),
  
  Cronbach_Alpha = if (
    is.null(incidentality_alpha)
  ) {
    NA_real_
  } else {
    unname(
      incidentality_alpha$total$raw_alpha
    )
  },
  
  Standardized_Alpha = if (
    is.null(incidentality_alpha)
  ) {
    NA_real_
  } else {
    unname(
      incidentality_alpha$total$std.alpha
    )
  },
  
  Omega_Total = if (
    is.null(incidentality_omega)
  ) {
    NA_real_
  } else {
    unname(
      incidentality_omega$omega.tot
    )
  },
  
  Note = paste0(
    "Omega hierarchical wird nicht berichtet, ",
    "da ein eindimensionales Modell mit einem Faktor angenommen wird."
  )
)


#===============================================================================
# 16 Overall sample description
#===============================================================================

sample_overview <- tibble(
  Indicator = c(
    "Zeilen in ursprünglicher Screening-Datei",
    "Eindeutige Codes in ursprünglicher Screening-Datei",
    "Vorläufig teilnahmeberechtigte Zeilen",
    "Eindeutige vorläufig teilnahmeberechtigte Personen",
    "Doppelte Participant Codes",
    "Personen mit vollständigem Incidentality-Index",
    "Personen mit nicht klassifizierbarem anderem Bildungsabschluss"
  ),
  
  Value = c(
    nrow(screening_raw),
    n_distinct(screening_raw$personalParticipantCode),
    nrow(screening),
    n_distinct(screening$personalParticipantCode),
    nrow(duplicate_participants),
    sum(!is.na(screening$incidentality_index)),
    sum(
      screening$education_other_unclassified %in% TRUE,
      na.rm = TRUE
    )
  )
)


age_summary <- continuous_summary(
  screening,
  "intro_age_num",
  "Alter"
)

gender_summary <- frequency_summary(
  screening,
  "gender",
  "Geschlecht"
)

education_summary <- frequency_summary(
  screening,
  "education",
  "Bildungsabschluss"
)

education_three_level_summary <- frequency_summary(
  screening,
  "education_three_level",
  "Bildung, dreistufig"
)


#===============================================================================
# 17 Usage intensity
#===============================================================================

usage_intensity_summary <- continuous_summary(
  screening,
  "intro_intensity",
  "Nutzungsintensität"
)

usage_intensity_distribution <- item_distribution(
  screening,
  "intro_intensity",
  "Nutzungsintensität"
)


#===============================================================================
# 18 Platform-use data in long format
#===============================================================================

platform_long <- screening %>%
  select(
    personalParticipantCode,
    Facebook = intro_freq_facebook,
    Instagram = intro_freq_instagram,
    TikTok = intro_freq_tiktok,
    X = intro_freq_x
  ) %>%
  pivot_longer(
    cols = c(
      Facebook,
      Instagram,
      TikTok,
      X
    ),
    names_to = "Platform",
    values_to = "Usage_Frequency"
  ) %>%
  mutate(
    Platform = factor(
      Platform,
      levels = c(
        "Facebook",
        "Instagram",
        "TikTok",
        "X"
      )
    ),
    
    Usage_Frequency_Label = factor(
      Usage_Frequency,
      levels = 1:8,
      labels = frequency_levels,
      ordered = TRUE
    ),
    
    Weekly_Use = case_when(
      is.na(Usage_Frequency) ~ NA,
      Usage_Frequency >= 5 ~ TRUE,
      TRUE ~ FALSE
    )
  )


platform_descriptives <- platform_long %>%
  group_by(
    Platform
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(Usage_Frequency)
    ),
    
    N_Missing = sum(
      is.na(Usage_Frequency)
    ),
    
    Mean = if (
      sum(!is.na(Usage_Frequency)) > 0
    ) {
      mean(
        Usage_Frequency,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    SD = if (
      sum(!is.na(Usage_Frequency)) > 1
    ) {
      sd(
        Usage_Frequency,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    Median = if (
      sum(!is.na(Usage_Frequency)) > 0
    ) {
      median(
        Usage_Frequency,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    Minimum = if (
      sum(!is.na(Usage_Frequency)) > 0
    ) {
      min(
        Usage_Frequency,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    Maximum = if (
      sum(!is.na(Usage_Frequency)) > 0
    ) {
      max(
        Usage_Frequency,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )


platform_weekly_summary <- platform_long %>%
  group_by(
    Platform
  ) %>%
  summarise(
    N_Valid = sum(!is.na(Weekly_Use)),
    N_Weekly = sum(Weekly_Use %in% TRUE, na.rm = TRUE),
    N_Less_Than_Weekly = sum(
      Weekly_Use %in% FALSE,
      na.rm = TRUE
    ),
    Percent_Weekly = 100 * N_Weekly / N_Valid,
    .groups = "drop"
  )


platform_distribution <- platform_long %>%
  mutate(
    Usage_Frequency_Label = as.character(
      Usage_Frequency_Label
    ),
    
    Usage_Frequency_Label = replace_na(
      Usage_Frequency_Label,
      "Missing"
    )
  ) %>%
  count(
    Platform,
    Usage_Frequency,
    Usage_Frequency_Label,
    name = "N"
  ) %>%
  group_by(
    Platform
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  ungroup() %>%
  arrange(
    Platform,
    is.na(Usage_Frequency),
    Usage_Frequency
  )


#===============================================================================
# 19 Information needs
#===============================================================================


information_needs_long <- screening %>%
  select(
    personalParticipantCode,
    Ungerichtet = intro_ib_undirected,
    Thematisch = intro_ib_thematic,
    Sozial = intro_ib_social,
    Problembezogen = intro_ib_problem
  ) %>%
  pivot_longer(
    cols = c(
      Ungerichtet,
      Thematisch,
      Sozial,
      Problembezogen
    ),
    names_to = "Information_Need",
    values_to = "Importance"
  ) %>%
  mutate(
    Information_Need = factor(
      Information_Need,
      levels = c(
        "Ungerichtet",
        "Thematisch",
        "Sozial",
        "Problembezogen"
      )
    )
  )


information_needs_descriptives <- information_needs_long %>%
  group_by(
    Information_Need
  ) %>%
  summarise(
    N_Valid = sum(!is.na(Importance)),
    N_Missing = sum(is.na(Importance)),
    Mean = mean(Importance, na.rm = TRUE),
    SD = sd(Importance, na.rm = TRUE),
    Median = median(Importance, na.rm = TRUE),
    Minimum = min(Importance, na.rm = TRUE),
    Maximum = max(Importance, na.rm = TRUE),
    
    CI95_Lower = {
      n_value <- sum(!is.na(Importance))
      mean_value <- mean(Importance, na.rm = TRUE)
      sd_value <- sd(Importance, na.rm = TRUE)
      
      if (n_value > 1) {
        mean_value -
          qt(0.975, df = n_value - 1) *
          sd_value / sqrt(n_value)
      } else {
        NA_real_
      }
    },
    
    CI95_Upper = {
      n_value <- sum(!is.na(Importance))
      mean_value <- mean(Importance, na.rm = TRUE)
      sd_value <- sd(Importance, na.rm = TRUE)
      
      if (n_value > 1) {
        mean_value +
          qt(0.975, df = n_value - 1) *
          sd_value / sqrt(n_value)
      } else {
        NA_real_
      }
    },
    
    .groups = "drop"
  )


information_needs_distribution <- information_needs_long %>%
  mutate(
    Response_Label = if_else(
      is.na(Importance),
      "Missing",
      as.character(Importance)
    )
  ) %>%
  count(
    Information_Need,
    Importance,
    Response_Label,
    name = "N"
  ) %>%
  group_by(
    Information_Need
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  ungroup() %>%
  arrange(
    Information_Need,
    is.na(Importance),
    Importance
  )


#===============================================================================
# 20 Incidentality
#===============================================================================

incidentality_index_summary <- continuous_summary(
  screening,
  "incidentality_index",
  "Incidentality-Index"
)


incidentality_items_long <- screening %>%
  select(
    personalParticipantCode,
    Item_1 = intro_incidentality_1,
    Item_2 = intro_incidentality_2,
    Item_3 = intro_incidentality_3,
    Item_4 = intro_incidentality_4,
    Item_5_reversed = intro_incidentality_5_reversed,
    Item_6 = intro_incidentality_6
  ) %>%
  pivot_longer(
    cols = starts_with("Item_"),
    names_to = "Item",
    values_to = "Response"
  ) %>%
  mutate(
    Item = factor(
      Item,
      levels = c(
        "Item_1",
        "Item_2",
        "Item_3",
        "Item_4",
        "Item_5_reversed",
        "Item_6"
      ),
      labels = c(
        "Item 1",
        "Item 2",
        "Item 3",
        "Item 4",
        "Item 5, invertiert",
        "Item 6"
      )
    )
  )


incidentality_item_descriptives <- incidentality_items_long %>%
  group_by(
    Item
  ) %>%
  summarise(
    N_Valid = sum(!is.na(Response)),
    N_Missing = sum(is.na(Response)),
    Mean = mean(Response, na.rm = TRUE),
    SD = sd(Response, na.rm = TRUE),
    Median = median(Response, na.rm = TRUE),
    Minimum = min(Response, na.rm = TRUE),
    Maximum = max(Response, na.rm = TRUE),
    .groups = "drop"
  )


incidentality_item_distribution <- incidentality_items_long %>%
  mutate(
    Response_Label = if_else(
      is.na(Response),
      "Missing",
      as.character(Response)
    )
  ) %>%
  count(
    Item,
    Response,
    Response_Label,
    name = "N"
  ) %>%
  group_by(
    Item
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  ) %>%
  ungroup() %>%
  arrange(
    Item,
    is.na(Response),
    Response
  )


#===============================================================================
# 21 Typical usage contexts
#===============================================================================

context_local_summary <- frequency_summary(
  screening,
  "context_local",
  "Typischer räumlicher Nutzungskontext"
)

context_social_summary <- frequency_summary(
  screening,
  "context_social",
  "Typischer sozialer Nutzungskontext"
)

context_summary <- bind_rows(
  context_local_summary,
  context_social_summary
)


#===============================================================================
# 22 Missing-data overview
#===============================================================================

analysis_variables <- c(
  "intro_age_num",
  "intro_gender",
  "intro_education",
  "intro_freq_facebook",
  "intro_freq_instagram",
  "intro_freq_tiktok",
  "intro_freq_x",
  "intro_intensity",
  "intro_ib_undirected",
  "intro_ib_thematic",
  "intro_ib_social",
  "intro_ib_problem",
  "intro_incidentality_1",
  "intro_incidentality_2",
  "intro_incidentality_3",
  "intro_incidentality_4",
  "intro_incidentality_5",
  "intro_incidentality_6",
  "incidentality_index",
  "intro_context_local",
  "intro_context_situation"
)

missing_data_summary <- purrr::map_dfr(
  analysis_variables,
  function(variable_name) {
    
    tibble(
      Variable = variable_name,
      N_Total = nrow(screening),
      N_Valid = sum(
        !is.na(screening[[variable_name]])
      ),
      N_Missing = sum(
        is.na(screening[[variable_name]])
      ),
      Percent_Missing = 100 * N_Missing / N_Total
    )
  }
)


#===============================================================================
# 23 Compact Table 1
#===============================================================================

table_1_continuous <- bind_rows(
  age_summary,
  usage_intensity_summary,
  incidentality_index_summary
) %>%
  transmute(
    Variable,
    Category = NA_character_,
    Statistic = paste0(
      round(Mean, 2),
      " (",
      round(SD, 2),
      ")"
    ),
    N = N_Valid
  )


table_1_gender <- gender_summary %>%
  filter(
    Level != "Missing"
  ) %>%
  transmute(
    Variable,
    Category = Level,
    Statistic = paste0(
      N,
      " (",
      round(Percent, 1),
      "%)"
    ),
    N
  )


table_1_education <- education_three_level_summary %>%
  filter(
    Level != "Missing"
  ) %>%
  transmute(
    Variable,
    Category = Level,
    Statistic = paste0(
      N,
      " (",
      round(Percent, 1),
      "%)"
    ),
    N
  )


table_1 <- bind_rows(
  table_1_continuous,
  table_1_gender,
  table_1_education
)


#===============================================================================
# 24 Save prepared data
#===============================================================================

saveRDS(
  screening,
  output_rds
)

saveRDS(
  list(
    alpha = incidentality_alpha,
    omega = incidentality_omega
  ),
  reliability_rds
)


#===============================================================================
# Alpha item statistics
#===============================================================================

alpha_item_statistics <- if (
  is.null(incidentality_alpha)
) {
  
  tibble(
    Item = character(),
    N = numeric(),
    Raw_R = numeric(),
    Standardized_R = numeric(),
    Corrected_Item_Total_R = numeric(),
    Alpha_If_Deleted = numeric()
  )
  
} else {
  
  incidentality_alpha$item.stats %>%
    as.data.frame() %>%
    tibble::rownames_to_column(
      var = "Item"
    ) %>%
    as_tibble() %>%
    transmute(
      Item = Item,
      N = n,
      Raw_R = raw.r,
      Standardized_R = std.r,
      Corrected_Item_Total_R = r.drop,
      Mean = mean,
      SD = sd
    ) %>%
    left_join(
      incidentality_alpha$alpha.drop %>%
        as.data.frame() %>%
        tibble::rownames_to_column(
          var = "Item"
        ) %>%
        as_tibble() %>%
        transmute(
          Item = Item,
          Alpha_If_Deleted = raw_alpha
        ),
      by = "Item"
    )
}

#===============================================================================
# 25 Create Excel workbook
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
  "Eligibility",
  eligibility_summary,
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
  "Table_1",
  table_1,
  header_style
)

add_excel_sheet(
  workbook,
  "Age",
  age_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Gender",
  gender_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Education_Detailed",
  education_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Education_3_Level",
  education_three_level_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Usage_Intensity",
  usage_intensity_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Intensity_Distribution",
  usage_intensity_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Descriptives",
  platform_descriptives,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Weekly",
  platform_weekly_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Distribution",
  platform_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Information_Needs",
  information_needs_descriptives,
  header_style
)

add_excel_sheet(
  workbook,
  "InfoNeeds_Distribution",
  information_needs_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Index",
  incidentality_index_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Items",
  incidentality_item_descriptives,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Distrib",
  incidentality_item_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Reliability",
  reliability_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Alpha_Item_Statistics",
  alpha_item_statistics,
  header_style
)

# add_excel_sheet(
#   workbook,
#   "Alpha_Item_Deleted",
#   alpha_if_item_deleted,
#   header_style
# )


# add_excel_sheet(
#   workbook,
#   "Omega_Loadings",
#   omega_loadings,
#   header_style
# )

add_excel_sheet(
  workbook,
  "Contexts",
  context_summary,
  header_style
)

openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = TRUE
)


#===============================================================================
# 26 Visual design
#===============================================================================

project_colors <- c(
  primary = "#315F6B",
  secondary = "#78999E",
  accent = "#C49A5A",
  dark = "#26383F",
  medium = "#66777D",
  light = "#E8EFF1",
  grid = "#DCE4E6",
  white = "#FFFFFF"
)


platform_colors <- c(
  Facebook = "#315F6B",
  Instagram = "#4F7E82",
  TikTok = "#78999E",
  X = "#65747B"
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
    bg = unname(
      project_colors["white"]
    )
  )
}


#===============================================================================
# 27 Figure: Age distribution
#===============================================================================

age_plot_data <- screening %>%
  filter(
    !is.na(intro_age_num)
  )


figure_age <- ggplot(
  age_plot_data,
  aes(
    x = intro_age_num
  )
) +
  geom_histogram(
    binwidth = 2,
    boundary = 0,
    fill = unname(
      project_colors["primary"]
    ),
    color = unname(
      project_colors["white"]
    ),
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = age_summary$Mean,
    color = unname(
      project_colors["accent"]
    ),
    linewidth = 0.9,
    linetype = "22"
  ) +
  annotate(
    geom = "text",
    x = age_summary$Mean,
    y = Inf,
    label = paste0(
      "M = ",
      format(
        round(
          age_summary$Mean,
          1
        ),
        decimal.mark = ","
      )
    ),
    color = unname(
      project_colors["accent"]
    ),
    fontface = "bold",
    hjust = -0.15,
    vjust = 1.5,
    size = 3.7
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(
      mult = c(
        0,
        0.10
      )
    )
  ) +
  labs(
    title = "Altersverteilung",
    subtitle = paste0(
      "N = ",
      age_summary$N_Valid,
      "; M = ",
      format(
        round(
          age_summary$Mean,
          1
        ),
        decimal.mark = ","
      ),
      "; SD = ",
      format(
        round(
          age_summary$SD,
          1
        ),
        decimal.mark = ","
      )
    ),
    x = "Alter in Jahren",
    y = "Anzahl der Teilnehmenden"
  )


save_project_plot(
  figure_age,
  "Screening_Age.png"
)


#===============================================================================
# 28 Figure: Gender
#===============================================================================

gender_plot_data <- screening %>%
  filter(
    !is.na(gender)
  ) %>%
  count(
    gender,
    .drop = FALSE,
    name = "N"
  )


figure_gender <- ggplot(
  gender_plot_data,
  aes(
    x = gender,
    y = N
  )
) +
  geom_col(
    width = 0.62,
    fill = unname(
      project_colors["primary"]
    )
  ) +
  geom_text(
    aes(
      label = N
    ),
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    vjust = -0.45,
    size = 3.8
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(
      mult = c(
        0,
        0.13
      )
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Geschlecht",
    subtitle = "Absolute Häufigkeiten",
    x = NULL,
    y = "Anzahl der Teilnehmenden"
  )


save_project_plot(
  figure_gender,
  "Screening_Gender.png"
)


#===============================================================================
# 29 Figure: Education
#===============================================================================

education_plot_data <- screening %>%
  filter(
    !is.na(education_three_level)
  ) %>%
  count(
    education_three_level,
    .drop = FALSE,
    name = "N"
  )


figure_education <- ggplot(
  education_plot_data,
  aes(
    x = education_three_level,
    y = N
  )
) +
  geom_col(
    width = 0.62,
    fill = unname(
      project_colors["primary"]
    )
  ) +
  geom_text(
    aes(
      label = N
    ),
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    vjust = -0.45,
    size = 3.8
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(
      mult = c(
        0,
        0.13
      )
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Bildungsniveau",
    subtitle = "Absolute Häufigkeiten der dreistufigen Rekodierung",
    x = NULL,
    y = "Anzahl der Teilnehmenden"
  )


save_project_plot(
  figure_education,
  "Screening_Education.png"
)


#===============================================================================
# 30 Figure: Weekly platform use
#===============================================================================

platform_weekly_plot_data <- platform_long %>%
  mutate(
    Weekly_Category = case_when(
      Weekly_Use %in% TRUE ~
        "Mindestens wöchentlich",
      
      Weekly_Use %in% FALSE ~
        "Seltener als wöchentlich",
      
      TRUE ~ NA_character_
    ),
    
    Weekly_Category = factor(
      Weekly_Category,
      levels = c(
        "Mindestens wöchentlich",
        "Seltener als wöchentlich"
      )
    )
  ) %>%
  filter(
    !is.na(Weekly_Category)
  ) %>%
  count(
    Platform,
    Weekly_Category,
    .drop = FALSE,
    name = "N"
  )


weekly_colors <- c(
  `Mindestens wöchentlich` =
    unname(
      project_colors["primary"]
    ),
  
  `Seltener als wöchentlich` =
    unname(
      project_colors["light"]
    )
)


figure_platform_weekly <- ggplot(
  platform_weekly_plot_data,
  aes(
    x = Platform,
    y = N,
    fill = Weekly_Category
  )
) +
  geom_col(
    position = position_dodge(
      width = 0.72
    ),
    width = 0.66
  ) +
  geom_text(
    aes(
      label = N
    ),
    position = position_dodge(
      width = 0.72
    ),
    vjust = -0.4,
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_manual(
    values = weekly_colors
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +
  labs(
    title = "Mindestens wöchentliche Plattformnutzung",
    subtitle = "Absolute Häufigkeiten",
    x = NULL,
    y = "Anzahl der Teilnehmenden"
  )


save_project_plot(
  figure_platform_weekly,
  "Screening_Platform_Weekly.png",
  width = 8
)


#===============================================================================
# 31 Figure: Platform-use frequency distributions
#===============================================================================

platform_frequency_plot_data <- platform_long %>%
  filter(
    !is.na(Usage_Frequency)
  ) %>%
  mutate(
    Usage_Frequency_Label = factor(
      Usage_Frequency,
      levels = 1:8,
      labels = frequency_levels,
      ordered = TRUE
    )
  ) %>%
  count(
    Platform,
    Usage_Frequency_Label,
    .drop = FALSE,
    name = "N"
  )


figure_platform_frequency <- ggplot(
  platform_frequency_plot_data,
  aes(
    x = N,
    y = Usage_Frequency_Label
  )
) +
  geom_col(
    width = 0.68,
    fill = unname(
      project_colors["primary"]
    )
  ) +
  geom_text(
    aes(
      label = if_else(
        N > 0,
        as.character(N),
        ""
      )
    ),
    hjust = -0.2,
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    size = 3.2
  ) +
  facet_wrap(
    ~ Platform,
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(
      mult = c(
        0,
        0.14
      )
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Häufigkeit der Plattformnutzung",
    subtitle = "Absolute Häufigkeiten der Antwortkategorien",
    x = "Anzahl der Teilnehmenden",
    y = NULL
  ) +
  theme(
    panel.grid.major.x = element_line(
      color = unname(
        project_colors["grid"]
      ),
      linewidth = 0.4
    ),
    
    panel.grid.major.y = element_blank()
  )


save_project_plot(
  figure_platform_frequency,
  "Screening_Platform_Frequencies.png",
  width = 11,
  height = 8
)


#===============================================================================
# 32 Figure: Information needs
#===============================================================================

information_needs_plot_data <- information_needs_descriptives %>%
  filter(
    N_Valid > 0,
    is.finite(Mean),
    is.finite(CI95_Lower),
    is.finite(CI95_Upper)
  )


figure_information_needs <- ggplot(
  information_needs_plot_data,
  aes(
    x = Information_Need,
    y = Mean
  )
) +
  geom_point(
    color = unname(
      project_colors["primary"]
    ),
    size = 3.2
  ) +
  geom_errorbar(
    aes(
      ymin = CI95_Lower,
      ymax = CI95_Upper
    ),
    color = unname(
      project_colors["accent"]
    ),
    width = 0.12,
    linewidth = 0.85
  ) +
  scale_y_continuous(
    limits = c(
      1,
      5
    ),
    breaks = 1:5
  ) +
  labs(
    title = "Informationsbedürfnisse",
    subtitle = "Mittelwerte und 95%-Konfidenzintervalle",
    x = NULL,
    y = "Wichtigkeit (1–5)",
    caption = "1 = überhaupt nicht wichtig; 5 = sehr wichtig"
  )


save_project_plot(
  figure_information_needs,
  "Screening_Information_Needs.png",
  width = 8
)


#===============================================================================
# 33 Figure: Incidentality index
#===============================================================================

incidentality_plot_data <- screening %>%
  filter(
    !is.na(incidentality_index),
    is.finite(incidentality_index)
  )


figure_incidentality <- ggplot(
  incidentality_plot_data,
  aes(
    x = incidentality_index
  )
) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 1,
    fill = unname(
      project_colors["primary"]
    ),
    color = unname(
      project_colors["white"]
    ),
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept =
      incidentality_index_summary$Mean,
    
    color = unname(
      project_colors["accent"]
    ),
    linewidth = 0.9,
    linetype = "22"
  ) +
  scale_x_continuous(
    breaks = 1:5
  ) +
  coord_cartesian(
    xlim = c(
      1,
      5
    )
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(
      mult = c(
        0,
        0.10
      )
    )
  ) +
  labs(
    title = "Incidentality-Index",
    subtitle = paste0(
      "N = ",
      incidentality_index_summary$N_Valid,
      "; M = ",
      format(
        round(
          incidentality_index_summary$Mean,
          2
        ),
        decimal.mark = ","
      ),
      "; SD = ",
      format(
        round(
          incidentality_index_summary$SD,
          2
        ),
        decimal.mark = ","
      )
    ),
    x = "Incidentality (1–5)",
    y = "Anzahl der Teilnehmenden",
    caption = paste0(
      "Höhere Werte stehen für eine stärker ",
      "inzidentelle Informationsnutzung."
    )
  )


save_project_plot(
  figure_incidentality,
  "Screening_Incidentality_Index.png"
)


#===============================================================================
# 34 Figure: Typical contexts
#===============================================================================

# Die beiden Dimensionen werden getrennt ausgezählt.
# .drop = FALSE sorgt dafür, dass auch unbeobachtete Kategorien mit N = 0
# im Diagramm erhalten bleiben.

context_local_plot_data <- screening %>%
  filter(
    !is.na(context_local)
  ) %>%
  count(
    context_local,
    .drop = FALSE,
    name = "N"
  ) %>%
  transmute(
    Context_Dimension = "Räumlich",
    Context = as.character(
      context_local
    ),
    N
  )


context_social_plot_data <- screening %>%
  filter(
    !is.na(context_social)
  ) %>%
  count(
    context_social,
    .drop = FALSE,
    name = "N"
  ) %>%
  transmute(
    Context_Dimension = "Sozial",
    Context = as.character(
      context_social
    ),
    N
  )


context_plot_data <- bind_rows(
  context_local_plot_data,
  context_social_plot_data
) %>%
  mutate(
    Context_Dimension = factor(
      Context_Dimension,
      levels = c(
        "Räumlich",
        "Sozial"
      )
    )
  )


context_colors <- c(
  Räumlich =
    unname(
      project_colors["primary"]
    ),
  
  Sozial =
    unname(
      project_colors["accent"]
    )
)


figure_contexts <- ggplot(
  context_plot_data,
  aes(
    x = N,
    y = Context,
    fill = Context_Dimension
  )
) +
  geom_col(
    width = 0.64
  ) +
  geom_text(
    aes(
      label = N
    ),
    hjust = -0.2,
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    size = 3.5
  ) +
  facet_wrap(
    ~ Context_Dimension,
    scales = "free_y",
    ncol = 2
  ) +
  scale_fill_manual(
    values = context_colors
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(
      mult = c(
        0,
        0.15
      )
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Typische Nutzungskontexte",
    subtitle = "Absolute Häufigkeiten; nicht gewählte Kategorien werden mit N = 0 dargestellt",
    x = "Anzahl der Teilnehmenden",
    y = NULL
  ) +
  theme(
    panel.grid.major.x = element_line(
      color = unname(
        project_colors["grid"]
      ),
      linewidth = 0.4
    ),
    
    panel.grid.major.y = element_blank()
  )


save_project_plot(
  figure_contexts,
  "Screening_Contexts.png",
  width = 11,
  height = 6
)
#===============================================================================
# 35 Console report
#===============================================================================

cat(
  "\n",
  "============================================================\n",
  "SCREENING ANALYSIS COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat(
  "Rows in raw screening data: ",
  nrow(screening_raw),
  "\n",
  sep = ""
)

cat(
  "Provisionally eligible participants: ",
  n_distinct(screening$personalParticipantCode),
  "\n",
  sep = ""
)

cat(
  "Mean age: ",
  round(age_summary$Mean, 2),
  " (SD = ",
  round(age_summary$SD, 2),
  ")\n",
  sep = ""
)

cat(
  "Incidentality index: M = ",
  round(incidentality_index_summary$Mean, 2),
  ", SD = ",
  round(incidentality_index_summary$SD, 2),
  "\n",
  sep = ""
)

cat(
  "Cronbach's alpha: ",
  round(reliability_summary$Cronbach_Alpha, 3),
  "\n",
  sep = ""
)

cat(
  "Omega total: ",
  round(
    reliability_summary$Omega_Total,
    3
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