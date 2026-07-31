################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    04a_Screening_Analysis.R
#
# REQUIRED INPUTS
# ---------------
# 01_Data/screening-befragung_tagebuchstudie.rds
#   Required variables:
#     personalParticipantCode
#     intro_stop_age, intro_stop_usage
#     intro_age_num, intro_gender, intro_education
#     intro_freq_facebook, intro_freq_instagram,
#     intro_freq_tiktok, intro_freq_x
#     intro_intensity
#     intro_ib_undirected, intro_ib_thematic,
#     intro_ib_social, intro_ib_problem
#     intro_incidentality_1 ... intro_incidentality_6
#     intro_context_local, intro_context_situation
#
#
# CENTRAL SETTINGS
# ----------------
# overwrite_outputs
#   TRUE overwrites the existing Excel and RDS outputs.
#
# create_figures
#   TRUE writes all Screening_*.png files. Plots are still constructed when
#   FALSE, but save_project_plot() returns without writing files.
#
# stop_on_duplicate_codes
#   TRUE stops before analysis when one participant code occurs more than once.
#   No row is removed automatically because this would require a substantive
#   decision about which registration is valid.
#
# stop_on_out_of_range
#   TRUE stops when an eligible case contains values outside the documented
#   response ranges. Set FALSE only for diagnostic runs; out-of-range values are
#   then retained and clearly reported in the Range_Check sheet.
#
# reliability_threshold
#   Reference value used for the preregistered item-deletion diagnostic. The
#   script never drops an item automatically. Any exclusion must be decided,
#   documented and implemented explicitly before the final analysis.
#
# ANALYSES
# --------
# A. Data integrity and eligibility
#    - required-variable check
#    - missing and duplicate participant codes
#    - mutually exclusive eligibility-status counts
#    - expected-range checks
#    - consistency of the weekly-use stop item with detailed platform reports
#    - missing-data overview
#
# B. Sociodemographic description
#    - age: N, missingness, mean, SD, median, range and distribution
#    - gender: absolute and relative frequencies
#    - detailed educational qualification
#    - exploratory three-level education recode
#    - compact Table 1
#
# C. Social-media use
#    - usage-intensity scale distribution and descriptives
#    - platform-specific ordinal frequency distributions
#    - weekly and daily platform-use indicators
#    - number of platforms used at all, weekly and daily
#    - primary platform, including ties
#    - weekly platform combinations and pairwise platform co-use
#
# D. Information needs
#    - descriptives and complete 1–5 response distributions for:
#      ungerichtet, thematisch, sozial and problembezogen
#    - within-person mean need importance
#    - differentiation between strongest and weakest need
#    - number of highly rated needs
#    - dominant information need, with ties retained as non-unique profiles
#
# E. Screening incidentality scale
#    - reverse coding of item 5
#    - complete-case mean index across all six items
#    - index and item descriptives
#    - raw and directionally aligned item distributions
#    - item- and index-level floor/ceiling diagnostics
#    - Cronbach's alpha, standardised alpha and omega total
#    - corrected item-total correlations and alpha if item deleted
#    - omega total if each single item were deleted
#    - inter-item correlations
#    NOTE: omega hierarchical is not reported for the one-factor solution,
#    because it is not substantively meaningful in this specification.
#
# F. Exploratory screening patterns
#    - Incidentality by age group, gender, education, context, platform
#      repertoire and primary platform
#    - central screening measures by age group, gender, education and platform
#      repertoire
#    - targeted Spearman correlations of Incidentality with age, use intensity,
#      platform use and information needs
#    - targeted Spearman correlations of age with central screening markers
#    - Benjamini–Hochberg adjusted p-values for each exploratory family
#
# OUTPUT FILES
# ------------
# 03_Output/Screening_Results.xlsx
#   Self-documenting workbook containing settings, codebook, quality checks,
#   descriptives, reliability diagnostics and exploratory analyses.
#
# 03_Output/screening_prepared.rds
#   One row per eligible participant. Includes cleaned original variables and
#   derived variables required downstream, including:
#     participant, gender, education, education_three_level, age_group,
#     platform-use indicators and labels, N_Platforms_Used/Weekly/Daily,
#     Primary_Platform, Platform_Repertoire, Weekly_Platform_Combination,
#     information-need profile variables and incidentality_index.
#
# 03_Output/screening_reliability_objects.rds
#   psych alpha/omega objects, complete reliability data, item diagnostics,
#   item-deletion results and inter-item correlations.
#
# EXCEL WORKBOOK SHEETS
# ---------------------
# Documentation:
#   Analysis_Settings, Variable_Codebook, Output_Manifest
# Sample and quality control:
#   Sample_Overview, Eligibility, Eligibility_Consistency, Duplicate_Codes,
#   Missing_Codes, Range_Check, Range_Issues, Missing_Data
# Compact and sociodemographic description:
#   Table_1, Age, Gender, Education_Detailed, Education_3_Level
# Usage and platform profiles:
#   Usage_Intensity, Intensity_Distribution, Platform_Descriptives,
#   Platform_Weekly, Platform_Distribution, Platform_Profile,
#   Platform_Repertoire, Platform_Combinations, Platform_CoUse,
#   Primary_Platform, Platform_Profile_Summary
# Information needs:
#   Information_Needs, InfoNeeds_Distribution, InfoNeed_Profiles,
#   InfoNeed_Profile_Summary, Dominant_Info_Need
# Incidentality and reliability:
#   Incidentality_Index, Incidentality_Item_Desc,
#   Incidentality_Scored_Dist, Incidentality_Raw_Dist,
#   Incidentality_Floor_Ceiling, Incidentality_Index_Floor,
#   Reliability, Reliability_Decision, Alpha_Item_Statistics,
#   Omega_Item_Deleted, Interitem_Correlations
# Contexts and exploratory analyses:
#   Contexts, Incidentality_Subgroups, Exploratory_Subgroups,
#   Exploratory_Correlations, Age_Correlations
#
# FIGURE FILES
# ------------
#   04_Figures/Screening_Age.png
#   04_Figures/Screening_Gender.png
#   04_Figures/Screening_Education.png
#   04_Figures/Screening_Platform_Weekly.png
#   04_Figures/Screening_Platform_Frequencies.png
#   04_Figures/Screening_Usage_Intensity.png
#   04_Figures/Screening_Information_Needs.png
#   04_Figures/Screening_Information_Need_Distributions.png
#   04_Figures/Screening_Incidentality_Index.png
#   04_Figures/Screening_Incidentality_Items.png
#   04_Figures/Screening_Contexts.png
#   04_Figures/Screening_Platform_Repertoire.png
#   04_Figures/Screening_Incidentality_Correlations.png
#


rm(list = ls())


#===============================================================================

# 00 Settings
#===============================================================================

analysis_version <- "2026-07-30"
overwrite_outputs <- TRUE
create_figures <- TRUE
stop_on_duplicate_codes <- TRUE
stop_on_out_of_range <- TRUE
reliability_threshold <- 0.70

#===============================================================================


# 01 Packages
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  psych,
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


if (
  !overwrite_outputs &&
  any(
    file.exists(
      c(
        output_excel,
        output_rds,
        reliability_rds
      )
    )
  )
) {
  stop(
    paste0(
      "Mindestens eine Output-Datei existiert bereits und ",
      "overwrite_outputs = FALSE."
    )
  )
}


#===============================================================================

# 04 Load data
#===============================================================================


if (file.exists(helper_script)) {
  source(helper_script)
} else {
  warning(
    "Helper-Script nicht gefunden: ",
    helper_script,
    "!!"
  )
}


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

participant_code_check <- screening_raw %>%
  mutate(
    Source_Row = row_number(),
    participant = clean_text(
      personalParticipantCode
    )
  )


missing_participant_codes <- participant_code_check %>%
  filter(
    is.na(
      participant
    )
  ) %>%
  select(
    Source_Row,
    personalParticipantCode
  )


duplicate_participants <- participant_code_check %>%
  filter(
    !is.na(
      participant
    )
  ) %>%
  count(
    participant,
    name = "Number_of_Rows"
  ) %>%
  filter(
    Number_of_Rows > 1
  )


if (nrow(missing_participant_codes) > 0) {
  
  warning(
    nrow(missing_participant_codes),
    " Screening-Zeilen besitzen keinen gültigen Participant Code ",
    "und werden aus der Analyse ausgeschlossen."
  )
}


if (nrow(duplicate_participants) > 0) {
  duplicate_message <- paste0(
    nrow(duplicate_participants),
    " Participant Codes kommen mehrfach im Screening-Datensatz vor: ",
    paste(
      duplicate_participants$participant,
      collapse = ", "
    ),
    ". Es wird keine Zeile automatisch entfernt."
  )
  
  if (stop_on_duplicate_codes) {
    stop(duplicate_message)
  } else {
    warning(
      duplicate_message,
      " stop_on_duplicate_codes = FALSE; Ergebnisse können dadurch ",
      "Personen mehrfach gewichten."
    )
  }
}


# 07 Prepare filter variables
#===============================================================================

screening_all <- screening_raw %>%
  mutate(
    Source_Row = row_number(),
    participant = clean_text(
      personalParticipantCode
    ),
    
    intro_stop_age_logical =
      as_logical_safe(
        intro_stop_age
      ),
    
    intro_stop_usage_logical =
      as_logical_safe(
        intro_stop_usage
      ),
    
    eligible_screening = case_when(
      is.na(intro_stop_age_logical) |
        is.na(intro_stop_usage_logical) ~ NA,
      
      intro_stop_age_logical %in% TRUE &
        intro_stop_usage_logical %in% TRUE ~ TRUE,
      
      TRUE ~ FALSE
    )
  )


# Dokumentation der Screening-Filter
eligibility_summary <- screening_all %>%
  summarise(
    N_Rows_Total = n(),
    
    N_Participants_Total = n_distinct(
      participant,
      na.rm = TRUE
    ),
    
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
      is.na(eligible_screening)
    )
  )


# Nur teilnahmeberechtigte Personen für die vorläufige Screening-Analyse
screening <- screening_all %>%
  filter(
    eligible_screening %in% TRUE,
    !is.na(participant)
  )


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
    intro_gender = recode_gender_numeric(
      intro_gender
    ),
    
    across(
      all_of(
        setdiff(
          numeric_screening_variables,
          "intro_gender"
        )
      ),
      ~ clean_numeric(.x)
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

range_issues <- purrr::imap_dfr(
  expected_ranges,
  function(expected_range, variable_name) {
    screening %>%
      transmute(
        Source_Row,
        participant,
        Variable = variable_name,
        Value = .data[[variable_name]],
        Expected_Minimum = expected_range[[1]],
        Expected_Maximum = expected_range[[2]]
      ) %>%
      filter(
        !is.na(Value),
        Value < Expected_Minimum |
          Value > Expected_Maximum
      )
  }
)


if (any(range_check$N_Outside_Expected_Range > 0)) {
  affected_variables <- range_check %>%
    filter(
      N_Outside_Expected_Range > 0
    ) %>%
    pull(
      Variable
    )
  
  range_examples <- range_issues %>%
    mutate(
      Example = paste0(
        participant,
        ": ",
        Variable,
        " = ",
        Value
      )
    ) %>%
    slice_head(
      n = 10
    ) %>%
    pull(
      Example
    )
  
  range_message <- paste0(
    nrow(range_issues),
    " Werte außerhalb des erwarteten Bereichs in: ",
    paste(
      affected_variables,
      collapse = ", "
    ),
    ". Beispiele: ",
    paste(
      range_examples,
      collapse = "; "
    ),
    "."
  )
  
  if (stop_on_out_of_range) {
    stop(
      range_message,
      " Setze stop_on_out_of_range nur für einen diagnostischen Lauf auf FALSE."
    )
  } else {
    warning(
      range_message,
      " Die Werte bleiben unverändert im Datensatz."
    )
  }
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
    all_of(
      incidentality_index_items
    )
  )


# The scale index requires complete responses. Reliability is therefore
# estimated on the same complete-case set.
incidentality_items_complete <- incidentality_items %>%
  drop_na()


reliability_data_valid <- function(data) {
  nrow(data) >= 3 &&
    ncol(data) >= 2 &&
    all(
      purrr::map_lgl(
        data,
        ~ dplyr::n_distinct(.x) >= 2
      )
    )
}


calculate_alpha_object <- function(
    data,
    warn_on_failure = TRUE
) {
  if (!reliability_data_valid(data)) {
    if (warn_on_failure) {
      warning(
        paste0(
          "Cronbachs Alpha konnte nicht berechnet werden: zu wenige ",
          "vollständige Fälle oder mindestens ein Item ohne Varianz."
        )
      )
    }
    
    return(NULL)
  }
  
  tryCatch(
    psych::alpha(
      data,
      check.keys = FALSE,
      warnings = FALSE
    ),
    error = function(e) {
      if (warn_on_failure) {
        warning(
          "Cronbachs Alpha konnte nicht berechnet werden: ",
          conditionMessage(e)
        )
      }
      
      NULL
    }
  )
}


calculate_omega_object <- function(
    data,
    warn_on_failure = TRUE
) {
  if (!reliability_data_valid(data)) {
    if (warn_on_failure) {
      warning(
        paste0(
          "Omega total konnte nicht berechnet werden: zu wenige ",
          "vollständige Fälle oder mindestens ein Item ohne Varianz."
        )
      )
    }
    
    return(NULL)
  }
  
  tryCatch(
    {
      omega_result <- NULL
      
      invisible(
        capture.output(
          omega_result <- suppressWarnings(
            suppressMessages(
              psych::omega(
                data,
                nfactors = 1,
                plot = FALSE
              )
            )
          ),
          type = "output"
        )
      )
      
      omega_result
    },
    error = function(e) {
      if (warn_on_failure) {
        warning(
          "Omega total konnte nicht berechnet werden: ",
          conditionMessage(e)
        )
      }
      
      NULL
    }
  )
}


incidentality_alpha <- calculate_alpha_object(
  incidentality_items_complete
)

incidentality_omega <- calculate_omega_object(
  incidentality_items_complete
)


reliability_summary <- tibble(
  Scale = "Incidentality",
  
  Number_of_Items = length(
    incidentality_index_items
  ),
  
  N_Complete = nrow(
    incidentality_items_complete
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
  
  Threshold = reliability_threshold,
  
  Note = paste0(
    "Berichtet wird Omega total. Omega hierarchical ist bei einem ",
    "eindimensionalen Ein-Faktor-Modell nicht sinnvoll interpretierbar."
  )
)


alpha_item_statistics <- if (
  is.null(incidentality_alpha)
) {
  tibble(
    Item = character(),
    N = numeric(),
    Raw_R = numeric(),
    Standardized_R = numeric(),
    Corrected_Item_Total_R = numeric(),
    Mean = numeric(),
    SD = numeric(),
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


omega_if_item_deleted <- purrr::map_dfr(
  incidentality_index_items,
  function(item_removed) {
    reduced_data <- incidentality_items_complete %>%
      select(
        -all_of(
          item_removed
        )
      )
    
    alpha_reduced <- calculate_alpha_object(
      reduced_data,
      warn_on_failure = FALSE
    )
    
    omega_reduced <- calculate_omega_object(
      reduced_data,
      warn_on_failure = FALSE
    )
    
    tibble(
      Item_Removed = item_removed,
      N_Complete = nrow(reduced_data),
      Number_of_Items = ncol(reduced_data),
      Cronbach_Alpha = if (
        is.null(alpha_reduced)
      ) {
        NA_real_
      } else {
        unname(
          alpha_reduced$total$raw_alpha
        )
      },
      Omega_Total = if (
        is.null(omega_reduced)
      ) {
        NA_real_
      } else {
        unname(
          omega_reduced$omega.tot
        )
      }
    )
  }
) %>%
  mutate(
    Exceeds_Threshold = Omega_Total >= reliability_threshold
  ) %>%
  arrange(
    desc(Omega_Total)
  )


best_item_deletion <- omega_if_item_deleted %>%
  filter(
    !is.na(Omega_Total)
  ) %>%
  slice_max(
    Omega_Total,
    n = 1,
    with_ties = FALSE
  )


reliability_decision <- tibble(
  Original_Number_of_Items = length(
    incidentality_index_items
  ),
  
  Original_Omega_Total = reliability_summary$Omega_Total,
  
  Reliability_Threshold = reliability_threshold,
  
  Best_Item_to_Remove = if (
    nrow(best_item_deletion) == 0
  ) {
    NA_character_
  } else {
    best_item_deletion$Item_Removed[[1]]
  },
  
  Best_Omega_After_Deletion = if (
    nrow(best_item_deletion) == 0
  ) {
    NA_real_
  } else {
    best_item_deletion$Omega_Total[[1]]
  },
  
  Automatic_Exclusion_Applied = FALSE,
  
  Recommendation = case_when(
    is.na(reliability_summary$Omega_Total) ~
      "Keine Entscheidung möglich; Reliabilität konnte nicht geschätzt werden.",
    
    reliability_summary$Omega_Total >= reliability_threshold ~
      paste0(
        "Kein Itemausschluss aufgrund des Schwellenwerts erforderlich; ",
        "Omega total liegt bei oder über ",
        reliability_threshold,
        "."
      ),
    
    nrow(best_item_deletion) > 0 &&
      best_item_deletion$Omega_Total[[1]] >= reliability_threshold ~
      paste0(
        "Die präregistrierte Ausschlussoption ist rechnerisch relevant. ",
        "Vor einer Änderung müssen Iteminhalt und Skalenvalidität geprüft ",
        "und die Entscheidung dokumentiert werden."
      ),
    
    TRUE ~
      paste0(
        "Kein einzelner Itemausschluss hebt Omega total auf den ",
        "Referenzwert. Skala unverändert berichten und Einschränkung ",
        "diskutieren."
      )
  )
)


incidentality_interitem_correlations <- if (
  reliability_data_valid(
    incidentality_items_complete
  )
) {
  cor(
    incidentality_items_complete,
    use = "complete.obs",
    method = "pearson"
  ) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(
      "Item"
    ) %>%
    as_tibble()
  
} else {
  tibble(
    Note = paste0(
      "Inter-Item-Korrelationen konnten wegen zu weniger vollständiger ",
      "Fälle oder fehlender Itemvarianz nicht berechnet werden."
    )
  )
}

#===============================================================================

incidentality_items <- screening %>%
  select(
    all_of(
      incidentality_index_items
    )
  )


# Die Reliabilitätsanalysen werden mit denselben vollständigen Fällen
# durchgeführt, auf denen auch der Index basiert.
incidentality_items_complete <- incidentality_items %>%
  drop_na()


incidentality_alpha <- tryCatch(
  psych::alpha(
    incidentality_items_complete,
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
# psych::omega() gibt bei nfactors = 1 einen Konsolenhinweis zu Omega_h aus.
# Dieser wird abgefangen; ausgewertet wird ausschließlich Omega total.

incidentality_omega <- tryCatch(
  {
    omega_result <- NULL
    
    invisible(
      capture.output(
        omega_result <- suppressWarnings(
          suppressMessages(
            psych::omega(
              incidentality_items_complete,
              nfactors = 1,
              plot = FALSE
            )
          )
        ),
        type = "output"
      )
    )
    
    omega_result
  },
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
    length(
      incidentality_index_items
    ),
  
  N_Complete = nrow(
    incidentality_items_complete
  ),
  
  Cronbach_Alpha = if (
    is.null(
      incidentality_alpha
    )
  ) {
    NA_real_
  } else {
    unname(
      incidentality_alpha$total$raw_alpha
    )
  },
  
  Standardized_Alpha = if (
    is.null(
      incidentality_alpha
    )
  ) {
    NA_real_
  } else {
    unname(
      incidentality_alpha$total$std.alpha
    )
  },
  
  Omega_Total = if (
    is.null(
      incidentality_omega
    )
  ) {
    NA_real_
  } else {
    unname(
      incidentality_omega$omega.tot
    )
  },
  
  Note = paste0(
    "Berichtet wird Omega total. Omega hierarchical ist bei einem ",
    "eindimensionalen Ein-Faktor-Modell nicht sinnvoll interpretierbar."
  )
)


alpha_item_statistics <- if (
  is.null(
    incidentality_alpha
  )
) {
  
  tibble(
    Item = character(),
    N = numeric(),
    Raw_R = numeric(),
    Standardized_R = numeric(),
    Corrected_Item_Total_R = numeric(),
    Mean = numeric(),
    SD = numeric(),
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


incidentality_interitem_correlations <- if (
  nrow(
    incidentality_items_complete
  ) >= 3
) {
  
  cor(
    incidentality_items_complete,
    use = "complete.obs",
    method = "pearson"
  ) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(
      "Item"
    ) %>%
    as_tibble()
  
} else {
  
  tibble(
    Note = paste0(
      "Inter-Item-Korrelationen konnten wegen zu weniger ",
      "vollständiger Fälle nicht berechnet werden."
    )
  )
}


# 16 Overall sample description
#===============================================================================

sample_overview <- tibble(
  Indicator = c(
    "Zeilen in ursprünglicher Screening-Datei",
    "Eindeutige Codes in ursprünglicher Screening-Datei",
    "Vorläufig teilnahmeberechtigte Zeilen",
    "Eindeutige vorläufig teilnahmeberechtigte Personen",
    "Zeilen ohne gültigen Participant Code",
    "Doppelte Participant Codes",
    "Personen mit vollständigem Incidentality-Index",
    "Personen mit nicht klassifizierbarem anderem Bildungsabschluss"
  ),
  
  Value = c(
    nrow(
      screening_raw
    ),
    
    n_distinct(
      participant_code_check$participant,
      na.rm = TRUE
    ),
    
    nrow(
      screening
    ),
    
    n_distinct(
      screening$participant,
      na.rm = TRUE
    ),
    
    nrow(
      missing_participant_codes
    ),
    
    nrow(
      duplicate_participants
    ),
    
    sum(
      !is.na(
        screening$incidentality_index
      )
    ),
    
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
    participant,
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
    
    Used_At_All = case_when(
      is.na(Usage_Frequency) ~ NA,
      Usage_Frequency > 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    Weekly_Use = case_when(
      is.na(Usage_Frequency) ~ NA,
      Usage_Frequency >= 5 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    Daily_Use = case_when(
      is.na(Usage_Frequency) ~ NA,
      Usage_Frequency >= 7 ~ TRUE,
      TRUE ~ FALSE
    )
  )


platform_descriptives <- platform_long %>%
  group_by(
    Platform
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(
        Usage_Frequency
      )
    ),
    
    N_Missing = sum(
      is.na(
        Usage_Frequency
      )
    ),
    
    Mean = safe_mean(
      Usage_Frequency
    ),
    
    SD = safe_sd(
      Usage_Frequency
    ),
    
    Median = safe_median(
      Usage_Frequency
    ),
    
    Minimum = safe_min(
      Usage_Frequency
    ),
    
    Maximum = safe_max(
      Usage_Frequency
    ),
    
    .groups = "drop"
  )


platform_weekly_summary <- platform_long %>%
  group_by(
    Platform
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(
        Weekly_Use
      )
    ),
    
    N_Weekly = sum(
      Weekly_Use %in% TRUE,
      na.rm = TRUE
    ),
    
    N_Less_Than_Weekly = sum(
      Weekly_Use %in% FALSE,
      na.rm = TRUE
    ),
    
    Percent_Weekly = safe_percent(
      N_Weekly,
      N_Valid
    ),
    
    .groups = "drop"
  )


# Für Tabellen und Grafiken werden die gültigen Antwortkategorien vollständig
# ergänzt. Dadurch erscheinen auch Kategorien mit N = 0.

platform_distribution <- platform_long %>%
  filter(
    !is.na(
      Usage_Frequency
    )
  ) %>%
  count(
    Platform,
    Usage_Frequency,
    name = "N"
  ) %>%
  tidyr::complete(
    Platform,
    Usage_Frequency = 1:8,
    fill = list(
      N = 0
    )
  ) %>%
  group_by(
    Platform
  ) %>%
  mutate(
    N_Valid = sum(
      N
    ),
    
    Percent_Valid = safe_percent(
      N,
      N_Valid
    ),
    
    Usage_Frequency_Label = factor(
      Usage_Frequency,
      levels = 1:8,
      labels = frequency_levels,
      ordered = TRUE
    )
  ) %>%
  ungroup() %>%
  arrange(
    Platform,
    Usage_Frequency
  )


# 19 Information needs
#===============================================================================

information_need_labels <- c(
  intro_ib_undirected =
    "Ungerichtetes Informationsbedürfnis",
  
  intro_ib_thematic =
    "Thematisches Informationsbedürfnis",
  
  intro_ib_social =
    "Soziales Informationsbedürfnis",
  
  intro_ib_problem =
    "Problembezogenes Informationsbedürfnis"
)


information_needs_long <- screening %>%
  select(
    participant,
    all_of(
      names(
        information_need_labels
      )
    )
  ) %>%
  pivot_longer(
    cols = all_of(
      names(
        information_need_labels
      )
    ),
    names_to = "Information_Need_Variable",
    values_to = "Importance"
  ) %>%
  mutate(
    Information_Need = factor(
      information_need_labels[
        Information_Need_Variable
      ],
      levels = unname(
        information_need_labels
      )
    )
  )


information_needs_descriptives <- information_needs_long %>%
  group_by(
    Information_Need
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(
        Importance
      )
    ),
    
    N_Missing = sum(
      is.na(
        Importance
      )
    ),
    
    Mean = safe_mean(
      Importance
    ),
    
    SD = safe_sd(
      Importance
    ),
    
    Median = safe_median(
      Importance
    ),
    
    Minimum = safe_min(
      Importance
    ),
    
    Maximum = safe_max(
      Importance
    ),
    
    CI95_Lower = {
      n_value <- sum(
        !is.na(
          Importance
        )
      )
      
      mean_value <- safe_mean(
        Importance
      )
      
      sd_value <- safe_sd(
        Importance
      )
      
      if (
        n_value > 1 &&
        !is.na(
          sd_value
        )
      ) {
        mean_value -
          qt(
            0.975,
            df = n_value - 1
          ) *
          sd_value /
          sqrt(
            n_value
          )
      } else {
        NA_real_
      }
    },
    
    CI95_Upper = {
      n_value <- sum(
        !is.na(
          Importance
        )
      )
      
      mean_value <- safe_mean(
        Importance
      )
      
      sd_value <- safe_sd(
        Importance
      )
      
      if (
        n_value > 1 &&
        !is.na(
          sd_value
        )
      ) {
        mean_value +
          qt(
            0.975,
            df = n_value - 1
          ) *
          sd_value /
          sqrt(
            n_value
          )
      } else {
        NA_real_
      }
    },
    
    .groups = "drop"
  )


information_needs_distribution <- information_needs_long %>%
  filter(
    !is.na(
      Importance
    )
  ) %>%
  count(
    Information_Need,
    Importance,
    name = "N"
  ) %>%
  tidyr::complete(
    Information_Need,
    Importance = 1:5,
    fill = list(
      N = 0
    )
  ) %>%
  group_by(
    Information_Need
  ) %>%
  mutate(
    N_Valid = sum(
      N
    ),
    
    Percent_Valid = safe_percent(
      N,
      N_Valid
    )
  ) %>%
  ungroup() %>%
  arrange(
    Information_Need,
    Importance
  )


# 20 Incidentality
#===============================================================================

incidentality_index_summary <- continuous_summary(
  screening,
  "incidentality_index",
  "Incidentality-Index"
)


incidentality_item_labels <- c(
  intro_incidentality_1 =
    "Item 1",
  
  intro_incidentality_2 =
    "Item 2",
  
  intro_incidentality_3 =
    "Item 3",
  
  intro_incidentality_4 =
    "Item 4",
  
  intro_incidentality_5_reversed =
    "Item 5, invertiert",
  
  intro_incidentality_6 =
    "Item 6"
)


# Skalenorientierte Fassung: Item 5 ist invertiert, sodass höhere Werte bei
# allen Items eine stärker inzidentelle Informationsnutzung anzeigen.

incidentality_scored_items_long <- screening %>%
  select(
    participant,
    all_of(
      incidentality_index_items
    )
  ) %>%
  pivot_longer(
    cols = all_of(
      incidentality_index_items
    ),
    names_to = "Item_Variable",
    values_to = "Response"
  ) %>%
  mutate(
    Item = factor(
      incidentality_item_labels[
        Item_Variable
      ],
      levels = unname(
        incidentality_item_labels
      )
    )
  )


incidentality_item_descriptives <- incidentality_scored_items_long %>%
  group_by(
    Item
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(
        Response
      )
    ),
    
    N_Missing = sum(
      is.na(
        Response
      )
    ),
    
    Mean = safe_mean(
      Response
    ),
    
    SD = safe_sd(
      Response
    ),
    
    Median = safe_median(
      Response
    ),
    
    Minimum = safe_min(
      Response
    ),
    
    Maximum = safe_max(
      Response
    ),
    
    .groups = "drop"
  )


incidentality_scored_distribution <- incidentality_scored_items_long %>%
  filter(
    !is.na(
      Response
    )
  ) %>%
  count(
    Item,
    Response,
    name = "N"
  ) %>%
  tidyr::complete(
    Item,
    Response = 1:5,
    fill = list(
      N = 0
    )
  ) %>%
  group_by(
    Item
  ) %>%
  mutate(
    N_Valid = sum(
      N
    ),
    
    Percent_Valid = safe_percent(
      N,
      N_Valid
    )
  ) %>%
  ungroup() %>%
  arrange(
    Item,
    Response
  )


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
      round(
        Mean,
        2
      ),
      " (",
      round(
        SD,
        2
      ),
      ")"
    ),
    
    N = N_Valid
  )


table_1_gender <- screening %>%
  filter(
    !is.na(
      gender
    )
  ) %>%
  count(
    gender,
    .drop = FALSE,
    name = "N"
  ) %>%
  mutate(
    Percent = safe_percent(
      N,
      sum(
        N
      )
    )
  ) %>%
  transmute(
    Variable = "Geschlecht",
    Category = as.character(
      gender
    ),
    
    Statistic = paste0(
      N,
      " (",
      round(
        Percent,
        1
      ),
      "%)"
    ),
    
    N
  )


table_1_education <- screening %>%
  filter(
    !is.na(
      education_three_level
    )
  ) %>%
  count(
    education_three_level,
    .drop = FALSE,
    name = "N"
  ) %>%
  mutate(
    Percent = safe_percent(
      N,
      sum(
        N
      )
    )
  ) %>%
  transmute(
    Variable = "Bildung, dreistufig",
    Category = as.character(
      education_three_level
    ),
    
    Statistic = paste0(
      N,
      " (",
      round(
        Percent,
        1
      ),
      "%)"
    ),
    
    N
  )


table_1 <- bind_rows(
  table_1_continuous,
  table_1_gender,
  table_1_education
)


# 24 Exploratory platform profiles
#===============================================================================

platform_profile <- platform_long %>%
  group_by(
    participant
  ) %>%
  summarise(
    N_Platforms_Used = if (
      all(
        is.na(
          Used_At_All
        )
      )
    ) {
      NA_integer_
    } else {
      sum(
        Used_At_All %in% TRUE,
        na.rm = TRUE
      )
    },
    
    N_Platforms_Weekly = if (
      all(
        is.na(
          Weekly_Use
        )
      )
    ) {
      NA_integer_
    } else {
      sum(
        Weekly_Use %in% TRUE,
        na.rm = TRUE
      )
    },
    
    N_Platforms_Daily = if (
      all(
        is.na(
          Daily_Use
        )
      )
    ) {
      NA_integer_
    } else {
      sum(
        Daily_Use %in% TRUE,
        na.rm = TRUE
      )
    },
    
    Maximum_Usage_Frequency = safe_max(
      Usage_Frequency
    ),
    
    Primary_Platform = {
      
      if (
        all(
          is.na(
            Usage_Frequency
          )
        )
      ) {
        
        NA_character_
        
      } else {
        
        maximum_frequency <- max(
          Usage_Frequency,
          na.rm = TRUE
        )
        
        if (maximum_frequency <= 1) {
          "Keine Plattform genutzt"
        } else {
          paste(
            as.character(
              Platform[
                !is.na(
                  Usage_Frequency
                ) &
                  Usage_Frequency ==
                  maximum_frequency &
                  Usage_Frequency > 1
              ]
            ),
            collapse = " / "
          )
        }
      }
    },
    
    Weekly_Platform_Combination = {
      
      weekly_platforms <- as.character(
        Platform[
          Weekly_Use %in% TRUE
        ]
      )
      
      if (
        all(
          is.na(
            Weekly_Use
          )
        )
      ) {
        
        NA_character_
        
      } else if (
        length(
          weekly_platforms
        ) == 0
      ) {
        
        "Keine Plattform wöchentlich"
        
      } else {
        
        paste(
          weekly_platforms,
          collapse = " + "
        )
      }
    },
    
    .groups = "drop"
  )


screening <- screening %>%
  left_join(
    platform_profile,
    by = "participant"
  ) %>%
  mutate(
    Platform_Repertoire = case_when(
      is.na(
        N_Platforms_Weekly
      ) ~ NA_character_,
      
      N_Platforms_Weekly == 0 ~
        "Keine Plattform wöchentlich",
      
      N_Platforms_Weekly == 1 ~
        "Eine Plattform wöchentlich",
      
      N_Platforms_Weekly >= 2 ~
        "Mehrere Plattformen wöchentlich"
    ),
    
    Platform_Repertoire = factor(
      Platform_Repertoire,
      levels = c(
        "Keine Plattform wöchentlich",
        "Eine Plattform wöchentlich",
        "Mehrere Plattformen wöchentlich"
      )
    ),
    
    age_group = cut(
      intro_age_num,
      breaks = c(
        59,
        64,
        69,
        74,
        Inf
      ),
      labels = c(
        "60–64 Jahre",
        "65–69 Jahre",
        "70–74 Jahre",
        "75 Jahre und älter"
      ),
      right = TRUE,
      ordered_result = TRUE
    )
  )


eligibility_consistency <- screening %>%
  transmute(
    participant,
    Stop_Usage_Passed = intro_stop_usage_logical,
    N_Platforms_Weekly,
    
    Detailed_Frequencies_Support_Eligibility = case_when(
      is.na(N_Platforms_Weekly) ~ NA,
      N_Platforms_Weekly >= 1 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    Consistency_Status = case_when(
      is.na(N_Platforms_Weekly) ~
        "Nicht prüfbar: alle Plattformfrequenzen fehlen",
      
      N_Platforms_Weekly >= 1 ~
        "Konsistent",
      
      TRUE ~
        paste0(
          "Inkonsistent: Stop-Item bestanden, aber keine Plattform ",
          "mindestens wöchentlich angegeben"
        )
    )
  )


screening <- screening %>%
  left_join(
    eligibility_consistency %>%
      transmute(
        participant,
        platform_eligibility_consistent =
          Detailed_Frequencies_Support_Eligibility,
        platform_eligibility_status =
          Consistency_Status
      ),
    by = "participant"
  )


n_eligibility_inconsistencies <- eligibility_consistency %>%
  summarise(
    N = sum(
      Detailed_Frequencies_Support_Eligibility %in% FALSE,
      na.rm = TRUE
    )
  ) %>%
  pull(N)


if (n_eligibility_inconsistencies > 0) {
  warning(
    n_eligibility_inconsistencies,
    " teilnahmeberechtigte Personen weisen in den detaillierten ",
    "Plattformfrequenzen keine mindestens wöchentliche Nutzung auf. ",
    "Siehe 'Eligibility_Consistency'."
  )
}


sample_overview <- bind_rows(
  sample_overview,
  tibble(
    Indicator = c(
      "Inkonsistenzen zwischen Nutzungs-Stop-Item und Plattformfrequenzen",
      "Nicht prüfbare Plattform-Eligibility wegen vollständig fehlender Frequenzen"
    ),
    Value = c(
      n_eligibility_inconsistencies,
      sum(
        is.na(
          eligibility_consistency$Detailed_Frequencies_Support_Eligibility
        )
      )
    )
  )
)


platform_repertoire_summary <- screening %>%
  filter(
    !is.na(
      N_Platforms_Weekly
    )
  ) %>%
  count(
    N_Platforms_Weekly,
    name = "N"
  ) %>%
  tidyr::complete(
    N_Platforms_Weekly = 0:4,
    fill = list(
      N = 0
    )
  ) %>%
  arrange(
    N_Platforms_Weekly
  )


primary_platform_summary <- screening %>%
  mutate(
    Primary_Platform = replace_na(
      Primary_Platform,
      "Keine Angabe"
    )
  ) %>%
  count(
    Primary_Platform,
    sort = TRUE,
    name = "N"
  )


weekly_platform_combination_summary <- screening %>%
  mutate(
    Weekly_Platform_Combination = replace_na(
      Weekly_Platform_Combination,
      "Keine Angabe"
    )
  ) %>%
  count(
    Weekly_Platform_Combination,
    sort = TRUE,
    name = "N"
  )


platform_profile_summary <- screening %>%
  summarise(
    N = n(),
    
    Mean_Platforms_Used = safe_mean(
      N_Platforms_Used
    ),
    
    SD_Platforms_Used = safe_sd(
      N_Platforms_Used
    ),
    
    Median_Platforms_Used = safe_median(
      N_Platforms_Used
    ),
    
    Mean_Platforms_Weekly = safe_mean(
      N_Platforms_Weekly
    ),
    
    SD_Platforms_Weekly = safe_sd(
      N_Platforms_Weekly
    ),
    
    Median_Platforms_Weekly = safe_median(
      N_Platforms_Weekly
    ),
    
    Mean_Platforms_Daily = safe_mean(
      N_Platforms_Daily
    ),
    
    SD_Platforms_Daily = safe_sd(
      N_Platforms_Daily
    ),
    
    Median_Platforms_Daily = safe_median(
      N_Platforms_Daily
    )
  )


weekly_platform_wide <- platform_long %>%
  select(
    participant,
    Platform,
    Weekly_Use
  ) %>%
  mutate(
    Platform = as.character(
      Platform
    )
  ) %>%
  pivot_wider(
    names_from = Platform,
    values_from = Weekly_Use
  )


platform_names <- c(
  "Facebook",
  "Instagram",
  "TikTok",
  "X"
)


platform_pair_list <- combn(
  platform_names,
  2,
  simplify = FALSE
)


platform_co_use_summary <- purrr::map_dfr(
  platform_pair_list,
  function(platform_pair) {
    
    platform_1 <- platform_pair[[1]]
    platform_2 <- platform_pair[[2]]
    
    tibble(
      Platform_1 = platform_1,
      Platform_2 = platform_2,
      
      N_Valid_Pairs = sum(
        !is.na(
          weekly_platform_wide[[platform_1]]
        ) &
          !is.na(
            weekly_platform_wide[[platform_2]]
          )
      ),
      
      N_Using_Both_Weekly = sum(
        weekly_platform_wide[[platform_1]] %in% TRUE &
          weekly_platform_wide[[platform_2]] %in% TRUE,
        na.rm = TRUE
      )
    ) %>%
      mutate(
        Percent_Using_Both_Weekly = safe_percent(
          N_Using_Both_Weekly,
          N_Valid_Pairs
        )
      )
  }
)


# 25 Exploratory information-need profiles
#===============================================================================

information_need_profiles <- information_needs_long %>%
  group_by(
    participant
  ) %>%
  summarise(
    Mean_Importance = safe_mean(
      Importance
    ),
    
    Maximum_Importance = safe_max(
      Importance
    ),
    
    Minimum_Importance = safe_min(
      Importance
    ),
    
    Need_Differentiation =
      Maximum_Importance -
      Minimum_Importance,
    
    Number_of_High_Needs = if (
      all(
        is.na(
          Importance
        )
      )
    ) {
      NA_integer_
    } else {
      sum(
        Importance >= 4,
        na.rm = TRUE
      )
    },
    
    Number_of_Top_Needs = if (
      all(
        is.na(
          Importance
        )
      )
    ) {
      NA_integer_
    } else {
      sum(
        Importance ==
          Maximum_Importance,
        na.rm = TRUE
      )
    },
    
    Dominant_Information_Need = case_when(
      all(
        is.na(
          Importance
        )
      ) ~ NA_character_,
      
      Number_of_Top_Needs > 1 ~
        "Kein eindeutiges dominantes Bedürfnis",
      
      TRUE ~ as.character(
        Information_Need[
          which.max(
            Importance
          )
        ]
      )
    ),
    
    .groups = "drop"
  )


screening <- screening %>%
  left_join(
    information_need_profiles,
    by = "participant"
  )


dominant_information_need_summary <- screening %>%
  mutate(
    Dominant_Information_Need = replace_na(
      Dominant_Information_Need,
      "Keine Angabe"
    )
  ) %>%
  count(
    Dominant_Information_Need,
    sort = TRUE,
    name = "N"
  )


information_need_profile_summary <- screening %>%
  summarise(
    N = n(),
    
    Mean_Need_Importance = safe_mean(
      Mean_Importance
    ),
    
    SD_Need_Importance = safe_sd(
      Mean_Importance
    ),
    
    Mean_Need_Differentiation = safe_mean(
      Need_Differentiation
    ),
    
    SD_Need_Differentiation = safe_sd(
      Need_Differentiation
    ),
    
    Mean_Number_of_High_Needs = safe_mean(
      Number_of_High_Needs
    ),
    
    SD_Number_of_High_Needs = safe_sd(
      Number_of_High_Needs
    )
  )


# 26 Incidentality item diagnostics
#===============================================================================

incidentality_raw_item_labels <- c(
  intro_incidentality_1 =
    "Item 1",
  
  intro_incidentality_2 =
    "Item 2",
  
  intro_incidentality_3 =
    "Item 3",
  
  intro_incidentality_4 =
    "Item 4",
  
  intro_incidentality_5 =
    "Item 5, negativ formuliert",
  
  intro_incidentality_6 =
    "Item 6"
)


incidentality_raw_items_long <- screening %>%
  select(
    participant,
    all_of(
      names(
        incidentality_raw_item_labels
      )
    )
  ) %>%
  pivot_longer(
    cols = all_of(
      names(
        incidentality_raw_item_labels
      )
    ),
    names_to = "Item_Variable",
    values_to = "Response"
  ) %>%
  mutate(
    Item = factor(
      incidentality_raw_item_labels[
        Item_Variable
      ],
      levels = unname(
        incidentality_raw_item_labels
      )
    )
  )


incidentality_raw_distribution <- incidentality_raw_items_long %>%
  filter(
    !is.na(
      Response
    )
  ) %>%
  count(
    Item,
    Response,
    name = "N"
  ) %>%
  tidyr::complete(
    Item,
    Response = 1:5,
    fill = list(
      N = 0
    )
  ) %>%
  group_by(
    Item
  ) %>%
  mutate(
    N_Valid = sum(
      N
    ),
    
    Percent_Valid = safe_percent(
      N,
      N_Valid
    )
  ) %>%
  ungroup() %>%
  arrange(
    Item,
    Response
  )


# Boden- und Deckeneffekte werden auf den richtungsbereinigten Items berechnet.
# Damit bedeutet Antwort 5 bei allen Items eine hohe Incidentality-Ausprägung.

incidentality_item_floor_ceiling <- incidentality_scored_items_long %>%
  group_by(
    Item
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(
        Response
      )
    ),
    
    N_Response_1 = sum(
      Response == 1,
      na.rm = TRUE
    ),
    
    Percent_Response_1 = safe_percent(
      N_Response_1,
      N_Valid
    ),
    
    N_Response_5 = sum(
      Response == 5,
      na.rm = TRUE
    ),
    
    Percent_Response_5 = safe_percent(
      N_Response_5,
      N_Valid
    ),
    
    .groups = "drop"
  )


incidentality_index_floor_ceiling <- screening %>%
  summarise(
    N_Valid = sum(
      !is.na(
        incidentality_index
      )
    ),
    
    N_At_Minimum = sum(
      incidentality_index == 1,
      na.rm = TRUE
    ),
    
    Percent_At_Minimum = safe_percent(
      N_At_Minimum,
      N_Valid
    ),
    
    N_At_Maximum = sum(
      incidentality_index == 5,
      na.rm = TRUE
    ),
    
    Percent_At_Maximum = safe_percent(
      N_At_Maximum,
      N_Valid
    )
  )


# 27 Descriptive subgroup summaries
#===============================================================================

summarise_incidentality_by_group <- function(
    data,
    group_variable,
    group_label
) {
  
  data %>%
    transmute(
      Group = as.character(
        .data[[
          group_variable
        ]]
      ),
      
      Incidentality_Index =
        incidentality_index
    ) %>%
    filter(
      !is.na(
        Group
      )
    ) %>%
    group_by(
      Group
    ) %>%
    summarise(
      N = sum(
        !is.na(
          Incidentality_Index
        )
      ),
      
      Mean = safe_mean(
        Incidentality_Index
      ),
      
      SD = safe_sd(
        Incidentality_Index
      ),
      
      Median = safe_median(
        Incidentality_Index
      ),
      
      Minimum = safe_min(
        Incidentality_Index
      ),
      
      Maximum = safe_max(
        Incidentality_Index
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      Grouping_Variable = group_label,
      .before = 1
    )
}


incidentality_subgroup_summary <- bind_rows(
  summarise_incidentality_by_group(
    screening,
    "age_group",
    "Altersgruppe"
  ),
  
  summarise_incidentality_by_group(
    screening,
    "gender",
    "Geschlecht"
  ),
  
  summarise_incidentality_by_group(
    screening,
    "education_three_level",
    "Bildungsniveau"
  ),
  
  summarise_incidentality_by_group(
    screening,
    "context_local",
    "Typischer räumlicher Kontext"
  ),
  
  summarise_incidentality_by_group(
    screening,
    "context_social",
    "Typischer sozialer Kontext"
  ),
  
  summarise_incidentality_by_group(
    screening,
    "Platform_Repertoire",
    "Plattformrepertoire"
  ),
  
  summarise_incidentality_by_group(
    screening,
    "Primary_Platform",
    "Primärplattform"
  )
)


#===============================================================================
# 28A Exploratory subgroup summaries of central screening measures
#===============================================================================

screening_measure_labels <- c(
  intro_intensity = "Nutzungsintensität",
  intro_ib_undirected = "Informationsbedürfnis: ungerichtet",
  intro_ib_thematic = "Informationsbedürfnis: thematisch",
  intro_ib_social = "Informationsbedürfnis: sozial",
  intro_ib_problem = "Informationsbedürfnis: problembezogen",
  incidentality_index = "Incidentality-Index"
)


summarise_screening_measures_by_group <- function(
    data,
    group_variable,
    group_label
) {
  data %>%
    transmute(
      Group = as.character(
        .data[[group_variable]]
      ),
      across(
        all_of(
          names(screening_measure_labels)
        )
      )
    ) %>%
    filter(
      !is.na(Group)
    ) %>%
    pivot_longer(
      cols = all_of(
        names(screening_measure_labels)
      ),
      names_to = "Measure_Variable",
      values_to = "Value"
    ) %>%
    mutate(
      Measure = unname(
        screening_measure_labels[
          Measure_Variable
        ]
      )
    ) %>%
    group_by(
      Group,
      Measure
    ) %>%
    summarise(
      N_Valid = sum(
        !is.na(Value)
      ),
      Mean = safe_mean(Value),
      SD = safe_sd(Value),
      Median = safe_median(Value),
      Minimum = safe_min(Value),
      Maximum = safe_max(Value),
      .groups = "drop"
    ) %>%
    mutate(
      Grouping_Variable = group_label,
      .before = 1
    )
}


exploratory_subgroup_summary <- bind_rows(
  summarise_screening_measures_by_group(
    screening,
    "age_group",
    "Altersgruppe"
  ),
  summarise_screening_measures_by_group(
    screening,
    "gender",
    "Geschlecht"
  ),
  summarise_screening_measures_by_group(
    screening,
    "education_three_level",
    "Bildungsniveau"
  ),
  summarise_screening_measures_by_group(
    screening,
    "Platform_Repertoire",
    "Plattformrepertoire"
  )
)


#===============================================================================

# 28B Exploratory age correlations
#===============================================================================

age_marker_data <- screening %>%
  transmute(
    Alter = intro_age_num,
    Nutzungsintensität = intro_intensity,
    `Genutzte Plattformen` = N_Platforms_Used,
    `Wöchentlich genutzte Plattformen` = N_Platforms_Weekly,
    `Täglich genutzte Plattformen` = N_Platforms_Daily,
    `Informationsbedürfnis: ungerichtet` = intro_ib_undirected,
    `Informationsbedürfnis: thematisch` = intro_ib_thematic,
    `Informationsbedürfnis: sozial` = intro_ib_social,
    `Informationsbedürfnis: problembezogen` = intro_ib_problem,
    `Mittlere Wichtigkeit der Informationsbedürfnisse` = Mean_Importance,
    `Differenzierung der Informationsbedürfnisse` = Need_Differentiation,
    Incidentality = incidentality_index
  )


calculate_spearman_with_age <- function(
    data,
    marker
) {
  pair_data <- data %>%
    select(
      Alter,
      all_of(marker)
    ) %>%
    drop_na()
  
  if (
    nrow(pair_data) < 3 ||
    n_distinct(pair_data$Alter) < 2 ||
    n_distinct(pair_data[[marker]]) < 2
  ) {
    return(
      tibble(
        Marker = marker,
        N = nrow(pair_data),
        Spearman_Rho = NA_real_,
        P_Value = NA_real_
      )
    )
  }
  
  test <- suppressWarnings(
    cor.test(
      pair_data$Alter,
      pair_data[[marker]],
      method = "spearman",
      exact = FALSE
    )
  )
  
  tibble(
    Marker = marker,
    N = nrow(pair_data),
    Spearman_Rho = unname(
      test$estimate
    ),
    P_Value = test$p.value
  )
}


age_markers <- setdiff(
  names(age_marker_data),
  "Alter"
)


age_correlations <- purrr::map_dfr(
  age_markers,
  ~ calculate_spearman_with_age(
    age_marker_data,
    .x
  )
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(
      P_Value,
      method = "BH"
    ),
    Analysis_Type = "Explorativ",
    Note = paste0(
      "Alter wird kontinuierlich analysiert. Die Altersgruppen dienen nur ",
      "der deskriptiven Darstellung."
    )
  ) %>%
  arrange(
    desc(
      abs(Spearman_Rho)
    )
  )


#===============================================================================

#===============================================================================
# 28C Exploratory Spearman correlations with Incidentality
#===============================================================================

# Präregistrierungsnaher Fokus:
# Der Incidentality-Index wird mit Nutzungsintensität, Alter,
# Plattformnutzung und Informationsbedürfnissen in Beziehung gesetzt.
# Es werden nicht wahllos sämtliche Screening-Variablen untereinander getestet.

correlation_data <- screening %>%
  transmute(
    Incidentality =
      incidentality_index,
    
    Nutzungsintensität =
      intro_intensity,
    
    Alter =
      intro_age_num,
    
    `Genutzte Plattformen` =
      N_Platforms_Used,
    
    `Wöchentlich genutzte Plattformen` =
      N_Platforms_Weekly,
    
    `Täglich genutzte Plattformen` =
      N_Platforms_Daily,
    
    `Facebook-Nutzung` =
      intro_freq_facebook,
    
    `Instagram-Nutzung` =
      intro_freq_instagram,
    
    `TikTok-Nutzung` =
      intro_freq_tiktok,
    
    `X-Nutzung` =
      intro_freq_x,
    
    `Informationsbedürfnis: ungerichtet` =
      intro_ib_undirected,
    
    `Informationsbedürfnis: thematisch` =
      intro_ib_thematic,
    
    `Informationsbedürfnis: sozial` =
      intro_ib_social,
    
    `Informationsbedürfnis: problembezogen` =
      intro_ib_problem,
    
    `Mittlere Wichtigkeit der Informationsbedürfnisse` =
      Mean_Importance,
    
    `Differenzierung der Informationsbedürfnisse` =
      Need_Differentiation
  )


calculate_spearman_with_incidentality <- function(
    data,
    predictor
) {
  
  pair_data <- data %>%
    select(
      Incidentality,
      all_of(
        predictor
      )
    ) %>%
    drop_na()
  
  if (
    nrow(
      pair_data
    ) < 3 ||
    n_distinct(
      pair_data$Incidentality
    ) < 2 ||
    n_distinct(
      pair_data[[
        predictor
      ]]
    ) < 2
  ) {
    
    return(
      tibble(
        Predictor = predictor,
        N = nrow(
          pair_data
        ),
        Spearman_Rho = NA_real_,
        P_Value = NA_real_
      )
    )
  }
  
  test <- suppressWarnings(
    cor.test(
      pair_data$Incidentality,
      pair_data[[
        predictor
      ]],
      method = "spearman",
      exact = FALSE
    )
  )
  
  tibble(
    Predictor = predictor,
    N = nrow(
      pair_data
    ),
    Spearman_Rho = unname(
      test$estimate
    ),
    P_Value = test$p.value
  )
}


correlation_predictors <- setdiff(
  names(
    correlation_data
  ),
  "Incidentality"
)


exploratory_correlations <- purrr::map_dfr(
  correlation_predictors,
  ~ calculate_spearman_with_incidentality(
    data = correlation_data,
    predictor = .x
  )
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(
      P_Value,
      method = "BH"
    ),
    
    Analysis_Type = "Explorativ",
    
    Note = paste0(
      "P-Werte dienen nur der explorativen Orientierung; ",
      "maßgeblich sind Richtung, Größe und Unsicherheit der Zusammenhänge."
    )
  ) %>%
  arrange(
    desc(
      abs(
        Spearman_Rho
      )
    )
  )


# 29 Save prepared data
#===============================================================================

# Erst an dieser Stelle speichern: Nun enthält der Datensatz auch die
# explorativen Personenmerkmale wie Plattformrepertoire, Altersgruppe und
# Informationsbedürfnisprofil.

saveRDS(
  screening,
  output_rds
)


saveRDS(
  list(
    alpha = incidentality_alpha,
    omega = incidentality_omega,
    incidentality_items_complete =
      incidentality_items_complete,
    alpha_item_statistics =
      alpha_item_statistics,
    omega_if_item_deleted =
      omega_if_item_deleted,
    reliability_decision =
      reliability_decision,
    interitem_correlations =
      incidentality_interitem_correlations
  ),
  reliability_rds
)


# 30 Create Excel workbook
#===============================================================================

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)


analysis_settings <- tibble(
  Setting = c(
    "Script version",
    "Input file",
    "Eligibility",
    "Duplicate-code action",
    "Out-of-range action",
    "Weekly platform use",
    "Daily platform use",
    "Incidentality index",
    "Reliability sample",
    "Reliability threshold",
    "Omega reported",
    "Automatic item exclusion",
    "Exploratory correlations",
    "Multiplicity adjustment",
    "Create figures",
    "Overwrite outputs"
  ),
  
  Value = c(
    analysis_version,
    data_file,
    "Beide Screening-Stop-Filter müssen TRUE sein; Participant Code erforderlich",
    if_else(
      stop_on_duplicate_codes,
      "Stop",
      "Warnung und Fälle behalten"
    ),
    if_else(
      stop_on_out_of_range,
      "Stop",
      "Warnung und Werte behalten"
    ),
    "Nutzungsfrequenzcodes 5–8",
    "Nutzungsfrequenzcodes 7–8",
    "Mittelwert aller sechs Items; Item 5 invertiert; vollständige Fälle",
    "Dieselben vollständigen Fälle wie für den Index",
    as.character(reliability_threshold),
    "Omega total aus Ein-Faktor-Lösung",
    "Nein; Item-Deletion wird nur diagnostisch ausgegeben",
    "Spearman-Korrelationen mit Incidentality bzw. Alter als Fokus",
    "Benjamini-Hochberg innerhalb der jeweiligen Korrelationsfamilie",
    as.character(create_figures),
    as.character(overwrite_outputs)
  )
)


variable_codebook <- tribble(
  ~Variable, ~Construct, ~Coding_or_Range, ~Analysis_Role, ~Notes,
  "participant", "Pseudonymous identifier", "Cleaned character code", "Linkage and person-level key", "Derived from personalParticipantCode",
  "intro_stop_age", "Age eligibility", "Yes/No", "Eligibility gate", "Recoded with as_logical_safe()",
  "intro_stop_usage", "Usage eligibility", "Yes/No", "Eligibility gate", "At least weekly use of one investigated platform for publicly relevant information",
  "intro_age_num", "Age", "60 years and older expected", "Description and exploration", "Continuous age is preferred over arbitrary groups",
  "intro_gender", "Gender", "1 = female; 2 = male; 3 = diverse", "Description and exploration", "Verify export coding if questionnaire changes",
  "intro_education", "Highest educational qualification", "1–8", "Detailed description", "Code 8 = other qualification",
  "education_three_level", "Education, exploratory recode", "Low / Medium / High", "Exploratory subgroup variable", "Other qualification remains unclassified",
  "intro_freq_facebook", "Facebook-use frequency", "1 = never ... 8 = several times daily", "Platform profile", "Ordinal response scale",
  "intro_freq_instagram", "Instagram-use frequency", "1 = never ... 8 = several times daily", "Platform profile", "Ordinal response scale",
  "intro_freq_tiktok", "TikTok-use frequency", "1 = never ... 8 = several times daily", "Platform profile", "Ordinal response scale",
  "intro_freq_x", "X-use frequency", "1 = never ... 8 = several times daily", "Platform profile", "Ordinal response scale",
  "intro_intensity", "General social-media-use intensity", "1–7", "Description and exploration", "Interpret according to questionnaire anchors",
  "intro_ib_undirected", "Undirected information need", "1 = not important ... 5 = very important", "Information-needs profile", "News and current affairs orientation",
  "intro_ib_thematic", "Thematic information need", "1–5", "Information-needs profile", "Personal topics and interests",
  "intro_ib_social", "Social information need", "1–5", "Information-needs profile", "Updates from the social environment",
  "intro_ib_problem", "Problem-related information need", "1–5", "Information-needs profile", "Solving concrete problems",
  "intro_incidentality_1", "Screening incidentality item 1", "1 = strongly disagree ... 5 = strongly agree", "Incidentality scale", "Come across information that would otherwise be missed",
  "intro_incidentality_2", "Screening incidentality item 2", "1–5", "Incidentality scale", "Find information that would otherwise not be read",
  "intro_incidentality_3", "Screening incidentality item 3", "1–5", "Incidentality scale", "Stumble upon new and interesting information",
  "intro_incidentality_4", "Screening incidentality item 4", "1–5", "Incidentality scale", "Exposure to information not normally sought",
  "intro_incidentality_5", "Screening incidentality item 5", "1–5", "Incidentality scale", "Negatively worded and reverse-coded",
  "intro_incidentality_6", "Screening incidentality item 6", "1–5", "Incidentality scale", "Unintentionally read information posted by others",
  "incidentality_index", "General screening incidentality", "Mean 1–5", "Description and downstream calibration", "All six items required; item 5 reversed",
  "intro_context_local", "Typical spatial context", "1 = home; 2 = out; 3 = both similarly", "Description and downstream calibration", "General self-report",
  "intro_context_situation", "Typical social context", "1 = mostly alone; 2 = mostly together; 3 = both similarly", "Description and downstream calibration", "General self-report",
  "N_Platforms_Used", "Platform repertoire", "0–4", "Derived screening marker", "Frequency above never",
  "N_Platforms_Weekly", "Weekly platform repertoire", "0–4", "Derived screening marker", "Frequency code 5–8",
  "N_Platforms_Daily", "Daily platform repertoire", "0–4", "Derived screening marker", "Frequency code 7–8",
  "platform_eligibility_consistent", "Eligibility consistency", "TRUE / FALSE / missing", "Quality-control marker", "Compares weekly stop item with detailed frequency responses",
  "Primary_Platform", "Highest-frequency platform", "Platform name or tied names", "Derived screening marker", "May contain multiple platforms",
  "Need_Differentiation", "Within-person need differentiation", "Maximum minus minimum need score", "Exploratory profile marker", "Not a validated scale"
)


output_manifest <- tribble(
  ~Output, ~Type, ~Description,
  output_excel, "Excel", "All screening results, documentation and quality checks",
  output_rds, "RDS", "Prepared eligible participant-level screening data",
  reliability_rds, "RDS", "Reliability objects and item diagnostics",
  file.path(figure_folder, "Screening_Age.png"), "PNG", "Age distribution",
  file.path(figure_folder, "Screening_Gender.png"), "PNG", "Gender frequencies",
  file.path(figure_folder, "Screening_Education.png"), "PNG", "Three-level education frequencies",
  file.path(figure_folder, "Screening_Platform_Weekly.png"), "PNG", "Weekly platform use",
  file.path(figure_folder, "Screening_Platform_Frequencies.png"), "PNG", "Full platform-frequency distributions",
  file.path(figure_folder, "Screening_Usage_Intensity.png"), "PNG", "Usage-intensity distribution",
  file.path(figure_folder, "Screening_Information_Needs.png"), "PNG", "Information-need means and confidence intervals",
  file.path(figure_folder, "Screening_Information_Need_Distributions.png"), "PNG", "Information-need response distributions",
  file.path(figure_folder, "Screening_Incidentality_Index.png"), "PNG", "Incidentality-index distribution",
  file.path(figure_folder, "Screening_Incidentality_Items.png"), "PNG", "Directionally aligned item distributions",
  file.path(figure_folder, "Screening_Contexts.png"), "PNG", "Typical local and social contexts",
  file.path(figure_folder, "Screening_Platform_Repertoire.png"), "PNG", "Weekly platform-repertoire size",
  file.path(figure_folder, "Screening_Incidentality_Correlations.png"), "PNG", "Exploratory Incidentality correlations"
) %>%
  mutate(
    Created = case_when(
      Type == "PNG" ~ create_figures,
      TRUE ~ TRUE
    )
  )


add_excel_sheet(
  workbook,
  "Analysis_Settings",
  analysis_settings,
  header_style
)

add_excel_sheet(
  workbook,
  "Variable_Codebook",
  variable_codebook,
  header_style
)

add_excel_sheet(
  workbook,
  "Output_Manifest",
  output_manifest,
  header_style
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
  "Eligibility_Consistency",
  eligibility_consistency,
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
  "Missing_Codes",
  missing_participant_codes,
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
  "Range_Issues",
  range_issues,
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
  "Platform_Profile",
  platform_profile,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Repertoire",
  platform_repertoire_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Combinations",
  weekly_platform_combination_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_CoUse",
  platform_co_use_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Primary_Platform",
  primary_platform_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Profile_Summary",
  platform_profile_summary,
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
  "InfoNeed_Profiles",
  information_need_profiles,
  header_style
)

add_excel_sheet(
  workbook,
  "InfoNeed_Profile_Summary",
  information_need_profile_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Dominant_Info_Need",
  dominant_information_need_summary,
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
  "Incidentality_Item_Desc",
  incidentality_item_descriptives,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Scored_Dist",
  incidentality_scored_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Raw_Dist",
  incidentality_raw_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Floor_Ceiling",
  incidentality_item_floor_ceiling,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Index_Floor",
  incidentality_index_floor_ceiling,
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
  "Reliability_Decision",
  reliability_decision,
  header_style
)

add_excel_sheet(
  workbook,
  "Alpha_Item_Statistics",
  alpha_item_statistics,
  header_style
)

add_excel_sheet(
  workbook,
  "Omega_Item_Deleted",
  omega_if_item_deleted,
  header_style
)

add_excel_sheet(
  workbook,
  "Interitem_Correlations",
  incidentality_interitem_correlations,
  header_style
)

add_excel_sheet(
  workbook,
  "Contexts",
  context_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Subgroups",
  incidentality_subgroup_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Exploratory_Subgroups",
  exploratory_subgroup_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Exploratory_Correlations",
  exploratory_correlations,
  header_style
)

add_excel_sheet(
  workbook,
  "Age_Correlations",
  age_correlations,
  header_style
)


openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = overwrite_outputs
)


# 31 Visual design
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
  if (!create_figures) {
    return(
      invisible(NULL)
    )
  }
  
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


# 32 Figure: Age distribution
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


# 33 Figure: Gender
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


# 34 Figure: Education
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


# 35 Figure: Weekly platform use
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


# 36 Figure: Platform-use frequency distributions
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


# 37 Figure: Usage intensity
#===============================================================================

usage_intensity_plot_data <- screening %>%
  filter(
    !is.na(
      intro_intensity
    )
  ) %>%
  count(
    intro_intensity,
    name = "N"
  ) %>%
  tidyr::complete(
    intro_intensity = 1:7,
    fill = list(
      N = 0
    )
  )


figure_usage_intensity <- ggplot(
  usage_intensity_plot_data,
  aes(
    x = factor(
      intro_intensity,
      levels = 1:7
    ),
    y = N
  )
) +
  geom_col(
    width = 0.64,
    fill = unname(
      project_colors["primary"]
    )
  ) +
  geom_text(
    aes(
      label = N
    ),
    vjust = -0.4,
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    size = 3.5
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
  labs(
    title = "Nutzungsintensität",
    subtitle = "Absolute Häufigkeiten der Antwortkategorien",
    x = "Nutzungsintensität (1–7)",
    y = "Anzahl der Teilnehmenden"
  )


save_project_plot(
  figure_usage_intensity,
  "Screening_Usage_Intensity.png",
  width = 8,
  height = 5
)


# 38 Figure: Information-needs means
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
    breaks = 1:5
  ) +
  coord_cartesian(
    ylim = c(
      1,
      5
    )
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


# 39 Figure: Information-needs distributions
#===============================================================================

information_needs_distribution_plot_data <-
  information_needs_distribution %>%
  mutate(
    Importance_Label = factor(
      Importance,
      levels = 1:5,
      ordered = TRUE
    )
  )


figure_information_need_distributions <- ggplot(
  information_needs_distribution_plot_data,
  aes(
    x = N,
    y = Importance_Label
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
        as.character(
          N
        ),
        ""
      )
    ),
    hjust = -0.2,
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    size = 3.1
  ) +
  facet_wrap(
    ~ Information_Need,
    ncol = 2
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
  labs(
    title = "Informationsbedürfnisse",
    subtitle = "Absolute Häufigkeiten der Antwortkategorien",
    x = "Anzahl der Teilnehmenden",
    y = "Wichtigkeit (1–5)"
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
  figure_information_need_distributions,
  "Screening_Information_Need_Distributions.png",
  width = 11,
  height = 7
)


# 40 Figure: Incidentality index
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


# 41 Figure: Incidentality-item distributions
#===============================================================================

incidentality_item_plot_data <-
  incidentality_scored_distribution %>%
  mutate(
    Response_Label = factor(
      Response,
      levels = 1:5,
      ordered = TRUE
    )
  )


figure_incidentality_items <- ggplot(
  incidentality_item_plot_data,
  aes(
    x = N,
    y = Response_Label
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
        as.character(
          N
        ),
        ""
      )
    ),
    hjust = -0.2,
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    size = 3
  ) +
  facet_wrap(
    ~ Item,
    ncol = 2
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
  labs(
    title = "Items der Incidentality-Skala",
    subtitle = paste0(
      "Absolute Häufigkeiten; Item 5 wurde für diese Darstellung ",
      "invertiert"
    ),
    x = "Anzahl der Teilnehmenden",
    y = "Skalenwert (1–5)",
    caption = paste0(
      "Höhere Werte stehen bei allen dargestellten Items für eine ",
      "stärker inzidentelle Informationsnutzung."
    )
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
  figure_incidentality_items,
  "Screening_Incidentality_Items.png",
  width = 11,
  height = 9
)


# 42 Figure: Typical contexts
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


# 43 Figure: Platform repertoire
#===============================================================================

figure_platform_repertoire <- ggplot(
  platform_repertoire_summary,
  aes(
    x = factor(
      N_Platforms_Weekly,
      levels = 0:4
    ),
    y = N
  )
) +
  geom_col(
    width = 0.64,
    fill = unname(
      project_colors["primary"]
    )
  ) +
  geom_text(
    aes(
      label = N
    ),
    vjust = -0.4,
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    size = 3.5
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
  labs(
    title = "Plattformrepertoire",
    subtitle = paste0(
      "Absolute Häufigkeiten mindestens wöchentlich ",
      "genutzter Plattformen"
    ),
    x = "Anzahl wöchentlich genutzter Plattformen",
    y = "Anzahl der Teilnehmenden"
  )


save_project_plot(
  figure_platform_repertoire,
  "Screening_Platform_Repertoire.png",
  width = 8,
  height = 5
)


# 44 Figure: Exploratory correlations with Incidentality
#===============================================================================

correlation_plot_data <- exploratory_correlations %>%
  filter(
    !is.na(
      Spearman_Rho
    )
  ) %>%
  mutate(
    Predictor = forcats::fct_reorder(
      Predictor,
      Spearman_Rho
    ),
    
    Correlation_Label = scales::number(
      Spearman_Rho,
      accuracy = 0.01,
      decimal.mark = ","
    ),
    
    Label_Hjust = if_else(
      Spearman_Rho >= 0,
      -0.35,
      1.35
    )
  )


figure_correlations <- ggplot(
  correlation_plot_data,
  aes(
    x = Spearman_Rho,
    y = Predictor
  )
) +
  geom_vline(
    xintercept = 0,
    color = unname(
      project_colors["grid"]
    ),
    linewidth = 0.8
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = Spearman_Rho,
      yend = Predictor
    ),
    color = unname(
      project_colors["secondary"]
    ),
    linewidth = 1
  ) +
  geom_point(
    color = unname(
      project_colors["primary"]
    ),
    size = 3
  ) +
  geom_text(
    aes(
      label = Correlation_Label,
      hjust = Label_Hjust
    ),
    color = unname(
      project_colors["dark"]
    ),
    fontface = "bold",
    size = 3.2
  ) +
  scale_x_continuous(
    limits = c(
      -1,
      1
    ),
    breaks = seq(
      -1,
      1,
      by = 0.25
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Explorative Zusammenhänge mit Incidentality",
    subtitle = "Spearman-Korrelationen",
    x = "Spearman ρ",
    y = NULL,
    caption = paste0(
      "Explorative Analyse. Die zugehörigen rohen und ",
      "BH-adjustierten p-Werte stehen im Excel-Export."
    )
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
  figure_correlations,
  "Screening_Incidentality_Correlations.png",
  width = 10,
  height = 8
)


# 45 Console report
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
  nrow(
    screening_raw
  ),
  "\n",
  sep = ""
)

cat(
  "Provisionally eligible participants: ",
  n_distinct(
    screening$participant
  ),
  "\n",
  sep = ""
)

cat(
  "Mean age: ",
  round(
    age_summary$Mean,
    2
  ),
  " (SD = ",
  round(
    age_summary$SD,
    2
  ),
  ")\n",
  sep = ""
)

cat(
  "Mean weekly platform repertoire: ",
  round(
    platform_profile_summary$Mean_Platforms_Weekly,
    2
  ),
  "\n",
  sep = ""
)

cat(
  "Incidentality index: M = ",
  round(
    incidentality_index_summary$Mean,
    2
  ),
  ", SD = ",
  round(
    incidentality_index_summary$SD,
    2
  ),
  "\n",
  sep = ""
)

cat(
  "Cronbach's alpha: ",
  round(
    reliability_summary$Cronbach_Alpha,
    3
  ),
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
  "Exploratory Incidentality correlations: ",
  sum(
    !is.na(
      exploratory_correlations$Spearman_Rho
    )
  ),
  "\n",
  sep = ""
)

cat(
  "Eligibility consistency issues: ",
  n_eligibility_inconsistencies,
  "\n",
  sep = ""
)

cat(
  "Exploratory age correlations: ",
  sum(
    !is.na(
      age_correlations$Spearman_Rho
    )
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
  "\nReliability objects:\n",
  reliability_rds,
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
