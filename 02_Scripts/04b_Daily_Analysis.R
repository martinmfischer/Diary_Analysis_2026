################################################################################
# Project: Tagebuchstudie
# File:    04b_Daily_Analysis.R
#
# Inputs:
#   06_Coding/coding_sheet.xlsx
#   03_Output/screening_prepared.rds
#
# Outputs:
#   03_Output/Daily_Results.xlsx
#   03_Output/daily_screenshot_level.rds
#   03_Output/daily_participant_level.rds
#   03_Output/daily_exploratory_models.rds
#
# Optional simulation output:
#   06_Coding/coding_sheet_simulated.xlsx
#
# Figures:
#   04_Figures/Daily_*.png
#   or, when simulation is active:
#   04_Figures/Simulated_Daily/Daily_*.png
################################################################################

rm(list = ls())


#===============================================================================
# 01 Settings
#===============================================================================

# Set TRUE while the coding sheet is not yet completed. Missing manual coding
# values are then filled with reproducible synthetic values. The original coding
# sheet is never overwritten.
simulate_coding <- TRUE
simulation_seed <- 20260729
simulation_overwrite_existing <- FALSE
write_simulated_coding_sheet <- TRUE

# The simulation can use screening variables to create plausible test patterns.
# This is useful for checking plots and models, but simulated associations must
# never be interpreted substantively.
simulation_use_screening_patterns <- TRUE

# Inclusion follows the preregistration: complete screening plus at least seven
# uploaded screenshots during the diary period.
minimum_screenshots <- 7
require_screening_match <- TRUE

# Stop when the final, non-simulated coding sheet still contains incomplete or
# invalid manual codes. While simulation is active, missing codes are filled.
strict_coding_check <- TRUE

# Optional exploratory generalized linear mixed models. Models are only fitted
# when there are enough observations, participants and outcome events.
run_mixed_models <- TRUE
minimum_model_n <- 100
minimum_model_participants <- 20
minimum_model_events <- 15

coding_sheet_name <- "Coding"
expected_study_days <- 1:7


#===============================================================================
# 02 Packages
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  janitor,
  openxlsx,
  fs,
  scales,
  lme4,
  broom.mixed
)


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
base_figure_folder <- "04_Figures"
coding_folder <- "06_Coding"

if (simulate_coding) {
  output_excel <- file.path(
    output_folder,
    "Daily_Results_SIMULATED.xlsx"
  )
  
  output_screenshot_rds <- file.path(
    output_folder,
    "daily_screenshot_level_SIMULATED.rds"
  )
  
  output_participant_rds <- file.path(
    output_folder,
    "daily_participant_level_SIMULATED.rds"
  )
  
  output_models_rds <- file.path(
    output_folder,
    "daily_exploratory_models_SIMULATED.rds"
  )
  
  simulated_coding_file <- file.path(
    coding_folder,
    "coding_sheet_simulated.xlsx"
  )
  
  figure_folder <- file.path(
    base_figure_folder,
    "Simulated_Daily"
  )
  
} else {
  output_excel <- file.path(
    output_folder,
    "Daily_Results.xlsx"
  )
  
  output_screenshot_rds <- file.path(
    output_folder,
    "daily_screenshot_level.rds"
  )
  
  output_participant_rds <- file.path(
    output_folder,
    "daily_participant_level.rds"
  )
  
  output_models_rds <- file.path(
    output_folder,
    "daily_exploratory_models.rds"
  )
  
  simulated_coding_file <- NA_character_
  figure_folder <- base_figure_folder
}

fs::dir_create(output_folder)
fs::dir_create(figure_folder)
fs::dir_create(coding_folder)


#===============================================================================
# 04 General helper functions
#===============================================================================

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\u00a0", " ")
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "N/A", "NULL", "-1")] <- NA_character_
  x
}


clean_numeric <- function(x) {
  suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


clean_binary <- function(x) {
  raw <- stringr::str_to_lower(
    clean_text(x)
  )
  
  numeric_value <- clean_numeric(raw)
  
  case_when(
    raw %in% c("true", "yes", "ja", "j", "completed", "complete") ~ 1L,
    raw %in% c("false", "no", "nein", "n", "incomplete") ~ 0L,
    numeric_value == 1 ~ 1L,
    numeric_value == 0 ~ 0L,
    numeric_value == -1 ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}


safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}


safe_sd <- function(x) {
  if (sum(!is.na(x)) < 2) return(NA_real_)
  sd(x, na.rm = TRUE)
}


safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}


safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}


safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}


safe_divide <- function(numerator, denominator) {
  result <- numerator / denominator
  result[is.na(denominator) | denominator == 0] <- NA_real_
  result
}


safe_percent <- function(numerator, denominator) {
  100 * safe_divide(numerator, denominator)
}


safe_z <- function(x) {
  x <- clean_numeric(x)
  
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  
  if (sum(!is.na(x)) < 2 || isTRUE(all.equal(sd(x, na.rm = TRUE), 0))) {
    return(if_else(is.na(x), NA_real_, 0))
  }
  
  as.numeric(scale(x))
}


share_true <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x %in% TRUE, na.rm = TRUE)
}


share_value <- function(x, value) {
  valid <- !is.na(x)
  if (!any(valid)) return(NA_real_)
  mean(x[valid] == value)
}


n_distinct_valid <- function(x) {
  dplyr::n_distinct(x[!is.na(x)])
}


shannon_entropy <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  counts <- table(x, useNA = "no")
  
  # Nicht beobachtete Faktorstufen entfernen
  counts <- counts[counts > 0]
  
  probabilities <- counts / sum(counts)
  
  -sum(
    probabilities * log(probabilities)
  )
}


shannon_evenness <- function(x) {
  richness <- n_distinct_valid(x)
  if (richness == 0) return(NA_real_)
  if (richness == 1) return(0)
  shannon_entropy(x) / log(richness)
}


dominant_share <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(prop.table(table(x)))
}


profile_alignment <- function(screening_values, observed_values) {
  complete <- complete.cases(screening_values, observed_values)
  
  if (
    sum(complete) < 3 ||
    n_distinct(screening_values[complete]) < 2 ||
    n_distinct(observed_values[complete]) < 2
  ) {
    return(NA_real_)
  }
  
  suppressWarnings(
    cor(
      screening_values[complete],
      observed_values[complete],
      method = "spearman"
    )
  )
}


parse_study_day <- function(x) {
  numeric_direct <- clean_numeric(x)
  
  extracted <- suppressWarnings(
    as.numeric(
      stringr::str_extract(
        as.character(x),
        "[0-9]+"
      )
    )
  )
  
  dplyr::coalesce(
    numeric_direct,
    extracted
  )
}


rename_first_available <- function(
    data,
    target,
    candidates,
    required = FALSE
) {
  if (target %in% names(data)) return(data)
  
  available <- intersect(
    candidates,
    names(data)
  )
  
  if (length(available) > 0) {
    names(data)[names(data) == available[[1]]] <- target
  } else if (required) {
    stop(
      "Benötigte Variable '",
      target,
      "' wurde nicht gefunden. Geprüfte Alternativen: ",
      paste(candidates, collapse = ", ")
    )
  } else {
    data[[target]] <- NA
  }
  
  data
}


add_excel_sheet <- function(
    workbook,
    sheet_name,
    data,
    header_style
) {
  sheet_name <- stringr::str_sub(sheet_name, 1, 31)
  
  if (sheet_name %in% names(workbook)) {
    stop("Doppelter Excel-Blattname: ", sheet_name)
  }
  
  if (is.null(data)) {
    data <- tibble(Note = "Object is NULL")
  }
  
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  if (ncol(data) == 0) {
    data <- tibble(Note = "No columns available")
  }
  
  openxlsx::addWorksheet(
    workbook,
    sheet_name
  )
  
  openxlsx::writeData(
    workbook,
    sheet_name,
    data,
    headerStyle = header_style,
    withFilter = nrow(data) > 0
  )
  
  openxlsx::freezePane(
    workbook,
    sheet_name,
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
}


frequency_distribution <- function(
    data,
    variable,
    variable_label,
    categories = NULL
) {
  raw_values <- clean_text(data[[variable]])
  
  if (is.null(categories)) {
    categories <- sort(unique(raw_values[!is.na(raw_values)]))
  }
  
  n_total <- length(raw_values)
  n_valid <- sum(!is.na(raw_values))
  
  valid_counts <- tibble(Category = raw_values) %>%
    filter(!is.na(Category)) %>%
    count(Category, name = "N") %>%
    tidyr::complete(
      Category = categories,
      fill = list(N = 0)
    )
  
  missing_count <- tibble(
    Category = "Missing",
    N = sum(is.na(raw_values))
  )
  
  bind_rows(
    valid_counts,
    missing_count
  ) %>%
    mutate(
      Variable = variable_label,
      N_Total = n_total,
      N_Valid = n_valid,
      Percent_Total = safe_percent(N, n_total),
      Percent_Valid = if_else(
        Category == "Missing",
        NA_real_,
        safe_percent(N, n_valid)
      ),
      .before = 1
    )
}


make_participant_shares <- function(
    data,
    variable,
    categories,
    variable_label
) {
  participant_ids <- sort(unique(data$participant))
  
  counts <- data %>%
    transmute(
      participant,
      Category = clean_text(.data[[variable]])
    ) %>%
    filter(!is.na(Category)) %>%
    count(
      participant,
      Category,
      name = "N"
    )
  
  denominators <- data %>%
    transmute(
      participant,
      Value = clean_text(.data[[variable]])
    ) %>%
    group_by(participant) %>%
    summarise(
      Denominator = sum(!is.na(Value)),
      .groups = "drop"
    )
  
  tidyr::expand_grid(
    participant = participant_ids,
    Category = categories
  ) %>%
    left_join(
      counts,
      by = c("participant", "Category")
    ) %>%
    left_join(
      denominators,
      by = "participant"
    ) %>%
    mutate(
      N = replace_na(N, 0L),
      Share = safe_divide(N, Denominator),
      Variable = variable_label,
      .before = 1
    )
}


summarise_participant_shares <- function(participant_shares) {
  participant_shares %>%
    group_by(
      Variable,
      Category
    ) %>%
    summarise(
      N_Participants = sum(!is.na(Share)),
      Mean_Share = safe_mean(Share),
      SD_Share = safe_sd(Share),
      Median_Share = safe_median(Share),
      Minimum_Share = safe_min(Share),
      Maximum_Share = safe_max(Share),
      Mean_Percent = 100 * Mean_Share,
      .groups = "drop"
    )
}


cross_tabulation <- function(
    data,
    row_variable,
    column_variable,
    row_label,
    column_label
) {
  data %>%
    transmute(
      Row = clean_text(.data[[row_variable]]),
      Column = clean_text(.data[[column_variable]])
    ) %>%
    filter(
      !is.na(Row),
      !is.na(Column)
    ) %>%
    count(
      Row,
      Column,
      name = "N"
    ) %>%
    group_by(Row) %>%
    mutate(
      Row_Percent = 100 * N / sum(N)
    ) %>%
    ungroup() %>%
    group_by(Column) %>%
    mutate(
      Column_Percent = 100 * N / sum(N)
    ) %>%
    ungroup() %>%
    mutate(
      Total_Percent = 100 * N / sum(N),
      Row_Variable = row_label,
      Column_Variable = column_label,
      .before = 1
    )
}


binary_group_summary <- function(
    data,
    group_variable,
    outcome_variable,
    group_label,
    outcome_label
) {
  data %>%
    transmute(
      Group = clean_text(.data[[group_variable]]),
      Outcome = clean_binary(.data[[outcome_variable]])
    ) %>%
    filter(!is.na(Group)) %>%
    group_by(Group) %>%
    summarise(
      N_Valid = sum(!is.na(Outcome)),
      N_Yes = sum(Outcome == 1, na.rm = TRUE),
      Percent_Yes = safe_percent(N_Yes, N_Valid),
      .groups = "drop"
    ) %>%
    mutate(
      Grouping_Variable = group_label,
      Outcome = outcome_label,
      .before = 1
    )
}


spearman_test <- function(
    data,
    x_variable,
    y_variable,
    x_label = x_variable,
    y_label = y_variable
) {
  test_data <- data %>%
    transmute(
      x = clean_numeric(.data[[x_variable]]),
      y = clean_numeric(.data[[y_variable]])
    ) %>%
    drop_na()
  
  if (
    nrow(test_data) < 3 ||
    n_distinct(test_data$x) < 2 ||
    n_distinct(test_data$y) < 2
  ) {
    return(
      tibble(
        Variable_1 = x_label,
        Variable_2 = y_label,
        N = nrow(test_data),
        Spearman_Rho = NA_real_,
        P_Value = NA_real_
      )
    )
  }
  
  result <- suppressWarnings(
    cor.test(
      test_data$x,
      test_data$y,
      method = "spearman",
      exact = FALSE
    )
  )
  
  tibble(
    Variable_1 = x_label,
    Variable_2 = y_label,
    N = nrow(test_data),
    Spearman_Rho = unname(result$estimate),
    P_Value = result$p.value
  )
}


#===============================================================================
# 05 Canonical category systems
#===============================================================================

topic_levels <- c(
  "Politik, Staat & Wahlen",
  "Internationales, Krieg & Sicherheit",
  "Wirtschaft, Arbeit, Finanzen & Verbraucher",
  "Gesellschaft, Soziales, Migration & Religion",
  "Bildung, Wissenschaft & Technologie",
  "Gesundheit & Pflege",
  "Klima, Umwelt & Energie",
  "Kriminalität & Justiz",
  "Verkehr, Infrastruktur & Wohnen",
  "Wetter & Naturereignisse",
  "Kultur, Medien & Unterhaltung",
  "Geschichte & Erinnerung",
  "Sport",
  "Veranstaltungen & öffentlicher Service",
  "Sonstiges / nicht eindeutig"
)


source_levels <- c(
  "Journalistisches Medium",
  "Alternatives oder parteiisches Medienangebot",
  "Partei oder Politiker:in",
  "Staatliche oder öffentliche Institution",
  "NGO, Verband, Verein, Initiative oder Bewegung",
  "Wissenschaft, Expert:in oder Faktencheck",
  "Unternehmen oder Marke",
  "Journalist:in, Creator, Influencer:in oder öffentliche Person",
  "Private Person / Peer",
  "Kollektiv, Meme-, Satire- oder Aggregator-Seite",
  "Sonstige / Quelle nicht erkennbar"
)


format_levels <- c(
  "Textbasiert",
  "Bildbasiert",
  "Videobasiert",
  "Mischform / nicht eindeutig"
)


platform_levels <- c(
  "Facebook",
  "Instagram",
  "TikTok",
  "X"
)


incidentality_levels <- c(
  "Gezielt gesucht",
  "Gefolgt, nicht gezielt gesucht",
  "Zufällig begegnet"
)


locality_levels <- c(
  "Zu Hause",
  "Unterwegs",
  "Weiß nicht"
)


situation_levels <- c(
  "Allein",
  "Gemeinsam mit jemandem",
  "Weiß nicht"
)


canonicalise_platform <- function(x) {
  x_clean <- stringr::str_to_lower(clean_text(x))
  x_numeric <- clean_numeric(x_clean)
  
  case_when(
    x_numeric == 1 ~ "Facebook",
    x_numeric == 2 ~ "Instagram",
    x_numeric == 3 ~ "TikTok",
    x_numeric == 4 ~ "X",
    str_detect(x_clean, "facebook|fb") ~ "Facebook",
    str_detect(x_clean, "instagram|insta") ~ "Instagram",
    str_detect(x_clean, "tiktok|tik tok") ~ "TikTok",
    str_detect(x_clean, "twitter|^x$|x \\(vormals") ~ "X",
    TRUE ~ NA_character_
  )
}


canonicalise_incidentality <- function(x) {
  x_clean <- stringr::str_to_lower(clean_text(x))
  x_numeric <- clean_numeric(x_clean)
  
  case_when(
    x_numeric == 1 ~ "Gezielt gesucht",
    x_numeric == 2 ~ "Gefolgt, nicht gezielt gesucht",
    x_numeric == 3 ~ "Zufällig begegnet",
    str_detect(x_clean, "gezielt|deliberately|intention") ~ "Gezielt gesucht",
    str_detect(x_clean, "folge|follow") ~ "Gefolgt, nicht gezielt gesucht",
    str_detect(x_clean, "zufällig|chance|random") ~ "Zufällig begegnet",
    TRUE ~ NA_character_
  )
}


canonicalise_locality <- function(x) {
  x_clean <- stringr::str_to_lower(clean_text(x))
  x_numeric <- clean_numeric(x_clean)
  
  case_when(
    x_numeric == 1 ~ "Zu Hause",
    x_numeric == 2 ~ "Unterwegs",
    x_numeric == 3 ~ "Weiß nicht",
    str_detect(x_clean, "hause|home") ~ "Zu Hause",
    str_detect(x_clean, "unterwegs|out and about|transport|café|park") ~ "Unterwegs",
    str_detect(x_clean, "weiß nicht|weiss nicht|don't know|dont know") ~ "Weiß nicht",
    TRUE ~ NA_character_
  )
}


canonicalise_situation <- function(x) {
  x_clean <- stringr::str_to_lower(clean_text(x))
  x_numeric <- clean_numeric(x_clean)
  
  case_when(
    x_numeric == 1 ~ "Allein",
    x_numeric == 2 ~ "Gemeinsam mit jemandem",
    x_numeric == 3 ~ "Weiß nicht",
    str_detect(x_clean, "allein|alone") ~ "Allein",
    str_detect(x_clean, "gemeinsam|together|jemand") ~ "Gemeinsam mit jemandem",
    str_detect(x_clean, "weiß nicht|weiss nicht|don't know|dont know") ~ "Weiß nicht",
    TRUE ~ NA_character_
  )
}


canonicalise_topic <- function(x) {
  x <- clean_text(x)
  
  dplyr::recode(
    x,
    "Politik & Regieren" = "Politik, Staat & Wahlen",
    "Internationales, Krieg & Sicherheit" = "Internationales, Krieg & Sicherheit",
    "Wirtschaft, Arbeit & Verbraucher" = "Wirtschaft, Arbeit, Finanzen & Verbraucher",
    "Soziales, Bildung & Wissenschaft" = "Gesellschaft, Soziales, Migration & Religion",
    "Gesundheit & Medizin" = "Gesundheit & Pflege",
    "Kriminalität, Justiz & Polizei" = "Kriminalität & Justiz",
    "Unterhaltung & Prominenz" = "Kultur, Medien & Unterhaltung",
    "Lifestyle, Alltag & Service" = "Veranstaltungen & öffentlicher Service",
    "Sonstiges / unklar" = "Sonstiges / nicht eindeutig",
    .default = x
  )
}


canonicalise_source <- function(x) {
  x <- clean_text(x)
  
  dplyr::recode(
    x,
    "Journalistische Medien" = "Journalistisches Medium",
    "Alternative / partizipative News" = "Alternatives oder parteiisches Medienangebot",
    "Partei / Politiker:in" = "Partei oder Politiker:in",
    "Staat / öffentliche Institution" = "Staatliche oder öffentliche Institution",
    "NGO / Initiative / Bewegung" = "NGO, Verband, Verein, Initiative oder Bewegung",
    "Wissenschaft / Expertise" = "Wissenschaft, Expert:in oder Faktencheck",
    "Unternehmen / Marke" = "Unternehmen oder Marke",
    "Creator / Influencer:in" = "Journalist:in, Creator, Influencer:in oder öffentliche Person",
    "Privatperson / Peer" = "Private Person / Peer",
    "Kollektiv / anonym / Meme-Aggregator" = "Kollektiv, Meme-, Satire- oder Aggregator-Seite",
    "Unklar / nicht sichtbar" = "Sonstige / Quelle nicht erkennbar",
    .default = x
  )
}


canonicalise_format <- function(x) {
  x_clean <- stringr::str_to_lower(clean_text(x))
  
  case_when(
    x_clean %in% c("text", "textbasiert", "text-based") ~ "Textbasiert",
    x_clean %in% c("bild", "bildbasiert", "image", "image-based", "foto") ~ "Bildbasiert",
    x_clean %in% c("video", "videobasiert", "video-based", "reel", "tiktok") ~ "Videobasiert",
    str_detect(x_clean, "misch|gemischt|mixed|nicht eindeutig|unklar") ~
      "Mischform / nicht eindeutig",
    TRUE ~ clean_text(x)
  )
}


#===============================================================================
# 06 Load and prepare screening data
#===============================================================================

if (!file.exists(screening_file)) {
  stop(
    "Die vorbereitete Screening-Datei wurde nicht gefunden: ",
    screening_file,
    "\nBitte zuerst 04a_Screening_Analysis.R ausführen."
  )
}

screening <- readRDS(screening_file) %>%
  janitor::clean_names()

screening <- rename_first_available(
  screening,
  target = "participant",
  candidates = c(
    "personal_participant_code",
    "personalparticipantcode"
  ),
  required = TRUE
)

screening <- screening %>%
  mutate(
    participant = clean_text(participant)
  ) %>%
  filter(!is.na(participant))

screening_duplicates <- screening %>%
  count(participant, name = "N_Rows") %>%
  filter(N_Rows > 1)

if (nrow(screening_duplicates) > 0) {
  stop(
    "Die vorbereitete Screening-Datei enthält doppelte Participant Codes. ",
    "Bitte diese in 04a bereinigen."
  )
}

# Ensure all variables needed for the cross-survey exploration exist.
screening_optional_variables <- c(
  "intro_age_num",
  "gender",
  "education_three_level",
  "age_group",
  "intro_intensity",
  "intro_ib_undirected",
  "intro_ib_thematic",
  "intro_ib_social",
  "intro_ib_problem",
  "incidentality_index",
  "context_local",
  "context_social",
  "intro_freq_facebook",
  "intro_freq_instagram",
  "intro_freq_tiktok",
  "intro_freq_x",
  "n_platforms_used",
  "n_platforms_weekly",
  "platform_repertoire",
  "primary_platform",
  "dominant_information_need"
)

for (variable_name in screening_optional_variables) {
  if (!variable_name %in% names(screening)) {
    screening[[variable_name]] <- NA
  }
}

# Reconstruct a few labels if an older screening_prepared.rds is used.
if (all(is.na(screening$gender)) && "intro_gender" %in% names(screening)) {
  screening <- screening %>%
    mutate(
      gender = factor(
        clean_numeric(intro_gender),
        levels = c(1, 2, 3),
        labels = c("Weiblich", "Männlich", "Divers")
      )
    )
}

if (
  all(is.na(screening$education_three_level)) &&
  "intro_education" %in% names(screening)
) {
  screening <- screening %>%
    mutate(
      education_three_level = case_when(
        clean_numeric(intro_education) %in% c(1, 2) ~ "Niedrig",
        clean_numeric(intro_education) %in% c(3, 4) ~ "Mittel",
        clean_numeric(intro_education) %in% c(5, 6, 7) ~ "Hoch",
        TRUE ~ NA_character_
      )
    )
}

if (all(is.na(screening$age_group))) {
  screening <- screening %>%
    mutate(
      age_group = cut(
        clean_numeric(intro_age_num),
        breaks = c(59, 64, 69, 74, Inf),
        labels = c(
          "60–64 Jahre",
          "65–69 Jahre",
          "70–74 Jahre",
          "75 Jahre und älter"
        ),
        ordered_result = TRUE
      )
    )
}


#===============================================================================
# 07 Load and standardise coding sheet
#===============================================================================

if (!file.exists(coding_file)) {
  stop(
    "Das Coding Sheet wurde nicht gefunden: ",
    coding_file
  )
}

available_sheets <- openxlsx::getSheetNames(coding_file)

if (!coding_sheet_name %in% available_sheets) {
  stop(
    "Das Tabellenblatt '",
    coding_sheet_name,
    "' wurde nicht gefunden. Vorhanden: ",
    paste(available_sheets, collapse = ", ")
  )
}

coding_raw <- openxlsx::read.xlsx(
  coding_file,
  sheet = coding_sheet_name,
  detectDates = TRUE,
  check.names = FALSE
) %>%
  janitor::clean_names()

coding <- coding_raw

alias_definitions <- list(
  participant = c(
    "personal_participant_code",
    "personalparticipantcode"
  ),
  screenshot_id = c(
    "screenshotid",
    "screenshot_id_system"
  ),
  study_day = c(
    "day",
    "tag",
    "studyday"
  ),
  photo = c(
    "photo_number",
    "screenshot_number",
    "slot"
  ),
  filename = c(
    "file_name",
    "original_filename"
  ),
  filepath = c(
    "file_path",
    "path"
  ),
  file_exists = c(
    "exists",
    "file_found"
  ),
  topic_coded = c(
    "topic_primary",
    "topic_code"
  ),
  source_coded = c(
    "source_type",
    "source_code"
  ),
  source_name_coded = c(
    "account_name_raw",
    "source_name",
    "account_name_coded"
  ),
  platform_coded = c(
    "platform_code_coded"
  ),
  media_format = c(
    "format_coded",
    "format"
  ),
  coding_completed = c(
    "coding_complete",
    "completed"
  ),
  coder = c(
    "coded_by"
  ),
  coding_date = c(
    "date_coded"
  ),
  topic_participant = c(
    "topic_reported",
    "topic_raw"
  ),
  account_participant = c(
    "account_reported",
    "account_raw"
  ),
  platform_reported = c(
    "platform_label",
    "incident_platform",
    "platform"
  ),
  incidentality_label = c(
    "incidentality_reported",
    "incidentality"
  ),
  interaction_read = c(
    "int_read",
    "interaction_1"
  ),
  interaction_research = c(
    "int_research",
    "interaction_2"
  ),
  interaction_engagement = c(
    "int_engage",
    "interaction_3"
  ),
  locality_label = c(
    "local_context",
    "locality"
  ),
  situation_label = c(
    "social_context",
    "situation"
  ),
  scheduled = c(
    "scheduled_date"
  ),
  committed = c(
    "committed_date"
  ),
  submission_row = c(
    "source_row"
  )
)

for (target_name in names(alias_definitions)) {
  coding <- rename_first_available(
    coding,
    target = target_name,
    candidates = alias_definitions[[target_name]],
    required = target_name == "participant"
  )
}

coding <- coding %>%
  mutate(
    original_coding_row = row_number(),
    participant = clean_text(participant),
    screenshot_id = clean_text(screenshot_id),
    filename = clean_text(filename),
    filepath = clean_text(filepath),
    study_day = parse_study_day(study_day),
    photo = clean_numeric(photo),
    topic_coded = canonicalise_topic(topic_coded),
    source_coded = canonicalise_source(source_coded),
    source_name_coded = clean_text(source_name_coded),
    platform_reported = canonicalise_platform(platform_reported),
    platform_coded = canonicalise_platform(platform_coded),
    media_format = canonicalise_format(media_format),
    incidentality = canonicalise_incidentality(incidentality_label),
    interaction_read = clean_binary(interaction_read),
    interaction_research = clean_binary(interaction_research),
    interaction_engagement = clean_binary(interaction_engagement),
    locality = canonicalise_locality(locality_label),
    situation = canonicalise_situation(situation_label),
    coding_completed_binary = clean_binary(coding_completed),
    file_exists_binary = clean_binary(file_exists)
  ) %>%
  filter(
    !is.na(participant),
    !is.na(filename) | !is.na(screenshot_id)
  )

# Fill study day from scheduled date if required.
if (any(is.na(coding$study_day)) && any(!is.na(coding$scheduled))) {
  coding <- coding %>%
    mutate(
      scheduled_date = suppressWarnings(as.Date(scheduled))
    ) %>%
    group_by(participant) %>%
    mutate(
      inferred_study_day = if_else(
        !is.na(scheduled_date),
        as.numeric(scheduled_date - min(scheduled_date, na.rm = TRUE)) + 1,
        NA_real_
      ),
      study_day = coalesce(study_day, inferred_study_day)
    ) %>%
    ungroup() %>%
    select(-inferred_study_day)
}

# Generate stable screenshot IDs where the sheet does not provide one.
coding <- coding %>%
  group_by(
    participant,
    study_day
  ) %>%
  arrange(
    original_coding_row,
    .by_group = TRUE
  ) %>%
  mutate(
    photo_within_day = row_number(),
    screenshot_id = coalesce(
      screenshot_id,
      paste0(
        participant,
        "_d",
        stringr::str_pad(
          replace_na(as.integer(study_day), 0L),
          width = 2,
          pad = "0"
        ),
        "_p",
        stringr::str_pad(
          photo_within_day,
          width = 2,
          pad = "0"
        )
      )
    )
  ) %>%
  ungroup()

# The diary-reported platform is the default when no manual platform validation
# was entered. The three genuinely manual content variables remain topic, source
# and format.
coding <- coding %>%
  mutate(
    platform_coded = coalesce(
      platform_coded,
      platform_reported
    )
  )


#===============================================================================
# 08 Optional simulated coding
#===============================================================================

source_name_examples <- list(
  "Journalistisches Medium" = c(
    "Tagesschau",
    "ZDFheute",
    "Süddeutsche Zeitung",
    "Frankfurter Rundschau",
    "MDR Aktuell"
  ),
  "Alternatives oder parteiisches Medienangebot" = c(
    "Alternative Nachrichten",
    "Politik Direkt",
    "Freie Stimme"
  ),
  "Partei oder Politiker:in" = c(
    "Bundestagsfraktion",
    "Kommunalpolitikerin",
    "Bundespolitiker"
  ),
  "Staatliche oder öffentliche Institution" = c(
    "Bundesregierung",
    "Stadtverwaltung",
    "Polizei",
    "Bundeszentrale für politische Bildung"
  ),
  "NGO, Verband, Verein, Initiative oder Bewegung" = c(
    "Verbraucherzentrale",
    "NABU",
    "Sozialverband",
    "Lokale Initiative"
  ),
  "Wissenschaft, Expert:in oder Faktencheck" = c(
    "Universität",
    "Forschungsinstitut",
    "Correctiv Faktencheck",
    "Wissenschaftlerin"
  ),
  "Unternehmen oder Marke" = c(
    "Deutsche Bahn",
    "Energieversorger",
    "Technologieunternehmen",
    "Einzelhandel"
  ),
  "Journalist:in, Creator, Influencer:in oder öffentliche Person" = c(
    "Journalistin",
    "Wissenscreator",
    "Kulturcreator",
    "Öffentliche Person"
  ),
  "Private Person / Peer" = c(
    "Privater Account",
    "Bekannte Person",
    "Familienkontakt"
  ),
  "Kollektiv, Meme-, Satire- oder Aggregator-Seite" = c(
    "Satireseite",
    "Meme-Aggregator",
    "Lokaler Sammelaccount"
  ),
  "Sonstige / Quelle nicht erkennbar" = c(
    "Quelle nicht erkennbar"
  )
)


simulate_manual_coding <- function(
    coding_data,
    screening_data,
    seed,
    overwrite_existing = FALSE,
    use_screening_patterns = TRUE
) {
  set.seed(seed)
  
  simulation_screening <- screening_data %>%
    select(
      participant,
      any_of(
        c(
          "intro_ib_undirected",
          "intro_ib_thematic",
          "intro_ib_social",
          "intro_ib_problem"
        )
      )
    )
  
  simulation_data <- coding_data %>%
    left_join(
      simulation_screening,
      by = "participant"
    )
  
  n_rows <- nrow(simulation_data)
  
  simulated_topic <- character(n_rows)
  simulated_source <- character(n_rows)
  simulated_source_name <- character(n_rows)
  simulated_format <- character(n_rows)
  simulated_platform <- character(n_rows)
  
  base_topic_weights <- c(
    0.12, 0.08, 0.08, 0.09, 0.07,
    0.08, 0.06, 0.06, 0.06, 0.06,
    0.09, 0.05, 0.06, 0.08, 0.06
  )
  
  base_source_weights <- c(
    0.28, 0.07, 0.08, 0.09, 0.08,
    0.07, 0.10, 0.11, 0.05, 0.04, 0.03
  )
  
  for (row_index in seq_len(n_rows)) {
    reported_platform <- simulation_data$platform_reported[[row_index]]
    
    if (is.na(reported_platform)) {
      reported_platform <- sample(
        platform_levels,
        size = 1,
        prob = c(0.50, 0.24, 0.12, 0.14)
      )
    }
    
    # Mostly preserve the self-reported platform, but generate a few validation
    # mismatches so that the quality-control output can be tested.
    if (runif(1) < 0.95) {
      platform_value <- reported_platform
    } else {
      platform_value <- sample(
        setdiff(platform_levels, reported_platform),
        size = 1
      )
    }
    
    topic_weights <- base_topic_weights
    
    if (use_screening_patterns) {
      undirected <- clean_numeric(
        simulation_data$intro_ib_undirected[[row_index]]
      )
      thematic <- clean_numeric(
        simulation_data$intro_ib_thematic[[row_index]]
      )
      problem <- clean_numeric(
        simulation_data$intro_ib_problem[[row_index]]
      )
      
      undirected_effect <- if_else(is.na(undirected), 0, undirected - 3)
      thematic_effect <- if_else(is.na(thematic), 0, thematic - 3)
      problem_effect <- if_else(is.na(problem), 0, problem - 3)
      
      topic_weights[c(1, 2, 3, 4, 7, 8)] <-
        topic_weights[c(1, 2, 3, 4, 7, 8)] * exp(0.18 * undirected_effect)
      
      topic_weights[c(5, 11, 12, 13)] <-
        topic_weights[c(5, 11, 12, 13)] * exp(0.22 * thematic_effect)
      
      topic_weights[c(3, 6, 9, 10, 14)] <-
        topic_weights[c(3, 6, 9, 10, 14)] * exp(0.22 * problem_effect)
    }
    
    if (platform_value == "Facebook") {
      topic_weights[c(1, 4, 11, 14)] <- topic_weights[c(1, 4, 11, 14)] * 1.25
    } else if (platform_value == "Instagram") {
      topic_weights[c(6, 11, 13, 14)] <- topic_weights[c(6, 11, 13, 14)] * 1.30
    } else if (platform_value == "TikTok") {
      topic_weights[c(5, 11, 13)] <- topic_weights[c(5, 11, 13)] * 1.45
    } else if (platform_value == "X") {
      topic_weights[c(1, 2, 5, 7)] <- topic_weights[c(1, 2, 5, 7)] * 1.35
    }
    
    topic_value <- sample(
      topic_levels,
      size = 1,
      prob = topic_weights
    )
    
    source_weights <- base_source_weights
    
    if (topic_value %in% topic_levels[c(1, 2)]) {
      source_weights[c(1, 3, 4)] <- source_weights[c(1, 3, 4)] * 1.60
    }
    
    if (topic_value %in% topic_levels[c(5, 6)]) {
      source_weights[c(4, 6)] <- source_weights[c(4, 6)] * 1.80
    }
    
    if (topic_value %in% topic_levels[c(7, 14)]) {
      source_weights[c(4, 5)] <- source_weights[c(4, 5)] * 1.50
    }
    
    if (topic_value %in% topic_levels[c(11, 13)]) {
      source_weights[c(1, 7, 8)] <- source_weights[c(1, 7, 8)] * 1.45
    }
    
    if (use_screening_patterns) {
      social_need <- clean_numeric(
        simulation_data$intro_ib_social[[row_index]]
      )
      social_effect <- if_else(is.na(social_need), 0, social_need - 3)
      source_weights[9] <- source_weights[9] * exp(0.25 * social_effect)
    }
    
    source_value <- sample(
      source_levels,
      size = 1,
      prob = source_weights
    )
    
    format_probabilities <- switch(
      platform_value,
      Facebook = c(0.36, 0.39, 0.18, 0.07),
      Instagram = c(0.08, 0.48, 0.38, 0.06),
      TikTok = c(0.03, 0.07, 0.86, 0.04),
      X = c(0.58, 0.27, 0.10, 0.05),
      c(0.30, 0.35, 0.28, 0.07)
    )
    
    format_value <- sample(
      format_levels,
      size = 1,
      prob = format_probabilities
    )
    
    simulated_platform[[row_index]] <- platform_value
    simulated_topic[[row_index]] <- topic_value
    simulated_source[[row_index]] <- source_value
    simulated_source_name[[row_index]] <- sample(
      source_name_examples[[source_value]],
      size = 1
    )
    simulated_format[[row_index]] <- format_value
  }
  
  simulation_data <- simulation_data %>%
    mutate(
      platform_coded = if (
        overwrite_existing
      ) simulated_platform else coalesce(platform_coded, simulated_platform),
      
      topic_coded = if (
        overwrite_existing
      ) simulated_topic else coalesce(topic_coded, simulated_topic),
      
      source_coded = if (
        overwrite_existing
      ) simulated_source else coalesce(source_coded, simulated_source),
      
      source_name_coded = if (
        overwrite_existing
      ) simulated_source_name else coalesce(source_name_coded, simulated_source_name),
      
      media_format = if (
        overwrite_existing
      ) simulated_format else coalesce(media_format, simulated_format),
      
      coding_completed = "Ja",
      coding_completed_binary = 1L,
      coder = if_else(
        overwrite_existing | is.na(clean_text(coder)),
        "SIMULATED",
        clean_text(coder)
      ),
      coding_date = as.character(Sys.Date()),
      coding_was_simulated = TRUE
    ) %>%
    select(-any_of(c(
      "intro_ib_undirected",
      "intro_ib_thematic",
      "intro_ib_social",
      "intro_ib_problem"
    )))
  
  simulation_data
}


if (simulate_coding) {
  coding <- simulate_manual_coding(
    coding_data = coding,
    screening_data = screening,
    seed = simulation_seed,
    overwrite_existing = simulation_overwrite_existing,
    use_screening_patterns = simulation_use_screening_patterns
  )
  
  if (write_simulated_coding_sheet) {
    openxlsx::write.xlsx(
      coding,
      file = simulated_coding_file,
      overwrite = TRUE,
      asTable = TRUE
    )
  }
} else {
  coding <- coding %>%
    mutate(
      coding_was_simulated = FALSE
    )
}


#===============================================================================
# 09 Coding quality checks
#===============================================================================

manual_coding_variables <- c(
  "topic_coded",
  "source_coded",
  "media_format"
)

coding <- coding %>%
  mutate(
    manual_coding_complete = if_all(
      all_of(manual_coding_variables),
      ~ !is.na(clean_text(.x))
    ),
    coding_completed_binary = coalesce(
      coding_completed_binary,
      as.integer(manual_coding_complete)
    ),
    analysis_coding_complete =
      manual_coding_complete & coding_completed_binary == 1L
  )

coding_completeness <- tibble(
  Variable = c(
    manual_coding_variables,
    "source_name_coded",
    "platform_coded",
    "coding_completed"
  )
) %>%
  mutate(
    N_Total = nrow(coding),
    N_Complete = map_int(
      Variable,
      ~ sum(!is.na(clean_text(coding[[.x]])))
    ),
    N_Missing = N_Total - N_Complete,
    Percent_Complete = safe_percent(N_Complete, N_Total)
  )


duplicate_screenshot_ids <- coding %>%
  count(screenshot_id, name = "N_Rows") %>%
  filter(
    !is.na(screenshot_id),
    N_Rows > 1
  )


duplicate_filenames <- coding %>%
  count(filename, name = "N_Rows") %>%
  filter(
    !is.na(filename),
    N_Rows > 1
  )


invalid_categories <- bind_rows(
  coding %>%
    filter(
      !is.na(topic_coded),
      !topic_coded %in% topic_levels
    ) %>%
    count(topic_coded, name = "N") %>%
    transmute(
      Variable = "topic_coded",
      Invalid_Value = topic_coded,
      N
    ),
  
  coding %>%
    filter(
      !is.na(source_coded),
      !source_coded %in% source_levels
    ) %>%
    count(source_coded, name = "N") %>%
    transmute(
      Variable = "source_coded",
      Invalid_Value = source_coded,
      N
    ),
  
  coding %>%
    filter(
      !is.na(media_format),
      !media_format %in% format_levels
    ) %>%
    count(media_format, name = "N") %>%
    transmute(
      Variable = "media_format",
      Invalid_Value = media_format,
      N
    ),
  
  coding %>%
    filter(
      !is.na(platform_coded),
      !platform_coded %in% platform_levels
    ) %>%
    count(platform_coded, name = "N") %>%
    transmute(
      Variable = "platform_coded",
      Invalid_Value = platform_coded,
      N
    )
)


platform_mismatches <- coding %>%
  filter(
    !is.na(platform_reported),
    !is.na(platform_coded),
    platform_reported != platform_coded
  ) %>%
  select(
    screenshot_id,
    participant,
    study_day,
    filename,
    platform_reported,
    platform_coded,
    coding_was_simulated
  )


study_day_issues <- coding %>%
  filter(
    is.na(study_day) |
      !study_day %in% expected_study_days
  ) %>%
  select(
    screenshot_id,
    participant,
    study_day,
    filename,
    scheduled
  )


file_issues <- coding %>%
  filter(file_exists_binary == 0L) %>%
  select(
    screenshot_id,
    participant,
    study_day,
    filename,
    filepath,
    file_exists_binary
  )


if (
  strict_coding_check &&
  !simulate_coding &&
  any(!coding$analysis_coding_complete)
) {
  stop(
    sum(!coding$analysis_coding_complete),
    " Screenshot-Zeilen sind noch nicht vollständig codiert. ",
    "Siehe coding_sheet.xlsx oder setze simulate_coding <- TRUE für einen Testlauf."
  )
}

if (
  strict_coding_check &&
  nrow(invalid_categories) > 0
) {
  stop(
    "Das Coding Sheet enthält Werte außerhalb des festgelegten Kategoriensystems. ",
    "Siehe Tabelle 'Invalid_Categories'."
  )
}


#===============================================================================
# 10 Build the analysis sample
#===============================================================================

coding_complete <- coding %>%
  filter(analysis_coding_complete)

participant_counts_before_screening <- coding_complete %>%
  count(
    participant,
    name = "N_Screenshots"
  ) %>%
  mutate(
    At_Least_Minimum = N_Screenshots >= minimum_screenshots,
    Screening_Available = participant %in% screening$participant
  )

eligible_diary_ids <- participant_counts_before_screening %>%
  filter(At_Least_Minimum) %>%
  pull(participant)

if (require_screening_match) {
  eligible_participant_ids <- intersect(
    eligible_diary_ids,
    screening$participant
  )
} else {
  eligible_participant_ids <- eligible_diary_ids
}

if (length(eligible_participant_ids) == 0) {
  stop(
    "Nach Anwendung der Einschlusskriterien verbleiben keine Teilnehmenden. ",
    "Prüfe minimum_screenshots, Screening-Matches und Coding-Vollständigkeit."
  )
}

analysis_exclusions <- participant_counts_before_screening %>%
  mutate(
    Exclusion_Reason = case_when(
      !At_Least_Minimum ~ paste0(
        "Weniger als ",
        minimum_screenshots,
        " Screenshots"
      ),
      require_screening_match & !Screening_Available ~
        "Kein vollständiges Screening-Match",
      TRUE ~ "Eingeschlossen"
    )
  )


daily <- coding_complete %>%
  filter(participant %in% eligible_participant_ids) %>%
  mutate(
    topic_coded = factor(
      topic_coded,
      levels = topic_levels
    ),
    source_coded = factor(
      source_coded,
      levels = source_levels
    ),
    media_format = factor(
      media_format,
      levels = format_levels
    ),
    platform = factor(
      platform_coded,
      levels = platform_levels
    ),
    incidentality = factor(
      incidentality,
      levels = incidentality_levels,
      ordered = TRUE
    ),
    locality = factor(
      locality,
      levels = locality_levels
    ),
    situation = factor(
      situation,
      levels = situation_levels
    ),
    incidental_strict = case_when(
      is.na(incidentality) ~ NA_integer_,
      incidentality == "Zufällig begegnet" ~ 1L,
      TRUE ~ 0L
    ),
    incidental_broad = case_when(
      is.na(incidentality) ~ NA_integer_,
      incidentality %in% c(
        "Gefolgt, nicht gezielt gesucht",
        "Zufällig begegnet"
      ) ~ 1L,
      TRUE ~ 0L
    ),
    interaction_count = rowSums(
      cbind(
        interaction_read,
        interaction_research,
        interaction_engagement
      ),
      na.rm = TRUE
    ),
    interaction_any = case_when(
      is.na(interaction_read) &
        is.na(interaction_research) &
        is.na(interaction_engagement) ~ NA_integer_,
      interaction_count > 0 ~ 1L,
      TRUE ~ 0L
    ),
    active_follow_up = case_when(
      is.na(interaction_research) &
        is.na(interaction_engagement) ~ NA_integer_,
      interaction_research == 1L |
        interaction_engagement == 1L ~ 1L,
      TRUE ~ 0L
    ),
    topic_macro = case_when(
      as.character(topic_coded) %in% topic_levels[c(1, 2, 3, 4, 7, 8)] ~
        "Aktuelles & öffentliche Angelegenheiten",
      as.character(topic_coded) %in% topic_levels[c(6, 9, 10, 14)] ~
        "Praktische Information & Service",
      as.character(topic_coded) %in% topic_levels[c(5, 11, 12, 13)] ~
        "Wissen, Interessen & Kultur",
      TRUE ~ "Sonstiges / nicht eindeutig"
    ),
    source_macro = case_when(
      as.character(source_coded) == source_levels[1] ~
        "Journalistische Medien",
      as.character(source_coded) %in% source_levels[c(2, 10)] ~
        "Alternative, aggregierte oder informelle Medien",
      as.character(source_coded) %in% source_levels[c(3, 4)] ~
        "Politik & öffentliche Institutionen",
      as.character(source_coded) %in% source_levels[c(5, 6)] ~
        "Zivilgesellschaft & Expertise",
      as.character(source_coded) %in% source_levels[c(7, 8)] ~
        "Kommerzielle & öffentliche Personenaccounts",
      as.character(source_coded) == source_levels[9] ~
        "Private Person / Peer",
      TRUE ~ "Sonstige / nicht erkennbar"
    ),
    platform_matches_report = case_when(
      is.na(platform_reported) | is.na(platform_coded) ~ NA_integer_,
      platform_reported == platform_coded ~ 1L,
      TRUE ~ 0L
    )
  )

screening_selected <- screening %>%
  select(
    participant,
    any_of(
      c(
        "intro_age_num",
        "gender",
        "education_three_level",
        "age_group",
        "intro_intensity",
        "intro_ib_undirected",
        "intro_ib_thematic",
        "intro_ib_social",
        "intro_ib_problem",
        "incidentality_index",
        "context_local",
        "context_social",
        "intro_freq_facebook",
        "intro_freq_instagram",
        "intro_freq_tiktok",
        "intro_freq_x",
        "n_platforms_used",
        "n_platforms_weekly",
        "platform_repertoire",
        "primary_platform",
        "dominant_information_need"
      )
    )
  ) %>%
  mutate(
    # Standardise person-level predictors before they are replicated across
    # screenshot rows. This avoids weighting participants with many uploads more
    # strongly when calculating z-scores.
    intro_intensity_z = safe_z(intro_intensity),
    incidentality_index_z = safe_z(incidentality_index),
    intro_ib_undirected_z = safe_z(intro_ib_undirected),
    intro_ib_thematic_z = safe_z(intro_ib_thematic),
    intro_ib_social_z = safe_z(intro_ib_social),
    intro_ib_problem_z = safe_z(intro_ib_problem)
  )


daily <- daily %>%
  left_join(
    screening_selected,
    by = "participant"
  ) %>%
  mutate(
    study_day_z = safe_z(study_day)
  )


#===============================================================================
# 11 Sample and participation descriptives
#===============================================================================

participant_counts <- daily %>%
  group_by(participant) %>%
  summarise(
    N_Screenshots = n(),
    N_Active_Days = n_distinct(study_day[study_day %in% expected_study_days]),
    First_Study_Day = safe_min(study_day),
    Last_Study_Day = safe_max(study_day),
    .groups = "drop"
  )

sample_overview <- tibble(
  Indicator = c(
    "Rows in coding sheet",
    "Complete coded screenshot rows",
    "Eligible participants before screening match",
    "Eligible participants in final daily sample",
    "Screenshots in final daily sample",
    "Minimum screenshots required",
    "Median screenshots per participant",
    "Mean screenshots per participant",
    "Median active diary days",
    "Simulation active",
    "Simulation uses screening patterns"
  ),
  Value = c(
    nrow(coding),
    nrow(coding_complete),
    length(eligible_diary_ids),
    n_distinct(daily$participant),
    nrow(daily),
    minimum_screenshots,
    safe_median(participant_counts$N_Screenshots),
    safe_mean(participant_counts$N_Screenshots),
    safe_median(participant_counts$N_Active_Days),
    simulate_coding,
    simulate_coding && simulation_use_screening_patterns
  )
)

participant_day_grid <- tidyr::expand_grid(
  participant = eligible_participant_ids,
  study_day = expected_study_days
) %>%
  left_join(
    daily %>%
      count(
        participant,
        study_day,
        name = "N_Posts"
      ),
    by = c("participant", "study_day")
  ) %>%
  mutate(
    N_Posts = replace_na(N_Posts, 0L),
    Any_Post = N_Posts > 0
  )


day_outcomes <- daily %>%
  filter(study_day %in% expected_study_days) %>%
  group_by(study_day) %>%
  summarise(
    N_Observed_Posts = n(),
    N_Contributing_Participants = n_distinct(participant),
    Percent_Incidental_Strict = 100 * safe_mean(incidental_strict),
    Percent_Incidental_Broad = 100 * safe_mean(incidental_broad),
    Percent_Read_Thoroughly = 100 * safe_mean(interaction_read),
    Percent_Researched = 100 * safe_mean(interaction_research),
    Percent_Engaged = 100 * safe_mean(interaction_engagement),
    Mean_Interaction_Count = safe_mean(interaction_count),
    .groups = "drop"
  )


day_summary <- participant_day_grid %>%
  group_by(study_day) %>%
  summarise(
    N_Eligible_Participants = n(),
    N_Participants_With_Post = sum(Any_Post),
    Percent_With_Post = 100 * mean(Any_Post),
    Mean_Posts_Per_Eligible_Participant = mean(N_Posts),
    SD_Posts_Per_Eligible_Participant = sd(N_Posts),
    Median_Posts_Per_Eligible_Participant = median(N_Posts),
    Total_Posts = sum(N_Posts),
    .groups = "drop"
  ) %>%
  left_join(
    day_outcomes,
    by = "study_day"
  )


#===============================================================================
# 12 Main screenshot-weighted distributions
#===============================================================================

topic_distribution <- frequency_distribution(
  daily,
  "topic_coded",
  "Topic",
  topic_levels
)

source_distribution <- frequency_distribution(
  daily,
  "source_coded",
  "Source/Accounttyp",
  source_levels
)

source_name_distribution <- daily %>%
  mutate(
    source_name_coded = replace_na(
      clean_text(source_name_coded),
      "Quelle nicht benannt"
    )
  ) %>%
  count(
    source_name_coded,
    sort = TRUE,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 * N / sum(N)
  )

platform_distribution <- frequency_distribution(
  daily,
  "platform",
  "Plattform",
  platform_levels
)

format_distribution <- frequency_distribution(
  daily,
  "media_format",
  "Medienformat",
  format_levels
)

incidentality_distribution <- frequency_distribution(
  daily,
  "incidentality",
  "Incidentality",
  incidentality_levels
)

locality_distribution <- frequency_distribution(
  daily,
  "locality",
  "Räumlicher Kontext",
  locality_levels
)

situation_distribution <- frequency_distribution(
  daily,
  "situation",
  "Sozialer Kontext",
  situation_levels
)

interaction_distribution <- tibble(
  Interaction = c(
    "Gründlich gelesen/angeschaut",
    "Weiter recherchiert",
    "Sichtbar interagiert",
    "Mindestens eine Interaktion",
    "Aktives Follow-up: Recherche oder Engagement"
  ),
  N_Valid = c(
    sum(!is.na(daily$interaction_read)),
    sum(!is.na(daily$interaction_research)),
    sum(!is.na(daily$interaction_engagement)),
    sum(!is.na(daily$interaction_any)),
    sum(!is.na(daily$active_follow_up))
  ),
  N_Yes = c(
    sum(daily$interaction_read == 1L, na.rm = TRUE),
    sum(daily$interaction_research == 1L, na.rm = TRUE),
    sum(daily$interaction_engagement == 1L, na.rm = TRUE),
    sum(daily$interaction_any == 1L, na.rm = TRUE),
    sum(daily$active_follow_up == 1L, na.rm = TRUE)
  )
) %>%
  mutate(
    Percent_Yes = safe_percent(N_Yes, N_Valid)
  )


#===============================================================================
# 13 Participant-weighted distributions
#===============================================================================

topic_shares <- make_participant_shares(
  daily,
  "topic_coded",
  topic_levels,
  "Topic"
)

source_shares <- make_participant_shares(
  daily,
  "source_coded",
  source_levels,
  "Source/Accounttyp"
)

platform_shares <- make_participant_shares(
  daily,
  "platform",
  platform_levels,
  "Plattform"
)

format_shares <- make_participant_shares(
  daily,
  "media_format",
  format_levels,
  "Medienformat"
)

incidentality_shares <- make_participant_shares(
  daily,
  "incidentality",
  incidentality_levels,
  "Incidentality"
)

topic_participant_summary <- summarise_participant_shares(topic_shares)
source_participant_summary <- summarise_participant_shares(source_shares)
platform_participant_summary <- summarise_participant_shares(platform_shares)
format_participant_summary <- summarise_participant_shares(format_shares)
incidentality_participant_summary <- summarise_participant_shares(
  incidentality_shares
)


#===============================================================================
# 14 Participant-level diary indicators
#===============================================================================

participant_metrics <- daily %>%
  group_by(participant) %>%
  summarise(
    N_Screenshots = n(),
    N_Active_Days = n_distinct(study_day[study_day %in% expected_study_days]),
    Mean_Posts_Per_Active_Day = safe_divide(N_Screenshots, N_Active_Days),
    
    Share_Incidental_Strict = safe_mean(incidental_strict),
    Share_Incidental_Broad = safe_mean(incidental_broad),
    Share_Read_Thoroughly = safe_mean(interaction_read),
    Share_Researched = safe_mean(interaction_research),
    Share_Engaged = safe_mean(interaction_engagement),
    Share_Any_Interaction = safe_mean(interaction_any),
    Share_Active_Follow_Up = safe_mean(active_follow_up),
    Mean_Interaction_Count = safe_mean(interaction_count),
    
    Share_Home = share_value(as.character(locality), "Zu Hause"),
    Share_Away = share_value(as.character(locality), "Unterwegs"),
    Share_Alone = share_value(as.character(situation), "Allein"),
    Share_Together = share_value(
      as.character(situation),
      "Gemeinsam mit jemandem"
    ),
    
    Topic_Richness = n_distinct_valid(topic_coded),
    Topic_Shannon = shannon_entropy(topic_coded),
    Topic_Evenness = shannon_evenness(topic_coded),
    Topic_Dominant_Share = dominant_share(topic_coded),
    
    Source_Richness = n_distinct_valid(source_coded),
    Source_Shannon = shannon_entropy(source_coded),
    Source_Evenness = shannon_evenness(source_coded),
    Source_Dominant_Share = dominant_share(source_coded),
    
    Platform_Richness = n_distinct_valid(platform),
    Platform_Shannon = shannon_entropy(platform),
    Platform_Evenness = shannon_evenness(platform),
    Platform_Dominant_Share = dominant_share(platform),
    
    Format_Richness = n_distinct_valid(media_format),
    Format_Shannon = shannon_entropy(media_format),
    Format_Evenness = shannon_evenness(media_format),
    Format_Dominant_Share = dominant_share(media_format),
    
    N_Unique_Account_Names = n_distinct_valid(source_name_coded),
    Account_Dominant_Share = dominant_share(source_name_coded),
    
    Share_Current_Affairs = share_value(
      topic_macro,
      "Aktuelles & öffentliche Angelegenheiten"
    ),
    Share_Practical_Service = share_value(
      topic_macro,
      "Praktische Information & Service"
    ),
    Share_Knowledge_Interests = share_value(
      topic_macro,
      "Wissen, Interessen & Kultur"
    ),
    Share_Journalistic_Sources = share_value(
      source_macro,
      "Journalistische Medien"
    ),
    Share_Peer_Sources = share_value(
      source_macro,
      "Private Person / Peer"
    ),
    
    Share_Facebook = share_value(as.character(platform), "Facebook"),
    Share_Instagram = share_value(as.character(platform), "Instagram"),
    Share_TikTok = share_value(as.character(platform), "TikTok"),
    Share_X = share_value(as.character(platform), "X"),
    
    Platform_Report_Match_Rate = safe_mean(platform_matches_report),
    .groups = "drop"
  ) %>%
  left_join(
    screening_selected,
    by = "participant"
  )

# Determine the diary-dominant platform, retaining ties.
daily_primary_platform <- platform_shares %>%
  group_by(participant) %>%
  filter(
    !is.na(Share),
    Share == max(Share, na.rm = TRUE),
    Share > 0
  ) %>%
  summarise(
    Daily_Primary_Platform = paste(Category, collapse = " / "),
    .groups = "drop"
  )

platform_sets_overlap <- function(x, y) {
  if (is.na(x) || is.na(y)) return(NA_integer_)
  
  x_set <- clean_text(str_split(x, " / ", simplify = FALSE)[[1]])
  y_set <- clean_text(str_split(y, " / ", simplify = FALSE)[[1]])
  
  as.integer(length(intersect(x_set, y_set)) > 0)
}

participant_metrics <- participant_metrics %>%
  left_join(
    daily_primary_platform,
    by = "participant"
  ) %>%
  mutate(
    Primary_Platform_Match = map2_int(
      clean_text(primary_platform),
      clean_text(Daily_Primary_Platform),
      platform_sets_overlap
    ),
    Local_Context_Alignment = case_when(
      clean_text(context_local) == "Zu Hause" ~ Share_Home,
      clean_text(context_local) == "Unterwegs" ~ Share_Away,
      str_detect(
        str_to_lower(clean_text(context_local)),
        "beiden|ähnlich|gleich"
      ) ~ 1 - abs(Share_Home - Share_Away),
      TRUE ~ NA_real_
    ),
    Social_Context_Alignment = case_when(
      str_detect(
        str_to_lower(clean_text(context_social)),
        "überwiegend allein|mostly alone"
      ) ~ Share_Alone,
      str_detect(
        str_to_lower(clean_text(context_social)),
        "überwiegend gemeinsam|mostly together"
      ) ~ Share_Together,
      str_detect(
        str_to_lower(clean_text(context_social)),
        "ähnlich|gleich"
      ) ~ 1 - abs(Share_Alone - Share_Together),
      TRUE ~ NA_real_
    )
  )

# Experimental profile alignment: four screening needs are compared with four
# observed proxies. The social need is linked to peer sources, not to a topic.
participant_metrics <- participant_metrics %>%
  rowwise() %>%
  mutate(
    Need_Content_Alignment = profile_alignment(
      screening_values = c(
        clean_numeric(intro_ib_undirected),
        clean_numeric(intro_ib_thematic),
        clean_numeric(intro_ib_social),
        clean_numeric(intro_ib_problem)
      ),
      observed_values = c(
        Share_Current_Affairs,
        Share_Knowledge_Interests,
        Share_Peer_Sources,
        Share_Practical_Service
      )
    ),
    Screening_Dominant_Need = {
      need_values <- c(
        Ungerichtet = clean_numeric(intro_ib_undirected),
        Thematisch = clean_numeric(intro_ib_thematic),
        Sozial = clean_numeric(intro_ib_social),
        Problembezogen = clean_numeric(intro_ib_problem)
      )
      
      if (all(is.na(need_values))) {
        NA_character_
      } else if (sum(need_values == max(need_values, na.rm = TRUE), na.rm = TRUE) > 1) {
        "Kein eindeutiges dominantes Bedürfnis"
      } else {
        names(which.max(need_values))
      }
    },
    Observed_Dominant_Proxy = {
      proxy_values <- c(
        Ungerichtet = Share_Current_Affairs,
        Thematisch = Share_Knowledge_Interests,
        Sozial = Share_Peer_Sources,
        Problembezogen = Share_Practical_Service
      )
      
      if (all(is.na(proxy_values))) {
        NA_character_
      } else if (sum(proxy_values == max(proxy_values, na.rm = TRUE), na.rm = TRUE) > 1) {
        "Kein eindeutiger dominanter Proxy"
      } else {
        names(which.max(proxy_values))
      }
    },
    Dominant_Need_Proxy_Match = case_when(
      is.na(Screening_Dominant_Need) |
        is.na(Observed_Dominant_Proxy) ~ NA_integer_,
      str_detect(Screening_Dominant_Need, "Kein eindeutiges") |
        str_detect(Observed_Dominant_Proxy, "Kein eindeutiger") ~ NA_integer_,
      Screening_Dominant_Need == Observed_Dominant_Proxy ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  ungroup()


participant_metrics_summary <- participant_metrics %>%
  select(
    N_Screenshots,
    N_Active_Days,
    Mean_Posts_Per_Active_Day,
    starts_with("Share_"),
    ends_with("_Richness"),
    ends_with("_Shannon"),
    ends_with("_Evenness"),
    ends_with("_Dominant_Share"),
    Platform_Report_Match_Rate,
    Local_Context_Alignment,
    Social_Context_Alignment,
    Need_Content_Alignment
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  group_by(Indicator) %>%
  summarise(
    N_Valid = sum(!is.na(Value)),
    Mean = safe_mean(Value),
    SD = safe_sd(Value),
    Median = safe_median(Value),
    Minimum = safe_min(Value),
    Maximum = safe_max(Value),
    .groups = "drop"
  )


#===============================================================================
# 15 Content, platform, incidentality and interaction patterns
#===============================================================================

topic_by_platform <- cross_tabulation(
  daily,
  "topic_coded",
  "platform",
  "Topic",
  "Plattform"
)

source_by_platform <- cross_tabulation(
  daily,
  "source_coded",
  "platform",
  "Source/Accounttyp",
  "Plattform"
)

format_by_platform <- cross_tabulation(
  daily,
  "media_format",
  "platform",
  "Medienformat",
  "Plattform"
)

topic_by_incidentality <- cross_tabulation(
  daily,
  "topic_coded",
  "incidentality",
  "Topic",
  "Incidentality"
)

source_by_incidentality <- cross_tabulation(
  daily,
  "source_coded",
  "incidentality",
  "Source/Accounttyp",
  "Incidentality"
)

format_by_incidentality <- cross_tabulation(
  daily,
  "media_format",
  "incidentality",
  "Medienformat",
  "Incidentality"
)

interaction_group_variables <- c(
  topic_coded = "Topic",
  source_coded = "Source/Accounttyp",
  platform = "Plattform",
  media_format = "Medienformat",
  incidentality = "Incidentality",
  locality = "Räumlicher Kontext",
  situation = "Sozialer Kontext",
  study_day = "Studientag"
)

interaction_outcomes <- c(
  interaction_read = "Gründlich gelesen/angeschaut",
  interaction_research = "Weiter recherchiert",
  interaction_engagement = "Sichtbar interagiert"
)

interaction_by_groups <- purrr::imap_dfr(
  interaction_group_variables,
  function(group_label, group_variable) {
    purrr::imap_dfr(
      interaction_outcomes,
      function(outcome_label, outcome_variable) {
        binary_group_summary(
          daily,
          group_variable = group_variable,
          outcome_variable = outcome_variable,
          group_label = group_label,
          outcome_label = outcome_label
        )
      }
    )
  }
)


#===============================================================================
# 16 Screening-diary alignment and targeted correlations
#===============================================================================

screening_diary_correlations <- bind_rows(
  spearman_test(
    participant_metrics,
    "incidentality_index",
    "Share_Incidental_Strict",
    "Screening Incidentality-Index",
    "Diary-Anteil incidental strict"
  ),
  spearman_test(
    participant_metrics,
    "incidentality_index",
    "Share_Incidental_Broad",
    "Screening Incidentality-Index",
    "Diary-Anteil incidental broad"
  ),
  spearman_test(
    participant_metrics,
    "intro_intensity",
    "Share_Read_Thoroughly",
    "Screening Nutzungsintensität",
    "Diary-Anteil gründlich gelesen"
  ),
  spearman_test(
    participant_metrics,
    "intro_intensity",
    "Share_Researched",
    "Screening Nutzungsintensität",
    "Diary-Anteil weiter recherchiert"
  ),
  spearman_test(
    participant_metrics,
    "intro_intensity",
    "Share_Engaged",
    "Screening Nutzungsintensität",
    "Diary-Anteil sichtbar interagiert"
  ),
  spearman_test(
    participant_metrics,
    "intro_intensity",
    "Mean_Interaction_Count",
    "Screening Nutzungsintensität",
    "Diary mittlere Interaktionszahl"
  ),
  spearman_test(
    participant_metrics,
    "intro_ib_undirected",
    "Share_Current_Affairs",
    "Ungerichtetes Informationsbedürfnis",
    "Anteil aktueller öffentlicher Angelegenheiten"
  ),
  spearman_test(
    participant_metrics,
    "intro_ib_thematic",
    "Share_Knowledge_Interests",
    "Thematisches Informationsbedürfnis",
    "Anteil Wissen, Interessen und Kultur"
  ),
  spearman_test(
    participant_metrics,
    "intro_ib_problem",
    "Share_Practical_Service",
    "Problembezogenes Informationsbedürfnis",
    "Anteil praktischer Information und Service"
  ),
  spearman_test(
    participant_metrics,
    "intro_ib_social",
    "Share_Peer_Sources",
    "Soziales Informationsbedürfnis",
    "Anteil Peer-Quellen"
  ),
  spearman_test(
    participant_metrics,
    "intro_ib_undirected",
    "Share_Incidental_Broad",
    "Ungerichtetes Informationsbedürfnis",
    "Diary-Anteil incidental broad"
  ),
  spearman_test(
    participant_metrics,
    "intro_ib_thematic",
    "Topic_Shannon",
    "Thematisches Informationsbedürfnis",
    "Thematische Diversität"
  ),
  spearman_test(
    participant_metrics,
    "intro_ib_problem",
    "Share_Researched",
    "Problembezogenes Informationsbedürfnis",
    "Diary-Anteil weiter recherchiert"
  ),
  spearman_test(
    participant_metrics,
    "intro_age_num",
    "Share_Incidental_Broad",
    "Alter",
    "Diary-Anteil incidental broad"
  ),
  spearman_test(
    participant_metrics,
    "intro_age_num",
    "Share_Engaged",
    "Alter",
    "Diary-Anteil sichtbar interagiert"
  )
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(P_Value, method = "BH"),
    Analysis_Type = "Explorativ"
  )


need_variables <- c(
  intro_ib_undirected = "Ungerichtet: Nachrichten & aktuelles Geschehen",
  intro_ib_thematic = "Thematisch: persönliche Interessen",
  intro_ib_social = "Sozial: soziales Umfeld",
  intro_ib_problem = "Problembezogen: konkrete Problemlösung"
)


topic_need_correlations <- tidyr::crossing(
  Topic = topic_levels,
  Need_Variable = names(need_variables)
) %>%
  mutate(
    Need = unname(need_variables[Need_Variable])
  ) %>%
  pmap_dfr(
    function(Topic, Need_Variable, Need) {
      analysis_data <- topic_shares %>%
        filter(Category == Topic) %>%
        select(participant, Share) %>%
        left_join(
          screening %>%
            select(
              participant,
              all_of(Need_Variable)
            ),
          by = "participant"
        )
      
      result <- spearman_test(
        analysis_data,
        Need_Variable,
        "Share",
        Need,
        Topic
      )
      
      result %>%
        transmute(
          Topic,
          Need,
          N,
          Spearman_Rho,
          P_Value
        )
    }
  ) %>%
  group_by(Need) %>%
  mutate(
    P_Adjusted_BH_Within_Need = p.adjust(P_Value, method = "BH")
  ) %>%
  ungroup() %>%
  mutate(
    Analysis_Type = "Explorativ"
  )


source_need_correlations <- tidyr::crossing(
  Source = source_levels,
  Need_Variable = names(need_variables)
) %>%
  mutate(
    Need = unname(need_variables[Need_Variable])
  ) %>%
  pmap_dfr(
    function(Source, Need_Variable, Need) {
      analysis_data <- source_shares %>%
        filter(Category == Source) %>%
        select(participant, Share) %>%
        left_join(
          screening %>%
            select(
              participant,
              all_of(Need_Variable)
            ),
          by = "participant"
        )
      
      result <- spearman_test(
        analysis_data,
        Need_Variable,
        "Share",
        Need,
        Source
      )
      
      result %>%
        transmute(
          Source,
          Need,
          N,
          Spearman_Rho,
          P_Value
        )
    }
  ) %>%
  group_by(Need) %>%
  mutate(
    P_Adjusted_BH_Within_Need = p.adjust(P_Value, method = "BH")
  ) %>%
  ungroup() %>%
  mutate(
    Analysis_Type = "Explorativ"
  )


platform_frequency_map <- c(
  Facebook = "intro_freq_facebook",
  Instagram = "intro_freq_instagram",
  TikTok = "intro_freq_tiktok",
  X = "intro_freq_x"
)

platform_alignment_correlations <- imap_dfr(
  platform_frequency_map,
  function(screening_variable, platform_name) {
    analysis_data <- platform_shares %>%
      filter(Category == platform_name) %>%
      select(participant, Share) %>%
      left_join(
        screening %>%
          select(
            participant,
            all_of(screening_variable)
          ),
        by = "participant"
      )
    
    result <- spearman_test(
      analysis_data,
      screening_variable,
      "Share",
      paste0(platform_name, ": Screening-Nutzungsfrequenz"),
      paste0(platform_name, ": Diary-Anteil")
    )
    
    result %>%
      mutate(
        Platform = platform_name,
        .before = 1
      )
  }
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(P_Value, method = "BH"),
    Analysis_Type = "Explorativ"
  )


alignment_summary <- participant_metrics %>%
  summarise(
    N_Primary_Platform_Valid = sum(!is.na(Primary_Platform_Match)),
    N_Primary_Platform_Match = sum(Primary_Platform_Match == 1L, na.rm = TRUE),
    Percent_Primary_Platform_Match = 100 * safe_mean(Primary_Platform_Match),
    Mean_Local_Context_Alignment = safe_mean(Local_Context_Alignment),
    Mean_Social_Context_Alignment = safe_mean(Social_Context_Alignment),
    Mean_Need_Content_Alignment = safe_mean(Need_Content_Alignment),
    N_Dominant_Need_Proxy_Valid = sum(!is.na(Dominant_Need_Proxy_Match)),
    Percent_Dominant_Need_Proxy_Match =
      100 * safe_mean(Dominant_Need_Proxy_Match)
  )


#===============================================================================
# 17 Descriptive subgroup comparisons
#===============================================================================

participant_metric_variables <- c(
  "Share_Incidental_Broad",
  "Share_Read_Thoroughly",
  "Share_Researched",
  "Share_Engaged",
  "Topic_Shannon",
  "Source_Shannon",
  "Share_Current_Affairs",
  "Share_Practical_Service",
  "Share_Knowledge_Interests",
  "Share_Journalistic_Sources"
)

subgroup_variables <- c(
  age_group = "Altersgruppe",
  gender = "Geschlecht",
  education_three_level = "Bildungsniveau",
  platform_repertoire = "Screening-Plattformrepertoire",
  primary_platform = "Screening-Primärplattform"
)

subgroup_summaries <- purrr::imap_dfr(
  subgroup_variables,
  function(group_label, group_variable) {
    purrr::map_dfr(
      participant_metric_variables,
      function(metric_variable) {
        participant_metrics %>%
          transmute(
            Group = clean_text(.data[[group_variable]]),
            Value = clean_numeric(.data[[metric_variable]])
          ) %>%
          filter(!is.na(Group)) %>%
          group_by(Group) %>%
          summarise(
            N = sum(!is.na(Value)),
            Mean = safe_mean(Value),
            SD = safe_sd(Value),
            Median = safe_median(Value),
            Minimum = safe_min(Value),
            Maximum = safe_max(Value),
            .groups = "drop"
          ) %>%
          mutate(
            Grouping_Variable = group_label,
            Metric = metric_variable,
            .before = 1
          )
      }
    )
  }
)


#===============================================================================
# 18 Optional exploratory mixed models
#===============================================================================

fit_binary_glmm <- function(
    data,
    formula,
    model_name,
    outcome_variable
) {
  formula_variables <- all.vars(formula)
  
  model_data <- data %>%
    select(all_of(formula_variables)) %>%
    drop_na() %>%
    mutate(
      across(
        where(is.character),
        as.factor
      ),
      across(
        where(is.factor),
        forcats::fct_drop
      )
    )
  
  n_observations <- nrow(model_data)
  n_participants <- n_distinct(model_data$participant)
  n_events <- sum(model_data[[outcome_variable]] == 1, na.rm = TRUE)
  n_non_events <- sum(model_data[[outcome_variable]] == 0, na.rm = TRUE)
  
  if (
    n_observations < minimum_model_n ||
    n_participants < minimum_model_participants ||
    n_events < minimum_model_events ||
    n_non_events < minimum_model_events
  ) {
    return(
      list(
        model = NULL,
        status = tibble(
          Model = model_name,
          Status = "Not fitted: insufficient data/events",
          N = n_observations,
          N_Participants = n_participants,
          N_Events = n_events,
          N_Non_Events = n_non_events,
          Singular = NA,
          Convergence_Message = NA_character_
        ),
        tidy = tibble()
      )
    )
  }
  
  fitted_model <- tryCatch(
    suppressWarnings(
      lme4::glmer(
        formula,
        data = model_data,
        family = binomial,
        control = lme4::glmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 200000)
        )
      )
    ),
    error = function(e) e
  )
  
  if (inherits(fitted_model, "error")) {
    return(
      list(
        model = NULL,
        status = tibble(
          Model = model_name,
          Status = paste0("Model error: ", conditionMessage(fitted_model)),
          N = n_observations,
          N_Participants = n_participants,
          N_Events = n_events,
          N_Non_Events = n_non_events,
          Singular = NA,
          Convergence_Message = conditionMessage(fitted_model)
        ),
        tidy = tibble()
      )
    )
  }
  
  convergence_messages <- fitted_model@optinfo$conv$lme4$messages
  
  status <- tibble(
    Model = model_name,
    Status = "Fitted",
    N = n_observations,
    N_Participants = n_participants,
    N_Events = n_events,
    N_Non_Events = n_non_events,
    Singular = lme4::isSingular(fitted_model, tol = 1e-4),
    Convergence_Message = if (
      is.null(convergence_messages)
    ) NA_character_ else paste(convergence_messages, collapse = " | ")
  )
  
  tidy_result <- tryCatch(
    broom.mixed::tidy(
      fitted_model,
      effects = "fixed",
      conf.int = TRUE,
      exponentiate = TRUE
    ) %>%
      mutate(
        Model = model_name,
        Effect_Scale = "Odds ratio",
        .before = 1
      ),
    error = function(e) {
      tibble(
        Model = model_name,
        term = NA_character_,
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        Effect_Scale = paste0("Tidy error: ", conditionMessage(e))
      )
    }
  )
  
  list(
    model = fitted_model,
    status = status,
    tidy = tidy_result
  )
}


model_results <- list()

if (run_mixed_models) {
  model_results$incidental_strict <- fit_binary_glmm(
    daily,
    incidental_strict ~
      incidentality_index_z +
      platform +
      topic_macro +
      study_day_z +
      (1 | participant),
    model_name = "Strict incidentality",
    outcome_variable = "incidental_strict"
  )
  
  model_results$read_thoroughly <- fit_binary_glmm(
    daily,
    interaction_read ~
      incidental_broad +
      locality +
      situation +
      media_format +
      study_day_z +
      (1 | participant),
    model_name = "Thorough reading",
    outcome_variable = "interaction_read"
  )
  
  model_results$further_research <- fit_binary_glmm(
    daily,
    interaction_research ~
      incidental_broad +
      topic_macro +
      intro_ib_problem_z +
      study_day_z +
      (1 | participant),
    model_name = "Further research",
    outcome_variable = "interaction_research"
  )
  
  model_results$visible_engagement <- fit_binary_glmm(
    daily,
    interaction_engagement ~
      incidental_broad +
      source_macro +
      situation +
      intro_intensity_z +
      study_day_z +
      (1 | participant),
    model_name = "Visible engagement",
    outcome_variable = "interaction_engagement"
  )
}

model_status <- if (length(model_results) == 0) {
  tibble(
    Model = "All models",
    Status = "Mixed models disabled",
    N = NA_integer_,
    N_Participants = NA_integer_,
    N_Events = NA_integer_,
    N_Non_Events = NA_integer_,
    Singular = NA,
    Convergence_Message = NA_character_
  )
} else {
  purrr::map_dfr(model_results, "status")
}

model_coefficients <- if (length(model_results) == 0) {
  tibble()
} else {
  purrr::map_dfr(model_results, "tidy")
}

model_objects <- purrr::map(model_results, "model")


#===============================================================================
# 19 Visual design
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


theme_project <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(
        fill = unname(project_colors["white"]),
        color = NA
      ),
      panel.background = element_rect(
        fill = unname(project_colors["white"]),
        color = NA
      ),
      plot.title = element_text(
        color = unname(project_colors["dark"]),
        face = "bold",
        size = rel(1.22),
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        color = unname(project_colors["medium"]),
        margin = margin(b = 11)
      ),
      plot.caption = element_text(
        color = unname(project_colors["medium"]),
        size = rel(0.80),
        hjust = 0,
        margin = margin(t = 10)
      ),
      axis.title = element_text(
        color = unname(project_colors["dark"]),
        face = "bold"
      ),
      axis.text = element_text(
        color = unname(project_colors["dark"])
      ),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = unname(project_colors["grid"]),
        linewidth = 0.4
      ),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(
        fill = unname(project_colors["light"]),
        color = NA
      ),
      strip.text = element_text(
        color = unname(project_colors["dark"]),
        face = "bold"
      ),
      legend.position = "bottom",
      plot.margin = margin(15, 24, 15, 15)
    )
}


theme_set(theme_project())


save_project_plot <- function(
    plot,
    filename,
    width = 8,
    height = 5
) {
  ggsave(
    filename = file.path(figure_folder, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = unname(project_colors["white"])
  )
}


simulation_caption <- if (simulate_coding) {
  "Achtung: Manuelle Codierspalten wurden simuliert. Keine inhaltliche Interpretation."
} else {
  NULL
}


#===============================================================================
# 20 Figures: participation and main distributions
#===============================================================================

figure_posts_participant <- ggplot(
  participant_counts,
  aes(x = N_Screenshots)
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    fill = unname(project_colors["primary"]),
    color = unname(project_colors["white"]),
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = minimum_screenshots,
    color = unname(project_colors["accent"]),
    linewidth = 0.9,
    linetype = "22"
  ) +
  labs(
    title = "Anzahl codierter Beiträge pro Person",
    subtitle = paste0(
      "N = ",
      nrow(participant_counts),
      " Teilnehmende; gestrichelte Linie = Einschlussgrenze"
    ),
    x = "Anzahl der Beiträge",
    y = "Anzahl der Teilnehmenden",
    caption = simulation_caption
  )

save_project_plot(
  figure_posts_participant,
  "Daily_Posts_Per_Participant.png"
)


figure_posts_day <- ggplot(
  day_summary,
  aes(
    x = factor(study_day),
    y = Total_Posts
  )
) +
  geom_col(
    width = 0.66,
    fill = unname(project_colors["primary"])
  ) +
  geom_text(
    aes(label = Total_Posts),
    vjust = -0.35,
    fontface = "bold",
    color = unname(project_colors["dark"])
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.13))
  ) +
  labs(
    title = "Beiträge nach Studientag",
    subtitle = "Absolute Zahl der codierten Beiträge",
    x = "Studientag",
    y = "Anzahl der Beiträge",
    caption = simulation_caption
  )

save_project_plot(
  figure_posts_day,
  "Daily_Posts_By_Day.png"
)


plot_horizontal_counts <- function(
    distribution_data,
    title,
    subtitle,
    filename,
    width = 10,
    height = 7
) {
  plot_data <- distribution_data %>%
    filter(Category != "Missing") %>%
    mutate(
      Category = forcats::fct_reorder(Category, N)
    )
  
  plot <- ggplot(
    plot_data,
    aes(
      x = N,
      y = Category
    )
  ) +
    geom_col(
      width = 0.68,
      fill = unname(project_colors["primary"])
    ) +
    geom_text(
      aes(label = N),
      hjust = -0.18,
      fontface = "bold",
      color = unname(project_colors["dark"]),
      size = 3.3
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(),
      expand = expansion(mult = c(0, 0.14))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Anzahl der Beiträge",
      y = NULL,
      caption = simulation_caption
    ) +
    theme(
      panel.grid.major.x = element_line(
        color = unname(project_colors["grid"]),
        linewidth = 0.4
      ),
      panel.grid.major.y = element_blank()
    )
  
  save_project_plot(
    plot,
    filename,
    width = width,
    height = height
  )
}


plot_horizontal_counts(
  topic_distribution,
  "Themen der Beiträge",
  "Screenshot-gewichtete absolute Häufigkeiten",
  "Daily_Topics.png",
  width = 11,
  height = 8
)

plot_horizontal_counts(
  source_distribution,
  "Quellen- und Accounttypen",
  "Screenshot-gewichtete absolute Häufigkeiten",
  "Daily_Sources.png",
  width = 11,
  height = 7
)

plot_horizontal_counts(
  platform_distribution,
  "Plattformen der Beiträge",
  "Screenshot-gewichtete absolute Häufigkeiten",
  "Daily_Platforms.png",
  width = 8,
  height = 5
)

plot_horizontal_counts(
  format_distribution,
  "Medienformate der Beiträge",
  "Screenshot-gewichtete absolute Häufigkeiten",
  "Daily_Formats.png",
  width = 8,
  height = 5
)

plot_horizontal_counts(
  incidentality_distribution,
  "Art der Informationsbegegnung",
  "Screenshot-gewichtete absolute Häufigkeiten",
  "Daily_Incidentality.png",
  width = 9,
  height = 5
)


#===============================================================================
# 21 Figures: interactions and contexts
#===============================================================================

figure_interactions <- ggplot(
  interaction_distribution,
  aes(
    x = Percent_Yes,
    y = forcats::fct_reorder(Interaction, Percent_Yes)
  )
) +
  geom_col(
    width = 0.66,
    fill = unname(project_colors["primary"])
  ) +
  geom_text(
    aes(
      label = paste0(
        N_Yes,
        " (",
        scales::number(Percent_Yes, accuracy = 0.1, decimal.mark = ","),
        " %)"
      )
    ),
    hjust = -0.12,
    fontface = "bold",
    color = unname(project_colors["dark"]),
    size = 3.2
  ) +
  scale_x_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Auswahl- und Interaktionspraktiken",
    subtitle = "Anteile an allen jeweils gültigen Beitragsangaben",
    x = "Anteil der Beiträge",
    y = NULL,
    caption = simulation_caption
  ) +
  theme(
    panel.grid.major.x = element_line(
      color = unname(project_colors["grid"]),
      linewidth = 0.4
    ),
    panel.grid.major.y = element_blank()
  )

save_project_plot(
  figure_interactions,
  "Daily_Interactions.png",
  width = 10,
  height = 5.5
)


contexts_plot_data <- bind_rows(
  locality_distribution %>%
    filter(Category != "Missing") %>%
    mutate(Context_Dimension = "Räumlich"),
  situation_distribution %>%
    filter(Category != "Missing") %>%
    mutate(Context_Dimension = "Sozial")
)

figure_contexts <- ggplot(
  contexts_plot_data,
  aes(
    x = N,
    y = Category,
    fill = Context_Dimension
  )
) +
  geom_col(width = 0.66) +
  geom_text(
    aes(label = N),
    hjust = -0.18,
    fontface = "bold",
    color = unname(project_colors["dark"])
  ) +
  facet_wrap(
    ~ Context_Dimension,
    scales = "free_y",
    ncol = 2
  ) +
  scale_fill_manual(
    values = c(
      Räumlich = unname(project_colors["primary"]),
      Sozial = unname(project_colors["accent"])
    )
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.15))
  ) +
  coord_cartesian(clip = "off") +
  guides(fill = "none") +
  labs(
    title = "Situative Nutzungskontexte",
    subtitle = "Absolute Häufigkeiten auf Beitragsebene",
    x = "Anzahl der Beiträge",
    y = NULL,
    caption = simulation_caption
  ) +
  theme(
    panel.grid.major.x = element_line(
      color = unname(project_colors["grid"]),
      linewidth = 0.4
    ),
    panel.grid.major.y = element_blank()
  )

save_project_plot(
  figure_contexts,
  "Daily_Contexts.png",
  width = 11,
  height = 5.5
)


#===============================================================================
# 22 Figures: content patterns and exploration
#===============================================================================

heatmap_topic_platform <- topic_by_platform %>%
  group_by(Column) %>%
  mutate(
    Platform_Percent = 100 * N / sum(N)
  ) %>%
  ungroup()

figure_topic_platform <- ggplot(
  heatmap_topic_platform,
  aes(
    x = Column,
    y = Row,
    fill = Platform_Percent
  )
) +
  geom_tile(
    color = unname(project_colors["white"]),
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = if_else(
        Platform_Percent >= 2,
        paste0(round(Platform_Percent), "%"),
        ""
      )
    ),
    size = 3
  ) +
  scale_fill_gradient(
    low = unname(project_colors["white"]),
    high = unname(project_colors["primary"])
  ) +
  labs(
    title = "Themenprofile der Plattformen",
    subtitle = "Spaltenprozente innerhalb jeder Plattform",
    x = NULL,
    y = NULL,
    fill = "Anteil",
    caption = simulation_caption
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )

save_project_plot(
  figure_topic_platform,
  "Daily_Topic_Platform_Heatmap.png",
  width = 10,
  height = 8
)


interaction_incidentality_plot_data <- interaction_by_groups %>%
  filter(Grouping_Variable == "Incidentality")

figure_interaction_incidentality <- ggplot(
  interaction_incidentality_plot_data,
  aes(
    x = Group,
    y = Percent_Yes,
    fill = Outcome
  )
) +
  geom_col(
    position = position_dodge(width = 0.76),
    width = 0.70
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Interaktion nach Art der Informationsbegegnung",
    subtitle = "Deskriptive Anteile auf Beitragsebene",
    x = NULL,
    y = "Anteil der Beiträge",
    fill = NULL,
    caption = simulation_caption
  ) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

save_project_plot(
  figure_interaction_incidentality,
  "Daily_Interactions_By_Incidentality.png",
  width = 11,
  height = 6
)


figure_topic_need_correlations <- ggplot(
  topic_need_correlations,
  aes(
    x = Need,
    y = Topic,
    fill = Spearman_Rho
  )
) +
  geom_tile(
    color = unname(project_colors["white"]),
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = if_else(
        is.na(Spearman_Rho),
        "",
        scales::number(
          Spearman_Rho,
          accuracy = 0.01,
          decimal.mark = ","
        )
      )
    ),
    size = 2.8
  ) +
  scale_fill_gradient2(
    low = unname(project_colors["accent"]),
    mid = unname(project_colors["white"]),
    high = unname(project_colors["primary"]),
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Informationsbedürfnisse und tatsächliche Themenwahl",
    subtitle = "Spearman-Korrelationen zwischen Screening-Scores und personenspezifischen Topic-Anteilen",
    x = NULL,
    y = NULL,
    fill = "Spearman ρ",
    caption = paste(
      "Explorative Analyse; keine konfirmatorischen Hypothesentests.",
      simulation_caption %||% ""
    )
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

save_project_plot(
  figure_topic_need_correlations,
  "Daily_Topic_Need_Correlations.png",
  width = 12,
  height = 9
)


incidentality_alignment_plot_data <- participant_metrics %>%
  filter(
    !is.na(incidentality_index),
    !is.na(Share_Incidental_Broad)
  )

figure_incidentality_alignment <- ggplot(
  incidentality_alignment_plot_data,
  aes(
    x = incidentality_index,
    y = 100 * Share_Incidental_Broad
  )
) +
  geom_point(
    size = 2.5,
    alpha = 0.75,
    color = unname(project_colors["primary"])
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    color = unname(project_colors["accent"])
  ) +
  scale_x_continuous(
    limits = c(1, 5),
    breaks = 1:5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Allgemeine und beobachtungsnahe Inzidentalität",
    subtitle = "Screening-Index versus personenspezifischer Anteil breit inzidenteller Beiträge",
    x = "Incidentality-Index im Screening",
    y = "Diary-Anteil incidental broad",
    caption = paste(
      "Explorative deskriptive Darstellung.",
      simulation_caption %||% ""
    )
  )

save_project_plot(
  figure_incidentality_alignment,
  "Daily_Screening_Diary_Incidentality.png",
  width = 8,
  height = 6
)


diversity_plot_data <- participant_metrics %>%
  select(
    participant,
    Topic = Topic_Shannon,
    Source = Source_Shannon,
    Platform = Platform_Shannon,
    Format = Format_Shannon
  ) %>%
  pivot_longer(
    cols = -participant,
    names_to = "Dimension",
    values_to = "Shannon"
  )

figure_diversity <- ggplot(
  diversity_plot_data,
  aes(
    x = Dimension,
    y = Shannon
  )
) +
  geom_boxplot(
    width = 0.58,
    outlier.shape = NA,
    fill = unname(project_colors["light"]),
    color = unname(project_colors["primary"])
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.50,
    size = 1.7,
    color = unname(project_colors["dark"])
  ) +
  labs(
    title = "Diversität der individuellen Informationsrepertoires",
    subtitle = "Shannon-Entropie auf Personenebene",
    x = NULL,
    y = "Shannon-Entropie",
    caption = simulation_caption
  )

save_project_plot(
  figure_diversity,
  "Daily_Participant_Diversity.png",
  width = 8,
  height = 6
)


need_alignment_plot_data <- participant_metrics %>%
  filter(!is.na(Need_Content_Alignment))

figure_need_alignment <- ggplot(
  need_alignment_plot_data,
  aes(x = Need_Content_Alignment)
) +
  geom_histogram(
    binwidth = 0.20,
    boundary = -1,
    fill = unname(project_colors["primary"]),
    color = unname(project_colors["white"]),
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = safe_mean(need_alignment_plot_data$Need_Content_Alignment),
    color = unname(project_colors["accent"]),
    linetype = "22",
    linewidth = 0.9
  ) +
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.5)
  ) +
  labs(
    title = "Übereinstimmung von Informationsbedürfnissen und Diary-Profil",
    subtitle = "Personeninterne Rangkorrelation über vier Bedürfnis-/Inhaltsproxys",
    x = "Profil-Übereinstimmung (Spearman ρ)",
    y = "Anzahl der Teilnehmenden",
    caption = paste(
      "Stark explorativer zusammengesetzter Indikator; soziales Bedürfnis wird durch Peer-Quellen approximiert.",
      simulation_caption %||% ""
    )
  )

save_project_plot(
  figure_need_alignment,
  "Daily_Need_Content_Alignment.png",
  width = 9,
  height = 6
)


#===============================================================================
# 23 Save prepared data and model objects
#===============================================================================

saveRDS(
  daily,
  output_screenshot_rds
)

saveRDS(
  participant_metrics,
  output_participant_rds
)

saveRDS(
  list(
    models = model_objects,
    status = model_status,
    coefficients = model_coefficients,
    simulated = simulate_coding
  ),
  output_models_rds
)


#===============================================================================
# 24 Create Excel workbook
#===============================================================================

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  fgFill = "#E8EFF1",
  border = "Bottom"
)

settings_table <- tibble(
  Setting = c(
    "simulate_coding",
    "simulation_seed",
    "simulation_overwrite_existing",
    "simulation_use_screening_patterns",
    "minimum_screenshots",
    "require_screening_match",
    "strict_coding_check",
    "run_mixed_models",
    "minimum_model_n",
    "minimum_model_participants",
    "minimum_model_events"
  ),
  Value = as.character(
    c(
      simulate_coding,
      simulation_seed,
      simulation_overwrite_existing,
      simulation_use_screening_patterns,
      minimum_screenshots,
      require_screening_match,
      strict_coding_check,
      run_mixed_models,
      minimum_model_n,
      minimum_model_participants,
      minimum_model_events
    )
  )
)

add_excel_sheet(workbook, "Settings", settings_table, header_style)
add_excel_sheet(workbook, "Sample_Overview", sample_overview, header_style)
add_excel_sheet(workbook, "Exclusions", analysis_exclusions, header_style)
add_excel_sheet(workbook, "Coding_Completeness", coding_completeness, header_style)
add_excel_sheet(workbook, "Invalid_Categories", invalid_categories, header_style)
add_excel_sheet(workbook, "Duplicate_Screenshot_IDs", duplicate_screenshot_ids, header_style)
add_excel_sheet(workbook, "Duplicate_Filenames", duplicate_filenames, header_style)
add_excel_sheet(workbook, "Platform_Mismatches", platform_mismatches, header_style)
add_excel_sheet(workbook, "Study_Day_Issues", study_day_issues, header_style)
add_excel_sheet(workbook, "File_Issues", file_issues, header_style)
add_excel_sheet(workbook, "Participant_Counts", participant_counts, header_style)
add_excel_sheet(workbook, "Day_Summary", day_summary, header_style)

add_excel_sheet(workbook, "Topic_Screenshot", topic_distribution, header_style)
add_excel_sheet(workbook, "Topic_Participant", topic_participant_summary, header_style)
add_excel_sheet(workbook, "Topic_Shares_Person", topic_shares, header_style)
add_excel_sheet(workbook, "Source_Screenshot", source_distribution, header_style)
add_excel_sheet(workbook, "Source_Participant", source_participant_summary, header_style)
add_excel_sheet(workbook, "Source_Shares_Person", source_shares, header_style)
add_excel_sheet(workbook, "Account_Names", source_name_distribution, header_style)
add_excel_sheet(workbook, "Platform_Screenshot", platform_distribution, header_style)
add_excel_sheet(workbook, "Platform_Participant", platform_participant_summary, header_style)
add_excel_sheet(workbook, "Platform_Shares_Person", platform_shares, header_style)
add_excel_sheet(workbook, "Format_Screenshot", format_distribution, header_style)
add_excel_sheet(workbook, "Format_Participant", format_participant_summary, header_style)
add_excel_sheet(workbook, "Format_Shares_Person", format_shares, header_style)
add_excel_sheet(workbook, "Incidentality", incidentality_distribution, header_style)
add_excel_sheet(workbook, "Incidentality_Person", incidentality_participant_summary, header_style)
add_excel_sheet(workbook, "Interactions", interaction_distribution, header_style)
add_excel_sheet(workbook, "Local_Context", locality_distribution, header_style)
add_excel_sheet(workbook, "Social_Context", situation_distribution, header_style)

add_excel_sheet(workbook, "Participant_Metrics", participant_metrics, header_style)
add_excel_sheet(workbook, "Participant_Metric_Summary", participant_metrics_summary, header_style)
add_excel_sheet(workbook, "Topic_Platform", topic_by_platform, header_style)
add_excel_sheet(workbook, "Source_Platform", source_by_platform, header_style)
add_excel_sheet(workbook, "Format_Platform", format_by_platform, header_style)
add_excel_sheet(workbook, "Topic_Incidentality", topic_by_incidentality, header_style)
add_excel_sheet(workbook, "Source_Incidentality", source_by_incidentality, header_style)
add_excel_sheet(workbook, "Format_Incidentality", format_by_incidentality, header_style)
add_excel_sheet(workbook, "Interaction_Groups", interaction_by_groups, header_style)

add_excel_sheet(workbook, "Screening_Diary_Cor", screening_diary_correlations, header_style)
add_excel_sheet(workbook, "Topic_Need_Cor", topic_need_correlations, header_style)
add_excel_sheet(workbook, "Source_Need_Cor", source_need_correlations, header_style)
add_excel_sheet(workbook, "Platform_Alignment", platform_alignment_correlations, header_style)
add_excel_sheet(workbook, "Alignment_Summary", alignment_summary, header_style)
add_excel_sheet(workbook, "Subgroup_Summaries", subgroup_summaries, header_style)
add_excel_sheet(workbook, "Model_Status", model_status, header_style)
add_excel_sheet(workbook, "Model_Odds_Ratios", model_coefficients, header_style)

openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = TRUE
)


#===============================================================================
# 25 Console report
#===============================================================================

cat(
  "\n",
  "============================================================\n",
  "DAILY ANALYSIS COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat(
  "Simulation active: ",
  simulate_coding,
  "\n",
  sep = ""
)

if (simulate_coding) {
  cat(
    "WARNING: Manual coding variables were simulated. ",
    "Do not interpret substantive results.\n",
    sep = ""
  )
}

cat(
  "Rows in coding sheet: ",
  nrow(coding),
  "\n",
  sep = ""
)

cat(
  "Participants in final daily sample: ",
  n_distinct(daily$participant),
  "\n",
  sep = ""
)

cat(
  "Screenshots in final daily sample: ",
  nrow(daily),
  "\n",
  sep = ""
)

cat(
  "Median screenshots per participant: ",
  round(safe_median(participant_counts$N_Screenshots), 2),
  "\n",
  sep = ""
)

cat(
  "Broad incidental exposure: ",
  round(100 * safe_mean(daily$incidental_broad), 1),
  "%\n",
  sep = ""
)

cat(
  "Strict incidental exposure: ",
  round(100 * safe_mean(daily$incidental_strict), 1),
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
  output_screenshot_rds,
  "\n",
  sep = ""
)

cat(
  "\nParticipant-level RDS:\n",
  output_participant_rds,
  "\n",
  sep = ""
)

cat(
  "\nFigures:\n",
  figure_folder,
  "\n",
  sep = ""
)

if (simulate_coding && write_simulated_coding_sheet) {
  cat(
    "\nSimulated coding sheet:\n",
    simulated_coding_file,
    "\n",
    sep = ""
  )
}

cat(
  "============================================================\n"
)
