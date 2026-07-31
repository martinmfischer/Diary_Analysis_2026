################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    04b_Daily_Analysis.R
# Version: 2026-07-30
#
#
# REQUIRED INPUTS
# ---------------
# 06_Coding/coding_sheet.xlsx
#   Sheet: `Coding`
#   Required identifier:
#     participant
#   Required diary/context variables or recognised aliases:
#     study_day, filename and/or screenshot_id, platform_reported,
#     incidentality_label, interaction_read, interaction_research,
#     interaction_engagement, locality_label, situation_label
#   Manual coding:
#     public_rel_coded  : 1 = publicly relevant, 0 = not publicly relevant
#     topic_coded       : main topic according to the project codebook
#     source_coded      : source/account type
#     source_name_coded : concrete visible account/source name; optional
#     platform_coded    : manual platform validation; falls back to self-report
#     media_format      : dominant media format
#     coding_completed  : coding row completed
#
# 03_Output/screening_prepared.rds
#   Created by 04a_Screening_Analysis.R. The script uses, where available:
#     age, gender, education, usage intensity, platform frequencies,
#     platform repertoire, information needs, screening incidentality,
#     typical local/social usage context and primary platform.
#
# CENTRAL SETTINGS
# ----------------
# simulate_coding
#   TRUE  = reproducibly fills missing coding values for pipeline tests.
#   FALSE = uses the actual completed coding sheet.
#   Simulated results are written to separate files/folders and MUST NOT be
#   interpreted substantively.
#
# public_relevance_mode
#   "required" = stop when the public-relevance column is absent/incomplete.
#   "auto"     = backward-compatible mode; if the column is entirely absent,
#                all screenshots are provisionally treated as publicly relevant.
#                Use only while the coding sheet is being migrated.
#
# minimum_screenshots
#   Inclusion threshold on ALL uploaded screenshots, not only on publicly
#   relevant or fully content-coded screenshots. This implements the study-level
#   diary inclusion rule independently of later content exclusions.
#
# strict_coding_check
#   TRUE stops the final non-simulated run when:
#     - public relevance is undecided,
#     - a publicly relevant contribution lacks Topic/Source/Format coding,
#     - coding_completed is false,
#     - coded categories are outside the canonical codebook.
#
# run_mixed_models / run_icc_models
#   Switches exploratory multilevel models and variance-partition estimates on
#   or off. Models are only estimated when minimum data/event thresholds are met.
#
# ANALYSES
# --------
# A. Data integrity and coding quality
#    - missing and duplicate screenshot IDs/filenames
#    - missing files, implausible study days, incomplete coding
#    - invalid category values
#    - reported versus manually coded platform
#    - reported versus coded account-name pairs for qualitative inspection
#
# B. Sample formation and diary participation
#    - screening match and minimum-screenshot inclusion
#    - exclusions and participation flow
#    - screenshots and active days per participant
#    - uploads and contributing participants by study day
#
# C. Public relevance
#    - absolute and relative frequency of publicly relevant screenshots
#    - public relevance by platform and study day
#    - person-specific public-relevance share
#    - exploratory associations with age, education, information needs and
#      platform repertoire
#    - only publicly relevant contributions enter the main content analyses
#
# D. Main descriptive content analyses
#    - screenshot-weighted and participant-weighted distributions of:
#      Topic, source/account type, source/account name, platform, media format,
#      incidentality, local/social context and interactions
#    - cross-tabulations: Topic/Source/Format × Platform and Incidentality
#
# E. Information repertoires
#    - richness, Shannon entropy, evenness and dominant-category share for
#      Topic, Source, Platform and Format
#    - unique-account count and account concentration
#    - broad content families: current affairs/public affairs,
#      practical/service information and knowledge/interests/culture
#
# F. Information needs and observed content
#    - targeted Screening–Diary correlations
#    - complete Topic × information-need and Source × information-need matrices
#    - post-level information-need fit based on the theoretically corresponding
#      screening need for the observed topic/source domain
#    - exploratory interaction of need fit with incidental exposure
#
# G. Self-report calibration
#    - screening incidentality versus diary incidentality
#    - signed and absolute standardised incidentality gaps
#    - screening versus diary platform profile
#    - screening versus diary local/social context
#    - calibration differences by sociodemographic groups
#    NOTE: screening and diary indicators refer to different temporal and
#    measurement frames. Discrepancies are therefore called calibration gaps,
#    not automatically self-report bias.
#
# H. Processing and engagement
#    - thorough reading/viewing, further research and visible interaction
#    - descriptive processing-pattern combinations
#    - descriptive conversion funnel; not interpreted as a directly observed
#      temporal sequence
#    - processing by Topic, Source, Platform, Format, Incidentality and context
#
# I. Novelty and productive serendipity
#    - first occurrence of a Topic, source type or account within each person's
#      diary sequence
#    - novelty by incidentality and study day
#    - productive serendipity: incidental exposure + repertoire novelty +
#      at least thorough reading, research or visible interaction
#    NOTE: novelty is diary-internal and depends on the uploaded sample, not the
#    participant's complete prior knowledge or total platform exposure.
#
# J. Platform complementarity
#    - within-person Jensen–Shannon divergence of platform-specific Topic,
#      Source and Format profiles when at least two platforms contain enough
#      observations
#    - larger values indicate more differentiated platform functions
#
# K. Sociodemographic and age exploration
#    - descriptive subgroup tables by age group, gender and education
#    - targeted continuous age correlations with content, format, source,
#      incidentality, engagement, diversity, public relevance and calibration
#    - results are exploratory and must not be interpreted as age deficits
#
# L. Between-person versus within-person variation
#    - optional logistic random-intercept ICCs for public relevance,
#      incidentality, reading, research and engagement
#
# M. Temporal patterns and possible reactivity
#    - uploads, public relevance, targeted/incidental use, processing, novelty
#      and account repetition across the seven diary days
#    - descriptive only; day trends can also reflect ordinary temporal variation
#      or mechanical decline in first-occurrence novelty.
#
# N. Optional exploratory mixed models
#    - public relevance
#    - strict incidental exposure
#    - thorough reading/viewing
#    - further research
#    - visible engagement
#    - models use participant random intercepts and are accompanied by status,
#      convergence and singularity checks.
#
# OUTPUT FILES
# ------------
# Real coding:
#   03_Output/Daily_Results.xlsx
#   03_Output/daily_screenshot_level.rds
#   03_Output/daily_public_content_level.rds
#   03_Output/daily_participant_level.rds
#   03_Output/daily_exploratory_models.rds
#   04_Figures/Daily_*.png
#
# Simulation:
#   03_Output/Daily_Results_SIMULATED.xlsx
#   03_Output/daily_screenshot_level_SIMULATED.rds
#   03_Output/daily_public_content_level_SIMULATED.rds
#   03_Output/daily_participant_level_SIMULATED.rds
#   03_Output/daily_exploratory_models_SIMULATED.rds
#   06_Coding/coding_sheet_simulated.xlsx
#   04_Figures/Simulated_Daily/Daily_*.png
#
# EXCEL WORKBOOK SHEETS
# ---------------------
# Documentation:
#   Settings, Method_Notes
# Sample and quality control:
#   Sample_Overview, Exclusions, Coding_Completeness, PublicRel_Issues,
#   Coding_Completion_Issues, Content_Coding_Issues, Invalid_Categories,
#   Duplicate_Screenshot_IDs, Duplicate_Filenames, Platform_Mismatches,
#   Reported_Coded_Accounts, Study_Day_Issues, File_Issues
# Participation and public relevance:
#   Participant_Counts, Day_Summary, PublicRel_Distribution,
#   PublicRel_Platform, PublicRel_Day
# Main distributions:
#   Topic_Screenshot, Topic_Participant, Topic_Shares_Person,
#   Source_Screenshot, Source_Participant, Source_Shares_Person, Account_Names,
#   Platform_Screenshot, Platform_Participant, Platform_Shares_Person,
#   Format_Screenshot, Format_Participant, Format_Shares_Person,
#   Incidentality, Incidentality_Person, Interactions, Local_Context,
#   Social_Context
# Processing, novelty and need fit:
#   Processing_Patterns, Conversion_Funnel, Incidental_Conversion,
#   Novelty_Summary, Novelty_Incidentality, Post_Need_Fit
# Participant-level and content-pattern tables:
#   Participant_Metrics, Participant_Metric_Summary, Topic_Platform,
#   Source_Platform, Format_Platform, Topic_Incidentality,
#   Source_Incidentality, Format_Incidentality, Interaction_Groups
# Screening–Diary integration and sociodemographics:
#   Screening_Diary_Cor, Topic_Need_Cor, Source_Need_Cor,
#   Platform_Alignment, Alignment_Summary, Calibration_Subgroups,
#   Age_Marker_Cor, Subgroup_Summaries
# Platform differentiation and multilevel models:
#   Platform_Complementarity, Platform_Complement_Person, ICC_Results,
#   Model_Status, Model_Odds_Ratios
#
# FIGURE FILES
# ------------
#   Daily_Posts_Per_Participant.png
#   Daily_Posts_By_Day.png
#   Daily_Topics.png
#   Daily_Sources.png
#   Daily_Platforms.png
#   Daily_Formats.png
#   Daily_Incidentality.png
#   Daily_Interactions.png
#   Daily_Contexts.png
#   Daily_Interactions_By_Incidentality.png
#   Daily_Topic_Platform_Heatmap.png
#   Daily_Topic_Need_Correlations.png
#   Daily_Screening_Diary_Incidentality.png
#   Daily_Participant_Diversity.png
#   Daily_Public_Relevance_By_Platform.png
#   Daily_Processing_Funnel.png
#   Daily_Novelty_By_Incidentality.png
#   Daily_Incidentality_Calibration_Gap.png
#   Daily_Age_Marker_Correlations.png
#   Daily_Platform_Complementarity.png
#   Daily_Study_Day_Trends.png
#   Daily_Post_Need_Fit.png
#

#
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

# Public-relevance gate:
#   "required" = require explicit 0/1 coding for every screenshot.
#   "auto"     = if the column is entirely absent, provisionally assume 1.
public_relevance_mode <- "auto"

# Stop when the final, non-simulated coding sheet still contains incomplete or
# invalid manual codes. While simulation is active, missing codes are filled.
strict_coding_check <- TRUE

# Optional exploratory generalized linear mixed models and logistic ICCs.
# Models are only fitted when there are enough observations, participants and
# outcome events/non-events.
run_mixed_models <- TRUE
run_icc_models <- TRUE
minimum_model_n <- 100
minimum_model_participants <- 20
minimum_model_events <- 15

# Minimum number of public-content posts on each platform required before a
# within-person platform-profile comparison is calculated.
minimum_posts_per_platform_profile <- 2

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


helper_script <- file.path(
  "02_Scripts",
  "00_Helpers.R"
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
  
  output_public_rds <- file.path(
    output_folder,
    "daily_public_content_level_SIMULATED.rds"
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
  
  output_public_rds <- file.path(
    output_folder,
    "daily_public_content_level.rds"
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


if (file.exists(helper_script)) {
  source(helper_script)
} else {
  warning(
    "Helper-Script nicht gefunden: ",
    helper_script,
    "!!"
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


canonicalise_public_relevance <- function(x) {
  x_clean <- stringr::str_to_lower(
    clean_text(x)
  )
  
  x_numeric <- clean_numeric(
    x_clean
  )
  
  case_when(
    x_numeric == 1 ~ 1L,
    x_numeric == 0 ~ 0L,
    x_clean %in% c(
      "ja",
      "yes",
      "true",
      "öffentlich relevant",
      "oeffentlich relevant",
      "publicly relevant",
      "relevant"
    ) ~ 1L,
    x_clean %in% c(
      "nein",
      "no",
      "false",
      "nicht öffentlich relevant",
      "nicht oeffentlich relevant",
      "not publicly relevant",
      "nicht relevant"
    ) ~ 0L,
    TRUE ~ NA_integer_
  )
}


canonicalise_topic <- function(x) {
  x_clean <- clean_text(
    x
  )
  
  x_numeric <- clean_numeric(
    x_clean
  )
  
  x_mapped <- x_clean
  
  valid_numeric_code <-
    !is.na(
      x_numeric
    ) &
    x_numeric ==
    floor(
      x_numeric
    ) &
    x_numeric >= 1 &
    x_numeric <=
    length(
      topic_levels
    )
  
  x_mapped[
    valid_numeric_code
  ] <- topic_levels[
    as.integer(
      x_numeric[
        valid_numeric_code
      ]
    )
  ]
  
  dplyr::recode(
    x_mapped,
    "Politik & Regieren" =
      "Politik, Staat & Wahlen",
    "Internationales, Krieg & Sicherheit" =
      "Internationales, Krieg & Sicherheit",
    "Wirtschaft, Arbeit & Verbraucher" =
      "Wirtschaft, Arbeit, Finanzen & Verbraucher",
    "Soziales, Bildung & Wissenschaft" =
      "Gesellschaft, Soziales, Migration & Religion",
    "Gesundheit & Medizin" =
      "Gesundheit & Pflege",
    "Kriminalität, Justiz & Polizei" =
      "Kriminalität & Justiz",
    "Unterhaltung & Prominenz" =
      "Kultur, Medien & Unterhaltung",
    "Lifestyle, Alltag & Service" =
      "Veranstaltungen & öffentlicher Service",
    "Sonstiges / unklar" =
      "Sonstiges / nicht eindeutig",
    .default = x_mapped
  )
}


canonicalise_source <- function(x) {
  x_clean <- clean_text(
    x
  )
  
  x_numeric <- clean_numeric(
    x_clean
  )
  
  x_mapped <- x_clean
  
  valid_numeric_code <-
    !is.na(
      x_numeric
    ) &
    x_numeric ==
    floor(
      x_numeric
    ) &
    x_numeric >= 1 &
    x_numeric <=
    length(
      source_levels
    )
  
  x_mapped[
    valid_numeric_code
  ] <- source_levels[
    as.integer(
      x_numeric[
        valid_numeric_code
      ]
    )
  ]
  
  dplyr::recode(
    x_mapped,
    "Journalistische Medien" =
      "Journalistisches Medium",
    "Alternative / partizipative News" =
      "Alternatives oder parteiisches Medienangebot",
    "Partei / Politiker:in" =
      "Partei oder Politiker:in",
    "Staat / öffentliche Institution" =
      "Staatliche oder öffentliche Institution",
    "NGO / Initiative / Bewegung" =
      "NGO, Verband, Verein, Initiative oder Bewegung",
    "Wissenschaft / Expertise" =
      "Wissenschaft, Expert:in oder Faktencheck",
    "Unternehmen / Marke" =
      "Unternehmen oder Marke",
    "Creator / Influencer:in" =
      "Journalist:in, Creator, Influencer:in oder öffentliche Person",
    "Privatperson / Peer" =
      "Private Person / Peer",
    "Kollektiv / anonym / Meme-Aggregator" =
      "Kollektiv, Meme-, Satire- oder Aggregator-Seite",
    "Unklar / nicht sichtbar" =
      "Sonstige / Quelle nicht erkennbar",
    .default = x_mapped
  )
}


canonicalise_format <- function(x) {
  x_original <- clean_text(
    x
  )
  
  x_clean <- stringr::str_to_lower(
    x_original
  )
  
  x_numeric <- clean_numeric(
    x_clean
  )
  
  mapped_numeric <- rep(
    NA_character_,
    length(
      x_clean
    )
  )
  
  valid_numeric_code <-
    !is.na(
      x_numeric
    ) &
    x_numeric ==
    floor(
      x_numeric
    ) &
    x_numeric >= 1 &
    x_numeric <=
    length(
      format_levels
    )
  
  mapped_numeric[
    valid_numeric_code
  ] <- format_levels[
    as.integer(
      x_numeric[
        valid_numeric_code
      ]
    )
  ]
  
  case_when(
    !is.na(
      mapped_numeric
    ) ~
      mapped_numeric,
    
    x_clean %in% c(
      "text",
      "textbasiert",
      "text-based"
    ) ~
      "Textbasiert",
    
    x_clean %in% c(
      "bild",
      "bildbasiert",
      "image",
      "image-based",
      "foto"
    ) ~
      "Bildbasiert",
    
    x_clean %in% c(
      "video",
      "videobasiert",
      "video-based",
      "reel",
      "tiktok"
    ) ~
      "Videobasiert",
    
    str_detect(
      x_clean,
      "misch|gemischt|mixed|nicht eindeutig|unklar"
    ) ~
      "Mischform / nicht eindeutig",
    
    TRUE ~ x_original
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

public_relevance_candidates <- c(
  "public_rel_coded",
  "public_relevant",
  "public_relevance",
  "public_rel",
  "public_relevant_coded"
)

public_relevance_column_present <- any(
  public_relevance_candidates %in% names(coding_raw)
)

coding_completed_candidates <- c(
  "coding_completed",
  "coding_complete",
  "completed"
)

coding_completed_column_present <- any(
  coding_completed_candidates %in%
    names(
      coding_raw
    )
)

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
  public_rel_coded = c(
    "public_relevant",
    "public_relevance",
    "public_rel",
    "public_relevant_coded"
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
    file_exists_binary = clean_binary(file_exists),
    public_relevance = canonicalise_public_relevance(public_rel_coded)
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
      participant_first_scheduled_date = safe_date_min(
        scheduled_date
      ),
      inferred_study_day = if_else(
        !is.na(scheduled_date) &
          !is.na(participant_first_scheduled_date),
        as.numeric(
          scheduled_date -
            participant_first_scheduled_date
        ) + 1,
        NA_real_
      ),
      study_day = coalesce(study_day, inferred_study_day)
    ) %>%
    ungroup() %>%
    select(
      -inferred_study_day,
      -participant_first_scheduled_date
    )
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
} else {
  coding <- coding %>%
    mutate(
      coding_was_simulated = FALSE
    )
}


# Simulate the public-relevance gate separately. This allows the test run to
# exercise the rule that non-public contributions do not require content coding.
if (simulate_coding) {
  set.seed(
    simulation_seed + 101
  )
  
  simulated_public_relevance_probability <- case_when(
    coding$topic_coded == "Sonstiges / nicht eindeutig" ~ 0.68,
    coding$source_coded == "Private Person / Peer" ~ 0.80,
    coding$source_coded ==
      "Kollektiv, Meme-, Satire- oder Aggregator-Seite" ~ 0.86,
    TRUE ~ 0.95
  )
  
  simulated_public_relevance <- rbinom(
    n = nrow(coding),
    size = 1,
    prob = simulated_public_relevance_probability
  )
  
  coding <- coding %>%
    mutate(
      public_relevance = if (
        simulation_overwrite_existing
      ) {
        simulated_public_relevance
      } else {
        coalesce(
          public_relevance,
          simulated_public_relevance
        )
      },
      public_rel_coded = public_relevance,
      coding_was_simulated = TRUE
    )
}


if (!simulate_coding && !public_relevance_column_present) {
  if (public_relevance_mode == "required") {
    stop(
      "Im Coding Sheet wurde keine Spalte für public_rel_coded gefunden. ",
      "Ergänze die Gate-Codierung oder setze public_relevance_mode <- 'auto' ",
      "nur für einen vorläufigen, rückwärtskompatiblen Lauf."
    )
  }
  
  if (public_relevance_mode == "auto") {
    warning(
      "Die Spalte public_rel_coded fehlt vollständig. ",
      "Alle Screenshots werden vorläufig als öffentlich relevant behandelt. ",
      "Für die finale Analyse public_relevance_mode <- 'required' verwenden."
    )
    
    coding <- coding %>%
      mutate(
        public_relevance = 1L,
        public_rel_coded = 1L
      )
  }
}


if (
  !public_relevance_mode %in% c(
    "required",
    "auto"
  )
) {
  stop(
    "public_relevance_mode muss 'required' oder 'auto' sein."
  )
}


if (
  simulate_coding &&
  write_simulated_coding_sheet
) {
  openxlsx::write.xlsx(
    coding,
    file = simulated_coding_file,
    overwrite = TRUE,
    asTable = TRUE
  )
}


#===============================================================================
# 09 Coding quality checks
#===============================================================================

content_coding_variables <- c(
  "topic_coded",
  "source_coded",
  "media_format"
)


coding <- coding %>%
  mutate(
    public_relevance_complete =
      !is.na(
        public_relevance
      ),
    
    content_coding_complete = case_when(
      public_relevance == 0L ~ TRUE,
      public_relevance == 1L ~ if_all(
        all_of(
          content_coding_variables
        ),
        ~ !is.na(
          clean_text(
            .x
          )
        )
      ),
      TRUE ~ FALSE
    ),
    
    coding_completed_binary = if (
      simulate_coding ||
      !coding_completed_column_present
    ) {
      coalesce(
        coding_completed_binary,
        as.integer(
          public_relevance_complete &
            content_coding_complete
        )
      )
    } else {
      coding_completed_binary
    },
    
    analysis_coding_complete =
      public_relevance_complete &
      content_coding_complete &
      coding_completed_binary == 1L
  )


coding_completeness <- tibble(
  Variable = c(
    "public_rel_coded",
    content_coding_variables,
    "source_name_coded",
    "platform_coded",
    "coding_completed"
  ),
  Required_For = c(
    "All screenshots",
    rep(
      "Publicly relevant screenshots only",
      length(
        content_coding_variables
      )
    ),
    "Optional descriptive account name",
    "All screenshots; defaults to reported platform",
    "All screenshots"
  ),
  N_Denominator = c(
    nrow(
      coding
    ),
    rep(
      sum(
        coding$public_relevance == 1L,
        na.rm = TRUE
      ),
      length(
        content_coding_variables
      )
    ),
    nrow(
      coding
    ),
    nrow(
      coding
    ),
    nrow(
      coding
    )
  ),
  N_Complete = c(
    sum(
      !is.na(
        coding$public_relevance
      )
    ),
    map_int(
      content_coding_variables,
      ~ sum(
        coding$public_relevance == 1L &
          !is.na(
            clean_text(
              coding[[.x]]
            )
          ),
        na.rm = TRUE
      )
    ),
    sum(
      !is.na(
        clean_text(
          coding$source_name_coded
        )
      )
    ),
    sum(
      !is.na(
        clean_text(
          coding$platform_coded
        )
      )
    ),
    sum(
      coding$coding_completed_binary == 1L,
      na.rm = TRUE
    )
  )
) %>%
  mutate(
    N_Missing =
      N_Denominator -
      N_Complete,
    
    Percent_Complete = safe_percent(
      N_Complete,
      N_Denominator
    )
  )


public_relevance_issues <- coding %>%
  filter(
    is.na(
      public_relevance
    )
  ) %>%
  select(
    screenshot_id,
    participant,
    study_day,
    filename,
    public_rel_coded,
    coding_completed
  )


coding_completion_issues <- coding %>%
  filter(
    is.na(
      coding_completed_binary
    ) |
      coding_completed_binary != 1L
  ) %>%
  select(
    screenshot_id,
    participant,
    study_day,
    filename,
    public_relevance,
    coding_completed,
    coding_completed_binary
  )


content_coding_issues <- coding %>%
  filter(
    public_relevance == 1L,
    !content_coding_complete |
      coding_completed_binary != 1L
  ) %>%
  select(
    screenshot_id,
    participant,
    study_day,
    filename,
    topic_coded,
    source_coded,
    source_name_coded,
    platform_coded,
    media_format,
    coding_completed_binary
  )


duplicate_screenshot_ids <- coding %>%
  count(
    screenshot_id,
    name = "N_Rows"
  ) %>%
  filter(
    !is.na(
      screenshot_id
    ),
    N_Rows > 1
  )


duplicate_filenames <- coding %>%
  count(
    filename,
    name = "N_Rows"
  ) %>%
  filter(
    !is.na(
      filename
    ),
    N_Rows > 1
  )


invalid_categories <- bind_rows(
  coding %>%
    filter(
      public_relevance == 1L,
      !is.na(
        topic_coded
      ),
      !topic_coded %in%
        topic_levels
    ) %>%
    count(
      topic_coded,
      name = "N"
    ) %>%
    transmute(
      Variable = "topic_coded",
      Invalid_Value = topic_coded,
      N
    ),
  
  coding %>%
    filter(
      public_relevance == 1L,
      !is.na(
        source_coded
      ),
      !source_coded %in%
        source_levels
    ) %>%
    count(
      source_coded,
      name = "N"
    ) %>%
    transmute(
      Variable = "source_coded",
      Invalid_Value = source_coded,
      N
    ),
  
  coding %>%
    filter(
      public_relevance == 1L,
      !is.na(
        media_format
      ),
      !media_format %in%
        format_levels
    ) %>%
    count(
      media_format,
      name = "N"
    ) %>%
    transmute(
      Variable = "media_format",
      Invalid_Value = media_format,
      N
    ),
  
  coding %>%
    filter(
      !is.na(
        platform_coded
      ),
      !platform_coded %in%
        platform_levels
    ) %>%
    count(
      platform_coded,
      name = "N"
    ) %>%
    transmute(
      Variable = "platform_coded",
      Invalid_Value = platform_coded,
      N
    ),
  
  coding %>%
    filter(
      !is.na(
        public_relevance
      ),
      !public_relevance %in%
        c(
          0L,
          1L
        )
    ) %>%
    count(
      public_relevance,
      name = "N"
    ) %>%
    transmute(
      Variable = "public_rel_coded",
      Invalid_Value = as.character(
        public_relevance
      ),
      N
    )
)


platform_mismatches <- coding %>%
  filter(
    !is.na(
      platform_reported
    ),
    !is.na(
      platform_coded
    ),
    platform_reported !=
      platform_coded
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


reported_coded_accounts <- coding %>%
  filter(
    !is.na(
      clean_text(
        account_participant
      )
    ) |
      !is.na(
        clean_text(
          source_name_coded
        )
      )
  ) %>%
  transmute(
    screenshot_id,
    participant,
    study_day,
    platform_coded,
    Account_Reported =
      clean_text(
        account_participant
      ),
    Account_Coded =
      clean_text(
        source_name_coded
      ),
    Exact_Normalised_Match = case_when(
      is.na(
        Account_Reported
      ) |
        is.na(
          Account_Coded
        ) ~ NA_integer_,
      str_to_lower(
        Account_Reported
      ) ==
        str_to_lower(
          Account_Coded
        ) ~ 1L,
      TRUE ~ 0L
    )
  )


study_day_issues <- coding %>%
  filter(
    is.na(
      study_day
    ) |
      !study_day %in%
      expected_study_days
  ) %>%
  select(
    screenshot_id,
    participant,
    study_day,
    filename,
    scheduled
  )


file_issues <- coding %>%
  filter(
    file_exists_binary == 0L
  ) %>%
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
  nrow(
    public_relevance_issues
  ) > 0
) {
  stop(
    nrow(
      public_relevance_issues
    ),
    " Screenshot-Zeilen besitzen keine gültige Public-Relevance-Codierung."
  )
}


if (
  strict_coding_check &&
  !simulate_coding &&
  nrow(
    coding_completion_issues
  ) > 0
) {
  stop(
    nrow(
      coding_completion_issues
    ),
    " Screenshot-Zeilen sind nicht als vollständig codiert markiert."
  )
}


if (
  strict_coding_check &&
  !simulate_coding &&
  nrow(
    content_coding_issues
  ) > 0
) {
  stop(
    nrow(
      content_coding_issues
    ),
    " öffentlich relevante Screenshot-Zeilen sind noch nicht vollständig ",
    "in Topic, Source und Format codiert oder nicht als abgeschlossen markiert."
  )
}


if (
  strict_coding_check &&
  nrow(
    invalid_categories
  ) > 0
) {
  stop(
    "Das Coding Sheet enthält Werte außerhalb des festgelegten ",
    "Kategoriensystems. Siehe Tabelle 'Invalid_Categories'."
  )
}


#===============================================================================
# 10 Build the analysis sample
#===============================================================================

participant_counts_before_screening <- coding %>%
  group_by(
    participant
  ) %>%
  summarise(
    N_Screenshots_Total = n(),
    
    N_Public_Relevance_Coded = sum(
      !is.na(
        public_relevance
      )
    ),
    
    N_Public_Relevant = sum(
      public_relevance == 1L,
      na.rm = TRUE
    ),
    
    N_Content_Analyzable = sum(
      public_relevance == 1L &
        analysis_coding_complete,
      na.rm = TRUE
    ),
    
    At_Least_Minimum =
      N_Screenshots_Total >=
      minimum_screenshots,
    
    Screening_Available =
      first(
        participant
      ) %in%
      screening$participant,
    
    .groups = "drop"
  )


eligible_diary_ids <- participant_counts_before_screening %>%
  filter(
    At_Least_Minimum
  ) %>%
  pull(
    participant
  )


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
    "Prüfe minimum_screenshots, Screening-Matches und Participant Codes."
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
      require_screening_match &
        !Screening_Available ~
        "Kein vollständiges Screening-Match",
      TRUE ~ "Eingeschlossen"
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
    intro_age_z = safe_z(
      intro_age_num
    ),
    
    intro_intensity_z = safe_z(
      intro_intensity
    ),
    
    incidentality_index_z = safe_z(
      incidentality_index
    ),
    
    intro_ib_undirected_z = safe_z(
      intro_ib_undirected
    ),
    
    intro_ib_thematic_z = safe_z(
      intro_ib_thematic
    ),
    
    intro_ib_social_z = safe_z(
      intro_ib_social
    ),
    
    intro_ib_problem_z = safe_z(
      intro_ib_problem
    )
  )


daily_all <- coding %>%
  filter(
    participant %in%
      eligible_participant_ids
  ) %>%
  mutate(
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
    
    # Canonical numeric alias retained for downstream integrated scripts.
    public_rel_coded = public_relevance,
    
    public_relevance_label = factor(
      public_relevance,
      levels = c(
        1L,
        0L
      ),
      labels = c(
        "Öffentlich relevant",
        "Nicht öffentlich relevant"
      )
    ),
    
    incidental_strict = case_when(
      is.na(
        incidentality
      ) ~ NA_integer_,
      incidentality ==
        "Zufällig begegnet" ~ 1L,
      TRUE ~ 0L
    ),
    
    incidental_broad = case_when(
      is.na(
        incidentality
      ) ~ NA_integer_,
      incidentality %in% c(
        "Gefolgt, nicht gezielt gesucht",
        "Zufällig begegnet"
      ) ~ 1L,
      TRUE ~ 0L
    ),
    
    targeted_exposure = case_when(
      is.na(
        incidentality
      ) ~ NA_integer_,
      incidentality ==
        "Gezielt gesucht" ~ 1L,
      TRUE ~ 0L
    ),
    
    interaction_count = if_else(
      is.na(
        interaction_read
      ) &
        is.na(
          interaction_research
        ) &
        is.na(
          interaction_engagement
        ),
      NA_integer_,
      rowSums(
        cbind(
          interaction_read,
          interaction_research,
          interaction_engagement
        ),
        na.rm = TRUE
      )
    ),
    
    interaction_any = case_when(
      is.na(
        interaction_count
      ) ~ NA_integer_,
      interaction_count > 0 ~ 1L,
      TRUE ~ 0L
    ),
    
    active_follow_up = case_when(
      is.na(
        interaction_research
      ) &
        is.na(
          interaction_engagement
        ) ~ NA_integer_,
      interaction_research == 1L |
        interaction_engagement == 1L ~ 1L,
      TRUE ~ 0L
    ),
    
    platform_matches_report = case_when(
      is.na(
        platform_reported
      ) |
        is.na(
          platform_coded
        ) ~ NA_integer_,
      platform_reported ==
        platform_coded ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  left_join(
    screening_selected,
    by = "participant"
  ) %>%
  mutate(
    study_day_z = safe_z(
      study_day
    )
  )


daily <- daily_all %>%
  filter(
    public_relevance == 1L,
    analysis_coding_complete
  ) %>%
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
    
    topic_macro = case_when(
      as.character(
        topic_coded
      ) %in%
        topic_levels[
          c(
            1,
            2,
            3,
            4,
            7,
            8
          )
        ] ~
        "Aktuelles & öffentliche Angelegenheiten",
      
      as.character(
        topic_coded
      ) %in%
        topic_levels[
          c(
            6,
            9,
            10,
            14
          )
        ] ~
        "Praktische Information & Service",
      
      as.character(
        topic_coded
      ) %in%
        topic_levels[
          c(
            5,
            11,
            12,
            13
          )
        ] ~
        "Wissen, Interessen & Kultur",
      
      TRUE ~
        "Sonstiges / nicht eindeutig"
    ),
    
    source_macro = case_when(
      as.character(
        source_coded
      ) ==
        source_levels[
          1
        ] ~
        "Journalistische Medien",
      
      as.character(
        source_coded
      ) %in%
        source_levels[
          c(
            2,
            10
          )
        ] ~
        "Alternative, aggregierte oder informelle Medien",
      
      as.character(
        source_coded
      ) %in%
        source_levels[
          c(
            3,
            4
          )
        ] ~
        "Politik & öffentliche Institutionen",
      
      as.character(
        source_coded
      ) %in%
        source_levels[
          c(
            5,
            6
          )
        ] ~
        "Zivilgesellschaft & Expertise",
      
      as.character(
        source_coded
      ) %in%
        source_levels[
          c(
            7,
            8
          )
        ] ~
        "Kommerzielle & öffentliche Personenaccounts",
      
      as.character(
        source_coded
      ) ==
        source_levels[
          9
        ] ~
        "Private Person / Peer",
      
      TRUE ~
        "Sonstige / nicht erkennbar"
    ),
    
    need_domain = case_when(
      source_macro ==
        "Private Person / Peer" ~
        "Soziales Informationsbedürfnis",
      
      topic_macro ==
        "Aktuelles & öffentliche Angelegenheiten" ~
        "Ungerichtetes Informationsbedürfnis",
      
      topic_macro ==
        "Praktische Information & Service" ~
        "Problembezogenes Informationsbedürfnis",
      
      topic_macro ==
        "Wissen, Interessen & Kultur" ~
        "Thematisches Informationsbedürfnis",
      
      TRUE ~ NA_character_
    ),
    
    post_need_fit_z = case_when(
      need_domain ==
        "Ungerichtetes Informationsbedürfnis" ~
        intro_ib_undirected_z,
      
      need_domain ==
        "Thematisches Informationsbedürfnis" ~
        intro_ib_thematic_z,
      
      need_domain ==
        "Soziales Informationsbedürfnis" ~
        intro_ib_social_z,
      
      need_domain ==
        "Problembezogenes Informationsbedürfnis" ~
        intro_ib_problem_z,
      
      TRUE ~ NA_real_
    ),
    
    # Compatibility alias for the integrated/outro analysis.
    need_fit_score = post_need_fit_z,
    
    processed_meaningfully = case_when(
      is.na(
        interaction_read
      ) &
        is.na(
          interaction_research
        ) &
        is.na(
          interaction_engagement
        ) ~ NA_integer_,
      
      interaction_read == 1L |
        interaction_research == 1L |
        interaction_engagement == 1L ~ 1L,
      
      TRUE ~ 0L
    ),
    
    processing_pattern = case_when(
      is.na(
        interaction_count
      ) ~ NA_character_,
      
      interaction_read == 0L &
        interaction_research == 0L &
        interaction_engagement == 0L ~
        "Keine berichtete Verarbeitung",
      
      interaction_read == 1L &
        interaction_research == 0L &
        interaction_engagement == 0L ~
        "Nur gründlich gelesen/angeschaut",
      
      interaction_read == 0L &
        interaction_research == 1L &
        interaction_engagement == 0L ~
        "Nur weiter recherchiert",
      
      interaction_read == 0L &
        interaction_research == 0L &
        interaction_engagement == 1L ~
        "Nur sichtbar interagiert",
      
      interaction_read == 1L &
        interaction_research == 1L &
        interaction_engagement == 0L ~
        "Gelesen + recherchiert",
      
      interaction_read == 1L &
        interaction_research == 0L &
        interaction_engagement == 1L ~
        "Gelesen + interagiert",
      
      interaction_read == 0L &
        interaction_research == 1L &
        interaction_engagement == 1L ~
        "Recherchiert + interagiert",
      
      interaction_read == 1L &
        interaction_research == 1L &
        interaction_engagement == 1L ~
        "Gelesen + recherchiert + interagiert",
      
      TRUE ~
        "Teilweise fehlende Angaben"
    )
  ) %>%
  arrange(
    participant,
    study_day,
    committed,
    scheduled,
    original_coding_row
  ) %>%
  group_by(
    participant
  ) %>%
  mutate(
    diary_sequence = row_number(),
    
    topic_novelty = as.integer(
      !duplicated(
        as.character(
          topic_coded
        )
      )
    ),
    
    source_type_novelty = as.integer(
      !duplicated(
        as.character(
          source_coded
        )
      )
    ),
    
    account_novelty = case_when(
      is.na(
        clean_text(
          source_name_coded
        )
      ) ~ NA_integer_,
      TRUE ~ as.integer(
        !duplicated(
          clean_text(
            source_name_coded
          )
        )
      )
    ),
    
    any_repertoire_novelty = case_when(
      is.na(
        topic_novelty
      ) &
        is.na(
          source_type_novelty
        ) &
        is.na(
          account_novelty
        ) ~ NA_integer_,
      
      topic_novelty == 1L |
        source_type_novelty == 1L |
        account_novelty == 1L ~ 1L,
      
      TRUE ~ 0L
    ),
    
    productive_serendipity_strict = case_when(
      is.na(
        incidental_strict
      ) |
        is.na(
          any_repertoire_novelty
        ) |
        is.na(
          processed_meaningfully
        ) ~ NA_integer_,
      
      incidental_strict == 1L &
        any_repertoire_novelty == 1L &
        processed_meaningfully == 1L ~ 1L,
      
      TRUE ~ 0L
    ),
    
    productive_serendipity_broad = case_when(
      is.na(
        incidental_broad
      ) |
        is.na(
          any_repertoire_novelty
        ) |
        is.na(
          processed_meaningfully
        ) ~ NA_integer_,
      
      incidental_broad == 1L &
        any_repertoire_novelty == 1L &
        processed_meaningfully == 1L ~ 1L,
      
      TRUE ~ 0L
    )
  ) %>%
  ungroup()


# Add selected derived content indicators back to the all-screenshot dataset so
# later integrated/outro scripts can analyse public relevance and reactivity
# without losing the contribution-level theory variables.
daily_all <- daily_all %>%
  left_join(
    daily %>%
      select(
        original_coding_row,
        topic_macro,
        source_macro,
        need_domain,
        post_need_fit_z,
        need_fit_score,
        processed_meaningfully,
        processing_pattern,
        diary_sequence,
        topic_novelty,
        source_type_novelty,
        account_novelty,
        any_repertoire_novelty,
        productive_serendipity_strict,
        productive_serendipity_broad
      ),
    by = "original_coding_row"
  )




if (
  nrow(
    daily
  ) == 0
) {
  stop(
    "Im eingeschlossenen Sample verbleiben keine öffentlich relevanten und ",
    "vollständig content-codierten Screenshots."
  )
}


#===============================================================================
# 11 Sample and participation descriptives
#===============================================================================

participant_content_counts <- daily %>%
  count(
    participant,
    name = "N_Public_Content_Analyzed"
  )


participant_counts <- daily_all %>%
  group_by(
    participant
  ) %>%
  summarise(
    N_Screenshots_Total = n(),
    
    N_Active_Days = n_distinct(
      study_day[
        study_day %in%
          expected_study_days
      ]
    ),
    
    First_Study_Day = safe_min(
      study_day
    ),
    
    Last_Study_Day = safe_max(
      study_day
    ),
    
    N_Public_Relevant = sum(
      public_relevance == 1L,
      na.rm = TRUE
    ),
    
    N_Not_Public_Relevant = sum(
      public_relevance == 0L,
      na.rm = TRUE
    ),
    
    N_Public_Relevance_Missing = sum(
      is.na(
        public_relevance
      )
    ),
    
    Share_Public_Relevant = safe_mean(
      public_relevance
    ),
    
    .groups = "drop"
  ) %>%
  left_join(
    participant_content_counts,
    by = "participant"
  ) %>%
  mutate(
    N_Public_Content_Analyzed = replace_na(
      N_Public_Content_Analyzed,
      0L
    )
  )


sample_overview <- tibble(
  Indicator = c(
    "Rows in coding sheet",
    "Participants meeting diary screenshot threshold before screening match",
    "Participants in final daily sample",
    "All screenshots in final daily sample",
    "Screenshots with public-relevance decision",
    "Publicly relevant screenshots",
    "Not publicly relevant screenshots",
    "Publicly relevant and content-analysed screenshots",
    "Minimum screenshots required",
    "Median total screenshots per participant",
    "Mean total screenshots per participant",
    "Median active diary days",
    "Simulation active",
    "Simulation uses screening patterns",
    "Public-relevance mode"
  ),
  Value = as.character(
    c(
      nrow(
        coding
      ),
      length(
        eligible_diary_ids
      ),
      n_distinct(
        daily_all$participant
      ),
      nrow(
        daily_all
      ),
      sum(
        !is.na(
          daily_all$public_relevance
        )
      ),
      sum(
        daily_all$public_relevance == 1L,
        na.rm = TRUE
      ),
      sum(
        daily_all$public_relevance == 0L,
        na.rm = TRUE
      ),
      nrow(
        daily
      ),
      minimum_screenshots,
      safe_median(
        participant_counts$N_Screenshots_Total
      ),
      safe_mean(
        participant_counts$N_Screenshots_Total
      ),
      safe_median(
        participant_counts$N_Active_Days
      ),
      simulate_coding,
      simulate_coding &&
        simulation_use_screening_patterns,
      public_relevance_mode
    )
  )
)


participant_day_grid <- tidyr::expand_grid(
  participant = eligible_participant_ids,
  study_day = expected_study_days
) %>%
  left_join(
    daily_all %>%
      count(
        participant,
        study_day,
        name = "N_Posts_Total"
      ),
    by = c(
      "participant",
      "study_day"
    )
  ) %>%
  left_join(
    daily %>%
      count(
        participant,
        study_day,
        name = "N_Public_Content_Posts"
      ),
    by = c(
      "participant",
      "study_day"
    )
  ) %>%
  mutate(
    N_Posts_Total = replace_na(
      N_Posts_Total,
      0L
    ),
    
    N_Public_Content_Posts = replace_na(
      N_Public_Content_Posts,
      0L
    ),
    
    Any_Post =
      N_Posts_Total > 0
  )


day_public_relevance <- daily_all %>%
  filter(
    study_day %in%
      expected_study_days
  ) %>%
  group_by(
    study_day
  ) %>%
  summarise(
    N_Public_Relevance_Valid = sum(
      !is.na(
        public_relevance
      )
    ),
    
    N_Public_Relevant = sum(
      public_relevance == 1L,
      na.rm = TRUE
    ),
    
    Percent_Public_Relevant = 100 *
      safe_mean(
        public_relevance
      ),
    
    .groups = "drop"
  )


day_outcomes <- daily %>%
  filter(
    study_day %in%
      expected_study_days
  ) %>%
  group_by(
    study_day
  ) %>%
  summarise(
    N_Observed_Public_Content_Posts = n(),
    
    N_Contributing_Participants =
      n_distinct(
        participant
      ),
    
    Percent_Targeted = 100 *
      safe_mean(
        targeted_exposure
      ),
    
    Percent_Incidental_Strict = 100 *
      safe_mean(
        incidental_strict
      ),
    
    Percent_Incidental_Broad = 100 *
      safe_mean(
        incidental_broad
      ),
    
    Percent_Read_Thoroughly = 100 *
      safe_mean(
        interaction_read
      ),
    
    Percent_Researched = 100 *
      safe_mean(
        interaction_research
      ),
    
    Percent_Engaged = 100 *
      safe_mean(
        interaction_engagement
      ),
    
    Mean_Interaction_Count = safe_mean(
      interaction_count
    ),
    
    Percent_Topic_Novelty = 100 *
      safe_mean(
        topic_novelty
      ),
    
    Percent_Account_Novelty = 100 *
      safe_mean(
        account_novelty
      ),
    
    Percent_Productive_Serendipity_Strict = 100 *
      safe_mean(
        productive_serendipity_strict
      ),
    
    Percent_Productive_Serendipity_Broad = 100 *
      safe_mean(
        productive_serendipity_broad
      ),
    
    .groups = "drop"
  )


day_summary <- participant_day_grid %>%
  group_by(
    study_day
  ) %>%
  summarise(
    N_Eligible_Participants = n(),
    
    N_Participants_With_Post = sum(
      Any_Post
    ),
    
    Percent_With_Post = 100 *
      mean(
        Any_Post
      ),
    
    Mean_Posts_Per_Eligible_Participant = mean(
      N_Posts_Total
    ),
    
    SD_Posts_Per_Eligible_Participant = sd(
      N_Posts_Total
    ),
    
    Median_Posts_Per_Eligible_Participant = median(
      N_Posts_Total
    ),
    
    Total_Posts = sum(
      N_Posts_Total
    ),
    
    Total_Public_Content_Posts = sum(
      N_Public_Content_Posts
    ),
    
    .groups = "drop"
  ) %>%
  left_join(
    day_public_relevance,
    by = "study_day"
  ) %>%
  left_join(
    day_outcomes,
    by = "study_day"
  )

#===============================================================================
# 12 Main screenshot-weighted distributions
#===============================================================================

public_relevance_distribution <- daily_all %>%
  mutate(
    Category = case_when(
      public_relevance == 1L ~
        "Öffentlich relevant",
      
      public_relevance == 0L ~
        "Nicht öffentlich relevant",
      
      TRUE ~ "Missing"
    )
  ) %>%
  count(
    Category,
    name = "N"
  ) %>%
  tidyr::complete(
    Category = c(
      "Öffentlich relevant",
      "Nicht öffentlich relevant",
      "Missing"
    ),
    fill = list(
      N = 0
    )
  ) %>%
  mutate(
    N_Total = sum(
      N
    ),
    
    N_Valid = sum(
      N[
        Category !=
          "Missing"
      ]
    ),
    
    Percent_Total = safe_percent(
      N,
      N_Total
    ),
    
    Percent_Valid = if_else(
      Category ==
        "Missing",
      NA_real_,
      safe_percent(
        N,
        N_Valid
      )
    )
  )


public_relevance_by_platform <- binary_group_summary(
  daily_all,
  group_variable = "platform",
  outcome_variable = "public_relevance",
  group_label = "Plattform",
  outcome_label = "Öffentlich relevant"
)


public_relevance_by_day <- binary_group_summary(
  daily_all,
  group_variable = "study_day",
  outcome_variable = "public_relevance",
  group_label = "Studientag",
  outcome_label = "Öffentlich relevant"
)


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
      clean_text(
        source_name_coded
      ),
      "Quelle nicht benannt"
    )
  ) %>%
  count(
    source_name_coded,
    sort = TRUE,
    name = "N"
  ) %>%
  mutate(
    Percent = 100 *
      N /
      sum(
        N
      )
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
    sum(
      !is.na(
        daily$interaction_read
      )
    ),
    
    sum(
      !is.na(
        daily$interaction_research
      )
    ),
    
    sum(
      !is.na(
        daily$interaction_engagement
      )
    ),
    
    sum(
      !is.na(
        daily$interaction_any
      )
    ),
    
    sum(
      !is.na(
        daily$active_follow_up
      )
    )
  ),
  
  N_Yes = c(
    sum(
      daily$interaction_read == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$interaction_research == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$interaction_engagement == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$interaction_any == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$active_follow_up == 1L,
      na.rm = TRUE
    )
  )
) %>%
  mutate(
    Percent_Yes = safe_percent(
      N_Yes,
      N_Valid
    )
  )


processing_pattern_distribution <- daily %>%
  mutate(
    processing_pattern = replace_na(
      clean_text(
        processing_pattern
      ),
      "Missing"
    )
  ) %>%
  count(
    processing_pattern,
    sort = TRUE,
    name = "N"
  ) %>%
  mutate(
    Percent = safe_percent(
      N,
      sum(
        N
      )
    )
  )


# This is a descriptive sequence, not an observed temporal process. The second
# part of the table reports conditional transitions to make the denominator
# explicit.
conversion_funnel <- bind_rows(
  tibble(
    Funnel = "All public content posts",
    Stage = c(
      "Public content post",
      "Thoroughly read/viewed",
      "Further researched",
      "Visibly engaged"
    ),
    N = c(
      nrow(
        daily
      ),
      sum(
        daily$interaction_read == 1L,
        na.rm = TRUE
      ),
      sum(
        daily$interaction_research == 1L,
        na.rm = TRUE
      ),
      sum(
        daily$interaction_engagement == 1L,
        na.rm = TRUE
      )
    ),
    Denominator = c(
      nrow(
        daily
      ),
      sum(
        !is.na(
          daily$interaction_read
        )
      ),
      sum(
        !is.na(
          daily$interaction_research
        )
      ),
      sum(
        !is.na(
          daily$interaction_engagement
        )
      )
    )
  ),
  
  tibble(
    Funnel = "Nested descriptive sequence",
    Stage = c(
      "Public content post",
      "Read/viewed among public posts",
      "Research among read/viewed posts",
      "Engagement among researched posts"
    ),
    N = c(
      nrow(
        daily
      ),
      sum(
        daily$interaction_read == 1L,
        na.rm = TRUE
      ),
      sum(
        daily$interaction_read == 1L &
          daily$interaction_research == 1L,
        na.rm = TRUE
      ),
      sum(
        daily$interaction_read == 1L &
          daily$interaction_research == 1L &
          daily$interaction_engagement == 1L,
        na.rm = TRUE
      )
    ),
    Denominator = c(
      nrow(
        daily
      ),
      sum(
        !is.na(
          daily$interaction_read
        )
      ),
      sum(
        daily$interaction_read == 1L &
          !is.na(
            daily$interaction_research
          ),
        na.rm = TRUE
      ),
      sum(
        daily$interaction_read == 1L &
          daily$interaction_research == 1L &
          !is.na(
            daily$interaction_engagement
          ),
        na.rm = TRUE
      )
    )
  )
) %>%
  mutate(
    Percent = safe_percent(
      N,
      Denominator
    )
  )


incidental_conversion <- daily %>%
  filter(
    !is.na(
      incidentality
    )
  ) %>%
  mutate(
    Incidentality_Group = as.character(
      incidentality
    )
  ) %>%
  group_by(
    Incidentality_Group
  ) %>%
  summarise(
    N_Posts = n(),
    
    Percent_Read = 100 *
      safe_mean(
        interaction_read
      ),
    
    Percent_Research = 100 *
      safe_mean(
        interaction_research
      ),
    
    Percent_Engagement = 100 *
      safe_mean(
        interaction_engagement
      ),
    
    Percent_Processed_Meaningfully = 100 *
      safe_mean(
        processed_meaningfully
      ),
    
    .groups = "drop"
  )


novelty_summary <- tibble(
  Marker = c(
    "Topic novelty",
    "Source-type novelty",
    "Account novelty",
    "Any repertoire novelty",
    "Productive serendipity, strict",
    "Productive serendipity, broad"
  ),
  
  N_Valid = c(
    sum(
      !is.na(
        daily$topic_novelty
      )
    ),
    
    sum(
      !is.na(
        daily$source_type_novelty
      )
    ),
    
    sum(
      !is.na(
        daily$account_novelty
      )
    ),
    
    sum(
      !is.na(
        daily$any_repertoire_novelty
      )
    ),
    
    sum(
      !is.na(
        daily$productive_serendipity_strict
      )
    ),
    
    sum(
      !is.na(
        daily$productive_serendipity_broad
      )
    )
  ),
  
  N_Yes = c(
    sum(
      daily$topic_novelty == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$source_type_novelty == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$account_novelty == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$any_repertoire_novelty == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$productive_serendipity_strict == 1L,
      na.rm = TRUE
    ),
    
    sum(
      daily$productive_serendipity_broad == 1L,
      na.rm = TRUE
    )
  )
) %>%
  mutate(
    Percent_Yes = safe_percent(
      N_Yes,
      N_Valid
    )
  )


novelty_by_incidentality <- bind_rows(
  binary_group_summary(
    daily,
    group_variable = "incidentality",
    outcome_variable = "topic_novelty",
    group_label = "Incidentality",
    outcome_label = "Topic novelty"
  ),
  
  binary_group_summary(
    daily,
    group_variable = "incidentality",
    outcome_variable = "account_novelty",
    group_label = "Incidentality",
    outcome_label = "Account novelty"
  ),
  
  binary_group_summary(
    daily,
    group_variable = "incidentality",
    outcome_variable = "productive_serendipity_strict",
    group_label = "Incidentality",
    outcome_label = "Productive serendipity, strict"
  )
)


post_need_fit_summary <- daily %>%
  filter(
    !is.na(
      post_need_fit_z
    )
  ) %>%
  group_by(
    need_domain,
    incidentality
  ) %>%
  summarise(
    N = n(),
    
    Mean_Need_Fit_Z = safe_mean(
      post_need_fit_z
    ),
    
    SD_Need_Fit_Z = safe_sd(
      post_need_fit_z
    ),
    
    Percent_Read = 100 *
      safe_mean(
        interaction_read
      ),
    
    Percent_Research = 100 *
      safe_mean(
        interaction_research
      ),
    
    Percent_Engagement = 100 *
      safe_mean(
        interaction_engagement
      ),
    
    .groups = "drop"
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

participant_public_metrics <- participant_counts %>%
  select(
    participant,
    N_Screenshots_Total,
    N_Active_Days,
    N_Public_Relevant,
    N_Not_Public_Relevant,
    N_Public_Relevance_Missing,
    Share_Public_Relevant,
    N_Public_Content_Analyzed
  )


participant_metrics <- daily %>%
  group_by(
    participant
  ) %>%
  summarise(
    N_Public_Content_Posts = n(),
    
    Share_Incidental_Strict = safe_mean(
      incidental_strict
    ),
    
    Share_Incidental_Broad = safe_mean(
      incidental_broad
    ),
    
    Share_Targeted = safe_mean(
      targeted_exposure
    ),
    
    Share_Read_Thoroughly = safe_mean(
      interaction_read
    ),
    
    Share_Researched = safe_mean(
      interaction_research
    ),
    
    Share_Engaged = safe_mean(
      interaction_engagement
    ),
    
    Share_Any_Interaction = safe_mean(
      interaction_any
    ),
    
    Share_Active_Follow_Up = safe_mean(
      active_follow_up
    ),
    
    Mean_Interaction_Count = safe_mean(
      interaction_count
    ),
    
    Share_Home = share_value(
      as.character(
        locality
      ),
      "Zu Hause"
    ),
    
    Share_Away = share_value(
      as.character(
        locality
      ),
      "Unterwegs"
    ),
    
    Share_Alone = share_value(
      as.character(
        situation
      ),
      "Allein"
    ),
    
    Share_Together = share_value(
      as.character(
        situation
      ),
      "Gemeinsam mit jemandem"
    ),
    
    Topic_Richness = n_distinct_valid(
      topic_coded
    ),
    
    Topic_Shannon = shannon_entropy(
      topic_coded
    ),
    
    Topic_Evenness = shannon_evenness(
      topic_coded
    ),
    
    Topic_Dominant_Share = dominant_share(
      topic_coded
    ),
    
    Source_Richness = n_distinct_valid(
      source_coded
    ),
    
    Source_Shannon = shannon_entropy(
      source_coded
    ),
    
    Source_Evenness = shannon_evenness(
      source_coded
    ),
    
    Source_Dominant_Share = dominant_share(
      source_coded
    ),
    
    Platform_Richness = n_distinct_valid(
      platform
    ),
    
    Platform_Shannon = shannon_entropy(
      platform
    ),
    
    Platform_Evenness = shannon_evenness(
      platform
    ),
    
    Platform_Dominant_Share = dominant_share(
      platform
    ),
    
    Format_Richness = n_distinct_valid(
      media_format
    ),
    
    Format_Shannon = shannon_entropy(
      media_format
    ),
    
    Format_Evenness = shannon_evenness(
      media_format
    ),
    
    Format_Dominant_Share = dominant_share(
      media_format
    ),
    
    N_Unique_Account_Names = n_distinct_valid(
      source_name_coded
    ),
    
    Account_Dominant_Share = dominant_share(
      source_name_coded
    ),
    
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
    
    Share_Facebook = share_value(
      as.character(
        platform
      ),
      "Facebook"
    ),
    
    Share_Instagram = share_value(
      as.character(
        platform
      ),
      "Instagram"
    ),
    
    Share_TikTok = share_value(
      as.character(
        platform
      ),
      "TikTok"
    ),
    
    Share_X = share_value(
      as.character(
        platform
      ),
      "X"
    ),
    
    Share_Topic_Novelty = safe_mean(
      topic_novelty
    ),
    
    Share_Source_Type_Novelty = safe_mean(
      source_type_novelty
    ),
    
    Share_Account_Novelty = safe_mean(
      account_novelty
    ),
    
    Share_Productive_Serendipity_Strict = safe_mean(
      productive_serendipity_strict
    ),
    
    Share_Productive_Serendipity_Broad = safe_mean(
      productive_serendipity_broad
    ),
    
    Mean_Post_Need_Fit_Z = safe_mean(
      post_need_fit_z
    ),
    
    Platform_Report_Match_Rate = safe_mean(
      platform_matches_report
    ),
    
    .groups = "drop"
  ) %>%
  right_join(
    participant_public_metrics,
    by = "participant"
  ) %>%
  mutate(
    N_Public_Content_Posts = replace_na(
      N_Public_Content_Posts,
      0L
    ),
    
    Mean_Posts_Per_Active_Day = safe_divide(
      N_Public_Content_Posts,
      N_Active_Days
    )
  ) %>%
  left_join(
    screening_selected,
    by = "participant"
  )


# Determine the diary-dominant platform, retaining ties.
daily_primary_platform <- platform_shares %>%
  group_by(
    participant
  ) %>%
  filter(
    !is.na(
      Share
    ),
    Share ==
      max(
        Share,
        na.rm = TRUE
      ),
    Share > 0
  ) %>%
  summarise(
    Daily_Primary_Platform = paste(
      Category,
      collapse = " / "
    ),
    .groups = "drop"
  )


platform_sets_overlap <- function(x, y) {
  if (
    is.na(
      x
    ) ||
    is.na(
      y
    )
  ) {
    return(
      NA_integer_
    )
  }
  
  x_set <- clean_text(
    str_split(
      x,
      " / ",
      simplify = FALSE
    )[[1]]
  )
  
  y_set <- clean_text(
    str_split(
      y,
      " / ",
      simplify = FALSE
    )[[1]]
  )
  
  as.integer(
    length(
      intersect(
        x_set,
        y_set
      )
    ) > 0
  )
}


participant_metrics <- participant_metrics %>%
  left_join(
    daily_primary_platform,
    by = "participant"
  ) %>%
  mutate(
    Primary_Platform_Match = map2_int(
      clean_text(
        primary_platform
      ),
      clean_text(
        Daily_Primary_Platform
      ),
      platform_sets_overlap
    ),
    
    Local_Context_Alignment = case_when(
      clean_text(
        context_local
      ) ==
        "Zu Hause" ~
        Share_Home,
      
      clean_text(
        context_local
      ) ==
        "Unterwegs" ~
        Share_Away,
      
      str_detect(
        str_to_lower(
          clean_text(
            context_local
          )
        ),
        "beiden|ähnlich|gleich"
      ) ~
        1 -
        abs(
          Share_Home -
            Share_Away
        ),
      
      TRUE ~ NA_real_
    ),
    
    Social_Context_Alignment = case_when(
      str_detect(
        str_to_lower(
          clean_text(
            context_social
          )
        ),
        "überwiegend allein|mostly alone"
      ) ~
        Share_Alone,
      
      str_detect(
        str_to_lower(
          clean_text(
            context_social
          )
        ),
        "überwiegend gemeinsam|mostly together"
      ) ~
        Share_Together,
      
      str_detect(
        str_to_lower(
          clean_text(
            context_social
          )
        ),
        "ähnlich|gleich"
      ) ~
        1 -
        abs(
          Share_Alone -
            Share_Together
        ),
      
      TRUE ~ NA_real_
    )
  )


# Screening–Diary calibration. Standardisation occurs on participant-level data,
# so participants with more uploads do not receive greater weight.
participant_metrics <- participant_metrics %>%
  mutate(
    Screening_Incidentality_Z = safe_z(
      incidentality_index
    ),
    
    Diary_Incidentality_Broad_Z = safe_z(
      Share_Incidental_Broad
    ),
    
    Diary_Incidentality_Strict_Z = safe_z(
      Share_Incidental_Strict
    ),
    
    Incidentality_Gap_Broad =
      Screening_Incidentality_Z -
      Diary_Incidentality_Broad_Z,
    
    Absolute_Incidentality_Gap_Broad = abs(
      Incidentality_Gap_Broad
    ),
    
    Incidentality_Gap_Strict =
      Screening_Incidentality_Z -
      Diary_Incidentality_Strict_Z,
    
    Absolute_Incidentality_Gap_Strict = abs(
      Incidentality_Gap_Strict
    )
  ) %>%
  rowwise() %>%
  mutate(
    Platform_Profile_Alignment = profile_alignment(
      screening_values = c(
        clean_numeric(
          intro_freq_facebook
        ),
        clean_numeric(
          intro_freq_instagram
        ),
        clean_numeric(
          intro_freq_tiktok
        ),
        clean_numeric(
          intro_freq_x
        )
      ),
      
      observed_values = c(
        Share_Facebook,
        Share_Instagram,
        Share_TikTok,
        Share_X
      )
    )
  ) %>%
  ungroup()


# Within-person platform complementarity.
calculate_platform_profile_jsd <- function(
    participant_data,
    variable,
    categories,
    dimension
) {
  platform_counts <- participant_data %>%
    transmute(
      platform,
      Category = as.character(
        .data[[
          variable
        ]]
      )
    ) %>%
    filter(
      !is.na(
        platform
      ),
      !is.na(
        Category
      )
    ) %>%
    count(
      platform,
      Category,
      name = "N"
    )
  
  platform_totals <- platform_counts %>%
    group_by(
      platform
    ) %>%
    summarise(
      N_Platform_Posts = sum(
        N
      ),
      .groups = "drop"
    ) %>%
    filter(
      N_Platform_Posts >=
        minimum_posts_per_platform_profile
    )
  
  valid_platforms <- as.character(
    platform_totals$platform
  )
  
  if (
    length(
      valid_platforms
    ) < 2
  ) {
    return(
      tibble()
    )
  }
  
  platform_pairs <- combn(
    valid_platforms,
    2,
    simplify = FALSE
  )
  
  map_dfr(
    platform_pairs,
    function(platform_pair) {
      make_profile <- function(platform_name) {
        platform_counts %>%
          filter(
            as.character(
              platform
            ) ==
              platform_name
          ) %>%
          select(
            Category,
            N
          ) %>%
          tidyr::complete(
            Category = categories,
            fill = list(
              N = 0
            )
          ) %>%
          arrange(
            match(
              Category,
              categories
            )
          ) %>%
          pull(
            N
          )
      }
      
      profile_1 <- make_profile(
        platform_pair[[1]]
      )
      
      profile_2 <- make_profile(
        platform_pair[[2]]
      )
      
      tibble(
        participant = first(
          participant_data$participant
        ),
        
        Dimension = dimension,
        
        Platform_1 = platform_pair[[1]],
        
        Platform_2 = platform_pair[[2]],
        
        N_Platform_1 = sum(
          profile_1
        ),
        
        N_Platform_2 = sum(
          profile_2
        ),
        
        Jensen_Shannon_Divergence =
          jensen_shannon_divergence(
            profile_1,
            profile_2
          )
      )
    }
  )
}


platform_complementarity <- daily %>%
  group_split(
    participant
  ) %>%
  map_dfr(
    function(participant_data) {
      bind_rows(
        calculate_platform_profile_jsd(
          participant_data,
          variable = "topic_coded",
          categories = topic_levels,
          dimension = "Topic"
        ),
        
        calculate_platform_profile_jsd(
          participant_data,
          variable = "source_coded",
          categories = source_levels,
          dimension = "Source"
        ),
        
        calculate_platform_profile_jsd(
          participant_data,
          variable = "media_format",
          categories = format_levels,
          dimension = "Format"
        )
      )
    }
  )


if (
  nrow(
    platform_complementarity
  ) == 0
) {
  platform_complementarity <- tibble(
    participant = character(),
    Dimension = character(),
    Platform_1 = character(),
    Platform_2 = character(),
    N_Platform_1 = integer(),
    N_Platform_2 = integer(),
    Jensen_Shannon_Divergence = numeric()
  )
  
  platform_complementarity_person <- tibble(
    participant = character()
  )
  
} else {
  platform_complementarity_person <- platform_complementarity %>%
    group_by(
      participant,
      Dimension
    ) %>%
    summarise(
      Mean_JSD = safe_mean(
        Jensen_Shannon_Divergence
      ),
      Maximum_JSD = safe_max(
        Jensen_Shannon_Divergence
      ),
      N_Platform_Pairs = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = Dimension,
      values_from = c(
        Mean_JSD,
        Maximum_JSD,
        N_Platform_Pairs
      ),
      names_glue = "{Dimension}_{.value}"
    )
}


participant_metrics <- participant_metrics %>%
  left_join(
    platform_complementarity_person,
    by = "participant"
  )


participant_metrics_summary <- participant_metrics %>%
  select(
    N_Screenshots_Total,
    N_Public_Content_Posts,
    N_Active_Days,
    Share_Public_Relevant,
    Mean_Posts_Per_Active_Day,
    starts_with(
      "Share_"
    ),
    ends_with(
      "_Richness"
    ),
    ends_with(
      "_Shannon"
    ),
    ends_with(
      "_Evenness"
    ),
    ends_with(
      "_Dominant_Share"
    ),
    Platform_Report_Match_Rate,
    Local_Context_Alignment,
    Social_Context_Alignment,
    Platform_Profile_Alignment,
    starts_with(
      "Absolute_Incidentality_Gap"
    ),
    contains(
      "_Mean_JSD"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  group_by(
    Indicator
  ) %>%
  summarise(
    N_Valid = sum(
      !is.na(
        Value
      )
    ),
    
    Mean = safe_mean(
      Value
    ),
    
    SD = safe_sd(
      Value
    ),
    
    Median = safe_median(
      Value
    ),
    
    Minimum = safe_min(
      Value
    ),
    
    Maximum = safe_max(
      Value
    ),
    
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
    "intro_ib_social",
    "Share_Together",
    "Soziales Informationsbedürfnis",
    "Diary-Anteil Nutzung gemeinsam"
  ),
  
  spearman_test(
    participant_metrics,
    "intro_ib_undirected",
    "Share_Public_Relevant",
    "Ungerichtetes Informationsbedürfnis",
    "Anteil öffentlich relevanter Uploads"
  ),
  
  spearman_test(
    participant_metrics,
    "intro_ib_problem",
    "Share_Public_Relevant",
    "Problembezogenes Informationsbedürfnis",
    "Anteil öffentlich relevanter Uploads"
  ),
  
  spearman_test(
    participant_metrics,
    "intro_age_num",
    "Share_Public_Relevant",
    "Alter",
    "Anteil öffentlich relevanter Uploads"
  ),
  
  spearman_test(
    participant_metrics,
    "intro_age_num",
    "Absolute_Incidentality_Gap_Broad",
    "Alter",
    "Absolute Screening–Diary-Inzidentalitätsabweichung"
  )
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(
      P_Value,
      method = "BH"
    ),
    
    Analysis_Type = "Explorativ"
  )


need_variables <- c(
  intro_ib_undirected =
    "Ungerichtet: Nachrichten & aktuelles Geschehen",
  
  intro_ib_thematic =
    "Thematisch: persönliche Interessen",
  
  intro_ib_social =
    "Sozial: soziales Umfeld",
  
  intro_ib_problem =
    "Problembezogen: konkrete Problemlösung"
)


topic_need_correlations <- tidyr::crossing(
  Topic = topic_levels,
  Need_Variable = names(
    need_variables
  )
) %>%
  mutate(
    Need = unname(
      need_variables[
        Need_Variable
      ]
    )
  ) %>%
  pmap_dfr(
    function(
    Topic,
    Need_Variable,
    Need
    ) {
      analysis_data <- topic_shares %>%
        filter(
          Category ==
            Topic
        ) %>%
        select(
          participant,
          Share
        ) %>%
        left_join(
          screening %>%
            select(
              participant,
              all_of(
                Need_Variable
              )
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
  group_by(
    Need
  ) %>%
  mutate(
    P_Adjusted_BH_Within_Need = p.adjust(
      P_Value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  mutate(
    Analysis_Type = "Explorativ; compositional topic shares"
  )


source_need_correlations <- tidyr::crossing(
  Source = source_levels,
  Need_Variable = names(
    need_variables
  )
) %>%
  mutate(
    Need = unname(
      need_variables[
        Need_Variable
      ]
    )
  ) %>%
  pmap_dfr(
    function(
    Source,
    Need_Variable,
    Need
    ) {
      analysis_data <- source_shares %>%
        filter(
          Category ==
            Source
        ) %>%
        select(
          participant,
          Share
        ) %>%
        left_join(
          screening %>%
            select(
              participant,
              all_of(
                Need_Variable
              )
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
  group_by(
    Need
  ) %>%
  mutate(
    P_Adjusted_BH_Within_Need = p.adjust(
      P_Value,
      method = "BH"
    )
  ) %>%
  ungroup() %>%
  mutate(
    Analysis_Type = "Explorativ; compositional source shares"
  )


platform_frequency_map <- c(
  Facebook = "intro_freq_facebook",
  Instagram = "intro_freq_instagram",
  TikTok = "intro_freq_tiktok",
  X = "intro_freq_x"
)


platform_alignment_correlations <- imap_dfr(
  platform_frequency_map,
  function(
    screening_variable,
    platform_name
  ) {
    analysis_data <- platform_shares %>%
      filter(
        Category ==
          platform_name
      ) %>%
      select(
        participant,
        Share
      ) %>%
      left_join(
        screening %>%
          select(
            participant,
            all_of(
              screening_variable
            )
          ),
        by = "participant"
      )
    
    result <- spearman_test(
      analysis_data,
      screening_variable,
      "Share",
      paste0(
        platform_name,
        ": Screening-Nutzungsfrequenz"
      ),
      paste0(
        platform_name,
        ": Diary-Anteil"
      )
    )
    
    result %>%
      mutate(
        Platform = platform_name,
        .before = 1
      )
  }
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(
      P_Value,
      method = "BH"
    ),
    
    Analysis_Type = "Explorativ"
  )


alignment_summary <- participant_metrics %>%
  summarise(
    N_Primary_Platform_Valid = sum(
      !is.na(
        Primary_Platform_Match
      )
    ),
    
    N_Primary_Platform_Match = sum(
      Primary_Platform_Match == 1L,
      na.rm = TRUE
    ),
    
    Percent_Primary_Platform_Match = 100 *
      safe_mean(
        Primary_Platform_Match
      ),
    
    Mean_Platform_Profile_Alignment = safe_mean(
      Platform_Profile_Alignment
    ),
    
    Mean_Local_Context_Alignment = safe_mean(
      Local_Context_Alignment
    ),
    
    Mean_Social_Context_Alignment = safe_mean(
      Social_Context_Alignment
    ),
    
    Mean_Incidentality_Gap_Broad = safe_mean(
      Incidentality_Gap_Broad
    ),
    
    Mean_Absolute_Incidentality_Gap_Broad = safe_mean(
      Absolute_Incidentality_Gap_Broad
    ),
    
    Mean_Incidentality_Gap_Strict = safe_mean(
      Incidentality_Gap_Strict
    ),
    
    Mean_Absolute_Incidentality_Gap_Strict = safe_mean(
      Absolute_Incidentality_Gap_Strict
    )
  )


calibration_subgroups <- bind_rows(
  participant_metrics %>%
    transmute(
      Grouping_Variable = "Altersgruppe",
      Group = clean_text(
        age_group
      ),
      Absolute_Gap =
        Absolute_Incidentality_Gap_Broad,
      Platform_Alignment =
        Platform_Profile_Alignment,
      Local_Alignment =
        Local_Context_Alignment,
      Social_Alignment =
        Social_Context_Alignment
    ),
  
  participant_metrics %>%
    transmute(
      Grouping_Variable = "Geschlecht",
      Group = clean_text(
        gender
      ),
      Absolute_Gap =
        Absolute_Incidentality_Gap_Broad,
      Platform_Alignment =
        Platform_Profile_Alignment,
      Local_Alignment =
        Local_Context_Alignment,
      Social_Alignment =
        Social_Context_Alignment
    ),
  
  participant_metrics %>%
    transmute(
      Grouping_Variable = "Bildungsniveau",
      Group = clean_text(
        education_three_level
      ),
      Absolute_Gap =
        Absolute_Incidentality_Gap_Broad,
      Platform_Alignment =
        Platform_Profile_Alignment,
      Local_Alignment =
        Local_Context_Alignment,
      Social_Alignment =
        Social_Context_Alignment
    )
) %>%
  filter(
    !is.na(
      Group
    )
  ) %>%
  pivot_longer(
    cols = c(
      Absolute_Gap,
      Platform_Alignment,
      Local_Alignment,
      Social_Alignment
    ),
    names_to = "Calibration_Metric",
    values_to = "Value"
  ) %>%
  group_by(
    Grouping_Variable,
    Group,
    Calibration_Metric
  ) %>%
  summarise(
    N = sum(
      !is.na(
        Value
      )
    ),
    
    Mean = safe_mean(
      Value
    ),
    
    SD = safe_sd(
      Value
    ),
    
    Median = safe_median(
      Value
    ),
    
    .groups = "drop"
  )


age_marker_variables <- c(
  "Share_Public_Relevant",
  "N_Screenshots_Total",
  "N_Active_Days",
  "Share_Incidental_Strict",
  "Share_Incidental_Broad",
  "Share_Targeted",
  "Share_Read_Thoroughly",
  "Share_Researched",
  "Share_Engaged",
  "Topic_Shannon",
  "Source_Shannon",
  "Platform_Shannon",
  "Format_Shannon",
  "Share_Current_Affairs",
  "Share_Practical_Service",
  "Share_Knowledge_Interests",
  "Share_Journalistic_Sources",
  "Share_Peer_Sources",
  "Share_Topic_Novelty",
  "Share_Account_Novelty",
  "Share_Productive_Serendipity_Broad",
  "Absolute_Incidentality_Gap_Broad",
  "Platform_Profile_Alignment"
)


age_marker_correlations <- map_dfr(
  age_marker_variables,
  function(
    marker
  ) {
    spearman_test(
      participant_metrics,
      "intro_age_num",
      marker,
      "Alter",
      marker
    ) %>%
      mutate(
        Marker = marker,
        .before = 1
      )
  }
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(
      P_Value,
      method = "BH"
    ),
    
    Analysis_Type = "Explorativ"
  ) %>%
  arrange(
    desc(
      abs(
        Spearman_Rho
      )
    )
  )

#===============================================================================
# 17 Descriptive subgroup comparisons
#===============================================================================

participant_metric_variables <- c(
  "Share_Public_Relevant",
  "N_Screenshots_Total",
  "N_Active_Days",
  "Share_Incidental_Strict",
  "Share_Incidental_Broad",
  "Share_Targeted",
  "Share_Read_Thoroughly",
  "Share_Researched",
  "Share_Engaged",
  "Topic_Shannon",
  "Source_Shannon",
  "Platform_Shannon",
  "Share_Current_Affairs",
  "Share_Practical_Service",
  "Share_Knowledge_Interests",
  "Share_Journalistic_Sources",
  "Share_Peer_Sources",
  "Share_Topic_Novelty",
  "Share_Account_Novelty",
  "Share_Productive_Serendipity_Broad",
  "Absolute_Incidentality_Gap_Broad",
  "Platform_Profile_Alignment"
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
# 18 Optional exploratory mixed models and ICCs
#===============================================================================

fit_binary_glmm <- function(
    data,
    formula,
    model_name,
    outcome_variable
) {
  formula_variables <- all.vars(
    formula
  )
  
  model_data <- data %>%
    select(
      all_of(
        formula_variables
      )
    ) %>%
    drop_na() %>%
    mutate(
      across(
        where(
          is.character
        ),
        as.factor
      ),
      
      across(
        where(
          is.factor
        ),
        forcats::fct_drop
      )
    )
  
  n_observations <- nrow(
    model_data
  )
  
  n_participants <- n_distinct(
    model_data$participant
  )
  
  n_events <- sum(
    model_data[[
      outcome_variable
    ]] == 1,
    na.rm = TRUE
  )
  
  n_non_events <- sum(
    model_data[[
      outcome_variable
    ]] == 0,
    na.rm = TRUE
  )
  
  if (
    n_observations <
    minimum_model_n ||
    n_participants <
    minimum_model_participants ||
    n_events <
    minimum_model_events ||
    n_non_events <
    minimum_model_events
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
          optCtrl = list(
            maxfun = 200000
          )
        )
      )
    ),
    error = function(
    e
    ) e
  )
  
  if (
    inherits(
      fitted_model,
      "error"
    )
  ) {
    return(
      list(
        model = NULL,
        
        status = tibble(
          Model = model_name,
          Status = paste0(
            "Model error: ",
            conditionMessage(
              fitted_model
            )
          ),
          N = n_observations,
          N_Participants = n_participants,
          N_Events = n_events,
          N_Non_Events = n_non_events,
          Singular = NA,
          Convergence_Message = conditionMessage(
            fitted_model
          )
        ),
        
        tidy = tibble()
      )
    )
  }
  
  convergence_messages <-
    fitted_model@optinfo$conv$lme4$messages
  
  status <- tibble(
    Model = model_name,
    Status = "Fitted",
    N = n_observations,
    N_Participants = n_participants,
    N_Events = n_events,
    N_Non_Events = n_non_events,
    Singular = lme4::isSingular(
      fitted_model,
      tol = 1e-4
    ),
    Convergence_Message = if (
      is.null(
        convergence_messages
      )
    ) {
      NA_character_
    } else {
      paste(
        convergence_messages,
        collapse = " | "
      )
    }
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
    
    error = function(
    e
    ) {
      tibble(
        Model = model_name,
        term = NA_character_,
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        Effect_Scale = paste0(
          "Tidy error: ",
          conditionMessage(
            e
          )
        )
      )
    }
  )
  
  list(
    model = fitted_model,
    status = status,
    tidy = tidy_result
  )
}


fit_binary_icc <- function(
    data,
    outcome_variable,
    outcome_label
) {
  icc_data <- data %>%
    transmute(
      participant,
      outcome = clean_binary(
        .data[[
          outcome_variable
        ]]
      )
    ) %>%
    drop_na() %>%
    group_by(
      participant
    ) %>%
    filter(
      n() >= 2
    ) %>%
    ungroup()
  
  n_observations <- nrow(
    icc_data
  )
  
  n_participants <- n_distinct(
    icc_data$participant
  )
  
  n_events <- sum(
    icc_data$outcome == 1L
  )
  
  n_non_events <- sum(
    icc_data$outcome == 0L
  )
  
  if (
    !run_icc_models
  ) {
    return(
      list(
        model = NULL,
        result = tibble(
          Outcome = outcome_label,
          Status = "ICC disabled",
          N = n_observations,
          N_Participants = n_participants,
          N_Events = n_events,
          N_Non_Events = n_non_events,
          Random_Intercept_Variance = NA_real_,
          Logistic_ICC = NA_real_,
          Singular = NA,
          Convergence_Message = NA_character_
        )
      )
    )
  }
  
  if (
    n_observations <
    minimum_model_n ||
    n_participants <
    minimum_model_participants ||
    n_events <
    minimum_model_events ||
    n_non_events <
    minimum_model_events
  ) {
    return(
      list(
        model = NULL,
        result = tibble(
          Outcome = outcome_label,
          Status = "Not fitted: insufficient data/events",
          N = n_observations,
          N_Participants = n_participants,
          N_Events = n_events,
          N_Non_Events = n_non_events,
          Random_Intercept_Variance = NA_real_,
          Logistic_ICC = NA_real_,
          Singular = NA,
          Convergence_Message = NA_character_
        )
      )
    )
  }
  
  model <- tryCatch(
    suppressWarnings(
      lme4::glmer(
        outcome ~
          1 +
          (
            1 |
              participant
          ),
        data = icc_data,
        family = binomial,
        control = lme4::glmerControl(
          optimizer = "bobyqa",
          optCtrl = list(
            maxfun = 200000
          )
        )
      )
    ),
    error = function(
    e
    ) e
  )
  
  if (
    inherits(
      model,
      "error"
    )
  ) {
    return(
      list(
        model = NULL,
        result = tibble(
          Outcome = outcome_label,
          Status = paste0(
            "Model error: ",
            conditionMessage(
              model
            )
          ),
          N = n_observations,
          N_Participants = n_participants,
          N_Events = n_events,
          N_Non_Events = n_non_events,
          Random_Intercept_Variance = NA_real_,
          Logistic_ICC = NA_real_,
          Singular = NA,
          Convergence_Message = conditionMessage(
            model
          )
        )
      )
    )
  }
  
  random_variance <- as.numeric(
    lme4::VarCorr(
      model
    )$participant[
      1
    ]
  )
  
  logistic_icc <- safe_divide(
    random_variance,
    random_variance +
      (
        pi^2 /
          3
      )
  )
  
  convergence_messages <-
    model@optinfo$conv$lme4$messages
  
  list(
    model = model,
    
    result = tibble(
      Outcome = outcome_label,
      Status = "Fitted",
      N = n_observations,
      N_Participants = n_participants,
      N_Events = n_events,
      N_Non_Events = n_non_events,
      Random_Intercept_Variance = random_variance,
      Logistic_ICC = logistic_icc,
      Singular = lme4::isSingular(
        model,
        tol = 1e-4
      ),
      Convergence_Message = if (
        is.null(
          convergence_messages
        )
      ) {
        NA_character_
      } else {
        paste(
          convergence_messages,
          collapse = " | "
        )
      }
    )
  )
}


model_results <- list()


if (run_mixed_models) {
  model_results$public_relevance <- fit_binary_glmm(
    daily_all,
    public_relevance ~
      platform +
      intro_age_z +
      study_day_z +
      (
        1 |
          participant
      ),
    model_name = "Public relevance",
    outcome_variable = "public_relevance"
  )
  
  model_results$incidental_strict <- fit_binary_glmm(
    daily,
    incidental_strict ~
      incidentality_index_z +
      intro_age_z +
      platform +
      topic_macro +
      study_day_z +
      (
        1 |
          participant
      ),
    model_name = "Strict incidentality",
    outcome_variable = "incidental_strict"
  )
  
  model_results$read_thoroughly <- fit_binary_glmm(
    daily,
    interaction_read ~
      incidental_broad *
      post_need_fit_z +
      locality +
      media_format +
      study_day_z +
      (
        1 |
          participant
      ),
    model_name = "Thorough reading",
    outcome_variable = "interaction_read"
  )
  
  model_results$further_research <- fit_binary_glmm(
    daily,
    interaction_research ~
      incidental_broad *
      post_need_fit_z +
      topic_macro +
      study_day_z +
      (
        1 |
          participant
      ),
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
      intro_age_z +
      study_day_z +
      (
        1 |
          participant
      ),
    model_name = "Visible engagement",
    outcome_variable = "interaction_engagement"
  )
}


model_status <- if (
  length(
    model_results
  ) == 0
) {
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
  purrr::map_dfr(
    model_results,
    "status"
  )
}


model_coefficients <- if (
  length(
    model_results
  ) == 0
) {
  tibble()
} else {
  purrr::map_dfr(
    model_results,
    "tidy"
  )
}


model_objects <- purrr::map(
  model_results,
  "model"
)


icc_results_list <- list(
  public_relevance = fit_binary_icc(
    daily_all,
    "public_relevance",
    "Public relevance"
  ),
  
  incidental_strict = fit_binary_icc(
    daily,
    "incidental_strict",
    "Strict incidentality"
  ),
  
  read_thoroughly = fit_binary_icc(
    daily,
    "interaction_read",
    "Thorough reading/viewing"
  ),
  
  further_research = fit_binary_icc(
    daily,
    "interaction_research",
    "Further research"
  ),
  
  visible_engagement = fit_binary_icc(
    daily,
    "interaction_engagement",
    "Visible engagement"
  )
)


icc_results <- map_dfr(
  icc_results_list,
  "result"
)


icc_model_objects <- map(
  icc_results_list,
  "model"
)

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
  aes(x = N_Screenshots_Total)
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
    title = "Anzahl hochgeladener Screenshots pro Person",
    subtitle = paste0(
      "N = ",
      nrow(participant_counts),
      " Teilnehmende; gestrichelte Linie = Einschlussgrenze"
    ),
    x = "Anzahl der Screenshots",
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
    subtitle = "Absolute Zahl aller hochgeladenen Screenshots",
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
  filter(
    Grouping_Variable ==
      "Incidentality",
    is.finite(
      Percent_Yes
    )
  )

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
    breaks = seq(
      0,
      100,
      20
    ),
    labels = label_percent(
      scale = 1
    ),
    expand = expansion(
      mult = c(
        0,
        0.05
      )
    )
  ) +
  coord_cartesian(
    ylim = c(
      0,
      100
    )
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
  ) %>%
  filter(
    is.finite(
      Shannon
    )
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





#===============================================================================
# 22A Additional figures: relevance, calibration and theory-building exploration
#===============================================================================

public_relevance_platform_plot_data <- public_relevance_by_platform %>%
  filter(
    N_Valid > 0
  )


if (
  nrow(
    public_relevance_platform_plot_data
  ) > 0
) {
  figure_public_relevance_platform <- ggplot(
    public_relevance_platform_plot_data,
    aes(
      x = Percent_Yes,
      y = forcats::fct_reorder(
        Group,
        Percent_Yes
      )
    )
  ) +
    geom_col(
      width = 0.66,
      fill = unname(
        project_colors["primary"]
      )
    ) +
    geom_text(
      aes(
        label = paste0(
          round(
            Percent_Yes,
            1
          ),
          "% (",
          N_Yes,
          "/",
          N_Valid,
          ")"
        )
      ),
      hjust = -0.12,
      fontface = "bold",
      color = unname(
        project_colors["dark"]
      ),
      size = 3.2
    ) +
    scale_x_continuous(
      limits = c(
        0,
        100
      ),
      breaks = seq(
        0,
        100,
        20
      ),
      labels = label_percent(
        scale = 1
      ),
      expand = expansion(
        mult = c(
          0,
          0.18
        )
      )
    ) +
    coord_cartesian(
      clip = "off"
    ) +
    labs(
      title = "Öffentliche Relevanz nach Plattform",
      subtitle = "Anteil der Uploads, die das Public-Relevance-Gate erfüllen",
      x = "Öffentlich relevante Uploads",
      y = NULL,
      caption = simulation_caption
    ) +
    theme(
      panel.grid.major.x = element_line(
        color = unname(
          project_colors["grid"]
        )
      ),
      panel.grid.major.y = element_blank()
    )
  
  save_project_plot(
    figure_public_relevance_platform,
    "Daily_Public_Relevance_By_Platform.png",
    width = 9,
    height = 5.5
  )
}


conversion_plot_data <- conversion_funnel %>%
  filter(
    Funnel ==
      "All public content posts"
  ) %>%
  mutate(
    Stage = factor(
      Stage,
      levels = Stage
    )
  )


figure_conversion <- ggplot(
  conversion_plot_data,
  aes(
    x = Percent,
    y = forcats::fct_rev(
      Stage
    )
  )
) +
  geom_col(
    width = 0.66,
    fill = unname(
      project_colors["primary"]
    )
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          Percent,
          1
        ),
        "% (N=",
        N,
        ")"
      )
    ),
    hjust = -0.12,
    fontface = "bold",
    color = unname(
      project_colors["dark"]
    ),
    size = 3.2
  ) +
  scale_x_continuous(
    limits = c(
      0,
      100
    ),
    labels = label_percent(
      scale = 1
    ),
    expand = expansion(
      mult = c(
        0,
        0.18
      )
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "Deskriptiver Verarbeitungsfunnel",
    subtitle = "Getrennte Anteile der berichteten Verarbeitungspraktiken",
    x = "Anteil gültiger Antworten",
    y = NULL,
    caption = paste(
      "Keine direkt beobachtete zeitliche Sequenz.",
      simulation_caption %||% ""
    )
  ) +
  theme(
    panel.grid.major.x = element_line(
      color = unname(
        project_colors["grid"]
      )
    ),
    panel.grid.major.y = element_blank()
  )


save_project_plot(
  figure_conversion,
  "Daily_Processing_Funnel.png",
  width = 9,
  height = 5.5
)


novelty_plot_data <- novelty_by_incidentality %>%
  filter(
    N_Valid > 0
  )


if (
  nrow(
    novelty_plot_data
  ) > 0
) {
  figure_novelty_incidentality <- ggplot(
    novelty_plot_data,
    aes(
      x = Group,
      y = Percent_Yes,
      fill = Outcome
    )
  ) +
    geom_col(
      position = position_dodge(
        width = 0.78
      ),
      width = 0.70
    ) +
    scale_y_continuous(
      limits = c(
        0,
        100
      ),
      labels = label_percent(
        scale = 1
      ),
      expand = expansion(
        mult = c(
          0,
          0.08
        )
      )
    ) +
    labs(
      title = "Novelty und produktive Serendipität nach Inzidentalität",
      subtitle = "Diary-interne erste Vorkommen und verarbeitete neue Inhalte",
      x = NULL,
      y = "Anteil der Beiträge",
      fill = NULL,
      caption = paste(
        "Novelty bezieht sich nur auf die hochgeladenen Diary-Beiträge.",
        simulation_caption %||% ""
      )
    ) +
    theme(
      axis.text.x = element_text(
        angle = 25,
        hjust = 1
      )
    )
  
  save_project_plot(
    figure_novelty_incidentality,
    "Daily_Novelty_By_Incidentality.png",
    width = 11,
    height = 6.5
  )
}


calibration_gap_plot_data <- participant_metrics %>%
  filter(
    is.finite(
      Incidentality_Gap_Broad
    )
  )


if (
  nrow(
    calibration_gap_plot_data
  ) > 0
) {
  figure_calibration_gap <- ggplot(
    calibration_gap_plot_data,
    aes(
      x = Incidentality_Gap_Broad
    )
  ) +
    geom_histogram(
      bins = 12,
      fill = unname(
        project_colors["primary"]
      ),
      color = unname(
        project_colors["white"]
      ),
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = 0,
      color = unname(
        project_colors["accent"]
      ),
      linewidth = 0.9,
      linetype = "22"
    ) +
    labs(
      title = "Screening–Diary-Kalibrierung der Inzidentalität",
      subtitle = "Standardisierter Screening-Wert minus standardisierter Diary-Anteil",
      x = "Gerichtete Kalibrierungsdifferenz",
      y = "Anzahl der Teilnehmenden",
      caption = paste(
        "Positive Werte: Screening relativ höher; keine automatische Interpretation als Bias.",
        simulation_caption %||% ""
      )
    )
  
  save_project_plot(
    figure_calibration_gap,
    "Daily_Incidentality_Calibration_Gap.png",
    width = 8.5,
    height = 5.5
  )
}


age_marker_plot_data <- age_marker_correlations %>%
  filter(
    is.finite(
      Spearman_Rho
    )
  ) %>%
  slice_max(
    order_by = abs(
      Spearman_Rho
    ),
    n = 15,
    with_ties = FALSE
  ) %>%
  mutate(
    Marker = forcats::fct_reorder(
      Marker,
      Spearman_Rho
    )
  )


if (
  nrow(
    age_marker_plot_data
  ) > 0
) {
  figure_age_markers <- ggplot(
    age_marker_plot_data,
    aes(
      x = Spearman_Rho,
      y = Marker
    )
  ) +
    geom_vline(
      xintercept = 0,
      color = unname(
        project_colors["grid"]
      ),
      linewidth = 0.7
    ) +
    geom_col(
      width = 0.64,
      fill = unname(
        project_colors["primary"]
      )
    ) +
    scale_x_continuous(
      limits = c(
        -1,
        1
      ),
      breaks = seq(
        -1,
        1,
        0.25
      )
    ) +
    labs(
      title = "Explorative Alterszusammenhänge",
      subtitle = "Die 15 betragsmäßig stärksten Spearman-Korrelationen",
      x = "Spearman ρ",
      y = NULL,
      caption = paste(
        "Deskriptiv-explorativ; BH-adjustierte p-Werte stehen im Excel-Output.",
        simulation_caption %||% ""
      )
    ) +
    theme(
      panel.grid.major.x = element_line(
        color = unname(
          project_colors["grid"]
        )
      ),
      panel.grid.major.y = element_blank()
    )
  
  save_project_plot(
    figure_age_markers,
    "Daily_Age_Marker_Correlations.png",
    width = 10,
    height = 8
  )
}


if (
  nrow(
    platform_complementarity
  ) > 0
) {
  platform_complementarity_plot_data <- platform_complementarity %>%
    filter(
      is.finite(
        Jensen_Shannon_Divergence
      )
    )
  
  figure_platform_complementarity <- ggplot(
    platform_complementarity_plot_data,
    aes(
      x = Dimension,
      y = Jensen_Shannon_Divergence
    )
  ) +
    geom_boxplot(
      width = 0.58,
      outlier.shape = NA,
      fill = unname(
        project_colors["light"]
      ),
      color = unname(
        project_colors["primary"]
      )
    ) +
    geom_jitter(
      width = 0.12,
      alpha = 0.55,
      size = 1.7,
      color = unname(
        project_colors["dark"]
      )
    ) +
    scale_y_continuous(
      limits = c(
        0,
        1
      )
    ) +
    labs(
      title = "Within-Person-Plattformkomplementarität",
      subtitle = "Jensen–Shannon-Divergenz zwischen Plattformprofilen",
      x = NULL,
      y = "Jensen–Shannon-Divergenz",
      caption = paste(
        "0 = identische Profile; 1 = maximal verschieden. Nur Plattformen mit ausreichenden Beiträgen.",
        simulation_caption %||% ""
      )
    )
  
  save_project_plot(
    figure_platform_complementarity,
    "Daily_Platform_Complementarity.png",
    width = 8.5,
    height = 6
  )
}


day_trend_plot_data <- day_summary %>%
  select(
    study_day,
    `Öffentlich relevant` =
      Percent_Public_Relevant,
    `Gezielt gesucht` =
      Percent_Targeted,
    `Breit inzidentell` =
      Percent_Incidental_Broad,
    `Gründlich verarbeitet` =
      Percent_Read_Thoroughly,
    `Topic-Novelty` =
      Percent_Topic_Novelty,
    `Produktive Serendipität` =
      Percent_Productive_Serendipity_Broad
  ) %>%
  pivot_longer(
    cols = -study_day,
    names_to = "Indicator",
    values_to = "Percent"
  ) %>%
  filter(
    is.finite(
      Percent
    )
  )


if (
  nrow(
    day_trend_plot_data
  ) > 0
) {
  figure_day_trends <- ggplot(
    day_trend_plot_data,
    aes(
      x = study_day,
      y = Percent,
      group = Indicator
    )
  ) +
    geom_line(
      linewidth = 0.85
    ) +
    geom_point(
      size = 2.1
    ) +
    facet_wrap(
      ~ Indicator,
      ncol = 2
    ) +
    scale_x_continuous(
      breaks = expected_study_days
    ) +
    scale_y_continuous(
      limits = c(
        0,
        100
      ),
      labels = label_percent(
        scale = 1
      )
    ) +
    labs(
      title = "Deskriptive Muster über die sieben Studientage",
      subtitle = "Mögliche Reaktivität, Ermüdung und gewöhnliche Tagesvariation",
      x = "Studientag",
      y = "Anteil",
      caption = paste(
        "Novelty sinkt teilweise mechanisch, weil erste Vorkommen im Zeitverlauf seltener werden.",
        simulation_caption %||% ""
      )
    )
  
  save_project_plot(
    figure_day_trends,
    "Daily_Study_Day_Trends.png",
    width = 11,
    height = 9
  )
}


need_fit_plot_data <- daily %>%
  filter(
    is.finite(
      post_need_fit_z
    ),
    !is.na(
      incidentality
    )
  )


if (
  nrow(
    need_fit_plot_data
  ) > 0
) {
  figure_need_fit <- ggplot(
    need_fit_plot_data,
    aes(
      x = incidentality,
      y = post_need_fit_z
    )
  ) +
    geom_boxplot(
      width = 0.58,
      outlier.shape = NA,
      fill = unname(
        project_colors["light"]
      ),
      color = unname(
        project_colors["primary"]
      )
    ) +
    geom_jitter(
      width = 0.12,
      alpha = 0.30,
      size = 1.2,
      color = unname(
        project_colors["dark"]
      )
    ) +
    facet_wrap(
      ~ need_domain
    ) +
    labs(
      title = "Beitragsbezogener Informationsbedürfnis-Fit",
      subtitle = "Standardisiertes Screening-Bedürfnis für die jeweilige Inhalts-/Quelldomäne",
      x = NULL,
      y = "Informationsbedürfnis, z-standardisiert",
      caption = paste(
        "Explorative Cross-Level-Operationalisierung; kein direkter individueller Relevanzbericht.",
        simulation_caption %||% ""
      )
    ) +
    theme(
      axis.text.x = element_text(
        angle = 25,
        hjust = 1
      )
    )
  
  save_project_plot(
    figure_need_fit,
    "Daily_Post_Need_Fit.png",
    width = 12,
    height = 8
  )
}


#===============================================================================
# 23 Save prepared data and model objects
#===============================================================================

# All included screenshots, including non-public contributions and rows excluded
# from the main content analysis by the public-relevance gate.
saveRDS(
  daily_all,
  output_screenshot_rds
)


# Publicly relevant and fully content-coded screenshots used for the main
# content, incidentality, context and interaction analyses.
saveRDS(
  daily,
  output_public_rds
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
    icc_models = icc_model_objects,
    icc_results = icc_results,
    simulated = simulate_coding,
    public_relevance_mode =
      public_relevance_mode
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
    "public_relevance_mode",
    "minimum_screenshots",
    "require_screening_match",
    "strict_coding_check",
    "run_mixed_models",
    "run_icc_models",
    "minimum_model_n",
    "minimum_model_participants",
    "minimum_model_events",
    "minimum_posts_per_platform_profile"
  ),
  
  Value = as.character(
    c(
      simulate_coding,
      simulation_seed,
      simulation_overwrite_existing,
      simulation_use_screening_patterns,
      public_relevance_mode,
      minimum_screenshots,
      require_screening_match,
      strict_coding_check,
      run_mixed_models,
      run_icc_models,
      minimum_model_n,
      minimum_model_participants,
      minimum_model_events,
      minimum_posts_per_platform_profile
    )
  )
)


method_notes <- tibble(
  Topic = c(
    "Analysis levels",
    "Public-relevance gate",
    "Screenshot inclusion",
    "Participant weighting",
    "Novelty",
    "Calibration",
    "Compositional data",
    "Exploratory tests",
    "Multilevel models",
    "Simulation"
  ),
  
  Note = c(
    "daily_all contains all included uploads; daily contains publicly relevant and fully content-coded posts.",
    "Rows coded 0 are retained for methodological analysis but excluded from Topic/Source/Format analyses.",
    "The minimum-screenshot criterion is based on all uploaded screenshots.",
    "Participant-weighted tables average person-specific category shares and complement screenshot-weighted totals.",
    "Diary-internal first occurrence; not prior knowledge or complete feed novelty.",
    "Screening–Diary discrepancies are not automatically self-report bias because measures differ in time frame and granularity.",
    "Topic/source shares sum to one within participants; individual correlations are not independent.",
    "BH-adjusted p-values are orientation only. Emphasise effect sizes, uncertainty and theoretical coherence.",
    "Models are exploratory and only fitted above minimum sample/event thresholds; inspect singularity and convergence.",
    "Simulated results are pipeline tests only and must not be interpreted substantively."
  )
)


add_excel_sheet(
  workbook,
  "Settings",
  settings_table,
  header_style
)

add_excel_sheet(
  workbook,
  "Method_Notes",
  method_notes,
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
  "Exclusions",
  analysis_exclusions,
  header_style
)

add_excel_sheet(
  workbook,
  "Coding_Completeness",
  coding_completeness,
  header_style
)

add_excel_sheet(
  workbook,
  "PublicRel_Issues",
  public_relevance_issues,
  header_style
)

add_excel_sheet(
  workbook,
  "Coding_Completion_Issues",
  coding_completion_issues,
  header_style
)

add_excel_sheet(
  workbook,
  "Content_Coding_Issues",
  content_coding_issues,
  header_style
)

add_excel_sheet(
  workbook,
  "Invalid_Categories",
  invalid_categories,
  header_style
)

add_excel_sheet(
  workbook,
  "Duplicate_Screenshot_IDs",
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
  "Reported_Coded_Accounts",
  reported_coded_accounts,
  header_style
)

add_excel_sheet(
  workbook,
  "Study_Day_Issues",
  study_day_issues,
  header_style
)

add_excel_sheet(
  workbook,
  "File_Issues",
  file_issues,
  header_style
)

add_excel_sheet(
  workbook,
  "Participant_Counts",
  participant_counts,
  header_style
)

add_excel_sheet(
  workbook,
  "Day_Summary",
  day_summary,
  header_style
)


add_excel_sheet(
  workbook,
  "PublicRel_Distribution",
  public_relevance_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "PublicRel_Platform",
  public_relevance_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "PublicRel_Day",
  public_relevance_by_day,
  header_style
)


add_excel_sheet(
  workbook,
  "Topic_Screenshot",
  topic_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_Participant",
  topic_participant_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_Shares_Person",
  topic_shares,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_Screenshot",
  source_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_Participant",
  source_participant_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_Shares_Person",
  source_shares,
  header_style
)

add_excel_sheet(
  workbook,
  "Account_Names",
  source_name_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Screenshot",
  platform_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Participant",
  platform_participant_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Shares_Person",
  platform_shares,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_Screenshot",
  format_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_Participant",
  format_participant_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_Shares_Person",
  format_shares,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality",
  incidentality_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidentality_Person",
  incidentality_participant_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Interactions",
  interaction_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Processing_Patterns",
  processing_pattern_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Conversion_Funnel",
  conversion_funnel,
  header_style
)

add_excel_sheet(
  workbook,
  "Incidental_Conversion",
  incidental_conversion,
  header_style
)

add_excel_sheet(
  workbook,
  "Novelty_Summary",
  novelty_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Novelty_Incidentality",
  novelty_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Post_Need_Fit",
  post_need_fit_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Local_Context",
  locality_distribution,
  header_style
)

add_excel_sheet(
  workbook,
  "Social_Context",
  situation_distribution,
  header_style
)


add_excel_sheet(
  workbook,
  "Participant_Metrics",
  participant_metrics,
  header_style
)

add_excel_sheet(
  workbook,
  "Participant_Metric_Summary",
  participant_metrics_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_Platform",
  topic_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_Platform",
  source_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_Platform",
  format_by_platform,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_Incidentality",
  topic_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_Incidentality",
  source_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Format_Incidentality",
  format_by_incidentality,
  header_style
)

add_excel_sheet(
  workbook,
  "Interaction_Groups",
  interaction_by_groups,
  header_style
)


add_excel_sheet(
  workbook,
  "Screening_Diary_Cor",
  screening_diary_correlations,
  header_style
)

add_excel_sheet(
  workbook,
  "Topic_Need_Cor",
  topic_need_correlations,
  header_style
)

add_excel_sheet(
  workbook,
  "Source_Need_Cor",
  source_need_correlations,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Alignment",
  platform_alignment_correlations,
  header_style
)

add_excel_sheet(
  workbook,
  "Alignment_Summary",
  alignment_summary,
  header_style
)

add_excel_sheet(
  workbook,
  "Calibration_Subgroups",
  calibration_subgroups,
  header_style
)

add_excel_sheet(
  workbook,
  "Age_Marker_Cor",
  age_marker_correlations,
  header_style
)

add_excel_sheet(
  workbook,
  "Subgroup_Summaries",
  subgroup_summaries,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Complementarity",
  platform_complementarity,
  header_style
)

add_excel_sheet(
  workbook,
  "Platform_Complement_Person",
  platform_complementarity_person,
  header_style
)

add_excel_sheet(
  workbook,
  "ICC_Results",
  icc_results,
  header_style
)

add_excel_sheet(
  workbook,
  "Model_Status",
  model_status,
  header_style
)

add_excel_sheet(
  workbook,
  "Model_Odds_Ratios",
  model_coefficients,
  header_style
)


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


if (
  simulate_coding
) {
  cat(
    "WARNING: Manual coding variables were simulated. ",
    "Do not interpret substantive results.\n",
    sep = ""
  )
}


cat(
  "Public-relevance mode: ",
  public_relevance_mode,
  "\n",
  sep = ""
)


cat(
  "Rows in coding sheet: ",
  nrow(
    coding
  ),
  "\n",
  sep = ""
)


cat(
  "Participants in final daily sample: ",
  n_distinct(
    daily_all$participant
  ),
  "\n",
  sep = ""
)


cat(
  "All screenshots in final daily sample: ",
  nrow(
    daily_all
  ),
  "\n",
  sep = ""
)


cat(
  "Publicly relevant screenshots: ",
  sum(
    daily_all$public_relevance == 1L,
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)


cat(
  "Not publicly relevant screenshots: ",
  sum(
    daily_all$public_relevance == 0L,
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)


cat(
  "Publicly relevant content-coded screenshots analysed: ",
  nrow(
    daily
  ),
  "\n",
  sep = ""
)


cat(
  "Median total screenshots per participant: ",
  round(
    safe_median(
      participant_counts$N_Screenshots_Total
    ),
    2
  ),
  "\n",
  sep = ""
)


cat(
  "Broad incidental exposure among public content: ",
  round(
    100 *
      safe_mean(
        daily$incidental_broad
      ),
    1
  ),
  "%\n",
  sep = ""
)


cat(
  "Strict incidental exposure among public content: ",
  round(
    100 *
      safe_mean(
        daily$incidental_strict
      ),
    1
  ),
  "%\n",
  sep = ""
)


cat(
  "Productive serendipity, broad: ",
  round(
    100 *
      safe_mean(
        daily$productive_serendipity_broad
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
  "\nAll-screenshot RDS:\n",
  output_screenshot_rds,
  "\n",
  sep = ""
)


cat(
  "\nPublic-content RDS:\n",
  output_public_rds,
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
  "\nModel/ICC RDS:\n",
  output_models_rds,
  "\n",
  sep = ""
)


cat(
  "\nFigures:\n",
  figure_folder,
  "\n",
  sep = ""
)


if (
  simulate_coding &&
  write_simulated_coding_sheet
) {
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
