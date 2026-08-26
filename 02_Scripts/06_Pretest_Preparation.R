################################################################################
# Project: Tagebuchstudie
# File:    06_Pretest_Preparation.R
#
# Purpose:
#   Bereitet zwei Kodier-Pretests vor:
#
#   1) Gemeinsame Kodierung / Codebuch-Check
#      - 60 Screenshots
#      - 15 pro Plattform (Facebook, Instagram, TikTok, X)
#      - identisches Sample für MF und LA
#
#   2) Reliabilitäts-Pretest
#      - getrennte Kodierung desselben Samples durch MF und LA
#      - 10 % des verfügbaren finalen Screenshot-Samples oder maximal 100 Beiträge
#      - wegen Plattformbalance immer gleiche Fallzahl pro Plattform
#      - Sample ist standardmäßig disjunkt von Stufe 1
#
# Sampling:
#   - stratifiziert nach Plattform
#   - innerhalb jeder Plattform möglichst breite Streuung über Teilnehmende:
#     zunächst höchstens ein Screenshot pro Person, bevor weitere Screenshots
#     derselben Person gezogen werden
#   - zufällige Auswahl bei reproduzierbarem Seed
#   - Coding-Reihenfolge: Plattformblöcke Facebook -> Instagram -> TikTok -> X;
#     innerhalb der Plattformen zufällige Reihenfolge
#
# Transfer ins finale Coding-Sheet:
#   - screenshot_id wird NIEMALS verändert und ist der verbindliche Merge-Key.
#   - Auch participant, study_day, photo und filename bleiben unverändert.
#   - Die manuellen Coding-Spalten haben exakt dieselben Namen wie im finalen
#     Coding-Sheet und können daher später per screenshot_id übernommen werden.
#   - Nur filepath wird im Pretest auf die kopierte Datei in 07_Pretest gesetzt;
#     original_filepath bewahrt den Pfad aus dem regulären Coding-Sample.
#
# Output:
#
#   07_Pretest/
#   ├── 01_Gemeinsame_Kodierung/
#   │   ├── Screenshots/
#   │   │   ├── Facebook/
#   │   │   ├── Instagram/
#   │   │   ├── TikTok/
#   │   │   └── X/
#   │   ├── coding_sheet_MF.xlsx
#   │   ├── coding_sheet_LA.xlsx
#   │   └── sample_manifest.csv
#   │
#   └── 02_Reliabilitaets_Pretest/
#       ├── Screenshots/
#       │   ├── Facebook/
#       │   ├── Instagram/
#       │   ├── TikTok/
#       │   └── X/
#       ├── coding_sheet_MF.xlsx
#       ├── coding_sheet_LA.xlsx
#       └── sample_manifest.csv
#
# Input:
#   01_Data/taeglicher_fragebogen_screenshot_upload.rds
#   05_Participants/...  (von 02_Sort_Files.R erzeugte Screenshots)
#
# Notes:
#   - Das Workbook-Layout orientiert sich an 03_Create_Coding_File.R.
#   - Nach Beginn der manuellen Kodierung sind die erzeugten Excel-Dateien
#     maßgeblich und sollten nicht erneut überschrieben werden.
################################################################################

rm(list = ls())


#===============================================================================
# 01 Setup
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  openxlsx,
  fs
)

source(file.path("02_Scripts", "00_Helpers.R"))


#-------------------------------------------------------------------------------
# User settings
#-------------------------------------------------------------------------------

overwrite_existing <- FALSE

# Reproduzierbares Sampling.
sampling_seed <- 20260818L

coders <- c("MF", "LA")

platform_order <- c(
  "Facebook",
  "Instagram",
  "TikTok",
  "X"
)

# Stufe 1: gemeinsame Kodierung.
common_n_per_platform <- 15L

# Stufe 2: Reliabilitäts-Pretest.
reliability_share <- 0.10
reliability_max_total <- 100L

# TRUE würde erlauben, dass Screenshots aus Stufe 1 erneut im Reliabilitäts-
# Pretest auftauchen. Methodisch ist FALSE vorzuziehen.
allow_overlap_between_stages <- FALSE


#===============================================================================
# 02 Paths
#===============================================================================

data_file <- file.path(
  "01_Data",
  "taeglicher_fragebogen_screenshot_upload.rds"
)

participant_folder <- "05_Participants"

output_root <- "07_Pretest"

common_folder <- file.path(
  output_root,
  "01_Gemeinsame_Kodierung"
)

reliability_folder <- file.path(
  output_root,
  "02_Reliabilitaets_Pretest"
)


#===============================================================================
# 03 Guard rails
#===============================================================================

if (!file.exists(data_file)) {
  stop("Daily-RDS nicht gefunden: ", data_file)
}

stage_folders <- c(
  common_folder,
  reliability_folder
)

if (overwrite_existing) {
  purrr::walk(
    stage_folders[fs::dir_exists(stage_folders)],
    fs::dir_delete
  )
} else {
  existing_stage_folders <- stage_folders[
    fs::dir_exists(stage_folders)
  ]
  
  if (length(existing_stage_folders) > 0) {
    stop(
      "Mindestens ein Pretest-Ordner existiert bereits und wird nicht ",
      "überschrieben:\n",
      paste0("  - ", existing_stage_folders, collapse = "\n"),
      "\n\nFür einen bewussten Neuaufbau overwrite_existing <- TRUE setzen."
    )
  }
}

fs::dir_create(output_root)


#===============================================================================
# 04 Small local helpers and labels
#===============================================================================

label_code <- function(x, labels) {
  dplyr::recode(
    as.character(x),
    !!!labels,
    .default = "Invalid code",
    .missing = NA_character_
  )
}


collapse_interactions <- function(read, research, engagement) {
  x <- c(
    if (!is.na(read)       && read       == 1) "Read/watched thoroughly",
    if (!is.na(research)   && research   == 1) "Sought further information",
    if (!is.na(engagement) && engagement == 1) "Engaged with the post"
  )
  
  if (length(x) == 0) {
    NA_character_
  } else {
    paste(x, collapse = "; ")
  }
}


platform_labels <- c(
  `1` = "Facebook",
  `2` = "Instagram",
  `3` = "TikTok",
  `4` = "X"
)

incidentality_labels <- c(
  `1` = "Deliberately searched for this topic or this account's posts",
  `2` = "Follows the account, but did not specifically seek the post",
  `3` = "Came across the post by chance"
)

locality_labels <- c(
  `1` = "At home",
  `2` = "Out and about",
  `3` = "Don't know"
)

situation_labels <- c(
  `1` = "Used the platform alone",
  `2` = "Used the platform together with someone else",
  `3` = "Don't know"
)

interaction_labels <- c(
  `1` = "Yes",
  `0` = "No",
  `-1` = "No answer"
)


#===============================================================================
# 05 Daily data -> master coding data
#===============================================================================

daily <- readRDS(data_file)

missing_base <- setdiff(
  c("personalParticipantCode", "scheduled"),
  names(daily)
)

if (length(missing_base) > 0) {
  stop(
    "Benötigte Variablen fehlen: ",
    paste(missing_base, collapse = ", ")
  )
}

if (!any(stringr::str_detect(
  names(daily),
  "^daily_[0-9]+_screenshot$"
))) {
  stop("Keine Variablen nach dem Muster daily_[n]_screenshot gefunden.")
}


# Studientag, Foto-Nummer, Dateiname und ursprünglicher Pfad stammen aus
# derselben Hilfsfunktion wie in 02_Sort_Files.R und 03_Create_Coding_File.R.
coding_master <- derive_screenshot_index(
  daily,
  participant_folder = participant_folder
)

expected_fields <- c(
  "screenshot",
  "topic",
  "account",
  "platform",
  "incidentality",
  "interaction_1",
  "interaction_2",
  "interaction_3",
  "locality",
  "situation",
  "startstop"
)

for (x in setdiff(expected_fields, names(coding_master))) {
  coding_master[[x]] <- NA
}

if (any(is.na(coding_master$participant))) {
  stop("Mindestens ein Screenshot besitzt keinen gültigen Participant Code.")
}


coding_master <- coding_master %>%
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
    
    platform_reported = label_code(
      platform_code,
      platform_labels
    ),
    
    incidentality_label = label_code(
      incidentality_code,
      incidentality_labels
    ),
    
    locality_label = label_code(
      locality_code,
      locality_labels
    ),
    
    situation_label = label_code(
      situation_code,
      situation_labels
    ),
    
    interaction_read = label_code(
      interaction_read_code,
      interaction_labels
    ),
    
    interaction_research = label_code(
      interaction_research_code,
      interaction_labels
    ),
    
    interaction_engagement = label_code(
      interaction_engagement_code,
      interaction_labels
    ),
    
    interaction_summary = purrr::pmap_chr(
      list(
        interaction_read_code,
        interaction_research_code,
        interaction_engagement_code
      ),
      collapse_interactions
    ),
    
    startstop_raw = startstop,
    
    startstop_label = case_when(
      str_to_lower(clean_text(startstop_raw)) %in%
        c("true", "t", "1") ~ "Weiter",
      
      str_to_lower(clean_text(startstop_raw)) %in%
        c("false", "f", "0") ~ "Stopp",
      
      TRUE ~ NA_character_
    ),
    
    original_filepath = filepath,
    file_exists = fs::file_exists(original_filepath)
  )


#===============================================================================
# 06 Define the usable sampling pool
#===============================================================================

# Für den Pretest werden nur tatsächlich vorhandene Dateien mit eindeutig
# zuordenbarer Plattform verwendet. Dies ist zugleich die Bezugsmenge für die
# 10-%-Regel des Reliabilitäts-Pretests.
sampling_pool <- coding_master %>%
  filter(
    file_exists %in% TRUE,
    platform_reported %in% platform_order
  )

n_master_total <- nrow(coding_master)
n_sampling_pool <- nrow(sampling_pool)

n_excluded_missing_file <- sum(
  coding_master$file_exists %in% FALSE
)

n_excluded_platform <- sum(
  !coding_master$platform_reported %in% platform_order |
    is.na(coding_master$platform_reported)
)

if (n_sampling_pool == 0) {
  stop("Es gibt keine nutzbaren Screenshots für das Pretest-Sampling.")
}


platform_availability <- sampling_pool %>%
  count(
    platform_reported,
    name = "N_available"
  ) %>%
  tidyr::complete(
    platform_reported = platform_order,
    fill = list(N_available = 0L)
  ) %>%
  arrange(
    match(platform_reported, platform_order)
  )


#===============================================================================
# 07 Sampling helpers
#===============================================================================

# Zieht innerhalb einer Plattform möglichst teilnehmerdivers:
# Runde 1 = maximal ein Screenshot je Person, Runde 2 = maximal der zweite usw.
# Zufallszahlen bestimmen sowohl die Reihenfolge der Personen als auch die
# Auswahl innerhalb einer Person.
sample_participant_diverse <- function(data, n) {
  
  if (n <= 0) {
    return(data[0, , drop = FALSE])
  }
  
  if (nrow(data) < n) {
    stop(
      "Zu wenige Fälle in einem Plattform-Stratum: benötigt ",
      n,
      ", vorhanden ",
      nrow(data),
      "."
    )
  }
  
  participant_order <- data %>%
    distinct(participant) %>%
    mutate(
      participant_random = runif(n())
    )
  
  data %>%
    mutate(
      row_random = runif(n())
    ) %>%
    group_by(participant) %>%
    arrange(
      row_random,
      .by_group = TRUE
    ) %>%
    mutate(
      within_participant_order = row_number()
    ) %>%
    ungroup() %>%
    left_join(
      participant_order,
      by = "participant"
    ) %>%
    arrange(
      within_participant_order,
      participant_random,
      row_random
    ) %>%
    slice_head(n = n) %>%
    select(
      -within_participant_order,
      -participant_random,
      -row_random
    )
}


sample_equal_platforms <- function(
    data,
    n_per_platform,
    seed
) {
  
  set.seed(seed)
  
  availability <- data %>%
    count(
      platform_reported,
      name = "N_available"
    ) %>%
    tidyr::complete(
      platform_reported = platform_order,
      fill = list(N_available = 0L)
    )
  
  insufficient <- availability %>%
    filter(
      N_available < n_per_platform
    )
  
  if (nrow(insufficient) > 0) {
    stop(
      "Nicht genügend Screenshots für ein plattformbalanciertes Sample.\n",
      paste0(
        insufficient$platform_reported,
        ": benötigt ",
        n_per_platform,
        ", vorhanden ",
        insufficient$N_available,
        collapse = "\n"
      )
    )
  }
  
  selected <- purrr::map_dfr(
    platform_order,
    function(current_platform) {
      
      platform_data <- data %>%
        filter(
          platform_reported == current_platform
        )
      
      sample_participant_diverse(
        platform_data,
        n_per_platform
      )
    }
  )
  
  # Lineare Coding-Reihenfolge:
  #   Facebook -> Instagram -> TikTok -> X.
  # Innerhalb jedes Plattformblocks bleibt die Reihenfolge zufällig.
  #
  # WICHTIG: pretest_order ist nur eine Arbeitsreihenfolge. screenshot_id,
  # filename und alle kanonischen Identifikatoren werden nicht verändert.
  selected %>%
    group_by(platform_reported) %>%
    mutate(
      pretest_order_within_platform = sample.int(n())
    ) %>%
    ungroup() %>%
    mutate(
      platform_sort = match(platform_reported, platform_order)
    ) %>%
    arrange(
      platform_sort,
      pretest_order_within_platform
    ) %>%
    mutate(
      pretest_order = row_number()
    ) %>%
    select(
      -platform_sort,
      -pretest_order_within_platform
    )
}


#===============================================================================
# 08 Determine sample sizes
#===============================================================================

common_n_total <- common_n_per_platform * length(platform_order)

# "10 % oder 100, whatever comes first".
# Für exakt gleiche Plattformanteile wird auf das nächstkleinere Vielfache von
# vier abgerundet.
reliability_requested_total <- min(
  floor(n_sampling_pool * reliability_share),
  reliability_max_total
)

reliability_n_per_platform <- floor(
  reliability_requested_total / length(platform_order)
)

reliability_n_total <- reliability_n_per_platform * length(platform_order)

if (reliability_n_per_platform < 1) {
  stop(
    "Das verfügbare Sample ist zu klein für einen plattformbalancierten ",
    "Reliabilitäts-Pretest nach der 10-%-Regel."
  )
}

if (reliability_n_total < reliability_requested_total) {
  message(
    "Reliabilitäts-Pretest wird für exakte Plattformbalance von ",
    reliability_requested_total,
    " auf ",
    reliability_n_total,
    " Fälle abgerundet (",
    reliability_n_per_platform,
    " pro Plattform)."
  )
}


#===============================================================================
# 09 Draw Stage 1: common coding sample
#===============================================================================

common_sample <- sample_equal_platforms(
  sampling_pool,
  n_per_platform = common_n_per_platform,
  seed = sampling_seed
) %>%
  mutate(
    pretest_stage = "Gemeinsame Kodierung"
  )


#===============================================================================
# 10 Draw Stage 2: reliability sample
#===============================================================================

if (allow_overlap_between_stages) {
  
  reliability_pool <- sampling_pool
  
} else {
  
  reliability_pool <- sampling_pool %>%
    filter(
      !screenshot_id %in% common_sample$screenshot_id
    )
}

reliability_sample <- sample_equal_platforms(
  reliability_pool,
  n_per_platform = reliability_n_per_platform,
  seed = sampling_seed + 1L
) %>%
  mutate(
    pretest_stage = "Reliabilitäts-Pretest"
  )


#-------------------------------------------------------------------------------
# Identity checks for later transfer into the final coding sheet
#-------------------------------------------------------------------------------
# The pretest must remain merge-compatible with 03_Create_Coding_File.R.
# screenshot_id is the canonical key. The other columns are checked as an
# additional safeguard against accidental renaming/re-numbering.
assert_sample_identity <- function(sample_data, master_data) {
  
  canonical_cols <- c(
    "screenshot_id",
    "participant",
    "study_day",
    "photo",
    "filename",
    "original_filename"
  )
  
  reference <- master_data %>%
    select(all_of(canonical_cols)) %>%
    distinct()
  
  checked <- sample_data %>%
    select(all_of(canonical_cols)) %>%
    left_join(
      reference,
      by = "screenshot_id",
      suffix = c("_pretest", "_master")
    )
  
  if (any(is.na(checked$participant_master))) {
    stop(
      "Interner Fehler: Mindestens eine screenshot_id des Pretests existiert ",
      "nicht im Master-Sample."
    )
  }
  
  compare_cols <- setdiff(canonical_cols, "screenshot_id")
  
  mismatch <- purrr::map_lgl(
    compare_cols,
    function(x) {
      pretest_col <- checked[[paste0(x, "_pretest")]]
      master_col  <- checked[[paste0(x, "_master")]]
      
      any(
        dplyr::coalesce(as.character(pretest_col), "<NA>") !=
          dplyr::coalesce(as.character(master_col), "<NA>")
      )
    }
  )
  
  if (any(mismatch)) {
    stop(
      "Interner Fehler: Kanonische Screenshot-Identität wurde verändert: ",
      paste(compare_cols[mismatch], collapse = ", ")
    )
  }
  
  invisible(TRUE)
}


assert_sample_identity(
  common_sample,
  coding_master
)

assert_sample_identity(
  reliability_sample,
  coding_master
)


# Safety checks.
if (
  !allow_overlap_between_stages &&
  any(
    common_sample$screenshot_id %in%
    reliability_sample$screenshot_id
  )
) {
  stop("Interner Fehler: Die beiden Pretest-Samples überlappen.")
}

for (sample_object in list(common_sample, reliability_sample)) {
  
  platform_counts <- sample_object %>%
    count(platform_reported)
  
  if (
    nrow(platform_counts) != length(platform_order) ||
    dplyr::n_distinct(platform_counts$n) != 1
  ) {
    stop("Interner Fehler: Plattformbalance des Samples ist verletzt.")
  }
  
  if (anyDuplicated(sample_object$screenshot_id) > 0) {
    stop("Interner Fehler: Doppelte screenshot_id im Pretest-Sample.")
  }
}


#===============================================================================
# 11 Copy sample files
#===============================================================================

copy_pretest_files <- function(
    sample_data,
    stage_folder
) {
  
  screenshots_folder <- file.path(
    stage_folder,
    "Screenshots"
  )
  
  fs::dir_create(screenshots_folder)
  
  copied <- sample_data
  
  # Der kanonische Screenshot-Key und filename bleiben unangetastet.
  # Nur filepath wird auf die Arbeitskopie des Pretests umgebogen.
  copied$filepath <- NA_character_
  
  for (i in seq_len(nrow(copied))) {
    
    current_platform <- copied$platform_reported[i]
    
    destination_folder <- file.path(
      screenshots_folder,
      current_platform
    )
    
    fs::dir_create(destination_folder)
    
    source_file <- copied$original_filepath[i]
    
    destination_file <- file.path(
      destination_folder,
      copied$filename[i]
    )
    
    if (!fs::file_exists(source_file)) {
      stop(
        "Quelldatei beim Kopieren nicht gefunden: ",
        source_file
      )
    }
    
    fs::file_copy(
      source_file,
      destination_file,
      overwrite = overwrite_existing
    )
    
    copied$filepath[i] <- destination_file
  }
  
  copied <- copied %>%
    mutate(
      file_exists = fs::file_exists(filepath)
    )
  
  if (!all(copied$file_exists %in% TRUE)) {
    stop(
      "Mindestens eine Pretest-Datei wurde nicht korrekt kopiert."
    )
  }
  
  copied
}


fs::dir_create(common_folder)
fs::dir_create(reliability_folder)

common_sample <- copy_pretest_files(
  common_sample,
  common_folder
)

reliability_sample <- copy_pretest_files(
  reliability_sample,
  reliability_folder
)


#===============================================================================
# 12 Coding-sheet structure
#===============================================================================

# screenshot_id bleibt exakt die ID aus derive_screenshot_index() und damit
# identisch zum regulären/finalen Coding-Sheet. pretest_order ist ausschließlich
# eine lineare Arbeitsreihenfolge innerhalb dieses Pretests.
id_cols <- c(
  "pretest_order",
  "screenshot_id",
  "participant",
  "study_day",
  "photo",
  "filename",
  "filepath",
  "file_exists"
)

coding_cols <- c(
  "public_rel_coded",
  "topic_coded",
  "source_coded",
  "source_name_coded",
  "platform_coded",
  "media_format",
  "notes",
  "coder",
  "coding_completed",
  "coding_date"
)

visible_info_cols <- c(
  "topic_participant",
  "account_participant",
  "platform_reported"
)

hidden_info_cols <- c(
  "incidentality_label",
  "interaction_read",
  "interaction_research",
  "interaction_engagement",
  "interaction_summary",
  "locality_label",
  "situation_label",
  "startstop_label"
)

technical_cols <- c(
  "pretest_stage",
  "platform_code",
  "incidentality_code",
  "interaction_read_code",
  "interaction_research_code",
  "interaction_engagement_code",
  "locality_code",
  "situation_code",
  "startstop_raw",
  "original_filepath",
  "original_filename",
  "screenshot_slot",
  "submission_row",
  "scheduled",
  "committed"
)


codebook <- tribble(
  ~Variable, ~Code, ~Category, ~Rule,
  
  "public_rel_coded", "1", "Publicly relevant",
  "Information, opinion, evaluation, contextualization or action orientation with meaning beyond the private circle.",
  
  "public_rel_coded", "0", "Not publicly relevant",
  "Purely private, personal, self-presentational, purely entertaining or purely commercial function without public reference.",
  
  "public_rel_coded", "-1", "Not assessable",
  "Screenshot technically unusable or content not reliably assessable.",
  
  "topic_coded", "", "Main topic",
  "Only if public_rel_coded = 1; per the topic codebook.",
  
  "source_coded", "", "Source type",
  "Only if public_rel_coded = 1; per the source codebook.",
  
  "source_name_coded", "", "Concrete source",
  "Only if public_rel_coded = 1; visible account/source name.",
  
  "platform_coded", "", "Verified platform",
  "Pre-filled; correct if needed.",
  
  "media_format", "1", "Text/link-based",
  "Native text post or standardized link/article preview; ignore the caption of an image/video post.",
  
  "media_format", "2", "Static visual format",
  "Photo, illustration, graphic, meme, text card, infographic or purely static carousel.",
  
  "media_format", "3", "Moving audiovisual format",
  "Video, reel, TikTok, GIF or animation; also with text overlays or subtitles.",
  
  "media_format", "4", "Mixed media format",
  "Actual combination of static and moving media elements within the same post.",
  
  "media_format", "-1", "Not determinable",
  "Post format cannot be reliably identified from the screenshot.",
  
  "notes", "", "Notes",
  "Only for borderline cases or particularities.",
  
  "coder", "", "Coder",
  "Initials or name.",
  
  "coding_completed", "TRUE/FALSE", "Coding completed",
  "TRUE only after final review; for public_rel_coded = 0/-1 leave topic, source and format empty.",
  
  "coding_date", "", "Coding date",
  "Date of the final coding."
)


#===============================================================================
# 13 Workbook helper
#===============================================================================

create_pretest_workbook <- function(
    sample_data,
    coder_name,
    stage_name,
    output_excel
) {
  
  coding_export <- sample_data %>%
    mutate(
      # Manual coding
      public_rel_coded  = NA_integer_,
      topic_coded       = NA_character_,
      source_coded      = NA_character_,
      source_name_coded = NA_character_,
      platform_coded    = platform_reported,
      media_format      = NA_integer_,
      notes             = NA_character_,
      coder             = coder_name,
      coding_completed  = FALSE,
      coding_date       = as.Date(NA)
    ) %>%
    select(
      all_of(
        c(
          id_cols,
          coding_cols,
          visible_info_cols,
          hidden_info_cols,
          technical_cols
        )
      )
    )
  
  #---------------------------------------------------------------------------
  # Quality information
  #---------------------------------------------------------------------------
  
  n_participant_days <- coding_export %>%
    filter(
      !is.na(participant),
      !is.na(study_day)
    ) %>%
    distinct(
      participant,
      study_day
    ) %>%
    nrow()
  
  quality_summary <- tibble(
    Indicator = c(
      "Pretest stage",
      "Coder",
      "Screenshots total",
      "Participants",
      "Participant-days",
      "Facebook screenshots",
      "Instagram screenshots",
      "TikTok screenshots",
      "X screenshots",
      "Files found",
      "Files not found"
    ),
    
    Value = c(
      stage_name,
      coder_name,
      as.character(nrow(coding_export)),
      as.character(
        n_distinct(
          coding_export$participant,
          na.rm = TRUE
        )
      ),
      as.character(n_participant_days),
      as.character(sum(coding_export$platform_reported == "Facebook")),
      as.character(sum(coding_export$platform_reported == "Instagram")),
      as.character(sum(coding_export$platform_reported == "TikTok")),
      as.character(sum(coding_export$platform_reported == "X")),
      as.character(sum(coding_export$file_exists %in% TRUE)),
      as.character(sum(coding_export$file_exists %in% FALSE))
    )
  )
  
  missing_files <- coding_export %>%
    filter(
      file_exists %in% FALSE
    ) %>%
    select(
      screenshot_id,
      participant,
      study_day,
      photo,
      filename,
      filepath
    )
  
  #---------------------------------------------------------------------------
  # Workbook
  #---------------------------------------------------------------------------
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(
    wb,
    "Coding",
    gridLines = FALSE
  )
  
  openxlsx::writeData(
    wb,
    "Coding",
    coding_export,
    withFilter = FALSE
  )
  
  
  id_idx <- match(
    id_cols,
    names(coding_export)
  )
  
  coding_idx <- match(
    coding_cols,
    names(coding_export)
  )
  
  visible_idx <- match(
    visible_info_cols,
    names(coding_export)
  )
  
  hidden_idx <- match(
    hidden_info_cols,
    names(coding_export)
  )
  
  technical_idx <- match(
    technical_cols,
    names(coding_export)
  )
  
  
  header_style <- function(fill) {
    openxlsx::createStyle(
      fgFill = fill,
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      wrapText = TRUE,
      border = "Bottom"
    )
  }
  
  
  openxlsx::addStyle(
    wb,
    "Coding",
    header_style("#DCE7EA"),
    1,
    id_idx,
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    "Coding",
    header_style("#D5B47A"),
    1,
    coding_idx,
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    "Coding",
    header_style("#E7EEEE"),
    1,
    c(visible_idx, hidden_idx),
    gridExpand = TRUE
  )
  
  openxlsx::addStyle(
    wb,
    "Coding",
    header_style("#E1E1E1"),
    1,
    technical_idx,
    gridExpand = TRUE
  )
  
  
  if (nrow(coding_export) > 0) {
    
    rows <- 2:(nrow(coding_export) + 1)
    
    # Coding block visually distinct.
    openxlsx::addStyle(
      wb,
      "Coding",
      openxlsx::createStyle(
        fgFill = "#FFF7E8",
        valign = "top",
        wrapText = TRUE
      ),
      rows,
      coding_idx,
      gridExpand = TRUE,
      stack = TRUE
    )
    
    # Thick separators before Coding and visible Daily info.
    openxlsx::addStyle(
      wb,
      "Coding",
      openxlsx::createStyle(
        border = "Left",
        borderStyle = "thick",
        borderColour = "#8A8A8A"
      ),
      1:(nrow(coding_export) + 1),
      c(
        min(coding_idx),
        min(visible_idx)
      ),
      gridExpand = TRUE,
      stack = TRUE
    )
    
    openxlsx::addFilter(
      wb,
      "Coding",
      rows = 1,
      cols = seq_len(ncol(coding_export))
    )
    
    validation <- c(
      public_rel_coded = '"1,0,-1"',
      platform_coded   = '"Facebook,Instagram,TikTok,X"',
      media_format     = '"1,2,3,4,-1"',
      coding_completed = '"FALSE,TRUE"'
    )
    
    purrr::iwalk(
      validation,
      ~ openxlsx::dataValidation(
        wb,
        "Coding",
        cols = match(
          .y,
          names(coding_export)
        ),
        rows = rows,
        type = "list",
        value = .x
      )
    )
    
    # public_rel_coded as gate: 0/-1 grays out Topic, Source and Format.
    rel_col <- match(
      "public_rel_coded",
      names(coding_export)
    )
    
    rel_chr <- openxlsx::int2col(rel_col)
    
    for (rule in list(
      list(value = 1,  fill = "#DDEBDD"),
      list(value = 0,  fill = "#E6E6E6"),
      list(value = -1, fill = "#F4E0C7")
    )) {
      
      openxlsx::conditionalFormatting(
        wb,
        "Coding",
        cols = rel_col,
        rows = rows,
        type = "expression",
        rule = paste0(
          "$",
          rel_chr,
          "2=",
          rule$value
        ),
        style = openxlsx::createStyle(
          fgFill = rule$fill
        )
      )
    }
    
    gated_cols <- match(
      c(
        "topic_coded",
        "source_coded",
        "source_name_coded",
        "media_format"
      ),
      names(coding_export)
    )
    
    openxlsx::conditionalFormatting(
      wb,
      "Coding",
      cols = gated_cols,
      rows = rows,
      type = "expression",
      rule = paste0(
        "$",
        rel_chr,
        "2<>1"
      ),
      style = openxlsx::createStyle(
        fgFill = "#EFEFEF",
        fontColour = "#999999"
      )
    )
    
    done_col <- match(
      "coding_completed",
      names(coding_export)
    )
    
    done_chr <- openxlsx::int2col(done_col)
    
    openxlsx::conditionalFormatting(
      wb,
      "Coding",
      cols = done_col,
      rows = rows,
      type = "expression",
      rule = paste0(
        "$",
        done_chr,
        "2=TRUE"
      ),
      style = openxlsx::createStyle(
        fgFill = "#DDEBDD"
      )
    )
  }
  
  
  # Concise instructions directly in relevant headers.
  openxlsx::writeComment(
    wb,
    "Coding",
    match(
      "public_rel_coded",
      names(coding_export)
    ),
    1,
    openxlsx::createComment(
      paste0(
        "1 = publicly relevant\n",
        "0 = not publicly relevant\n",
        "-1 = not assessable\n\n",
        "For 0/-1 leave topic, source and format empty."
      ),
      author = "Codebook"
    )
  )
  
  openxlsx::writeComment(
    wb,
    "Coding",
    match(
      "media_format",
      names(coding_export)
    ),
    1,
    openxlsx::createComment(
      paste0(
        "1 = text/link\n",
        "2 = static visual\n",
        "3 = moving/audiovisual\n",
        "4 = static + moving\n",
        "-1 = not determinable\n\n",
        "Ignore the caption."
      ),
      author = "Codebook"
    )
  )
  
  
  openxlsx::freezePane(
    wb,
    "Coding",
    firstActiveRow = 2,
    firstActiveCol = length(id_cols) + 1
  )
  
  
  widths <- c(
    pretest_order = 12,
    screenshot_id = 18,
    participant = 14,
    study_day = 9,
    photo = 8,
    filename = 28,
    filepath = 42,
    file_exists = 10,
    public_rel_coded = 14,
    topic_coded = 25,
    source_coded = 27,
    source_name_coded = 27,
    platform_coded = 14,
    media_format = 13,
    notes = 32,
    coder = 12,
    coding_completed = 15,
    coding_date = 12,
    topic_participant = 28,
    account_participant = 28,
    platform_reported = 14
  )
  
  purrr::iwalk(
    widths,
    ~ openxlsx::setColWidths(
      wb,
      "Coding",
      match(
        .y,
        names(coding_export)
      ),
      .x
    )
  )
  
  # Analysis information stays in the same sheet but out of the coder's way.
  openxlsx::setColWidths(
    wb,
    "Coding",
    cols = c(
      hidden_idx,
      technical_idx
    ),
    widths = 12,
    hidden = TRUE
  )
  
  openxlsx::setRowHeights(
    wb,
    "Coding",
    rows = 1,
    heights = 34
  )
  
  
  #---------------------------------------------------------------------------
  # Codebook
  #---------------------------------------------------------------------------
  
  openxlsx::addWorksheet(
    wb,
    "Codebook",
    gridLines = FALSE
  )
  
  openxlsx::writeDataTable(
    wb,
    "Codebook",
    codebook,
    tableStyle = "TableStyleMedium2"
  )
  
  openxlsx::freezePane(
    wb,
    "Codebook",
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    "Codebook",
    1:3,
    "auto"
  )
  
  openxlsx::setColWidths(
    wb,
    "Codebook",
    4,
    70
  )
  
  openxlsx::addStyle(
    wb,
    "Codebook",
    openxlsx::createStyle(
      wrapText = TRUE,
      valign = "top"
    ),
    rows = 2:(nrow(codebook) + 1),
    cols = 1:4,
    gridExpand = TRUE
  )
  
  
  #---------------------------------------------------------------------------
  # Quality
  #---------------------------------------------------------------------------
  
  openxlsx::addWorksheet(
    wb,
    "Quality",
    gridLines = FALSE
  )
  
  openxlsx::writeDataTable(
    wb,
    "Quality",
    quality_summary,
    tableStyle = "TableStyleMedium2"
  )
  
  if (nrow(missing_files) > 0) {
    
    start <- nrow(quality_summary) + 4
    
    openxlsx::writeData(
      wb,
      "Quality",
      "Files not found",
      startRow = start
    )
    
    openxlsx::writeDataTable(
      wb,
      "Quality",
      missing_files,
      startRow = start + 1,
      tableStyle = "TableStyleMedium2"
    )
  }
  
  openxlsx::setColWidths(
    wb,
    "Quality",
    1:10,
    "auto"
  )
  
  
  #---------------------------------------------------------------------------
  # Save
  #---------------------------------------------------------------------------
  
  openxlsx::saveWorkbook(
    wb,
    output_excel,
    overwrite = overwrite_existing
  )
  
  invisible(coding_export)
}


#===============================================================================
# 14 Create the four coding sheets
#===============================================================================

for (current_coder in coders) {
  
  create_pretest_workbook(
    common_sample,
    coder_name = current_coder,
    stage_name = "Gemeinsame Kodierung",
    output_excel = file.path(
      common_folder,
      paste0(
        "coding_sheet_",
        current_coder,
        ".xlsx"
      )
    )
  )
  
  create_pretest_workbook(
    reliability_sample,
    coder_name = current_coder,
    stage_name = "Reliabilitäts-Pretest",
    output_excel = file.path(
      reliability_folder,
      paste0(
        "coding_sheet_",
        current_coder,
        ".xlsx"
      )
    )
  )
}


#===============================================================================
# 15 Save sampling manifests
#===============================================================================

manifest_cols <- c(
  "pretest_order",
  "screenshot_id",
  "participant",
  "study_day",
  "photo",
  "platform_reported",
  "filename",
  "filepath",
  "original_filepath"
)

readr::write_csv(
  common_sample %>%
    arrange(pretest_order) %>%
    select(
      all_of(manifest_cols)
    ),
  file.path(
    common_folder,
    "sample_manifest.csv"
  )
)

readr::write_csv(
  reliability_sample %>%
    arrange(pretest_order) %>%
    select(
      all_of(manifest_cols)
    ),
  file.path(
    reliability_folder,
    "sample_manifest.csv"
  )
)


#===============================================================================
# 16 Console summary
#===============================================================================

common_participants <- n_distinct(
  common_sample$participant,
  na.rm = TRUE
)

reliability_participants <- n_distinct(
  reliability_sample$participant,
  na.rm = TRUE
)

overlap_n <- length(
  intersect(
    common_sample$screenshot_id,
    reliability_sample$screenshot_id
  )
)


cat(
  "\nPRETEST PREPARATION COMPLETED\n",
  "========================================\n",
  "Available master screenshots:       ", n_master_total, "\n",
  "Usable sampling pool:               ", n_sampling_pool, "\n",
  "Excluded because file is missing:   ", n_excluded_missing_file, "\n",
  "Excluded because platform invalid:  ", n_excluded_platform, "\n",
  "\n",
  "STAGE 1 – COMMON CODING\n",
  "Screenshots:                        ", nrow(common_sample), "\n",
  "Per platform:                       ", common_n_per_platform, "\n",
  "Participants represented:           ", common_participants, "\n",
  "\n",
  "STAGE 2 – RELIABILITY PRETEST\n",
  "10% / 100 requested before balance: ", reliability_requested_total, "\n",
  "Screenshots after platform balance: ", reliability_n_total, "\n",
  "Per platform:                       ", reliability_n_per_platform, "\n",
  "Participants represented:           ", reliability_participants, "\n",
  "\n",
  "Overlap between stages:             ", overlap_n, "\n",
  "Merge key for final coding sheet:   screenshot_id\n",
  "Coding order:                       Facebook -> Instagram -> TikTok -> X\n",
  "Output folder:                      ", output_root, "\n",
  sep = ""
)


cat(
  "\nPlatform availability before sampling:\n"
)

print(platform_availability)


cat(
  "\nStage 1 distribution:\n"
)

print(
  common_sample %>%
    count(
      platform_reported,
      name = "N"
    )
)


cat(
  "\nStage 2 distribution:\n"
)

print(
  reliability_sample %>%
    count(
      platform_reported,
      name = "N"
    )
)


message("Finished preparing both pretest stages.")
