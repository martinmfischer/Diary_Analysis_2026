################################################################################
# Project: Tagebuchstudie
# File:    07_Reli_Pretest_Ergaenzung_40.R
#
# Purpose:
#   Zieht 40 weitere, bislang in KEINEM Pretest verwendete Screenshots:
#   - 10 Facebook
#   - 10 Instagram
#   - 10 TikTok
#   - 10 X
#
#   Die bisherigen 200 Pretest-Faelle sind direkt im Skript hinterlegt:
#   - 40 aeltere Pretest-Screenshots (als Dateinamen)
#   - 60 aeltere Pretest-Screenshots (als screenshot_id)
#   - 100 tatsaechlich verwendete Screenshots des Reliabilitaets-Pretests
#
#   Alte sample_manifest.csv-Dateien werden NICHT benoetigt.
#   Ausschluss erfolgt robust ueber participant + study_day + photo.
#
# Output:
#   07_Pretest/04_Reliabilitaets_Pretest_Ergaenzung/
#   ├── Screenshots/Facebook|Instagram|TikTok|X/
#   ├── Reli_Pretest_Ergaenzung_40.xlsx
#   └── sample_manifest.csv
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


#===============================================================================
# 02 User settings
#===============================================================================

overwrite_existing <- FALSE

# Originaler Pretest-Seed war 20260818.
# +2 wird fuer die neue, unabhaengige Ergaenzungsziehung verwendet.
sampling_seed <- 20260820L

n_per_platform <- 10L

platform_order <- c(
  "Facebook",
  "Instagram",
  "TikTok",
  "X"
)


#===============================================================================
# 03 Paths
#===============================================================================

data_file <- file.path(
  "01_Data",
  "taeglicher_fragebogen_screenshot_upload.rds"
)

participant_folder <- "05_Participants"

output_folder <- file.path(
  "07_Pretest",
  "04_Reliabilitaets_Pretest_Ergaenzung"
)

screenshots_folder <- file.path(
  output_folder,
  "Screenshots"
)

output_excel <- file.path(
  output_folder,
  "Reli_Pretest_Ergaenzung_40.xlsx"
)

output_manifest <- file.path(
  output_folder,
  "sample_manifest.csv"
)


#===============================================================================
# 04 Harte Ausschlusslisten: alle bisher verwendeten 200 Screenshots
#===============================================================================

# 40 fruehere Pretest-Faelle, in der damals vorliegenden Dateinamenform.
used_files_40 <- c(
  "02hc7dvz_Tag_2_Photo_1.png",
  "4fkmpl0t_Tag_5_Photo_3.jpg",
  "ankdmy5q_Tag_7_Photo_4.jpg",
  "bwd6dx4v_Tag_1_Photo_3.jpg",
  "gdz8g1x0_Tag_1_Photo_4.jpg",
  "h4jncpan_Tag_2_Photo_3.jpg",
  "i307pmik_Tag_4_Photo_2.png",
  "q8squb4l_Tag_4_Photo_2.png",
  "uon8ev51_Tag_2_Photo_1.png",
  "v9r3ewpq_Tag_7_Photo_5.png",
  "38kgvrw0_Tag_4_Photo_2.jpg",
  "5lv2dy2y_Tag_2_Photo_1.jpg",
  "8unf4xdd_Tag_6_Photo_2.jpg",
  "9187xqbn_Tag_4_Photo_3.jpg",
  "jf2ti2kd_Tag_1_Photo_1.png",
  "lstkj97s_Tag_3_Photo_9.png",
  "lx9qbc0c_Tag_7_Photo_10.jpg",
  "mod7cwip_Tag_2_Photo_1.jpg",
  "tfcrp7gt_Tag_6_Photo_5.jpg",
  "uqk7jup4_Tag_4_Photo_2.jpg",
  "0ckf7kww_Tag_2_Photo_8.jpg",
  "0ckf7kww_Tag_4_Photo_3.jpg",
  "aygez65h_Tag_2_Photo_3.jpg",
  "bsffafmo_Tag_3_Photo_7.jpg",
  "bsffafmo_Tag_6_Photo_7.jpg",
  "lvaovr1a_Tag_2_Photo_3.jpg",
  "lvaovr1a_Tag_5_Photo_1.jpg",
  "u7xjqnf7_Tag_4_Photo_5.jpg",
  "u7xjqnf7_Tag_7_Photo_3.jpg",
  "v9r3ewpq_Tag_5_Photo_6.png",
  "1j2qf7sb_Tag_6_Photo_3.jpg",
  "4fkmpl0t_Tag_6_Photo_5.jpg",
  "bsffafmo_Tag_4_Photo_8.jpg",
  "bsffafmo_Tag_5_Photo_5.jpg",
  "it5klml9_Tag_4_Photo_2.jpg",
  "k401d9b5_Tag_4_Photo_2.png",
  "kgfkzwyp_Tag_2_Photo_1.png",
  "mhbm4oja_Tag_6_Photo_5.png",
  "u7kjr0xs_Tag_7_Photo_3.png",
  "y6mmse6u_Tag_3_Photo_5.png"
)

# 60 fruehere Pretest-Faelle.
used_ids_60 <- c(
  "02hc7dvz_D4_P7",
  "4a45imez_D5_P1",
  "pdus4hpc_D4_P1",
  "e9hhromj_D5_P3",
  "ankdmy5q_D7_P5",
  "deexhb69_D4_P6",
  "yb8tcvqe_D7_P5",
  "wqso8u8v_D3_P4",
  "19r0tw6b_D5_P2",
  "6ppuypb2_D6_P1",
  "gv33szdx_D6_P1",
  "tfcrp7gt_D6_P2",
  "u7xjqnf7_D5_P1",
  "p03zl58f_D2_P3",
  "lvaovr1a_D4_P3",
  "h5v5lt6o_D3_P3",
  "n6umsvm5_D2_P1",
  "dbkhhmyl_D2_P1",
  "0ckf7kww_D6_P5",
  "bw0qd30f_D6_P8",
  "on9iokv6_D2_P1",
  "t2mz057i_D4_P4",
  "lvaovr1a_D6_P3",
  "e3elj562_D3_P4",
  "u7xjqnf7_D1_P4",
  "it5klml9_D2_P4",
  "4uc0fpd9_D6_P1",
  "wqso8u8v_D4_P3",
  "mvlgg33x_D1_P1",
  "a1ud7rld_D5_P1",
  "zccekn3q_D5_P4",
  "aygez65h_D1_P1",
  "lvaovr1a_D2_P1",
  "u7xjqnf7_D3_P1",
  "aygez65h_D4_P3",
  "v9r3ewpq_D7_P2",
  "0ckf7kww_D3_P1",
  "bsffafmo_D4_P9",
  "it5klml9_D3_P4",
  "0ckf7kww_D2_P9",
  "u7kjr0xs_D3_P1",
  "zccekn3q_D5_P1",
  "bsffafmo_D3_P9",
  "u7kjr0xs_D3_P8",
  "v9r3ewpq_D5_P5",
  "1j2qf7sb_D4_P5",
  "n91q8dxe_D7_P3",
  "it5klml9_D6_P4",
  "k401d9b5_D6_P1",
  "y6mmse6u_D2_P4",
  "v9r3ewpq_D3_P2",
  "bsffafmo_D6_P5",
  "4fkmpl0t_D7_P2",
  "wrogy83s_D5_P10",
  "wrogy83s_D3_P10",
  "v9r3ewpq_D3_P1",
  "kgfkzwyp_D5_P1",
  "4fkmpl0t_D3_P6",
  "mhbm4oja_D5_P10",
  "u7kjr0xs_D5_P10"
)

# 100 TATSAECHLICH im Reliabilitaets-Pretest verwendete Faelle.
# Direkt aus dem durchgefuehrten Coding-Sheet uebernommen; keine Rekonstruktion
# ueber Seeds oder alte Manifest-Dateien erforderlich.
used_ids_reli_100 <- c(
  "0w6mdaqz_D7_P1",
  "1o0hlcpt_D2_P3",
  "1px0s1da_D6_P8",
  "3gp14hmn_D6_P3",
  "4a45imez_D4_P4",
  "4fkmpl0t_D4_P2",
  "7pqnfmrv_D4_P6",
  "9187xqbn_D6_P5",
  "ankdmy5q_D1_P5",
  "ckkc7lj5_D5_P1",
  "cxeqy3f5_D7_P1",
  "gxgszmi4_D5_P1",
  "h6csyig3_D5_P4",
  "it5klml9_D4_P3",
  "k401d9b5_D1_P1",
  "ounnv3ed_D7_P2",
  "p74ygvvz_D3_P2",
  "pdus4hpc_D2_P1",
  "pp3eahpm_D4_P2",
  "t4qz0p5t_D7_P2",
  "uop0erjb_D5_P3",
  "v9r3ewpq_D6_P4",
  "yb8tcvqe_D3_P1",
  "zqh6nr20_D3_P1",
  "zyfro6j7_D5_P3",
  "0ckf7kww_D6_P7",
  "0w6mdaqz_D6_P4",
  "19r0tw6b_D4_P4",
  "1px0s1da_D3_P4",
  "27ynjsrg_D2_P5",
  "373cy2x4_D2_P8",
  "d58yx6nv_D5_P1",
  "du94csba_D2_P7",
  "e3elj562_D3_P3",
  "gdz8g1x0_D3_P3",
  "h5v5lt6o_D6_P3",
  "idcxwlrz_D1_P5",
  "it5klml9_D2_P2",
  "jf2ti2kd_D4_P1",
  "jtih5moz_D4_P7",
  "lcs3oz76_D1_P8",
  "lstkj97s_D2_P7",
  "lyj7657n_D3_P3",
  "mkuybnhn_D6_P4",
  "n6umsvm5_D5_P1",
  "t2mz057i_D3_P1",
  "u7xjqnf7_D2_P3",
  "uqk7jup4_D3_P2",
  "v9r3ewpq_D5_P4",
  "xz4x508z_D4_P5",
  "0ckf7kww_D2_P4",
  "0ckf7kww_D5_P1",
  "0ckf7kww_D5_P2",
  "0ckf7kww_D6_P1",
  "0ckf7kww_D7_P2",
  "aygez65h_D4_P4",
  "bsffafmo_D5_P7",
  "bsffafmo_D5_P8",
  "bsffafmo_D6_P8",
  "bsffafmo_D7_P7",
  "bsffafmo_D7_P8",
  "lvaovr1a_D2_P2",
  "lvaovr1a_D3_P4",
  "lvaovr1a_D5_P2",
  "lvaovr1a_D5_P3",
  "lvaovr1a_D5_P4",
  "u7xjqnf7_D3_P2",
  "u7xjqnf7_D4_P4",
  "u7xjqnf7_D6_P1",
  "u7xjqnf7_D6_P2",
  "v9r3ewpq_D1_P1",
  "v9r3ewpq_D2_P1",
  "v9r3ewpq_D2_P2",
  "v9r3ewpq_D4_P5",
  "v9r3ewpq_D4_P7",
  "1j2qf7sb_D7_P1",
  "1j2qf7sb_D7_P2",
  "1j2qf7sb_D7_P4",
  "4fkmpl0t_D6_P3",
  "bsffafmo_D2_P1",
  "bsffafmo_D4_P7",
  "bsffafmo_D7_P5",
  "it5klml9_D2_P3",
  "it5klml9_D3_P5",
  "it5klml9_D5_P4",
  "k401d9b5_D2_P1",
  "k401d9b5_D5_P1",
  "k401d9b5_D5_P4",
  "kgfkzwyp_D2_P4",
  "kgfkzwyp_D3_P1",
  "kgfkzwyp_D7_P2",
  "mhbm4oja_D1_P3",
  "mhbm4oja_D5_P5",
  "mhbm4oja_D5_P9",
  "u7kjr0xs_D4_P2",
  "u7kjr0xs_D5_P1",
  "u7kjr0xs_D7_P1",
  "y6mmse6u_D4_P6",
  "y6mmse6u_D5_P6",
  "y6mmse6u_D7_P10"
)

if (
  length(used_files_40) != 40L ||
  length(used_ids_60) != 60L ||
  length(used_ids_reli_100) != 100L
) {
  stop("Interner Fehler: Eine historische Ausschlussliste hat die falsche Laenge.")
}


#-------------------------------------------------------------------------------
# Historische IDs auf participant + study_day + photo vereinheitlichen
#-------------------------------------------------------------------------------

used_cases_40 <- tibble(
  historical_id = used_files_40
) %>%
  tidyr::extract(
    historical_id,
    into = c("participant", "study_day", "photo"),
    regex = "^(.*)_Tag_([0-9]+)_Photo_([0-9]+)\\.[^.]+$",
    remove = FALSE,
    convert = TRUE
  )

parse_dp_ids <- function(x) {
  
  tibble(
    historical_id = x
  ) %>%
    tidyr::extract(
      historical_id,
      into = c("participant", "study_day", "photo"),
      regex = "^(.*)_D([0-9]+)_P([0-9]+)$",
      remove = FALSE,
      convert = TRUE
    )
}

used_cases <- bind_rows(
  used_cases_40,
  parse_dp_ids(used_ids_60),
  parse_dp_ids(used_ids_reli_100)
) %>%
  transmute(
    participant = as.character(participant),
    study_day = as.integer(study_day),
    photo = as.integer(photo)
  )

if (anyNA(used_cases)) {
  stop("Mindestens eine historische Ausschluss-ID konnte nicht geparst werden.")
}

if (nrow(used_cases) != 200L) {
  stop("Interner Fehler: Es wurden nicht exakt 200 historische Faelle erzeugt.")
}

if (anyDuplicated(used_cases) > 0) {
  stop(
    "Interner Fehler: Die historischen Ausschlusslisten enthalten ",
    "ueberlappende oder doppelte Faelle."
  )
}


#===============================================================================
# 05 Guard rails
#===============================================================================

if (!file.exists(data_file)) {
  stop("Daily-RDS nicht gefunden: ", data_file)
}

if (fs::dir_exists(output_folder)) {
  
  if (!overwrite_existing) {
    stop(
      "Der Ausgabeordner existiert bereits und wird nicht ueberschrieben:\n  ",
      output_folder,
      "\n\nFuer einen bewussten Neuaufbau overwrite_existing <- TRUE setzen."
    )
  }
  
  fs::dir_delete(output_folder)
}

fs::dir_create(output_folder)
fs::dir_create(screenshots_folder)


#===============================================================================
# 06 Kleine Helper
#===============================================================================

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

label_code <- function(x, labels) {
  
  dplyr::recode(
    as.character(x),
    !!!labels,
    .default = NA_character_,
    .missing = NA_character_
  )
}

collapse_interactions <- function(read, research, engagement) {
  
  x <- c(
    if (!is.na(read) && read == 1) "Read/watched thoroughly",
    if (!is.na(research) && research == 1) "Sought further information",
    if (!is.na(engagement) && engagement == 1) "Engaged with the post"
  )
  
  if (length(x) == 0) {
    NA_character_
  } else {
    paste(x, collapse = "; ")
  }
}


#===============================================================================
# 07 Gleiche Sampling-Logik wie im bisherigen Pretest
#===============================================================================

sample_participant_diverse <- function(data, n) {
  
  if (n <= 0) {
    return(data[0, , drop = FALSE])
  }
  
  if (nrow(data) < n) {
    stop(
      "Zu wenige Faelle in einem Plattform-Stratum: benoetigt ",
      n,
      ", vorhanden ",
      nrow(data),
      "."
    )
  }
  
  participant_order <- data %>%
    distinct(participant) %>%
    mutate(participant_random = runif(n()))
  
  data %>%
    mutate(row_random = runif(n())) %>%
    group_by(participant) %>%
    arrange(row_random, .by_group = TRUE) %>%
    mutate(within_participant_order = row_number()) %>%
    ungroup() %>%
    left_join(participant_order, by = "participant") %>%
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

sample_equal_platforms <- function(data, n_per_platform, seed) {
  
  set.seed(seed)
  
  availability <- data %>%
    count(platform_reported, name = "N_available") %>%
    tidyr::complete(
      platform_reported = platform_order,
      fill = list(N_available = 0L)
    )
  
  insufficient <- availability %>%
    filter(N_available < n_per_platform)
  
  if (nrow(insufficient) > 0) {
    stop(
      "Nicht genuegend ungenutzte Screenshots fuer 10 pro Plattform:\n",
      paste0(
        insufficient$platform_reported,
        ": benoetigt ",
        n_per_platform,
        ", vorhanden ",
        insufficient$N_available,
        collapse = "\n"
      )
    )
  }
  
  purrr::map_dfr(
    platform_order,
    function(current_platform) {
      
      data %>%
        filter(platform_reported == current_platform) %>%
        sample_participant_diverse(n_per_platform)
    }
  ) %>%
    mutate(
      platform_sort = match(platform_reported, platform_order)
    ) %>%
    arrange(
      platform_sort,
      screenshot_id
    ) %>%
    select(-platform_sort)
}


#===============================================================================
# 08 Daily data -> master coding data
#===============================================================================

daily <- readRDS(data_file)

coding_master <- derive_screenshot_index(
  daily,
  participant_folder = participant_folder
)

required_master_cols <- c(
  "screenshot_id",
  "participant",
  "study_day",
  "photo",
  "filename",
  "filepath",
  "platform"
)

missing_master_cols <- setdiff(
  required_master_cols,
  names(coding_master)
)

if (length(missing_master_cols) > 0) {
  stop(
    "derive_screenshot_index() liefert nicht alle benoetigten Spalten: ",
    paste(missing_master_cols, collapse = ", ")
  )
}

# Felder, die fuer die kopierbare Coding-Tabelle gebraucht werden.
expected_fields <- c(
  "topic",
  "account",
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

optional_master_cols <- c(
  "original_filename",
  "screenshot_slot",
  "submission_row",
  "scheduled",
  "committed"
)

for (x in setdiff(optional_master_cols, names(coding_master))) {
  coding_master[[x]] <- NA
}

coding_master <- coding_master %>%
  mutate(
    participant = as.character(participant),
    study_day = as.integer(study_day),
    photo = as.integer(photo),
    
    topic_participant = clean_text(topic),
    account_participant = clean_text(account),
    
    platform_code = na_if(clean_numeric(platform), -1),
    incidentality_code = na_if(clean_numeric(incidentality), -1),
    locality_code = na_if(clean_numeric(locality), -1),
    situation_code = na_if(clean_numeric(situation), -1),
    
    interaction_read_code = clean_numeric(interaction_1),
    interaction_research_code = clean_numeric(interaction_2),
    interaction_engagement_code = clean_numeric(interaction_3),
    
    platform_reported = label_code(platform_code, platform_labels),
    incidentality_label = label_code(incidentality_code, incidentality_labels),
    locality_label = label_code(locality_code, locality_labels),
    situation_label = label_code(situation_code, situation_labels),
    
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
      str_to_lower(clean_text(startstop_raw)) %in% c("true", "t", "1") ~ "Weiter",
      str_to_lower(clean_text(startstop_raw)) %in% c("false", "f", "0") ~ "Stopp",
      TRUE ~ NA_character_
    ),
    
    original_filepath = filepath,
    file_exists = fs::file_exists(original_filepath)
  )


#===============================================================================
# 09 Alle 200 alten Faelle ausschliessen
#===============================================================================

sampling_pool <- coding_master %>%
  filter(
    file_exists %in% TRUE,
    platform_reported %in% platform_order
  ) %>%
  anti_join(
    used_cases,
    by = c("participant", "study_day", "photo")
  )

if (nrow(sampling_pool) == 0) {
  stop("Nach Ausschluss der 200 bisherigen Pretest-Faelle bleibt kein Pool uebrig.")
}

platform_availability <- sampling_pool %>%
  count(platform_reported, name = "N_available") %>%
  tidyr::complete(
    platform_reported = platform_order,
    fill = list(N_available = 0L)
  ) %>%
  arrange(match(platform_reported, platform_order))

print(platform_availability)


#===============================================================================
# 10 40 neue Faelle ziehen: exakt 10 je Plattform
#===============================================================================

additional_sample <- sample_equal_platforms(
  sampling_pool,
  n_per_platform = n_per_platform,
  seed = sampling_seed
) %>%
  mutate(
    # Die bestehende Reli-Tabelle hat 100 Zeilen.
    pretest_order = 100L + row_number(),
    pretest_stage = "Reliabilitaets-Pretest Ergaenzung"
  )


#===============================================================================
# 11 Sicherheitschecks
#===============================================================================

if (nrow(additional_sample) != 40L) {
  stop("Interner Fehler: Ergaenzungssample enthaelt nicht exakt 40 Faelle.")
}

platform_check <- additional_sample %>%
  count(platform_reported)

if (
  nrow(platform_check) != 4L ||
  any(platform_check$n != 10L)
) {
  stop("Interner Fehler: Ergaenzungssample enthaelt nicht exakt 10 Faelle pro Plattform.")
}

if (anyDuplicated(additional_sample$screenshot_id) > 0) {
  stop("Interner Fehler: Doppelte screenshot_id im Ergaenzungssample.")
}

overlap_used <- additional_sample %>%
  semi_join(
    used_cases,
    by = c("participant", "study_day", "photo")
  )

if (nrow(overlap_used) > 0) {
  stop(
    "SICHERHEITSSTOPP: Mindestens ein gezogener Fall wurde bereits ",
    "in einem frueheren Pretest verwendet."
  )
}


#===============================================================================
# 12 Screenshots kopieren
#===============================================================================

additional_sample$filepath <- NA_character_

for (i in seq_len(nrow(additional_sample))) {
  
  current_platform <- additional_sample$platform_reported[i]
  
  destination_folder <- file.path(
    screenshots_folder,
    current_platform
  )
  
  fs::dir_create(destination_folder)
  
  source_file <- additional_sample$original_filepath[i]
  
  destination_file <- file.path(
    destination_folder,
    additional_sample$filename[i]
  )
  
  if (!fs::file_exists(source_file)) {
    stop("Quelldatei beim Kopieren nicht gefunden: ", source_file)
  }
  
  fs::file_copy(
    source_file,
    destination_file,
    overwrite = FALSE
  )
  
  additional_sample$filepath[i] <- destination_file
}

additional_sample <- additional_sample %>%
  mutate(
    file_exists = fs::file_exists(filepath)
  )

if (!all(additional_sample$file_exists %in% TRUE)) {
  stop("Mindestens eine Ergaenzungsdatei wurde nicht korrekt kopiert.")
}


#===============================================================================
# 13 Kopierbare Tabelle in derselben Struktur wie das Coding-Sheet
#===============================================================================

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
  "advertisement_coded",
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

additional_table <- additional_sample %>%
  mutate(
    public_rel_coded = NA_integer_,
    advertisement_coded = NA_integer_,
    topic_coded = NA_character_,
    source_coded = NA_character_,
    source_name_coded = NA_character_,
    platform_coded = platform_reported,
    media_format = NA_integer_,
    notes = NA_character_,
    coder = NA_character_,
    coding_completed = FALSE,
    coding_date = as.Date(NA)
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


#===============================================================================
# 14 Output
#===============================================================================

openxlsx::write.xlsx(
  list(
    Coding = additional_table
  ),
  file = output_excel,
  overwrite = FALSE
)

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
  additional_sample %>%
    select(all_of(manifest_cols)),
  output_manifest
)


#===============================================================================
# 15 Finale Kontrolle
#===============================================================================

cat(
  "\nRELI-PRETEST ERGAENZUNG COMPLETED\n",
  "========================================\n",
  "Historisch ausgeschlossen: 200\n",
  "Neue Screenshots gesamt:   ", nrow(additional_sample), "\n",
  "Facebook:                  ", sum(additional_sample$platform_reported == "Facebook"), "\n",
  "Instagram:                 ", sum(additional_sample$platform_reported == "Instagram"), "\n",
  "TikTok:                    ", sum(additional_sample$platform_reported == "TikTok"), "\n",
  "X:                         ", sum(additional_sample$platform_reported == "X"), "\n",
  "Ueberschneidung alt:       ", nrow(overlap_used), "\n",
  "Excel:                     ", output_excel, "\n",
  sep = ""
)

message(
  "Safety check passed: alle 40 neuen Faelle sind disjunkt zu den 200 bisherigen."
)
