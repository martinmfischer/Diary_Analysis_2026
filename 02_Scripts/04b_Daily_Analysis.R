################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    04b_Daily_Analysis.R
# Simulation: optional via 02_Scripts/03_Simulate_Daily_Coding.R
# Purpose: Kompakte Hauptanalyse der 7-Tage-Diary-Daten.
#
# Inputs:
#   06_Coding/coding_sheet.xlsx  (Sheet "Coding")
#   03_Output/screening_prepared.rds
#
# Outputs:
#   03_Output/Daily_Results.xlsx
#   03_Output/daily_screenshot_level.rds
#   03_Output/daily_participant_level.rds
#   optional: 04_Figures/Daily_*.png
#
# Analyselogik:
#   - Einschluss: Screening-Match + mindestens 7 hochgeladene Screenshots.
#   - public_rel_coded = 1: inhaltlich analysieren.
#   - public_rel_coded = 0: nicht öffentlich relevant; keine Inhaltscodierung.
#   - public_rel_coded = -1: nicht beurteilbar; aus Relevanzquote/Inhaltsanalyse raus.
#   - Hauptanalysen beschreiben Thema, Quelle, Format, Plattform, Incidentality,
#     Nutzungskontext und Verarbeitung.
#   - Explorationen fokussieren Screening–Diary-Bezüge, Informationsbedürfnisse,
#     Neuheit/Serendipität, Plattformunterschiede und mögliche Tageseffekte.
################################################################################

rm(list = ls())


#===============================================================================
# 01 Settings
#===============================================================================

# Diese Einstellungen steuern nur Stichprobenbildung und Output; die finale
# Analyse erwartet ein vollständig bearbeitetes Coding Sheet.
minimum_screenshots <- 7
require_screening_match <- TRUE
strict_coding_check <- TRUE
create_figures <- TRUE
overwrite_outputs <- TRUE
expected_study_days <- 1:7
coding_sheet_name <- "Coding"

# Optional pipeline test: Das eigentliche Simulationsverfahren liegt bewusst in
# einem separaten Source-Modul. Reale Coding-Werte werden standardmäßig erhalten.
simulate_coding <- TRUE
simulation_seed <- 20260810
simulation_overwrite_existing <- FALSE
simulation_use_screening_patterns <- TRUE


#===============================================================================
# 02 Packages, helpers and paths
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, fs)

helper_script <- file.path("02_Scripts", "00_Helpers.R")
simulation_script <- file.path("02_Scripts", "03_Simulate_Daily_Coding.R")
coding_file <- file.path("06_Coding", "coding_sheet.xlsx")
screening_file <- file.path("03_Output", "screening_prepared.rds")
output_folder <- "03_Output"

# Simulation (falls aktiv) fuellt das Coding nur im Speicher und schreibt in die
# regulaeren Output-Pfade. Bei simulate_coding = TRUE enthalten die Ergebnisse
# folglich Testdaten; fuer die reale Auswertung simulate_coding = FALSE setzen.
output_excel <- file.path(output_folder, "Daily_Results.xlsx")
output_screenshot_rds <- file.path(output_folder, "daily_screenshot_level.rds")
output_participant_rds <- file.path(output_folder, "daily_participant_level.rds")
figure_folder <- "04_Figures"
tables_folder <- file.path(output_folder, "Tables")

if (!file.exists(helper_script)) stop("Helper-Script nicht gefunden: ", helper_script)
source(helper_script)

if (simulate_coding && !file.exists(simulation_script)) {
  stop("Simulationsmodul nicht gefunden: ", simulation_script)
}
if (!file.exists(coding_file)) stop("Coding Sheet nicht gefunden: ", coding_file)
if (!file.exists(screening_file)) stop("Screening-RDS nicht gefunden: ", screening_file)

fs::dir_create(output_folder)
fs::dir_create(figure_folder)
fs::dir_create(tables_folder)

if (!overwrite_outputs && any(file.exists(c(
  output_excel, output_screenshot_rds, output_participant_rds
)))) {
  stop("Mindestens eine Output-Datei existiert bereits.")
}


#===============================================================================
# 03 Category systems
#===============================================================================

# Die Listen entsprechen dem Coding-Schema und dienen zugleich der Validierung.
topic_levels <- c(
  "Politics, government & elections",
  "International affairs, war & security",
  "Economy, labor, finance & consumers",
  "Society, social affairs, migration & religion",
  "Education, science & technology",
  "Health & care",
  "Climate, environment & energy",
  "Crime & justice",
  "Transport, infrastructure & housing",
  "Weather & natural events",
  "Culture, media & entertainment",
  "History & remembrance",
  "Sports",
  "Events & public service",
  "Other / not clear"
)

source_levels <- c(
  "Journalistic media outlet",
  "Alternative or partisan media outlet",
  "Party or politician",
  "State or public institution",
  "NGO, association, or movement",
  "Science, expert, or fact-check",
  "Company or brand",
  "Journalist, creator, influencer, or public figure",
  "Private person / peer",
  "Collective, meme, satire, or aggregator page",
  "Other / source not identifiable"
)

platform_levels <- c("Facebook", "Instagram", "TikTok", "X")

format_labels <- c(
  `1` = "Text/link-based",
  `2` = "Static visual format",
  `3` = "Moving audiovisual format",
  `4` = "Mixed media format",
  `-1` = "Not determinable"
)

incidentality_levels <- c(
  "Searched deliberately",
  "Followed, not deliberately sought",
  "Encountered by chance"
)


#===============================================================================
# 04 Load and validate inputs
#===============================================================================

# Nur strukturell notwendige Checks bleiben erhalten. Das Coding Sheet wird vom
# vorgelagerten Skript erzeugt; deshalb werden keine alten Alias-Systeme gepflegt.
screening <- readRDS(screening_file)
coding <- openxlsx::read.xlsx(
  coding_file,
  sheet = coding_sheet_name,
  detectDates = TRUE,
  check.names = FALSE
)

screening <- rename_first_available(
  screening,
  target = "participant",
  candidates = c("personalParticipantCode", "personal_participant_code"),
  required = TRUE
)

required_coding <- c(
  "participant", "screenshot_id", "study_day", "filename", "file_exists",
  "public_rel_coded", "topic_coded", "source_coded", "source_name_coded",
  "platform_coded", "media_format", "coding_completed", "platform_reported",
  "incidentality_code", "interaction_read_code", "interaction_research_code",
  "interaction_engagement_code", "locality_code", "situation_code"
)

missing_coding <- setdiff(required_coding, names(coding))
if (length(missing_coding) > 0) {
  stop("Im Coding Sheet fehlen: ", paste(missing_coding, collapse = ", "))
}

screening <- screening %>%
  mutate(participant = clean_text(participant)) %>%
  filter(!is.na(participant))

if (anyDuplicated(screening$participant)) {
  stop("screening_prepared.rds enthält doppelte Participant Codes.")
}

# Fehlende optionale Screening-Merkmale verhindern keine Diary-Deskription;
# entsprechende integrierte Explorationen bleiben dann einfach leer.
screening_optional <- c(
  "intro_age_num", "gender", "education_three_level", "age_group",
  "intro_intensity", "intro_ib_undirected", "intro_ib_thematic",
  "intro_ib_social", "intro_ib_problem", "incidentality_index",
  "context_local", "context_social", "intro_freq_facebook",
  "intro_freq_instagram", "intro_freq_tiktok", "intro_freq_x",
  "N_Platforms_Weekly", "Platform_Repertoire", "Primary_Platform"
)

for (variable_name in screening_optional) {
  if (!variable_name %in% names(screening)) screening[[variable_name]] <- NA
}

screening_selected <- screening %>%
  select(participant, all_of(screening_optional)) %>%
  mutate(
    intro_age_z = safe_z(intro_age_num),
    intro_intensity_z = safe_z(intro_intensity),
    incidentality_index_z = safe_z(incidentality_index),
    intro_ib_undirected_z = safe_z(intro_ib_undirected),
    intro_ib_thematic_z = safe_z(intro_ib_thematic),
    intro_ib_social_z = safe_z(intro_ib_social),
    intro_ib_problem_z = safe_z(intro_ib_problem)
  )


#-------------------------------------------------------------------------------
# Optional simulation
#-------------------------------------------------------------------------------

# Für Pipeline-Tests füllt das separate Modul ausschließlich die manuellen
# Coding-Spalten. Die realen Diary-Angaben zu Incidentality, Kontext und
# Interaktionen bleiben unverändert. Das Coding Sheet auf der Festplatte wird
# nicht überschrieben; simuliert wird nur das in-memory Objekt `coding`.
if (simulate_coding) {
  # Das Modul arbeitet direkt auf dem bereits geladenen Objekt `coding` und darf
  # deshalb erst an dieser Stelle ausgeführt werden. `local = environment()` hält
  # alle simulierten Objekte im Environment dieses Analyse-Skripts.
  source(simulation_script, local = environment())
  
  # Harte Rückmeldung, falls versehentlich ein falsches/älteres Modul eingebunden
  # wurde. Das aktuelle Modul setzt diese Kennzeichnung nach erfolgreichem Lauf.
  if (!"coding_was_simulated" %in% names(coding)) {
    stop("Simulationsmodul wurde gesourct, hat `coding` aber nicht simuliert.")
  }
}


#===============================================================================
# 05 Prepare coding data
#===============================================================================

# Die technischen Codes aus dem Coding Sheet werden direkt genutzt. Das macht
# die Aufbereitung deutlich eindeutiger als eine nachträgliche Text-Erkennung.
coding <- coding %>%
  mutate(
    participant = clean_text(participant),
    screenshot_id = clean_text(screenshot_id),
    filename = clean_text(filename),
    study_day = parse_study_day(study_day),
    public_rel_coded = as.integer(clean_numeric(public_rel_coded)),
    topic_coded = clean_text(topic_coded),
    source_coded = clean_text(source_coded),
    source_name_coded = clean_text(source_name_coded),
    platform_reported = clean_text(platform_reported),
    platform_coded = coalesce(clean_text(platform_coded), platform_reported),
    media_format_code = as.integer(clean_numeric(media_format)),
    media_format_display = unname(format_labels[as.character(media_format_code)]),
    media_format = factor(
      if_else(media_format_code %in% 1:4, media_format_display, NA_character_),
      levels = unname(format_labels[c("1", "2", "3", "4")])
    ),
    incidentality_code = clean_numeric(incidentality_code),
    incidentality = case_when(
      incidentality_code == 1 ~ "Searched deliberately",
      incidentality_code == 2 ~ "Followed, not deliberately sought",
      incidentality_code == 3 ~ "Encountered by chance",
      TRUE ~ NA_character_
    ),
    interaction_read = clean_binary(interaction_read_code),
    interaction_research = clean_binary(interaction_research_code),
    interaction_engagement = clean_binary(interaction_engagement_code),
    locality = case_when(
      clean_numeric(locality_code) == 1 ~ "At home",
      clean_numeric(locality_code) == 2 ~ "Out and about",
      clean_numeric(locality_code) == 3 ~ "Don't know",
      TRUE ~ NA_character_
    ),
    situation = case_when(
      clean_numeric(situation_code) == 1 ~ "Alone",
      clean_numeric(situation_code) == 2 ~ "With someone else",
      clean_numeric(situation_code) == 3 ~ "Don't know",
      TRUE ~ NA_character_
    ),
    coding_completed_binary = as_logical_safe(coding_completed),
    file_exists_binary = as_logical_safe(file_exists),
    platform = factor(platform_coded, levels = platform_levels),
    incidentality = factor(incidentality, levels = incidentality_levels, ordered = TRUE),
    public_relevance = case_when(
      public_rel_coded == 1L ~ 1L,
      public_rel_coded == 0L ~ 0L,
      TRUE ~ NA_integer_
    )
  )


#===============================================================================
# 06 Minimal coding-quality checks
#===============================================================================

# Gestoppt wird nur bei Fehlern, die die Analyse tatsächlich unklar machen.
missing_ids <- sum(is.na(coding$participant) | is.na(coding$screenshot_id))
duplicate_ids <- sum(duplicated(coding$screenshot_id[!is.na(coding$screenshot_id)]))
invalid_public <- sum(
  is.na(coding$public_rel_coded) | !coding$public_rel_coded %in% c(-1L, 0L, 1L)
)
invalid_platform <- sum(is.na(coding$platform))
invalid_topic <- sum(
  coding$public_rel_coded == 1L &
    (is.na(coding$topic_coded) | !coding$topic_coded %in% topic_levels),
  na.rm = TRUE
)
invalid_source <- sum(
  coding$public_rel_coded == 1L &
    (is.na(coding$source_coded) | !coding$source_coded %in% source_levels),
  na.rm = TRUE
)
invalid_format <- sum(
  coding$public_rel_coded == 1L &
    (is.na(coding$media_format_code) | !coding$media_format_code %in% c(-1L, 1:4)),
  na.rm = TRUE
)
incomplete_rows <- sum(
  is.na(coding$coding_completed_binary) | !coding$coding_completed_binary
)

if (missing_ids > 0) stop("Coding Sheet enthält Zeilen ohne Participant-/Screenshot-ID.")
if (duplicate_ids > 0) stop("Coding Sheet enthält doppelte screenshot_id-Werte.")

if (strict_coding_check && any(c(
  invalid_public, invalid_platform, invalid_topic, invalid_source,
  invalid_format, incomplete_rows
) > 0)) {
  stop(
    "Coding unvollständig/ungültig: public_rel=", invalid_public,
    ", platform=", invalid_platform,
    ", topic=", invalid_topic,
    ", source=", invalid_source,
    ", format=", invalid_format,
    ", nicht abgeschlossen=", incomplete_rows, "."
  )
}

study_day_issues <- sum(
  is.na(coding$study_day) | !coding$study_day %in% expected_study_days
)
missing_files <- sum(coding$file_exists_binary %in% FALSE, na.rm = TRUE)
platform_mismatches <- sum(
  !is.na(coding$platform_reported) &
    !is.na(coding$platform_coded) &
    coding$platform_reported != coding$platform_coded
)

if (study_day_issues > 0) warning(study_day_issues, " Zeilen mit Studientag außerhalb 1–7.")
if (missing_files > 0) warning(missing_files, " Screenshot-Dateien wurden am erwarteten Ort nicht gefunden.")


#===============================================================================
# 07 Build analysis sample
#===============================================================================

# Der Einschluss basiert auf allen Uploads. Öffentliche Relevanz entscheidet erst
# danach, welche Beiträge in die Inhaltsanalyse eingehen.
participant_pre <- coding %>%
  filter(!is.na(participant)) %>%
  group_by(participant) %>%
  summarise(
    N_Screenshots = n(),
    At_Least_Minimum = N_Screenshots >= minimum_screenshots,
    Screening_Available = first(participant) %in% screening$participant,
    .groups = "drop"
  )

eligible_ids <- participant_pre %>%
  filter(At_Least_Minimum) %>%
  { if (require_screening_match) filter(., Screening_Available) else . } %>%
  pull(participant)

if (length(eligible_ids) == 0) stop("Nach den Einschlusskriterien verbleiben keine Teilnehmenden.")

exclusion_summary <- participant_pre %>%
  mutate(
    Reason = case_when(
      !At_Least_Minimum ~ paste0("< ", minimum_screenshots, " Screenshots"),
      require_screening_match & !Screening_Available ~ "Kein Screening-Match",
      TRUE ~ "Eingeschlossen"
    )
  ) %>%
  count(Reason, name = "N")

# Auf Screenshot-Ebene werden Incidentality und Verarbeitung einmal zentral
# abgeleitet und anschließend sowohl deskriptiv als auch integriert verwendet.
daily_all <- coding %>%
  filter(participant %in% eligible_ids) %>%
  left_join(screening_selected, by = "participant") %>%
  mutate(
    incidental_strict = case_when(
      is.na(incidentality) ~ NA_integer_,
      incidentality == "Encountered by chance" ~ 1L,
      TRUE ~ 0L
    ),
    incidental_broad = case_when(
      is.na(incidentality) ~ NA_integer_,
      incidentality %in% c("Followed, not deliberately sought", "Encountered by chance") ~ 1L,
      TRUE ~ 0L
    ),
    targeted_exposure = case_when(
      is.na(incidentality) ~ NA_integer_,
      incidentality == "Searched deliberately" ~ 1L,
      TRUE ~ 0L
    ),
    interaction_count = if_else(
      is.na(interaction_read) & is.na(interaction_research) & is.na(interaction_engagement),
      NA_integer_,
      rowSums(cbind(interaction_read, interaction_research, interaction_engagement), na.rm = TRUE)
    ),
    interaction_any = case_when(
      is.na(interaction_count) ~ NA_integer_,
      interaction_count > 0 ~ 1L,
      TRUE ~ 0L
    ),
    platform_matches_report = case_when(
      is.na(platform_reported) | is.na(platform_coded) ~ NA_integer_,
      platform_reported == platform_coded ~ 1L,
      TRUE ~ 0L
    )
  )


#===============================================================================
# 08 Public-content variables and theory-building indicators
#===============================================================================

# Nur öffentlich relevante Beiträge werden thematisch analysiert. Die Makros
# bündeln Themen/Quellen für sparsame, theoretisch gerichtete Explorationen.
daily <- daily_all %>%
  filter(public_relevance == 1L, coding_completed_binary %in% TRUE) %>%
  mutate(
    topic_coded = factor(topic_coded, levels = topic_levels),
    source_coded = factor(source_coded, levels = source_levels),
    topic_macro = case_when(
      as.character(topic_coded) %in% topic_levels[c(1, 2, 3, 4, 7, 8)] ~
        "Current affairs & public issues",
      as.character(topic_coded) %in% topic_levels[c(6, 9, 10, 14)] ~
        "Practical information & service",
      as.character(topic_coded) %in% topic_levels[c(5, 11, 12, 13)] ~
        "Knowledge, interests & culture",
      TRUE ~ "Other / not clear"
    ),
    source_macro = case_when(
      as.character(source_coded) == source_levels[1] ~ "Journalistic media",
      as.character(source_coded) %in% source_levels[c(2, 10)] ~ "Alternative/aggregated media",
      as.character(source_coded) %in% source_levels[c(3, 4)] ~ "Politics & public institutions",
      as.character(source_coded) %in% source_levels[c(5, 6)] ~ "Civil society & expertise",
      as.character(source_coded) %in% source_levels[c(7, 8)] ~ "Commercial/public-figure accounts",
      as.character(source_coded) == source_levels[9] ~ "Private person / peer",
      TRUE ~ "Other / not identifiable"
    ),
    need_domain = case_when(
      source_macro == "Private person / peer" ~ "Social information need",
      topic_macro == "Current affairs & public issues" ~ "Undirected information need",
      topic_macro == "Practical information & service" ~ "Problem-related information need",
      topic_macro == "Knowledge, interests & culture" ~ "Thematic information need",
      TRUE ~ NA_character_
    ),
    post_need_fit_z = case_when(
      need_domain == "Ungerichtetes Informationsbedürfnis" ~ intro_ib_undirected_z,
      need_domain == "Thematisches Informationsbedürfnis" ~ intro_ib_thematic_z,
      need_domain == "Soziales Informationsbedürfnis" ~ intro_ib_social_z,
      need_domain == "Problembezogenes Informationsbedürfnis" ~ intro_ib_problem_z,
      TRUE ~ NA_real_
    ),
    # Alias für die integrierte Outro-Analyse.
    need_fit_score = post_need_fit_z,
    processed_meaningfully = case_when(
      is.na(interaction_read) & is.na(interaction_research) & is.na(interaction_engagement) ~ NA_integer_,
      interaction_read == 1L | interaction_research == 1L | interaction_engagement == 1L ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  arrange(participant, study_day, screenshot_id) %>%
  group_by(participant) %>%
  mutate(
    diary_sequence = row_number(),
    topic_novelty = as.integer(!duplicated(as.character(topic_coded))),
    source_type_novelty = as.integer(!duplicated(as.character(source_coded))),
    account_novelty = case_when(
      is.na(source_name_coded) ~ NA_integer_,
      TRUE ~ as.integer(!duplicated(source_name_coded))
    ),
    any_repertoire_novelty = case_when(
      topic_novelty == 1L | source_type_novelty == 1L | account_novelty == 1L ~ 1L,
      is.na(topic_novelty) & is.na(source_type_novelty) & is.na(account_novelty) ~ NA_integer_,
      TRUE ~ 0L
    ),
    productive_serendipity_strict = case_when(
      is.na(incidental_strict) | is.na(any_repertoire_novelty) | is.na(processed_meaningfully) ~ NA_integer_,
      incidental_strict == 1L & any_repertoire_novelty == 1L & processed_meaningfully == 1L ~ 1L,
      TRUE ~ 0L
    ),
    productive_serendipity_broad = case_when(
      is.na(incidental_broad) | is.na(any_repertoire_novelty) | is.na(processed_meaningfully) ~ NA_integer_,
      incidental_broad == 1L & any_repertoire_novelty == 1L & processed_meaningfully == 1L ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  ungroup()

if (nrow(daily) == 0) stop("Keine öffentlich relevanten, vollständig codierten Beiträge im Sample.")

# Theorievariablen werden auch im vollständigen Screenshot-RDS verfügbar gemacht.
daily_all <- daily_all %>%
  left_join(
    daily %>%
      select(
        screenshot_id, topic_macro, source_macro, need_domain, post_need_fit_z,
        need_fit_score, processed_meaningfully, diary_sequence, topic_novelty,
        source_type_novelty, account_novelty, any_repertoire_novelty,
        productive_serendipity_strict, productive_serendipity_broad
      ),
    by = "screenshot_id"
  )


#===============================================================================
# 09 Sample overview and diary-day summary
#===============================================================================

# Der Überblick dokumentiert nur die zentralen Entscheidungen und Qualitätsmarker.
participant_counts <- daily_all %>%
  group_by(participant) %>%
  summarise(
    N_Screenshots = n(),
    N_Active_Days = n_distinct(study_day[!is.na(study_day)]),
    N_Public_Relevant = sum(public_relevance == 1L, na.rm = TRUE),
    N_Not_Public_Relevant = sum(public_relevance == 0L, na.rm = TRUE),
    N_Not_Assessable = sum(public_rel_coded == -1L, na.rm = TRUE),
    Share_Publicly_Relevant = safe_divide(
      N_Public_Relevant,
      N_Public_Relevant + N_Not_Public_Relevant
    ),
    .groups = "drop"
  )

sample_overview <- bind_rows(
  tibble(Section = "Sample", Metric = "Coding screenshots total", Value = nrow(coding), Note = NA_character_),
  tibble(Section = "Sample", Metric = "Coding participants total", Value = n_distinct(coding$participant), Note = NA_character_),
  tibble(Section = "Sample", Metric = "Included participants", Value = length(eligible_ids), Note = paste0(">= ", minimum_screenshots, " screenshots")),
  tibble(Section = "Sample", Metric = "Screenshots in analysis sample", Value = nrow(daily_all), Note = NA_character_),
  tibble(Section = "Public relevance", Metric = "Publicly relevant", Value = sum(daily_all$public_relevance == 1L, na.rm = TRUE), Note = NA_character_),
  tibble(Section = "Public relevance", Metric = "Not publicly relevant", Value = sum(daily_all$public_relevance == 0L, na.rm = TRUE), Note = NA_character_),
  tibble(Section = "Public relevance", Metric = "Not assessable (-1)", Value = sum(daily_all$public_rel_coded == -1L, na.rm = TRUE), Note = "Not in the denominator of the relevance rate"),
  tibble(Section = "Coding/QC", Metric = "Missing files", Value = missing_files, Note = NA_character_),
  tibble(Section = "Coding/QC", Metric = "Study day outside 1-7", Value = study_day_issues, Note = NA_character_),
  tibble(Section = "Coding/QC", Metric = "Platform corrections", Value = platform_mismatches, Note = "reported != coded")
) %>%
  bind_rows(
    exclusion_summary %>%
      transmute(Section = "Sample", Metric = paste0("Status: ", Reason), Value = N, Note = NA_character_)
  )

# Day effects are descriptive and mainly serve as a reactivity / trajectory check.
participant_day_grid <- tidyr::expand_grid(
  participant = eligible_ids,
  study_day = expected_study_days
)

uploads_by_day <- daily_all %>% count(participant, study_day, name = "N_Uploads")

public_by_day <- daily_all %>%
  filter(!is.na(public_relevance)) %>%
  group_by(participant, study_day) %>%
  summarise(Share_Public = safe_mean(public_relevance), .groups = "drop")

content_by_day <- daily %>%
  group_by(participant, study_day) %>%
  summarise(
    Share_Targeted = safe_mean(targeted_exposure),
    Share_Incidental_Broad = safe_mean(incidental_broad),
    Share_Read = safe_mean(interaction_read),
    Share_Research = safe_mean(interaction_research),
    Share_Engaged = safe_mean(interaction_engagement),
    Share_Topic_Novelty = safe_mean(topic_novelty),
    Share_Serendipity_Strict = safe_mean(productive_serendipity_strict),
    .groups = "drop"
  )

participant_day <- participant_day_grid %>%
  left_join(uploads_by_day, by = c("participant", "study_day")) %>%
  left_join(public_by_day, by = c("participant", "study_day")) %>%
  left_join(content_by_day, by = c("participant", "study_day")) %>%
  mutate(N_Uploads = replace_na(N_Uploads, 0L))

day_summary <- participant_day %>%
  group_by(study_day) %>%
  summarise(
    N_Eligible = n_distinct(participant),
    N_Contributors = sum(N_Uploads > 0),
    N_Uploads = sum(N_Uploads),
    Public_Relevance_Percent = 100 * safe_mean(Share_Public),
    Targeted_Percent = 100 * safe_mean(Share_Targeted),
    Incidental_Broad_Percent = 100 * safe_mean(Share_Incidental_Broad),
    Read_Percent = 100 * safe_mean(Share_Read),
    Research_Percent = 100 * safe_mean(Share_Research),
    Engagement_Percent = 100 * safe_mean(Share_Engaged),
    Topic_Novelty_Percent = 100 * safe_mean(Share_Topic_Novelty),
    Serendipity_Strict_Percent = 100 * safe_mean(Share_Serendipity_Strict),
    .groups = "drop"
  )


#===============================================================================
# 10 Main distributions
#===============================================================================

# Screenshot-Gewichtung beschreibt den hochgeladenen Informationsstrom;
# Teilnehmergewichtung verhindert, dass Viel-Uploader die Verteilung dominieren.
daily_distribution_data <- daily %>%
  mutate(
    topic_value = as.character(topic_coded),
    source_value = as.character(source_coded),
    platform_value = as.character(platform),
    format_value = if_else(media_format_code == -1L, "Not determinable", as.character(media_format)),
    incidentality_value = as.character(incidentality),
    locality_value = locality,
    situation_value = situation
  )

screenshot_distributions <- bind_rows(
  frequency_distribution(daily_distribution_data, "topic_value", "Topic", topic_levels),
  frequency_distribution(daily_distribution_data, "source_value", "Source", source_levels),
  frequency_distribution(daily_distribution_data, "platform_value", "Platform", platform_levels),
  frequency_distribution(daily_distribution_data, "format_value", "Format", c(unname(format_labels[c("1", "2", "3", "4")]), "Not determinable")),
  frequency_distribution(daily_distribution_data, "incidentality_value", "Incidental exposure", incidentality_levels),
  frequency_distribution(daily_distribution_data, "locality_value", "Spatial context", c("At home", "Out and about", "Don't know")),
  frequency_distribution(daily_distribution_data, "situation_value", "Social context", c("Alone", "With someone else", "Don't know"))
) %>%
  transmute(
    Weighting = "Screenshot",
    Variable, Category,
    N_Units = N,
    N_Valid,
    Percent = Percent_Valid,
    SD_Percent = NA_real_
  )

participant_share_tables <- bind_rows(
  summarise_participant_shares(make_participant_shares(daily_distribution_data, "topic_value", topic_levels, "Topic")),
  summarise_participant_shares(make_participant_shares(daily_distribution_data, "source_value", source_levels, "Source")),
  summarise_participant_shares(make_participant_shares(daily_distribution_data, "platform_value", platform_levels, "Platform")),
  summarise_participant_shares(make_participant_shares(daily_distribution_data %>% mutate(format_analysis = as.character(media_format)), "format_analysis", unname(format_labels[c("1", "2", "3", "4")]), "Format")),
  summarise_participant_shares(make_participant_shares(daily_distribution_data, "incidentality_value", incidentality_levels, "Incidental exposure"))
) %>%
  transmute(
    Weighting = "Participant",
    Variable, Category,
    N_Units = N_Participants,
    N_Valid = N_Participants,
    Percent = Mean_Percent,
    SD_Percent = 100 * SD_Share
  )

main_distributions <- bind_rows(screenshot_distributions, participant_share_tables)


#===============================================================================
# 11 Participant-level indicators
#===============================================================================

# Eine Zeile pro Person bildet die zentrale Ebene für Screening–Diary-Vergleiche
# und ist zugleich der Input für die spätere Outro-Integration.
participant_content_metrics <- daily %>%
  group_by(participant) %>%
  summarise(
    N_Public_Content_Posts = n(),
    Share_Incidental_Strict = safe_mean(incidental_strict),
    Share_Incidental_Broad = safe_mean(incidental_broad),
    Share_Targeted = safe_mean(targeted_exposure),
    Share_Read_Thoroughly = safe_mean(interaction_read),
    Share_Researched = safe_mean(interaction_research),
    Share_Engaged = safe_mean(interaction_engagement),
    Share_Any_Interaction = safe_mean(interaction_any),
    Share_Home = share_value(locality, "At home"),
    Share_Away = share_value(locality, "Out and about"),
    Share_Alone = share_value(situation, "Alone"),
    Share_Together = share_value(situation, "With someone else"),
    Topic_Richness = n_distinct_valid(topic_coded),
    Topic_Shannon = shannon_entropy(topic_coded),
    Source_Richness = n_distinct_valid(source_coded),
    Source_Shannon = shannon_entropy(source_coded),
    Platform_Richness = n_distinct_valid(platform),
    Platform_Shannon = shannon_entropy(platform),
    Format_Richness = n_distinct_valid(media_format),
    Format_Shannon = shannon_entropy(media_format),
    N_Unique_Account_Names = n_distinct_valid(source_name_coded),
    Share_Current_Affairs = share_value(topic_macro, "Current affairs & public issues"),
    Share_Practical_Service = share_value(topic_macro, "Practical information & service"),
    Share_Knowledge_Interests = share_value(topic_macro, "Knowledge, interests & culture"),
    Share_Journalistic_Sources = share_value(source_macro, "Journalistic media"),
    Share_Peer_Sources = share_value(source_macro, "Private person / peer"),
    Share_Facebook = share_value(as.character(platform), "Facebook"),
    Share_Instagram = share_value(as.character(platform), "Instagram"),
    Share_TikTok = share_value(as.character(platform), "TikTok"),
    Share_X = share_value(as.character(platform), "X"),
    Share_Video = share_value(as.character(media_format), "Moving audiovisual format"),
    Share_Topic_Novelty = safe_mean(topic_novelty),
    Share_Account_Novelty = safe_mean(account_novelty),
    Share_Productive_Serendipity_Strict = safe_mean(productive_serendipity_strict),
    Share_Productive_Serendipity_Broad = safe_mean(productive_serendipity_broad),
    Mean_Post_Need_Fit_Z = safe_mean(post_need_fit_z),
    Platform_Report_Match_Rate = safe_mean(platform_matches_report),
    .groups = "drop"
  )

participant_metrics <- participant_counts %>%
  left_join(participant_content_metrics, by = "participant") %>%
  left_join(screening_selected, by = "participant") %>%
  mutate(N_Public_Content_Posts = replace_na(N_Public_Content_Posts, 0L))

# Primärplattform und typische Nutzungskontexte erlauben einen knappen Abgleich
# zwischen generellem Screening-Selbstbericht und beobachtetem Diary-Muster.
daily_primary_platform <- daily %>%
  count(participant, platform, name = "N") %>%
  group_by(participant) %>%
  filter(N == max(N), N > 0) %>%
  summarise(
    Daily_Primary_Platform = paste(as.character(platform), collapse = " / "),
    .groups = "drop"
  )

participant_metrics <- participant_metrics %>%
  left_join(daily_primary_platform, by = "participant") %>%
  mutate(
    Primary_Platform_Match = map2_int(Primary_Platform, Daily_Primary_Platform, ~ {
      if (is.na(.x) || is.na(.y)) {
        NA_integer_
      } else {
        x_set <- str_split(clean_text(.x), " / ")[[1]]
        y_set <- str_split(clean_text(.y), " / ")[[1]]
        as.integer(length(intersect(x_set, y_set)) > 0)
      }
    }),
    Local_Context_Alignment = case_when(
      clean_text(context_local) == "At home" ~ Share_Home,
      clean_text(context_local) == "Out and about" ~ Share_Away,
      str_detect(str_to_lower(clean_text(context_local)), "equally|both") ~
        1 - abs(Share_Home - Share_Away),
      TRUE ~ NA_real_
    ),
    Social_Context_Alignment = case_when(
      str_detect(str_to_lower(clean_text(context_social)), "mostly alone") ~ Share_Alone,
      str_detect(str_to_lower(clean_text(context_social)), "mostly with others") ~ Share_Together,
      str_detect(str_to_lower(clean_text(context_social)), "equally") ~
        1 - abs(Share_Alone - Share_Together),
      TRUE ~ NA_real_
    ),
    Screening_Incidentality_Z = safe_z(incidentality_index),
    Diary_Incidentality_Broad_Z = safe_z(Share_Incidental_Broad),
    Diary_Incidentality_Strict_Z = safe_z(Share_Incidental_Strict),
    Incidentality_Gap_Broad = Screening_Incidentality_Z - Diary_Incidentality_Broad_Z,
    Absolute_Incidentality_Gap_Broad = abs(Incidentality_Gap_Broad),
    Incidentality_Gap_Strict = Screening_Incidentality_Z - Diary_Incidentality_Strict_Z,
    Absolute_Incidentality_Gap_Strict = abs(Incidentality_Gap_Strict)
  ) %>%
  rowwise() %>%
  mutate(
    Platform_Profile_Alignment = profile_alignment(
      c(intro_freq_facebook, intro_freq_instagram, intro_freq_tiktok, intro_freq_x),
      c(Share_Facebook, Share_Instagram, Share_TikTok, Share_X)
    )
  ) %>%
  ungroup()

# Einfache personenspezifische Tagesslopes werden für die spätere Reaktivitäts-
# analyse im Outro erhalten. Sie sind rein deskriptiv, keine kausalen Trends.
participant_day_slopes <- participant_day %>%
  group_by(participant) %>%
  summarise(
    upload_count_day_slope = {
      d <- tibble(x = study_day, y = N_Uploads) %>% drop_na()
      if (nrow(d) < 3 || n_distinct(d$y) < 2) NA_real_ else unname(coef(lm(y ~ x, data = d))[2])
    },
    public_relevance_day_slope = {
      d <- tibble(x = study_day, y = Share_Public) %>% drop_na()
      if (nrow(d) < 3 || n_distinct(d$y) < 2) NA_real_ else unname(coef(lm(y ~ x, data = d))[2])
    },
    targeted_post_day_slope = {
      d <- tibble(x = study_day, y = Share_Targeted) %>% drop_na()
      if (nrow(d) < 3 || n_distinct(d$y) < 2) NA_real_ else unname(coef(lm(y ~ x, data = d))[2])
    },
    thorough_reading_day_slope = {
      d <- tibble(x = study_day, y = Share_Read) %>% drop_na()
      if (nrow(d) < 3 || n_distinct(d$y) < 2) NA_real_ else unname(coef(lm(y ~ x, data = d))[2])
    },
    .groups = "drop"
  )

participant_metrics <- participant_metrics %>%
  left_join(participant_day_slopes, by = "participant")


#===============================================================================
# 12 Descriptive content and platform patterns
#===============================================================================

# Diese Kreuztabellen beantworten, ob Themen/Quellen/Formate auf den Plattformen
# unterschiedlich verteilt sind. Es sind rein deskriptive, kompositionale Muster.
cross_tabs <- bind_rows(
  cross_tabulation(daily, "topic_coded", "platform", "Topic", "Platform") %>%
    mutate(Analysis = "Topic x Platform", .before = 1),
  cross_tabulation(daily, "source_coded", "platform", "Source", "Platform") %>%
    mutate(Analysis = "Source x Platform", .before = 1),
  cross_tabulation(daily, "media_format", "platform", "Format", "Platform") %>%
    mutate(Analysis = "Format x Platform", .before = 1),
  cross_tabulation(daily, "topic_macro", "incidentality", "Topic family", "Incidental exposure") %>%
    mutate(Analysis = "Topic family x Incidental exposure", .before = 1),
  cross_tabulation(daily, "source_macro", "incidentality", "Source family", "Incidental exposure") %>%
    mutate(Analysis = "Source family x Incidental exposure", .before = 1)
)


#===============================================================================
# 13 Processing, novelty and productive serendipity
#===============================================================================

# Der Block prüft, was mit gefundenen Beiträgen passiert. Incidentality wird als
# Auffindungsweg verstanden; Neuheit bezieht sich nur auf das beobachtete Diary.
processing_overall <- daily %>%
  mutate(Overall = "All publicly relevant posts") %>%
  { bind_rows(
    binary_group_summary(., "Overall", "interaction_read", "Total", "Read/watched thoroughly"),
    binary_group_summary(., "Overall", "interaction_research", "Total", "Sought further information"),
    binary_group_summary(., "Overall", "interaction_engagement", "Total", "Visibly engaged"),
    binary_group_summary(., "Overall", "interaction_any", "Total", "At least one form of processing")
  ) }

processing_incidentality <- bind_rows(
  binary_group_summary(daily, "incidentality", "interaction_read", "Incidental exposure", "Read/watched thoroughly"),
  binary_group_summary(daily, "incidentality", "interaction_research", "Incidental exposure", "Sought further information"),
  binary_group_summary(daily, "incidentality", "interaction_engagement", "Incidental exposure", "Visibly engaged")
)

novelty_incidentality <- bind_rows(
  binary_group_summary(daily, "incidentality", "topic_novelty", "Incidental exposure", "New topic in diary"),
  binary_group_summary(daily, "incidentality", "account_novelty", "Incidental exposure", "New account in diary"),
  binary_group_summary(daily, "incidentality", "productive_serendipity_strict", "Incidental exposure", "Productive serendipity, strict"),
  binary_group_summary(daily, "incidentality", "productive_serendipity_broad", "Incidental exposure", "Productive serendipity, broad")
)

processing_novelty <- bind_rows(
  processing_overall %>% mutate(Analysis = "Processing overall", .before = 1),
  processing_incidentality %>% mutate(Analysis = "Processing by incidental exposure", .before = 1),
  novelty_incidentality %>% mutate(Analysis = "Novelty/serendipity by incidental exposure", .before = 1)
)


#===============================================================================
# 14 Targeted Screening–Diary exploration
#===============================================================================

# Statt vollständiger Korrelationsmatrizen werden nur theoretisch anschlussfähige
# Beziehungen geprüft. Alle Tests sind explorativ; BH wird je Familie korrigiert.
correlation_specs <- tribble(
  ~Family, ~X, ~Y, ~X_Label, ~Y_Label,
  "Calibration", "incidentality_index", "Share_Incidental_Broad", "Screening incidental exposure", "Diary incidental (broad)",
  "Calibration", "incidentality_index", "Share_Incidental_Strict", "Screening incidental exposure", "Diary incidental (strict)",
  "Information needs", "intro_ib_undirected", "Share_Current_Affairs", "Undirected need", "Share current/public affairs",
  "Information needs", "intro_ib_thematic", "Share_Knowledge_Interests", "Thematic need", "Share knowledge/interests/culture",
  "Information needs", "intro_ib_thematic", "Topic_Shannon", "Thematic need", "Topic diversity",
  "Information needs", "intro_ib_problem", "Share_Practical_Service", "Problem-related need", "Share practical information/service",
  "Information needs", "intro_ib_problem", "Share_Researched", "Problem-related need", "Sought further information",
  "Information needs", "intro_ib_social", "Share_Peer_Sources", "Social need", "Share peer sources",
  "Information needs", "intro_ib_social", "Share_Together", "Social need", "Use together with others",
  "Usage intensity", "intro_intensity", "Share_Read_Thoroughly", "Usage intensity", "Read/watched thoroughly",
  "Usage intensity", "intro_intensity", "Share_Researched", "Usage intensity", "Sought further information",
  "Usage intensity", "intro_intensity", "Share_Engaged", "Usage intensity", "Visibly engaged",
  "Age", "intro_age_num", "Share_Publicly_Relevant", "Age", "Share publicly relevant",
  "Age", "intro_age_num", "Share_Incidental_Broad", "Age", "Diary incidental (broad)",
  "Age", "intro_age_num", "Topic_Shannon", "Age", "Topic diversity",
  "Age", "intro_age_num", "Share_Video", "Age", "Share video"
)

integration_correlations <- pmap_dfr(
  correlation_specs,
  function(Family, X, Y, X_Label, Y_Label) {
    spearman_test(participant_metrics, X, Y, X_Label, Y_Label) %>%
      mutate(Family = Family, .before = 1)
  }
) %>%
  group_by(Family) %>%
  mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH")) %>%
  ungroup()

# Kalibrierungsmaße werden als Übereinstimmung, nicht automatisch als Bias gelesen,
# weil Screening und Diary unterschiedliche Zeit- und Messrahmen verwenden.
calibration_summary <- participant_metrics %>%
  select(
    Primary_Platform_Match, Platform_Profile_Alignment,
    Local_Context_Alignment, Social_Context_Alignment,
    Absolute_Incidentality_Gap_Broad, Absolute_Incidentality_Gap_Strict
  ) %>%
  pivot_longer(everything(), names_to = "Measure", values_to = "Value") %>%
  group_by(Measure) %>%
  summarise(
    N = sum(!is.na(Value)),
    Estimate = safe_mean(Value),
    SD = safe_sd(Value),
    Median = safe_median(Value),
    .groups = "drop"
  )

integration_export <- bind_rows(
  integration_correlations %>%
    transmute(
      Type = "Spearman correlation",
      Family,
      Measure = paste(Variable_1, "~", Variable_2),
      N,
      Estimate = Spearman_Rho,
      SD = NA_real_,
      Median = NA_real_,
      P_Value,
      P_Adjusted_BH,
      Note = "Exploratory; participant level"
    ),
  calibration_summary %>%
    transmute(
      Type = "Calibration description",
      Family = "Calibration",
      Measure,
      N,
      Estimate,
      SD,
      Median,
      P_Value = NA_real_,
      P_Adjusted_BH = NA_real_,
      Note = "Higher alignment = stronger agreement; gaps = standardized difference"
    )
)


#===============================================================================
# 15 Save analysis-level data
#===============================================================================

# Nur zwei RDS-Dateien bleiben: vollständige Screenshot-Ebene und Personen-Ebene.
# Öffentlich relevante Beiträge lassen sich jederzeit aus daily_all filtern.
saveRDS(daily_all, output_screenshot_rds)
saveRDS(participant_metrics, output_participant_rds)


#===============================================================================
# 16 Excel summary
#===============================================================================

# Ein Workbook mit wenigen, funktional getrennten Blättern ersetzt die frühere
# Vielzahl einzelner QC-, Verteilungs- und Explorations-Sheets.
workbook <- openxlsx::createWorkbook()
header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

participant_export <- participant_metrics %>%
  select(
    participant, N_Screenshots, N_Active_Days, N_Public_Relevant,
    N_Not_Public_Relevant, N_Not_Assessable, Share_Publicly_Relevant,
    N_Public_Content_Posts, Share_Incidental_Strict, Share_Incidental_Broad,
    Share_Targeted, Share_Read_Thoroughly, Share_Researched, Share_Engaged,
    Share_Any_Interaction, Share_Home, Share_Away, Share_Alone, Share_Together,
    Topic_Richness, Topic_Shannon, Source_Richness, Source_Shannon,
    Platform_Richness, Platform_Shannon, Format_Richness, Format_Shannon,
    Share_Current_Affairs, Share_Practical_Service, Share_Knowledge_Interests,
    Share_Journalistic_Sources, Share_Peer_Sources, Share_Video,
    Share_Topic_Novelty, Share_Account_Novelty,
    Share_Productive_Serendipity_Strict, Share_Productive_Serendipity_Broad,
    Daily_Primary_Platform, Primary_Platform_Match, Platform_Profile_Alignment,
    Local_Context_Alignment, Social_Context_Alignment,
    Incidentality_Gap_Broad, Absolute_Incidentality_Gap_Broad,
    upload_count_day_slope, public_relevance_day_slope,
    targeted_post_day_slope, thorough_reading_day_slope,
    intro_age_num, gender, education_three_level, intro_intensity,
    intro_ib_undirected, intro_ib_thematic, intro_ib_social, intro_ib_problem,
    incidentality_index, N_Platforms_Weekly, Platform_Repertoire, Primary_Platform
  )

add_excel_sheet(workbook, "Overview", sample_overview, header_style)
add_excel_sheet(workbook, "Day_Summary", day_summary, header_style)
add_excel_sheet(workbook, "Distributions", main_distributions, header_style)
add_excel_sheet(workbook, "Cross_Tabs", cross_tabs, header_style)
add_excel_sheet(workbook, "Processing_Novelty", processing_novelty, header_style)
add_excel_sheet(workbook, "Participants", participant_export, header_style)
add_excel_sheet(workbook, "Integration", integration_export, header_style)

openxlsx::saveWorkbook(workbook, output_excel, overwrite = overwrite_outputs)

# Publication-ready tables (.docx), APA-style. RQ1 = content; RQ2 = use/contexts;
# trajectories; and the exploratory screening-diary integration.

# Combines post-weighted percentages with participant-weighted means (SD).
build_dist_table <- function(vars) {
  screenshot <- main_distributions %>%
    filter(Weighting == "Screenshot", Variable %in% vars) %>%
    transmute(Dimension = Variable, Category, n = N_Units,
              `% (posts)` = fmt_num(Percent, 1))
  participant <- main_distributions %>%
    filter(Weighting == "Participant", Variable %in% vars) %>%
    transmute(Dimension = Variable, Category,
              `% (participants, M)` = fmt_num(Percent, 1),
              SD = fmt_num(SD_Percent, 1))
  screenshot %>% left_join(participant, by = c("Dimension", "Category"))
}

save_pub_table(
  build_dist_table(c("Topic", "Source", "Platform", "Format")),
  file.path(tables_folder, "Tab_Daily_Content_Distributions.docx"),
  table_number = 4,
  title = "Content of publicly relevant posts (RQ1)",
  note = "Post-weighted percentages and participant-weighted means (SD across participants). Percentages are based on valid cases."
)
save_pub_table(
  build_dist_table(c("Incidental exposure", "Spatial context", "Social context")),
  file.path(tables_folder, "Tab_Daily_Use_Contexts.docx"),
  table_number = 5,
  title = "Incidental exposure and situational contexts (RQ2)",
  note = "Incidental exposure uses the three-point measure; the broad definition also counts 'Followed, not deliberately sought' (per preregistration)."
)
save_pub_table(
  day_summary,
  file.path(tables_folder, "Tab_Daily_Day_Summary.docx"),
  table_number = 6,
  title = "Trajectories across the seven diary days"
)
save_pub_table(
  integration_export,
  file.path(tables_folder, "Tab_Daily_Integration_Exploratory.docx"),
  table_number = 7,
  title = "Exploratory screening-diary associations",
  note = "Exploratory; participant-level Spearman correlations, BH-corrected within each family."
)


#===============================================================================
# 17 Figures
#===============================================================================
# Main figures use the shared project theme from 00_Helpers.R. Categorical
# descriptions deliberately stay with absolute counts; colour, typography and
# direct labels serve only faster readability.

if (create_figures) {
  
  # Topics: horizontal bars are more readable with long category names.
  topic_plot_data <- daily %>%
    count(topic_coded, name = "N") %>%
    mutate(
      Topic = stringr::str_wrap(topic_coded, width = 38)
    )
  
  p_topic <- ggplot(
    topic_plot_data,
    aes(x = N, y = forcats::fct_reorder(Topic, N))
  ) +
    geom_col(
      width = 0.68,
      fill = unname(project_colors["primary"])
    ) +
    geom_text(
      aes(label = N),
      hjust = -0.18,
      size = 3.2,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = "Topics of publicly relevant posts",
      subtitle = paste0("Absolute counts; N = ", nrow(daily), " posts"),
      x = "Number of posts",
      y = NULL
    ) +
    theme_project(base_size = 11.5, legend_position = "none") +
    theme(panel.grid.major.y = element_blank())
  
  save_project_plot(
    p_topic,
    file.path(figure_folder, "Daily_Topics.png"),
    width = 9.4,
    height = 6.4
  )
  
  # Sources: the same display logic as the topic plot eases direct comparison of
  # the two central content dimensions.
  source_plot_data <- daily %>%
    count(source_coded, name = "N") %>%
    mutate(
      Source = stringr::str_wrap(source_coded, width = 43)
    )
  
  p_source <- ggplot(
    source_plot_data,
    aes(x = N, y = forcats::fct_reorder(Source, N))
  ) +
    geom_col(
      width = 0.68,
      fill = unname(project_colors["secondary"])
    ) +
    geom_text(
      aes(label = N),
      hjust = -0.18,
      size = 3.2,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = "Sources of publicly relevant posts",
      subtitle = "Coded account / source types",
      x = "Number of posts",
      y = NULL
    ) +
    theme_project(base_size = 11.5, legend_position = "none") +
    theme(panel.grid.major.y = element_blank())
  
  save_project_plot(
    p_source,
    file.path(figure_folder, "Daily_Sources.png"),
    width = 9.4,
    height = 6.1
  )
  
  # Format: not-determinable cases stay visible but are visually de-emphasized,
  # so data quality is apparent without implying a genuine fifth format category.
  format_plot_data <- daily %>%
    mutate(
      Format = if_else(
        media_format_code == -1L,
        "Not determinable",
        as.character(media_format)
      ),
      Format = stringr::str_wrap(Format, width = 34),
      Assessability = if_else(Format == "Not determinable", "Not determinable", "Format")
    ) %>%
    count(Format, Assessability, name = "N")
  
  p_format <- ggplot(
    format_plot_data,
    aes(
      x = N,
      y = forcats::fct_reorder(Format, N),
      fill = Assessability
    )
  ) +
    geom_col(width = 0.64) +
    geom_text(
      aes(label = N),
      hjust = -0.18,
      size = 3.2,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_fill_manual(
      values = c(
        "Format" = unname(project_colors["primary"]),
        "Not determinable" = unname(project_colors["light"])
      )
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.14))
    ) +
    labs(
      title = "Formats of publicly relevant posts",
      x = "Number of posts",
      y = NULL,
      fill = NULL
    ) +
    theme_project(base_size = 11.5, legend_position = "none") +
    theme(panel.grid.major.y = element_blank())
  
  save_project_plot(
    p_format,
    file.path(figure_folder, "Daily_Formats.png"),
    width = 8.4,
    height = 4.7
  )
  
  # Discovery mode: direct N labels instead of slanted axis text.
  incidentality_plot_data <- daily %>%
    count(incidentality, name = "N") %>%
    mutate(
      incidentality = forcats::fct_reorder(incidentality, N)
    )
  
  p_incidentality <- ggplot(
    incidentality_plot_data,
    aes(x = N, y = incidentality)
  ) +
    geom_col(
      width = 0.64,
      fill = unname(project_colors["accent"])
    ) +
    geom_text(
      aes(label = N),
      hjust = -0.18,
      size = 3.3,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.14))
    ) +
    labs(
      title = "How posts were encountered",
      subtitle = "Self-report immediately after upload",
      x = "Number of posts",
      y = NULL
    ) +
    theme_project(base_size = 12, legend_position = "none") +
    theme(panel.grid.major.y = element_blank())
  
  save_project_plot(
    p_incidentality,
    file.path(figure_folder, "Daily_Incidentality.png"),
    width = 8.3,
    height = 4.8
  )
  
  # Processing by discovery mode: three actions are shown side by side within
  # each discovery mode; values are percentages of the respective group, not of
  # all posts.
  processing_plot_data <- processing_incidentality %>%
    filter(!is.na(Group)) %>%
    mutate(
      Group = stringr::str_wrap(Group, width = 23),
      Label = if_else(
        is.na(Percent_Yes),
        NA_character_,
        paste0(round(Percent_Yes, 1), " %")
      )
    )
  
  p_processing <- ggplot(
    processing_plot_data,
    aes(x = Group, y = Percent_Yes, fill = Outcome)
  ) +
    geom_col(
      position = position_dodge(width = 0.78),
      width = 0.68
    ) +
    geom_text(
      aes(label = Label),
      position = position_dodge(width = 0.78),
      vjust = -0.35,
      size = 2.9,
      colour = unname(project_colors["dark"])
    ) +
    scale_fill_project() +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, " %"),
      expand = expansion(mult = c(0, 0.07))
    ) +
    labs(
      title = "Processing by discovery mode",
      subtitle = "Share within each discovery mode",
      x = NULL,
      y = "Share",
      fill = NULL
    ) +
    theme_project(base_size = 11.5, legend_position = "bottom") +
    theme(
      panel.grid.major.x = element_blank(),
      legend.position = "bottom"
    )
  
  save_project_plot(
    p_processing,
    file.path(figure_folder, "Daily_Processing_By_Incidentality.png"),
    width = 10.2,
    height = 6.1
  )
  
  # Daily trends: facets avoid a shared scale for very different indicators.
  # Lines show purely descriptive trajectories.
  day_plot_data <- day_summary %>%
    select(
      study_day,
      Public_Relevance_Percent,
      Incidental_Broad_Percent,
      Read_Percent,
      Topic_Novelty_Percent,
      Serendipity_Strict_Percent
    ) %>%
    pivot_longer(
      -study_day,
      names_to = "Indicator",
      values_to = "Percent"
    ) %>%
    mutate(
      Indicator = recode(
        Indicator,
        Public_Relevance_Percent = "Publicly relevant",
        Incidental_Broad_Percent = "Incidental (broad)",
        Read_Percent = "Read thoroughly",
        Topic_Novelty_Percent = "New topic in diary",
        Serendipity_Strict_Percent = "Productive serendipity"
      )
    )
  
  p_day <- ggplot(
    day_plot_data,
    aes(x = study_day, y = Percent)
  ) +
    geom_line(
      linewidth = 0.85,
      colour = unname(project_colors["primary"])
    ) +
    geom_point(
      size = 2.3,
      colour = unname(project_colors["accent"])
    ) +
    facet_wrap(~ Indicator, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = expected_study_days) +
    scale_y_continuous(labels = function(x) paste0(round(x), " %")) +
    labs(
      title = "Trajectories across the seven diary days",
      subtitle = "Descriptive; novelty can decline in a diary through repeated observation alone",
      x = "Study day",
      y = "Share"
    ) +
    theme_project(base_size = 10.8, legend_position = "none") +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 10)
    )
  
  save_project_plot(
    p_day,
    file.path(figure_folder, "Daily_Day_Trends.png"),
    width = 10.2,
    height = 7.6
  )
  
  # Screening-diary calibration: points are participants. The LM line serves only
  # visual orientation; inference stays with Spearman rho.
  calibration_plot_data <- participant_metrics %>%
    filter(!is.na(incidentality_index), !is.na(Share_Incidental_Broad))
  
  if (nrow(calibration_plot_data) >= 3) {
    p_calibration <- ggplot(
      calibration_plot_data,
      aes(x = incidentality_index, y = 100 * Share_Incidental_Broad)
    ) +
      geom_point(
        size = 2.6,
        alpha = 0.72,
        colour = unname(project_colors["primary"])
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        linewidth = 0.8,
        colour = unname(project_colors["accent"]),
        fill = unname(project_colors["light"])
      ) +
      scale_x_continuous(
        limits = c(1, 5),
        breaks = 1:5
      ) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        labels = function(x) paste0(x, " %")
      ) +
      labs(
        title = "Screening vs. diary incidental exposure",
        subtitle = "Participant level; regression line for visual orientation only",
        x = "Screening incidental exposure (1–5)",
        y = "Broadly incidental diary posts"
      ) +
      theme_project(base_size = 12, legend_position = "none")
    
    save_project_plot(
      p_calibration,
      file.path(figure_folder, "Daily_Screening_Diary_Incidentality.png"),
      width = 7.4,
      height = 5.6
    )
  }
}


#===============================================================================
# 18 Console report
#===============================================================================

cat(
  "\nDAILY ANALYSIS COMPLETED\n",
  "Participants: ", length(eligible_ids), "\n",
  "Screenshots: ", nrow(daily_all), "\n",
  "Public content posts: ", nrow(daily), "\n",
  "Not assessable (-1): ", sum(daily_all$public_rel_coded == -1L, na.rm = TRUE), "\n",
  "Excel: ", output_excel, "\n",
  sep = ""
)
