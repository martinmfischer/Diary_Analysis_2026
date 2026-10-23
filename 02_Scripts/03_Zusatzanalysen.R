# ==============================================================================
# Zusätzliche Analysen und Methodenprüfungen
# ==============================================================================
# Separat vom Paper-Rumpf: Plattformpaare, Verlauf, weitere Theoriebezüge,
# Omega-Diagnostik, Datenqualität und Sensitivität der Uploadschwelle.
# Kein Markdown-/PDF-Export. Ergebnisse: 03_Output/Zusatzanalysen_Tabellen.xlsx.

# 01 Paper-Ergebnisse als gemeinsame Grundlage ----------------------------------
source("02_Scripts/02_Paper_Analysen.R", encoding = "UTF-8", local = TRUE)

minimum_platform_posts <- 3
scored_items <- incidentality_scores(intro)
sample_counts <- tibble(Eingeschlossen = length(included_ids),
  Daily_Personen = n_distinct(daily$participant), Screenshots = nrow(daily),
  Oeffentlich_relevant = nrow(daily_public))

# 02 Facebook und Instagram bei denselben Personen ------------------------------
# Mindestens drei gültige Beiträge je Plattform; kein kausaler Plattformeffekt.
platform_coverage <- daily_public %>%
  group_by(platform) %>%
  summarise(Posts = n(), Personen = n_distinct(participant), .groups = "drop")
paired_ids <- daily_public %>%
  filter(platform %in% c("Facebook", "Instagram")) %>%
  count(participant, platform) %>%
  filter(n >= minimum_platform_posts) %>%
  count(participant) %>%
  filter(n == 2) %>%
  pull(participant)
paired_posts <- daily_public %>%
  filter(participant %in% paired_ids, platform %in% c("Facebook", "Instagram"))

profile_tables <- list()
for (variable in c("topic_coded_label", "source_coded_label")) {
  valid <- paired_posts %>% filter(!is.na(.data[[variable]]))
  # Je Merkmal nur Personen mit gültigen Codes auf beiden Plattformen.
  valid_ids <- valid %>%
    distinct(participant, platform) %>%
    count(participant) %>%
    filter(n == 2) %>%
    pull(participant)
  valid <- valid %>% filter(participant %in% valid_ids)
  categories <- levels(daily_public[[variable]])
  denominators <- valid %>% count(participant, platform, name = "N")
  counts <- valid %>%
    transmute(participant,
      platform,
      Kategorie = as.character(.data[[variable]])
    ) %>%
    count(participant, platform, Kategorie, name = "n")
  shares <- crossing(
    participant = valid_ids, platform = c("Facebook", "Instagram"),
    Kategorie = categories
  ) %>%
    left_join(denominators, by = c("participant", "platform")) %>%
    left_join(counts, by = c("participant", "platform", "Kategorie")) %>%
    mutate(Anteil = 100 * replace_na(n, 0) / N)
  profile_tables[[variable]] <- shares %>%
    group_by(Kategorie, platform) %>%
    summarise(Personen = n(), Anteil = mean(Anteil), .groups = "drop") %>%
    pivot_wider(names_from = platform, values_from = Anteil) %>%
    mutate(Differenz_IG_minus_FB = Instagram - Facebook) %>%
    arrange(desc(Differenz_IG_minus_FB))
}

# 03 Verläufe und studienbedingte Suche ------------------------------------------
# Nachträgliche Methodenprüfung: wechselnde vs. konstante Personenbasis.
day_person <- daily_public %>%
  group_by(participant, study_day) %>%
  summarise(
    Zufall = safe_mean(incidental_strict), Lesen = safe_mean(read),
    Recherche = safe_mean(research), Interaktion = safe_mean(engage),
    Fehlende_Interaktion = mean(is.na(read)), Posts = n(), .groups = "drop"
  ) %>%
  pivot_longer(c(Zufall, Lesen, Recherche, Interaktion), names_to = "Merkmal", values_to = "Anteil")
complete_week <- day_person %>%
  filter(!is.na(Anteil), study_day %in% 1:7) %>%
  count(participant, Merkmal) %>%
  filter(n == 7)
day_all <- day_person %>%
  group_by(study_day, Merkmal) %>%
  summarise(Personen = sum(!is.na(Anteil)), Prozent = 100 * safe_mean(Anteil), .groups = "drop") %>%
  mutate(Basis = "Alle verfügbaren Personen")
day_balanced <- day_person %>%
  semi_join(complete_week, by = c("participant", "Merkmal")) %>%
  group_by(study_day, Merkmal) %>%
  summarise(Personen = n(), Prozent = 100 * mean(Anteil), .groups = "drop") %>%
  mutate(Basis = "Angaben an allen 7 Tagen")
day_table <- bind_rows(day_all, day_balanced)

missing_by_day <- daily_public %>%
  group_by(study_day) %>%
  summarise(
    Posts = n(), Fehlend = sum(is.na(read)),
    Fehlend_Prozent = 100 * mean(is.na(read)), .groups = "drop"
  )

# Einzelitem statt des psychometrisch problematischen Reaktivitätsindex.
sensitivity_table <- bind_rows(lapply(c("Zufall", "Ungeplant"), function(definition) {
  spearman_summary(
    filter(calibration, Definition == definition, study_search <= 3),
    paste(definition, "ohne Zustimmung zur studienbedingten Suche")
  )
}))

# 04 Weitere Theoriebezüge und Methodenprüfungen ---------------------------------
source("02_Scripts/zusatz/04_Theoriebezogene_Exploration.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/zusatz/01_Messqualitaet.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/zusatz/02_Datenqualitaet.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/zusatz/03_Uploadschwelle.R", encoding = "UTF-8", local = TRUE)

# 05 Zusatzergebnisse separat speichern -----------------------------------------
additional_tables <- c(profile_tables, list(
  Verlauf = day_table, Suche_Sensitivitaet = sensitivity_table,
  Beduerfnisse = needs_results, Kontext_gepaart = context_table,
  Begegnung_gepaart = encounter_pair_table, Muster = pattern_table,
  Fehlende_Praktiken = missing_encounter, Itemkorrelationen = item_correlations,
  Omega_Intro = incidentality_reliability_table, Omega_Outro = outro_reliability,
  Omega_Diagnostik = omega_diagnostics, Coding_Abdeckung = coverage_summary,
  Pretest = pretest_table, Upload_Sensitivity = threshold_comparison,
  Upload_Kategorien = category_comparison, Upload_Basis = threshold_basis
))
writexl::write_xlsx(additional_tables, "03_Output/Zusatzanalysen_Tabellen.xlsx")
message("Zusatzanalysen abgeschlossen: 03_Output/Zusatzanalysen_Tabellen.xlsx")
