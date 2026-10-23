# Zusätzliche theoriegeleitete Exploration; keine gerichteten Hypothesentests.
# Benötigt person_data, intro, daily_public und scored_items aus 03_Zusatzanalysen.R.
# Liefert Bedürfnisse, gepaarte Kontexte, Verarbeitungsmuster und Itemdiagnostik.

# 01 Informationsbedürfnisse und Nutzungsintensität ------------------------------

needs_data <- person_data %>%
  mutate(Gezielte_Suche = 1 - Ungeplant) %>%
  left_join(
    intro %>% select(
      participant,
      starts_with("intro_ib_"),
      intro_intensity
    ),
    by = "participant"
  ) %>%
  mutate(across(c(starts_with("intro_ib_"), intro_intensity), ~ na_if(clean_numeric(.x), -1)))
needs_plan <- tibble::tribble(
  ~Praediktor, ~Praxis, ~Bezug,
  "intro_ib_problem", "Recherche", "Problemlösung: weiterführende Recherche",
  "intro_ib_thematic", "Gezielte_Suche", "Thematisches Interesse: gezielte Suche",
  "intro_ib_undirected", "Lesen", "Allgemeine Orientierung: gründliches Lesen",
  "intro_intensity", "Interaktion", "Nutzungsintensität: sichtbare Beteiligung"
)
needs_results <- bind_rows(lapply(seq_len(nrow(needs_plan)), function(i) {
  predictor_values <- needs_data[[needs_plan$Praediktor[i]]]
  practice_values <- needs_data[[needs_plan$Praxis[i]]]
  complete_pairs <- is.finite(predictor_values) & is.finite(practice_values)
  # Pro Person ein Wertepaar; ohne Streuung ist eine Korrelation nicht definiert
  tibble(
    Bezug = needs_plan$Bezug[i], Personen = sum(complete_pairs),
    Spearman_rho = safe_spearman(predictor_values, practice_values)
  )
}))
needs_long <- bind_rows(lapply(seq_len(nrow(needs_plan)), function(i) {
  tibble(
    X = needs_data[[needs_plan$Praediktor[i]]],
    Anteil = 100 * needs_data[[needs_plan$Praxis[i]]], Bezug = needs_plan$Bezug[i]
  )
}))
# 02 Gepaarte Vergleiche: Ort und Begegnung --------------------------------------

paired_rates <- function(data, group, levels, minimum = 3) {
  # Zuerst Personenanteile je Gruppe und Praxis bilden
  # Anschließend nur Personen behalten, die in beiden Gruppen genügend Angaben haben
  rates <- data %>%
    transmute(participant,
      Gruppe = as.character(.data[[group]]),
      Lesen = read, Recherche = research, Interaktion = engage
    ) %>%
    filter(Gruppe %in% levels) %>%
    pivot_longer(c(Lesen, Recherche, Interaktion), names_to = "Praxis", values_to = "Wert") %>%
    filter(!is.na(Wert)) %>%
    group_by(participant, Gruppe, Praxis) %>%
    summarise(N = n(), Anteil = 100 * mean(Wert), .groups = "drop") %>%
    filter(N >= minimum) %>%
    select(-N) %>%
    pivot_wider(names_from = Gruppe, values_from = Anteil)
  for (level in levels) {
    if (!level %in% names(rates)) {
      rates[[level]] <- NA_real_
    }
  }
  rates %>%
    filter(if_all(all_of(levels), ~ !is.na(.x))) %>%
    transmute(participant,
      Praxis,
      A = .data[[levels[1]]],
      B = .data[[levels[2]]],
      Differenz = B - A
    )
}
context_pairs <- paired_rates(daily_public, "locality_code", c("1", "2"))
encounter_pairs <- paired_rates(daily_public, "encounter", c("Zufällig", "Gezielt"))
pair_summary <- function(data) {
  data %>%
    group_by(Praxis) %>%
    summarise(
      Personen = n(), Mittel_A = mean(A), Mittel_B = mean(B),
      Differenz_pp = mean(Differenz), Median_Differenz = median(Differenz), .groups = "drop"
    )
}
context_table <- pair_summary(context_pairs)
encounter_pair_table <- pair_summary(encounter_pairs)
social_coverage <- daily_public %>%
  mutate(Kontext = factor(situation_code,
    levels = 1:3,
    labels = c(
      "Allein",
      "Gemeinsam",
      "Weiß nicht mehr"
    )
  )) %>%
  group_by(Kontext) %>%
  summarise(Posts = n(), Personen = n_distinct(participant), .groups = "drop")

# 03 Verarbeitungsmuster bei ungeplanten Begegnungen -----------------------------

pattern_data <- daily_public %>%
  filter(incidentality_code %in% c(2, 3)) %>%
  mutate(
    Vollstaendig = !is.na(read) & !is.na(research) & !is.na(engage),
    Muster = if_else(Vollstaendig,
      paste0(read, research, engage),
      "Angaben fehlen"
    )
  )
pattern_levels <- c("000", "100", "010", "001", "110", "101", "011", "111", "Angaben fehlen")
pattern_labels <- c(
  "Keine Praxis berichtet", "Nur Lesen", "Nur Recherche", "Nur Interaktion",
  "Lesen und Recherche", "Lesen und Interaktion", "Recherche und Interaktion",
  "Alle drei Praktiken", "Angaben fehlen"
)
pattern_data$Muster <- factor(pattern_data$Muster, levels = pattern_levels, labels = pattern_labels)
pattern_table <- bind_rows(lapply(
  c(
    "Alle öffentlichen Themen",
    "Politik, Staat und Wahlen"
  ),
  function(scope) {
    selected_posts <- if (scope == "Alle öffentlichen Themen") {
      pattern_data
    } else {
      filter(
        pattern_data,
        topic_coded == 1
      )
    }
    selected_posts %>%
      count(Muster, name = "Posts", .drop = FALSE) %>%
      mutate(Basis = scope, Prozent = 100 * Posts / nrow(selected_posts), .before = 1)
  }
))
# 04 Fehlende Praktiken: Verteilung und Extremwertgrenzen ------------------------

missing_encounter <- daily_public %>%
  select(participant, encounter, read, research, engage) %>%
  pivot_longer(c(read, research, engage), names_to = "Praxis", values_to = "Wert") %>%
  group_by(encounter, Praxis) %>%
  summarise(
    Posts = n(), Fehlend = sum(is.na(Wert)), Fehlend_Prozent = 100 * mean(is.na(Wert)),
    Gueltig_Prozent = 100 * safe_mean(Wert),
    Untergrenze = 100 * sum(Wert == 1, na.rm = TRUE) / n(),
    Obergrenze = 100 * (sum(Wert == 1, na.rm = TRUE) + sum(is.na(Wert))) / n(),
    .groups = "drop"
  ) %>%
  mutate(Praxis = recode(Praxis, read = "Lesen", research = "Recherche", engage = "Interaktion"))

# 05 Itemdiagnostik ohne datengetriebene Änderung des Index -----------------------

item_data <- scored_items
item_data$participant <- intro$participant
item_data <- left_join(item_data,
  select(
    person_data,
    participant,
    Zufall,
    Ungeplant
  ),
  by = "participant"
)
item_correlations <- bind_rows(lapply(incidentality_items, function(item) {
  bind_rows(lapply(c("Zufall", "Ungeplant"), function(definition) {
    item_values <- item_data[[item]]
    encounter_shares <- item_data[[definition]]
    complete_pairs <- is.finite(item_values) & is.finite(encounter_shares)
    tibble(
      Item = item, Definition = definition, Personen = sum(complete_pairs),
      Spearman_rho = safe_spearman(item_values, encounter_shares)
    )
  }))
}))
