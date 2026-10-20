# 04 Ergänzende theoriebezogene Exploration -------------------------------------
# Aufgerufen durch: 03_Explorative_Auswertung.R nach den Verlaufsauswertungen
# Benötigt: person_data, intro, daily_public, scored_items, incidentality_items,
#          save_plot(), markdown_table() und gemeinsame Funktionen
# Liefert: needs_results, context_table, encounter_pair_table, social_coverage,
#          pattern_table, missing_encounter, item_correlations und theory_report
# Schreibt: zusätzliche Grafiken in das vom Einstiegsskript gesetzte figure_dir

# Ergänzende Exploration; wird nur vom zugehörigen Einstiegsskript geladen
# Alle Vergleiche wurden nach Sichtung der Daten festgelegt.

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
needs_figure <- save_plot(
  ggplot(needs_long, aes(X, Anteil)) +
    geom_point(
      position = position_jitter(width = .08, height = 0, seed = 42),
      alpha = .45, colour = "#276B87", na.rm = TRUE
    ) +
    facet_wrap(~Bezug, scales = "free_x", labeller = label_wrap_gen(32)) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      x = "Intro-Antwort (Originalskala)", y = "Anteil je Person (%)",
      title = "Bedürfnisse, Nutzung und berichtete Praktiken"
    ),
  "beduerfnisse", "Informationsbedürfnisse und Praktiken",
  height = 6
)

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
context_figure <- save_plot(
  ggplot(context_pairs, aes(Differenz, Praxis)) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_boxplot(outlier.shape = NA, fill = "grey93", width = .4) +
    geom_point(
      position = position_jitter(width = 0, height = .12, seed = 42),
      colour = "#276B87", alpha = .5
    ) +
    labs(
      title = "Dieselben Personen: unterwegs gegenüber zu Hause", y = NULL,
      x = "Differenz in Prozentpunkten (unterwegs minus zu Hause)"
    ),
  "kontext_gepaart", "Gepaarte Kontextunterschiede"
)
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
pattern_figure <- save_plot(
  ggplot(pattern_table, aes(Prozent, Muster)) +
    geom_col(fill = "#276B87") +
    facet_wrap(~Basis, ncol = 1) +
    labs(
      x = "Anteil aller ungeplanten Uploads in der Teilmenge (%)", y = NULL,
      title = "Lesen, Recherche und Interaktion treten kombiniert auf",
      caption = "Selbstberichte. Fehlende Angaben bleiben eine eigene Gruppe."
    ),
  "verarbeitungsmuster", "Kombinationen berichteter Praktiken",
  height = 8
)

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

# 06 Berichtstext: Zweck und Grenzen direkt an der jeweiligen Analyse ------------

theory_report <- c(
  "## 7. Informationsbedürfnisse und Nutzungsintensität", "",
  "**Zweck und Theorie:** Die Bedürfnisdimensionen nach Hasebrink und Domeyer sowie Kümpel, Anter und Unkel mit den beobachteten Praktiken verbinden. Die vier Dimensionen bleiben getrennt. Soziales Informationsbedürfnis wird nicht mit öffentlichem Engagement gleichgesetzt; private Updates liegen außerhalb der Inhaltsauswertung. Nutzungsintensität ergänzt die Perspektive auf sichtbare Beteiligung.",
  "",
  "**Status:** Theoriegeleitete Exploration. Die Präregistrierung erlaubt Zusammenhänge und Subgruppen, enthält aber keine gerichteten Hypothesen zu diesen vier Paarungen. Daher kein konfirmatorischer Theorietest. Pro Person geht ein Wertepaar ein; fehlende Werte entfallen paarweise.",
  "",
  markdown_table(needs_results), "", needs_figure, "",
  "## 8. Nutzungskontext innerhalb derselben Personen", "",
  "**Zweck und Theorie:** Die situative Perspektive (Karnowski; Ohme et al.; Mayer et al.) legt Unterschiede zwischen häuslicher und mobiler Nutzung nahe. Der Vergleich beschreibt, ob dieselben Personen je nach Ort andere Praktiken berichten. Er kontrolliert keine wechselnden Inhalte, Plattformen oder Nutzungsmotive.",
  "",
  "**Status:** Theoriegeleitete Exploration zu RQ2a/c. Mindestens drei gültige Beiträge je Ort und Praxis; diese Grenze ist nachträglich festgelegt. A = zu Hause, B = unterwegs. Differenzen sind B minus A in Prozentpunkten.",
  "",
  markdown_table(context_table), "", context_figure, "",
  "### Sozialer Kontext", "",
  "**Zweck und Status:** Deskriptiv prüfen, ob RQ2a auch einen Vergleich alleiniger und gemeinsamer Nutzung erlaubt. Die Fallzahlen bestimmen hier die Grenze der weiterführenden Auswertung.",
  "",
  markdown_table(social_coverage), "",
  "Gemeinsame Nutzung ist zu selten für einen belastbaren gepaarten Vergleich. Masurs Datenschutzperspektive kann hier nur die Diskussion anleiten: Datenschutzbedenken und Selbstoffenbarung wurden nicht direkt gemessen.",
  "",
  "## 9. Ungeplante Begegnungen und Verarbeitung", "",
  "**Zweck und Theorie:** An PINE (Matthes et al., 2020) angelehnte Prüfung, welche Kombinationen von Lesen, Recherche und Interaktion bei ungeplanten Begegnungen vorkommen. Auswahl und sichtbare Beteiligung bleiben eigenständige Praktiken.",
  "",
  "**Status:** Theoriegeleitete Exploration zu RQ2b/c. Die Kombinationen sind keine Verarbeitungsstufen. Weder zeitliche Übergänge noch persönliche Relevanzbewertungen oder Wissenseffekte sind beobachtet. Öffentliche Relevanz ersetzt persönliche Relevanz nicht. Ein vollständiger PINE-Theorietest ist damit nicht möglich.",
  "",
  "Codes 2 und 3 gelten als ungeplant. Die zusätzliche Teilmenge Politik, Staat und Wahlen (Themen-Code 1) nähert den politischen Gegenstandsbereich an; sie bildet nicht alle politischen Inhalte ab. Prozentwerte sind screenshotgewichtet.",
  "",
  markdown_table(pattern_table), "", pattern_figure, "",
  "### Gezielte und zufällige Begegnungen bei denselben Personen", "",
  "**Zweck:** Prüfen, ob die bisherigen Praxisunterschiede auch bei identischer Personenbasis auftreten. Nachträgliche Robustheitsanalyse, kein Nachweis einer Wirkung der Begegnungsart. Mindestens drei gültige Beiträge pro Begegnungsart und Praxis. A = zufällig, B = gezielt.",
  "",
  markdown_table(encounter_pair_table), "",
  "## 10. Fehlende Angaben nach Begegnungsart", "",
  "**Zweck:** Selektive Antwortausfälle als mögliche Erklärung der Praxisunterschiede prüfen. Nachträgliche Sensitivitätsanalyse ohne Theorieprüfung. Unter- und Obergrenze setzen alle fehlenden Antworten hypothetisch auf 0 bzw. 1. Das sind Extremwertgrenzen, keine Schätzungen oder Konfidenzintervalle. Die regulären Analysen lassen fehlende Antworten weiterhin aus.",
  "",
  markdown_table(missing_encounter), "",
  "## 11. Incidentality: einzelne Items und Tagebuch", "",
  "**Zweck:** Den schwachen Zusammenhang zwischen Mittelwertindex und Tagebuch auf Itemebene untersuchen. Nachträgliche Messdiagnostik, kein Theorietest. Die sechs angepassten Items werden nicht mit der ursprünglichen sieben Items umfassenden Skala gleichgesetzt. Item 5 ist umgepolt. Der vollständige präregistrierte Mittelwertindex bleibt unverändert; die zwölf Korrelationen dienen nicht zur Itemauswahl.",
  "",
  markdown_table(item_correlations), "",
  "### Theoriebezüge", "",
  "[PINE: Matthes et al. (2020)](https://journals.sagepub.com/doi/10.1177/1464884920915371); [Incidentality: Ahmadi und Wohn (2018)](https://journals.sagepub.com/doi/10.1177/2056305118772827); [Informationsbedürfnisse: Kümpel, Anter und Unkel (2022)](https://www.cogitatiopress.com/mediaandcommunication/article/view/5310). Weitere Theoriebezüge folgen der bereitgestellten Präregistrierung.",
  ""
)
