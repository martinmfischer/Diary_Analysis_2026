# Sensitivität: mindestens ein statt mindestens sieben Uploads.
# Screening und abgeschlossene Intro-/Outro-Fragebögen bleiben erforderlich.
# Nachträgliche Robustheitsanalyse; ändert die Hauptstichprobe nicht.
# Benötigt study_ids, sample_audit, intro_raw und gemeinsame Funktionen.

# 01 Vergleichsgruppen und gleich gepolten Intro-Index vorbereiten ----------------

sensitivity_ids <- list(
  Alle = sample_audit$participant[sample_audit$included_any_upload],
  Ab_sieben = sample_audit$participant[sample_audit$included_7]
)
sensitivity_coding <- openxlsx::read.xlsx("06_Coding/coding_sheet.xlsx", sheet = "Coding") %>%
  mutate(participant = clean_text(participant))
sensitivity_intro <- intro_raw %>% select(participant, starts_with("intro_incidentality_"))
incidentality_item_values <- incidentality_scores(sensitivity_intro)
sensitivity_intro$Index <- rowMeans(incidentality_item_values, na.rm = FALSE)

# 02 Praktiken und Begegnungsarten: Beiträge, Personenanteile und Korrelationen ----

threshold_results <- lapply(names(sensitivity_ids), function(basis) {
  participant_ids <- sensitivity_ids[[basis]]
  public_posts <- sensitivity_coding %>%
    filter(participant %in% participant_ids, public_rel_coded == 1) %>%
    decode_practices() %>%
    transmute(participant,
      Lesen = read,
      Recherche = research,
      Interaktion = engage,
      Zufall = if_else(incidentality_code %in% 1:3,
        as.numeric(incidentality_code == 3),
        NA_real_
      ),
      Ungeplant = if_else(incidentality_code %in% 1:3,
        as.numeric(incidentality_code %in% 2:3),
        NA_real_
      )
    )
  practice_responses <- public_posts %>%
    pivot_longer(-participant,
      names_to = "Merkmal",
      values_to = "Wert"
    ) %>%
    filter(!is.na(Wert))
  participant_practices <- practice_responses %>%
    group_by(
      participant,
      Merkmal
    ) %>%
    summarise(
      Wert = mean(Wert),
      .groups = "drop"
    )
  shares <- bind_rows(
    practice_responses %>% group_by(Merkmal) %>% summarise(
      N = n(),
      Wert = 100 * mean(Wert),
      .groups = "drop"
    ) %>% mutate(Ebene = "Beiträge (%)"),
    participant_practices %>% group_by(Merkmal) %>% summarise(
      N = n(),
      Wert = 100 * mean(Wert),
      .groups = "drop"
    ) %>% mutate(Ebene = "Personenmittel (%)")
  )
  correlations <- participant_practices %>%
    filter(Merkmal %in% c("Zufall", "Ungeplant")) %>%
    left_join(select(sensitivity_intro, participant, Index), by = "participant") %>%
    filter(is.finite(Wert), is.finite(Index)) %>%
    group_by(Merkmal) %>%
    summarise(
      N = n(),
      Wert = if (n() >= 3 && sd(Wert) > 0 && sd(Index) > 0) {
        cor(Index,
          Wert,
          method = "spearman"
        )
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(Ebene = "Intro–Tagebuch: rho")
  bind_rows(shares, correlations) %>% mutate(Basis = basis)
})
threshold_comparison <- bind_rows(threshold_results) %>%
  pivot_wider(id_cols = c(Merkmal, Ebene), names_from = Basis, values_from = c(N, Wert)) %>%
  mutate(Differenz = Wert_Alle - Wert_Ab_sieben)
readr::write_csv(threshold_comparison, "03_Output/Uploadschwelle_Robustheit.csv")

# Vollständige Kategorieverteilungen, nicht nur ausgewählte Kategorien

# 03 Themen-, Quellen- und Formatverteilungen vergleichen -------------------------

category_results <- bind_rows(lapply(names(sensitivity_ids), function(basis) {
  coded_posts <- sensitivity_coding %>% filter(
    participant %in% sensitivity_ids[[basis]],
    public_rel_coded == 1
  )
  bind_rows(lapply(c("topic_coded", "source_coded", "media_format"), function(category_variable) {
    coded_posts %>%
      filter(!is.na(.data[[category_variable]])) %>%
      count(Code = as.character(.data[[category_variable]]), name = "n") %>%
      mutate(Variable = category_variable, Prozent = 100 * n / sum(n), Basis = basis)
  }))
}))
category_comparison <- category_results %>%
  pivot_wider(
    id_cols = c(
      Variable,
      Code
    ),
    names_from = Basis,
    values_from = c(
      n,
      Prozent
    ),
    values_fill = 0
  ) %>%
  mutate(Differenz_pp = Prozent_Alle - Prozent_Ab_sieben)
readr::write_csv(category_comparison, "03_Output/Uploadschwelle_Kategorien.csv")
category_max <- category_comparison %>%
  group_by(Variable) %>%
  summarise(Maximale_absolute_Differenz_pp = max(abs(Differenz_pp)), .groups = "drop")

# 04 Fallzahlen beider Vergleichsgruppen dokumentieren ----------------------------

threshold_basis <- bind_rows(lapply(names(sensitivity_ids), function(basis) {
  participant_ids <- sensitivity_ids[[basis]]
  coded_posts <- sensitivity_coding %>% filter(participant %in% participant_ids)
  tibble(
    Basis = basis,
    Personen = length(participant_ids),
    Uploads = sum(sample_audit$n_uploads[sample_audit$participant %in% participant_ids]),
    Personen_mit_Coding = n_distinct(coded_posts$participant), Coding_Zeilen = nrow(coded_posts),
    Oeffentlich_relevant = sum(coded_posts$public_rel_coded == 1, na.rm = TRUE)
  )
}))
readr::write_csv(threshold_basis, "03_Output/Uploadschwelle_Stichproben.csv")
stopifnot(
  all(sensitivity_ids$Ab_sieben %in% sensitivity_ids$Alle),
  all(threshold_comparison$N_Alle >= threshold_comparison$N_Ab_sieben),
  sum(sample_audit$included) == length(study_ids)
)
stopifnot(setequal(sensitivity_ids$Ab_sieben, study_ids))
