# 03 Bestehende Sensitivitätsanalyse der Uploadschwelle --------------------------
# Aufgerufen durch: beide Auswertungen vor dem abschließenden Berichtsexport
# Benötigt: study_ids, sample_audit, intro_raw und gemeinsame Funktionen
# Liest zusätzlich: vorhandenes Hauptcoding
# Liefert: threshold_comparison, category_comparison, threshold_basis,
#          category_max und upload_sensitivity_report
# Schreibt: drei Uploadschwelle_*.csv-Dateien nach 03_Output
# Verglichen werden dieselben Kennzahlen in zwei unterschiedlich großen Gruppen

# Vergleich derselben Kennzahlen ab einem bzw. sieben Uploads
# Grundlage: vorhandenes Hauptcoding, keine imputierten Inhaltscodes

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
    transmute(participant,
      Lesen = clean_binary(interaction_read_code),
      Recherche = clean_binary(interaction_research_code),
      Interaktion = clean_binary(interaction_engagement_code),
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

# 05 Berichtsteil mit den jeweiligen Bezugsgrößen erstellen ------------------------

upload_sensitivity_report <- c(
  "## Robustheit gegenüber der Uploadschwelle", "",
  "Die Hauptauswertung verlangt erfülltes Screening, abgeschlossenes Intro und Outro sowie mindestens sieben unterschiedliche Uploads nach Abzug bekannter Duplikate. Ergänzend wird die Uploadgrenze auf einen Upload gelockert; Intro und Outro bleiben auch dort verpflichtend. Beide Varianten verwenden dieselben verfügbaren Codierungen einschließlich der übernommenen Pretest-Dateien.",
  "",
  as.character(knitr::kable(threshold_basis, format = "pipe", row.names = FALSE)), "",
  "Ab_sieben bezeichnet die Hauptstichprobe; Alle die ergänzende Vergleichsgruppe ab einem Upload bei ebenfalls abgeschlossenem Intro und Outro. N zählt in der Beitragsebene gültige Beiträge, in den anderen Zeilen Personen. Wert enthält entweder einen Prozentwert oder Spearman-rho. Differenz ist Vergleichsgruppe minus Hauptstichprobe (Alle minus Ab_sieben): bei Anteilen in Prozentpunkten, bei Korrelationen in rho-Einheiten. Intro–Tagebuch korreliert den sechs Items umfassenden Intro-Index mit dem individuellen Zufalls- bzw. Ungeplant-Anteil.",
  "",
  as.character(knitr::kable(threshold_comparison,
    format = "pipe",
    digits = 3,
    row.names = FALSE
  )),
  "",
  "Für Thema, Quelle und Format zeigt die nächste Tabelle die größte absolute Veränderung eines Kategorieanteils über alle Kategorien. Grundlage sind jeweils öffentlich relevante Beiträge mit gültigem Code. Die vollständigen Verteilungen stehen in Uploadschwelle_Kategorien.csv.",
  "",
  as.character(knitr::kable(category_max, format = "pipe", digits = 3, row.names = FALSE)), "",
  "Wichtig: Diese Prüfung betrifft die Uploadschwelle innerhalb des verfügbaren Hauptcodings. Sie zeigt nicht, welche Ergebnisse zusätzliche, bislang nicht im Hauptcoding enthaltene Uploads liefern würden. Personenanteile aus wenigen Beiträgen bleiben unsicher; die Mindestzahl je Plattform oder Vergleichssituation bei gepaarten Analysen ist eine eigenständige, weiterhin explizit ausgewiesene Analyseregel.",
  ""
)
