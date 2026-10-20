# ==============================================================================
# Themenprofile je Begegnungsart mit dem gesamten Screenshot-Sample vergleichen
# ==============================================================================
# Benötigt: topic_encounters, encounter_topic_profile, topic_order, facet_labels,
#           save_plot(), markdown_table() aus dem Explorations-Einstiegsskript
# Modell: Thema ~ Begegnungsart (multinomial, ohne weitere Kovariaten)
# Unsicherheit: Personen-Cluster-Bootstrap mit gemeinsamer Maximum-Statistik

if (!requireNamespace("nnet", quietly = TRUE) ||
    !requireNamespace("boot", quietly = TRUE)) {
  stop("Für den Profilvergleich werden die R-Pakete nnet und boot benötigt")
}

# 01 Häufigkeiten nach Person, Thema und Begegnungsart --------------------------

profile_data <- topic_encounters %>%
  transmute(
    participant = factor(participant),
    Thema = droplevels(topic_coded_label),
    Begegnungsart = factor(incidentality_code, levels = 1:3)
  )
topic_levels <- levels(profile_data$Thema)
number_of_topics <- length(topic_levels)
number_of_routes <- nlevels(profile_data$Begegnungsart)
person_topic_counts <- xtabs(~ participant + Thema + Begegnungsart, data = profile_data)

# Eine Matrixzeile pro Person; je Begegnungsart folgen alle Themen aufeinander
# Damit wird im Bootstrap immer eine ganze Person samt allen Beiträgen gezogen
person_count_matrix <- matrix(as.numeric(person_topic_counts),
  nrow = dim(person_topic_counts)[1]
)

profile_from_counts <- function(count_vector) {
  counts <- matrix(count_vector, nrow = number_of_topics, ncol = number_of_routes)
  route_totals <- colSums(counts)
  if (any(route_totals == 0)) stop("Begegnungsart im Bootstrap nicht vertreten")
  route_probabilities <- sweep(counts, 2, route_totals, "/")
  overall_probabilities <- rowSums(counts) / sum(counts)
  # Gemeinsame Referenz wird aus genau demselben Sample neu berechnet
  differences <- sweep(route_probabilities, 1, overall_probabilities, "-")
  list(counts = counts, route_totals = route_totals,
    route = route_probabilities, overall = overall_probabilities,
    difference_pp = 100 * as.vector(differences))
}
observed_profiles <- profile_from_counts(colSums(person_count_matrix))

# 02 Multinomiales Modell und seine Referenzverteilung --------------------------
# Mit drei Begegnungsarten und ohne weitere Prädiktoren ist das Modell gesättigt:
# Seine geschätzten Themenanteile entsprechen den beobachteten Gruppenanteilen
# Die Referenz ist das nach Screenshotzahl gewichtete Mittel der drei Profile
route_model_data <- data.frame(Begegnungsart = factor(1:3))
topic_response_counts <- t(observed_profiles$counts)
colnames(topic_response_counts) <- topic_levels
profile_multinomial_model <- nnet::multinom(
  topic_response_counts ~ Begegnungsart, data = route_model_data,
  trace = FALSE, maxit = 2000, reltol = 1e-12
)
if (profile_multinomial_model$convergence != 0) stop("Profilmodell nicht konvergiert")
model_probabilities <- predict(profile_multinomial_model,
  newdata = route_model_data, type = "probs"
)
stopifnot(
  max(abs(t(model_probabilities) - observed_profiles$route)) < 1e-5,
  max(abs(colSums(observed_profiles$route) - 1)) < 1e-8,
  abs(sum(observed_profiles$overall) - 1) < 1e-8
)

# 03 Bootstrap auf Personenebene -----------------------------------------------
# Die geschlossene Lösung des gesättigten Modells ist in jeder Wiederholung
# identisch mit den Häufigkeitsanteilen; erneute Optimierung wäre überflüssig
profile_bootstrap_statistic <- function(data, indices) {
  profile_from_counts(colSums(data[indices, , drop = FALSE]))$difference_pp
}
bootstrap_repetitions <- 19999L
set.seed(20261016)
profile_bootstrap <- boot::boot(person_count_matrix,
  statistic = profile_bootstrap_statistic, R = bootstrap_repetitions
)
bootstrap_se <- apply(profile_bootstrap$t, 2, sd)
stopifnot(all(is.finite(profile_bootstrap$t)), all(bootstrap_se > 0))

# Gemeinsame Kalibrierung aller 45 Abweichungen, nicht 45 einzelne 95%-Intervalle
# Zentrierte Bootstrap-Abweichungen werden mit ihrem Bootstrap-SE standardisiert
# Pro Wiederholung wird die größte absolute Abweichung über alle Vergleiche genutzt
centered_bootstrap <- sweep(profile_bootstrap$t, 2, profile_bootstrap$t0, "-")
standardized_bootstrap <- sweep(centered_bootstrap, 2, bootstrap_se, "/")
bootstrap_maximum <- apply(abs(standardized_bootstrap), 1, max)
simultaneous_critical_value <- unname(quantile(bootstrap_maximum, .95, type = 1))
observed_statistics <- abs(profile_bootstrap$t0 / bootstrap_se)
adjusted_p <- vapply(observed_statistics, function(statistic) {
  (1 + sum(bootstrap_maximum >= statistic)) / (bootstrap_repetitions + 1)
}, numeric(1))

# 04 Ergebnistabelle und Plausibilitätskontrollen -------------------------------

profile_comparison <- tibble(
  incidentality_code = rep(1:3, each = number_of_topics),
  Thema = rep(topic_levels, times = number_of_routes),
  Teilgruppe_Prozent = 100 * as.vector(observed_profiles$route),
  Gesamtsample_Prozent = rep(100 * observed_profiles$overall, times = number_of_routes),
  Differenz_pp = profile_bootstrap$t0,
  Bootstrap_SE_pp = bootstrap_se,
  KI_unten = profile_bootstrap$t0 - simultaneous_critical_value * bootstrap_se,
  KI_oben = profile_bootstrap$t0 + simultaneous_critical_value * bootstrap_se,
  p_Maximum = adjusted_p
) %>%
  mutate(
    Begegnungsart = factor(incidentality_code, levels = 1:3,
      labels = levels(encounter_topic_profile$Begegnungsart)),
    Befund = case_when(
      KI_unten > 0 ~ "Häufiger vertreten",
      KI_oben < 0 ~ "Seltener vertreten",
      TRUE ~ "Nicht abgesichert"
    )
  )
stopifnot(
  max(abs(tapply(profile_comparison$Differenz_pp,
    profile_comparison$incidentality_code, sum))) < 1e-8,
  max(abs(profile_comparison$Differenz_pp -
    (profile_comparison$Teilgruppe_Prozent - profile_comparison$Gesamtsample_Prozent))) < 1e-8
)
write.csv(profile_comparison, "03_Output/Themenprofile_vs_Gesamtsample.csv",
  row.names = FALSE, fileEncoding = "UTF-8"
)
saveRDS(list(model = profile_multinomial_model, bootstrap = profile_bootstrap,
  critical_value = simultaneous_critical_value, results = profile_comparison),
  "03_Output/Themenprofile_vs_Gesamtsample.rds"
)

# 05 Drei Facetten: Abweichung vom Gesamtprofil in Prozentpunkten ----------------

baseline_labels <- setNames(paste0(str_wrap(topic_levels, 31), " (",
  formatC(100 * observed_profiles$overall, format = "f", digits = 1, decimal.mark = ","),
  " %)"), topic_levels)
comparison_plot_data <- profile_comparison %>%
  mutate(Thema = factor(Thema, levels = rev(topic_order)))
comparison_plot <- ggplot(comparison_plot_data,
  aes(x = Differenz_pp, y = Thema, colour = Befund)
) +
  geom_vline(xintercept = 0, colour = "#526575", linetype = "dashed") +
  geom_segment(aes(x = KI_unten, xend = KI_oben, yend = Thema), linewidth = .65) +
  geom_point(size = 2.4) +
  facet_wrap(~ Begegnungsart, nrow = 1, labeller = as_labeller(facet_labels)) +
  scale_y_discrete(labels = baseline_labels) +
  scale_colour_manual(values = c("Häufiger vertreten" = "#276B87",
    "Seltener vertreten" = "#B06A2B", "Nicht abgesichert" = "#9DA9AF"),
    drop = FALSE) +
  labs(title = "Welche Themen weichen vom gesamten Screenshot-Sample ab?",
    subtitle = "Punkte: Teilgruppenanteil minus Gesamtanteil · Null: gleiche Themenhäufigkeit",
    x = "Abweichung vom Gesamtprofil (Prozentpunkte)", y = NULL, colour = NULL,
    caption = "Linien: simultane approximative 95%-Intervalle für alle 45 Vergleiche\nPersonen-Cluster-Bootstrap (19.999 Ziehungen) · Themenlabels: Anteil im Gesamtsample")
profile_comparison_figure <- save_plot(comparison_plot,
  "themenprofile_vs_gesamtsample", "Themenprofile gegenüber dem gesamten Screenshot-Sample",
  height = 8.5, width = 14)

# 06 Datenabhängige Interpretation ---------------------------------------------

significant_profile_deviations <- profile_comparison %>%
  filter(Befund != "Nicht abgesichert") %>%
  arrange(incidentality_code, desc(abs(Differenz_pp)))
profile_findings <- if (nrow(significant_profile_deviations) == 0L) {
  "Nach gemeinsamer Berücksichtigung aller 45 Vergleiche ist keine einzelne Themenabweichung statistisch abgesichert. Das beweist keine Gleichheit der Profile; die Intervalle zeigen die mit diesen Daten verbleibende Unsicherheit."
} else {
  vapply(seq_len(nrow(significant_profile_deviations)), function(i) {
    result <- significant_profile_deviations[i, ]
    sprintf("**%s – %s:** %s %% in der Teilgruppe gegenüber %s %% im Gesamtsample; Differenz %s Prozentpunkte, simultanes 95%%-Intervall [%s; %s], korrigiertes p %s.",
      result$Begegnungsart, result$Thema,
      format_model_number(result$Teilgruppe_Prozent, 1),
      format_model_number(result$Gesamtsample_Prozent, 1),
      format_model_number(result$Differenz_pp, 1),
      format_model_number(result$KI_unten, 1), format_model_number(result$KI_oben, 1),
      format_model_p(result$p_Maximum))
  }, character(1))
}
profile_comparison_report <- c(
  "### Welche Themen sind gegenüber dem Gesamtsample über- oder unterrepräsentiert?", "",
  "**Referenz und Fragestellung:** Referenz sind alle 3.074 öffentlich relevanten Screenshots unserer 105 eingeschlossenen Personen. Für jedes Thema wird der Anteil im gesamten Screenshot-Sample vom entsprechenden Anteil innerhalb einer Begegnungsart abgezogen: Teilgruppenanteil minus Gesamtanteil. Positive Werte bedeuten häufiger, negative Werte seltener vertreten. Grundgesamtheit meint hier diese gemeinsame Datenbasis, nicht alle älteren Menschen oder deren gesamten Social-Media-Konsum.", "",
  "**So liest man die Grafik:** Die drei Facetten entsprechen gezielter Suche, gefolgten Accounts ohne gezielte Suche und zufälliger Entdeckung. Die gestrichelte Null-Linie steht für dieselbe Themenzusammensetzung wie im Gesamtsample. Blau beziehungsweise orange markiert sind Abweichungen, deren simultanes Intervall vollständig über beziehungsweise unter null liegt. Graue Intervalle lassen null zu. Eine hohe Themenhäufigkeit allein bedeutet keine Überrepräsentation.", "",
  profile_comparison_figure, "",
  "**Statistische Absicherung:** Verwendet wird ein multinomiales Themenmodell (Thema ~ Begegnungsart). Ohne weitere Kovariaten stimmen seine geschätzten Themenanteile mit den beobachteten Anteilen überein. Die Gesamtverteilung ist das nach Screenshotzahl gewichtete Mittel der drei Profile. Zur Unsicherheitsschätzung werden 19.999-mal ganze Personen mit Zurücklegen gezogen, jeweils einschließlich aller ihrer Screenshots. Teilgruppen- und Gesamtanteile werden in jeder Wiederholung gemeinsam neu berechnet. Damit berücksichtigt die Auswertung sowohl die Abhängigkeit der Screenshots innerhalb einer Person als auch die Überschneidung zwischen Teilgruppe und Gesamtsample.", "",
  "Die Intervalle und korrigierten p-Werte verwenden eine gemeinsame standardisierte Maximum-Statistik über alle 45 Vergleiche (15 Themen × 3 Begegnungsarten). Es handelt sich um simultane approximative 95%-Bootstrapintervalle. Das ist eine zusätzliche explorative Methodenentscheidung nach Datensichtung. Die Absicherung gilt für diese Vergleichsfamilie, nicht gemeinsam mit sämtlichen anderen Tests des Berichts.", "",
  profile_findings, "",
  "**Abgrenzung zu den logistischen Modellen:** Hier wird die Themenzusammensetzung einer Begegnungsart mit dem Gesamtprofil verglichen. Die logistischen Modelle prüfen dagegen, wie wahrscheinlich eine bestimmte Begegnungsart bei einem Thema ist. Beide Fragen hängen zusammen, haben aber unterschiedliche Nenner und Vergleichsmaßstäbe. Aus unterschiedlichen Signifikanzentscheidungen folgt deshalb kein Widerspruch.", "",
  "**Grenzen:** Die Schätzungen sind screenshotgewichtet und nicht um Plattform, Quelle oder Studientag bereinigt. Das Resampling berücksichtigt die Personenstruktur, beseitigt aber keine selektive Auswahl eingereichter Beiträge und ersetzt keine repräsentative Stichprobe. Ein Vergleich mit dem Gesamtprofil belegt auch nicht automatisch einen Unterschied zwischen jedem Paar der drei Begegnungsarten.", "",
  "Alle 45 Schätzungen, Intervalle und korrigierten p-Werte stehen in Themenprofile_vs_Gesamtsample.csv; Modell und Bootstrap-Ziehungen werden als RDS gespeichert. Methodendokumentation: [nnet::multinom](https://stat.ethz.ch/R-manual/R-devel/library/nnet/html/multinom.html) und [boot::boot](https://stat.ethz.ch/R-manual/R-devel/library/boot/html/boot.html)", ""
)

# Direkt hinter die deskriptive Zusammensetzung und vor die andere Bezugsfrage
profile_insert_at <- match("### Wie häufig wird ein bestimmtes Thema zufällig entdeckt?",
  topic_encounter_report)
stopifnot(!is.na(profile_insert_at))
topic_encounter_report <- append(topic_encounter_report, profile_comparison_report,
  after = profile_insert_at - 1L)
topic_encounter_highlight <- c(topic_encounter_highlight,
  sprintf("- Gegenüber dem gesamten Screenshot-Sample sind %s von 45 Themenabweichungen innerhalb der drei Begegnungsarten statistisch abgesichert (simultane Personen-Cluster-Bootstrapintervalle)",
    nrow(significant_profile_deviations)))
