# Themenprofile der drei Begegnungsarten mit dem Gesamtprofil vergleichen.
# Benötigt: topic_encounters und encounter_topic_profile aus dem vorherigen Schritt.
# Liefert: profile_comparison, multinomiales Modell und Personen-Bootstrap.
# Explorativ; 19.999 Ziehungen, gemeinsame Korrektur über alle 45 Vergleiche.
# Der Bootstrap zieht ganze Personen, damit ihre Posts zusammenbleiben.

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
