# Themen und Begegnungsart: RQ1 mit RQ2b verbinden (explorativ).
# Benötigt: daily_public. Liefert Häufigkeiten, zwei Mehrebenenmodelle,
# Gesamttests, alle Themenpaare und Diagnostik. Keine Berichtserstellung.
# Die Modellentscheidungen wurden nach Datensichtung getroffen.

# 01 Gültige Beiträge und Nenner je Thema ---------------------------------------

topic_encounters <- daily_public %>%
  filter(!is.na(topic_coded_label), incidentality_code %in% 1:3)

topic_encounter_counts <- topic_encounters %>%
  group_by(Thema = topic_coded_label) %>%
  summarise(
    Screenshots = n(), Personen = n_distinct(participant),
    Gezielt_n = sum(incidentality_code == 1),
    Gefolgt_n = sum(incidentality_code == 2),
    Zufaellig_n = sum(incidentality_code == 3),
    .groups = "drop"
  ) %>%
  mutate(
    Zufaellig_Prozent = 100 * Zufaellig_n / Screenshots,
    Ungeplant_Prozent = 100 * (Gefolgt_n + Zufaellig_n) / Screenshots
  ) %>%
  arrange(desc(Zufaellig_Prozent))

stopifnot(
  sum(topic_encounter_counts$Screenshots) == nrow(topic_encounters),
  all(with(topic_encounter_counts,
    Gezielt_n + Gefolgt_n + Zufaellig_n == Screenshots)),
  all(topic_encounter_counts$Ungeplant_Prozent >= topic_encounter_counts$Zufaellig_Prozent)
)

topic_encounter_table <- topic_encounter_counts %>%
  transmute(
    Thema, Screenshots, Personen,
    `Zufällig (%)` = Zufaellig_Prozent,
    `Ungeplant insgesamt (%)` = Ungeplant_Prozent
  )

encounter_topic_profile <- topic_encounters %>%
  count(incidentality_code, Thema = topic_coded_label, name = "Screenshots") %>%
  complete(incidentality_code = 1:3, Thema, fill = list(Screenshots = 0)) %>%
  group_by(incidentality_code) %>%
  mutate(
    Gesamt_Begegnungsart = sum(Screenshots),
    Prozent = 100 * Screenshots / Gesamt_Begegnungsart,
    Top_3 = min_rank(desc(Screenshots)) <= 3
  ) %>%
  ungroup() %>%
  mutate(Begegnungsart = factor(incidentality_code, levels = 1:3,
    labels = c("Gezielt gesucht", "Gefolgter Account, ungeplant", "Zufällig entdeckt")))

profile_totals <- encounter_topic_profile %>%
  distinct(incidentality_code, Begegnungsart, Gesamt_Begegnungsart)
stopifnot(
  sum(encounter_topic_profile$Screenshots) == nrow(topic_encounters),
  all(abs(tapply(encounter_topic_profile$Prozent,
    encounter_topic_profile$incidentality_code, sum) - 100) < 1e-8)
)

# 01 Gemeinsame Datenbasis und gut lesbare Ausgabe ------------------------------

topic_model_data <- topic_encounters %>%
  transmute(
    participant = factor(participant),
    Thema = droplevels(topic_coded_label),
    Zufaellig = as.integer(incidentality_code == 3),
    Ungeplant = as.integer(incidentality_code %in% 2:3)
  )
# 02 Zwei Modelle und jeweils ein Nullmodell auf denselben Fällen ---------------

topic_models <- list()
topic_model_tests <- list()
topic_model_probabilities <- list()
topic_model_contrasts <- list()
topic_model_diagnostics <- list()

for (outcome in c("Zufaellig", "Ungeplant")) {
  # Der Personen-Interzept erlaubt unterschiedliche Grundwahrscheinlichkeiten
  # Alle Themen werden als Kategorien behandelt, ohne lineare Rangordnung
  full_formula <- as.formula(paste(outcome, "~ Thema + (1 | participant)"))
  null_formula <- as.formula(paste(outcome, "~ 1 + (1 | participant)"))
  model_control <- lme4::glmerControl(
    optimizer = "bobyqa", optCtrl = list(maxfun = 200000)
  )
  full_model <- lme4::glmer(full_formula, data = topic_model_data,
    family = binomial(link = "logit"), nAGQ = 9, control = model_control
  )
  null_model <- lme4::glmer(null_formula, data = topic_model_data,
    family = binomial(link = "logit"), nAGQ = 9, control = model_control
  )
  # Warnungen nicht stillschweigend als belastbare Ergebnisse weiterreichen
  for (model in list(full_model, null_model)) {
    convergence_messages <- model@optinfo$conv$lme4$messages
    if (length(convergence_messages) > 0L || model@optinfo$conv$opt != 0) {
      stop("Themenmodell nicht zuverlässig konvergiert: ", outcome, " ",
        paste(convergence_messages, collapse = "; "))
    }
  }
  model_comparison <- anova(null_model, full_model, test = "Chisq")
  topic_model_tests[[outcome]] <- tibble(
    Definition = outcome, Screenshots = nobs(full_model),
    Personen = n_distinct(topic_model_data$participant),
    Chi_Quadrat = model_comparison$Chisq[2],
    Freiheitsgrade = model_comparison$Df[2],
    p_roh = model_comparison$`Pr(>Chisq)`[2]
  )

  # 03 Modellwahrscheinlichkeiten für eine Person mit Interzeptabweichung null --
  # Keine über alle Personen gemittelten Wahrscheinlichkeiten
  # Wald-Intervalle auf der Logit-Skala berechnen, dann zurücktransformieren
  prediction_data <- data.frame(Thema = levels(topic_model_data$Thema))
  prediction_data$Thema <- factor(prediction_data$Thema,
    levels = levels(topic_model_data$Thema)
  )
  design_matrix <- model.matrix(~ Thema, prediction_data)
  coefficients <- lme4::fixef(full_model)
  covariance_matrix <- as.matrix(vcov(full_model))
  linear_predictor <- as.vector(design_matrix %*% coefficients)
  standard_error <- sqrt(rowSums((design_matrix %*% covariance_matrix) * design_matrix))
  topic_model_probabilities[[outcome]] <- tibble(
    Definition = outcome, Thema = as.character(prediction_data$Thema),
    Wahrscheinlichkeit = 100 * plogis(linear_predictor),
    KI_unten = 100 * plogis(linear_predictor - qnorm(.975) * standard_error),
    KI_oben = 100 * plogis(linear_predictor + qnorm(.975) * standard_error)
  )

  # 04 Alle Themenpaare: Kontraste auf der Logit-Skala --------------------------
  # Vergleich A minus B; exp(Differenz) ist die Odds Ratio A gegenüber B
  # Die Kovarianz beider Schätzungen muss in den Standardfehler eingehen
  topic_pairs <- combn(seq_len(nrow(prediction_data)), 2, simplify = FALSE)
  topic_model_contrasts[[outcome]] <- bind_rows(lapply(topic_pairs, function(pair) {
    contrast <- design_matrix[pair[1], ] - design_matrix[pair[2], ]
    difference <- sum(contrast * coefficients)
    contrast_se <- sqrt(as.numeric(t(contrast) %*% covariance_matrix %*% contrast))
    tibble(
      Definition = outcome,
      Thema_A = as.character(prediction_data$Thema[pair[1]]),
      Thema_B = as.character(prediction_data$Thema[pair[2]]),
      Log_Odds_Differenz = difference, Standardfehler = contrast_se,
      Odds_Ratio = exp(difference),
      p_roh = 2 * pnorm(-abs(difference / contrast_se))
    )
  }))
  random_sd <- as.numeric(attr(lme4::VarCorr(full_model)$participant, "stddev"))
  hessian <- full_model@optinfo$derivs$Hessian
  topic_model_diagnostics[[outcome]] <- tibble(
    Definition = outcome, Konvergiert = TRUE,
    Singulaer = lme4::isSingular(full_model), Personen_SD = random_sd,
    Hessian_positiv = min(eigen(hessian, symmetric = TRUE, only.values = TRUE)$values) > 0,
    LogLik_Null = as.numeric(logLik(null_model)),
    LogLik_Themen = as.numeric(logLik(full_model))
  )
  topic_models[[outcome]] <- list(full = full_model, null = null_model)
}

# 05 Mehrfachtests: zwei Gesamttests und alle 210 Paarvergleiche -----------------

topic_model_tests <- bind_rows(topic_model_tests) %>%
  mutate(p_Holm = p.adjust(p_roh, method = "holm"))
topic_model_contrasts <- bind_rows(topic_model_contrasts) %>%
  mutate(p_Holm = p.adjust(p_roh, method = "holm")) %>%
  arrange(p_Holm, p_roh)
topic_model_probabilities <- bind_rows(topic_model_probabilities)
topic_model_diagnostics <- bind_rows(topic_model_diagnostics)
stopifnot(
  all(topic_model_diagnostics$Hessian_positiv),
  !any(topic_model_diagnostics$Singulaer),
  all(topic_model_tests$Screenshots == nrow(topic_model_data)),
  all(topic_model_diagnostics$LogLik_Themen >= topic_model_diagnostics$LogLik_Null),
  all(topic_model_contrasts$p_Holm >= topic_model_contrasts$p_roh)
)
