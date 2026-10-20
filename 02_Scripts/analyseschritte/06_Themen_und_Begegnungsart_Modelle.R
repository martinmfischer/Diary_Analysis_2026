# ==============================================================================
# Themenunterschiede mit logistischer Mehrebenenregression prüfen
# ==============================================================================
# Aufgerufen nach 05_Themen_und_Begegnungsart.R im Explorations-Einstiegsskript
# Ziel: zufällige Entdeckung (primär), ungeplante Begegnung (ergänzend)
# Jede Zeile ist ein Screenshot; zufälliger Interzept je Person
# Alle Modellentscheidungen sind nachträglich und explorativ

if (!requireNamespace("lme4", quietly = TRUE)) {
  stop("Für die Themenmodelle bitte das R-Paket lme4 installieren")
}

# 01 Gemeinsame Datenbasis und gut lesbare Ausgabe ------------------------------

topic_model_data <- topic_encounters %>%
  transmute(
    participant = factor(participant),
    Thema = droplevels(topic_coded_label),
    Zufaellig = as.integer(incidentality_code == 3),
    Ungeplant = as.integer(incidentality_code %in% 2:3)
  )
format_model_number <- function(value, digits = 2) {
  formatC(value, format = "f", digits = digits, decimal.mark = ",")
}
format_model_p <- function(value) {
  ifelse(value < .001, "< ,001", paste0("= ", format_model_number(value, 3)))
}

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

model_exports <- list(
  Gesamttests = topic_model_tests, Wahrscheinlichkeiten = topic_model_probabilities,
  Paarvergleiche = topic_model_contrasts, Diagnostik = topic_model_diagnostics
)
for (export_name in names(model_exports)) {
  write.csv(model_exports[[export_name]],
    paste0("03_Output/Themenmodell_", export_name, ".csv"),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
}
saveRDS(topic_models, "03_Output/Themenmodelle.rds")

# 06 Verständliche Tabellen und datenabhängige Interpretation --------------------

model_test_table <- topic_model_tests %>%
  transmute(Definition, `Chi²` = Chi_Quadrat, df = Freiheitsgrade,
    `p (Holm)` = format_model_p(p_Holm))
model_probability_table <- topic_model_probabilities %>%
  mutate(Schaetzung = paste0(format_model_number(Wahrscheinlichkeit, 1), " [",
    format_model_number(KI_unten, 1), "; ", format_model_number(KI_oben, 1), "]")) %>%
  select(Thema, Definition, Schaetzung) %>%
  pivot_wider(names_from = Definition, values_from = Schaetzung) %>%
  rename(`Zufällig: % [95%-KI]` = Zufaellig, `Ungeplant: % [95%-KI]` = Ungeplant)

global_interpretation <- vapply(seq_len(nrow(topic_model_tests)), function(i) {
  result <- topic_model_tests[i, ]
  label <- if (result$Definition == "Zufaellig") "zufällige Entdeckung" else "ungeplante Begegnungen insgesamt"
  conclusion <- if (result$p_Holm < .05) {
    "Das Modell liefert Hinweise darauf, dass sich die Wahrscheinlichkeit zwischen den Themen unterscheidet"
  } else {
    "Das Modell liefert bei einer Schwelle von 5 % keinen statistisch abgesicherten Hinweis auf Themenunterschiede; gleiche Wahrscheinlichkeiten sind damit nicht bewiesen"
  }
  sprintf("**Gesamtergebnis für %s:** %s (Chi²(%s) = %s; Holm-korrigiertes p %s).",
    label, conclusion, result$Freiheitsgrade,
    format_model_number(result$Chi_Quadrat), format_model_p(result$p_Holm))
}, character(1))

significant_pairs <- topic_model_contrasts %>% filter(p_Holm < .05)
pair_interpretation <- if (nrow(significant_pairs) == 0L) {
  "**Einzelne Themenpaare:** Nach der Holm-Korrektur über alle Vergleiche beider Definitionen ist kein einzelnes Themenpaar statistisch abgesichert. Die Rangfolge der geschätzten Wahrscheinlichkeiten allein reicht deshalb nicht aus, um bestimmte Themen als zuverlässig häufiger zufällig entdeckt zu bezeichnen. Ein signifikanter Gesamttest und nicht signifikante Einzelvergleiche können gleichzeitig auftreten."
} else {
  sprintf("**Einzelne Themenpaare:** Anzahl statistisch auffälliger Vergleiche nach Holm-Korrektur: %s von %s. Die folgende Tabelle zeigt diese Vergleiche vollständig; OR größer als 1 bedeutet höhere Odds für Thema A gegenüber Thema B, OR kleiner als 1 niedrigere Odds. Odds sind das Verhältnis der Wahrscheinlichkeit eines Ja zu der eines Nein; eine OR von 2 bedeutet keine Verdoppelung der Wahrscheinlichkeit.",
    nrow(significant_pairs), nrow(topic_model_contrasts))
}
significant_pair_table <- significant_pairs %>%
  transmute(Definition, `Thema A` = Thema_A, `Thema B` = Thema_B,
    OR = Odds_Ratio, `p (Holm)` = format_model_p(p_Holm))

# Den stärksten abgesicherten Vergleich zusätzlich in Alltagssprache erläutern
pair_example <- if (nrow(significant_pairs) > 0L) {
  example <- significant_pairs[1, ]
  probabilities <- topic_model_probabilities %>%
    filter(Definition == example$Definition)
  probability_a <- probabilities$Wahrscheinlichkeit[probabilities$Thema == example$Thema_A]
  probability_b <- probabilities$Wahrscheinlichkeit[probabilities$Thema == example$Thema_B]
  outcome_label <- if (example$Definition == "Ungeplant") "ungeplant gesehen" else "zufällig entdeckt"
  sprintf("**Konkret bedeutet das:** Bei gleicher modellierter Grundneigung einer Person werden Beiträge zu %s mit einer geschätzten Wahrscheinlichkeit von %s %% %s; bei %s sind es %s %%. Die Differenz beträgt %s Prozentpunkte. Dieser Vergleich bleibt auch nach Korrektur aller %s Paarvergleiche auffällig (p %s). Die Prozentwerte beziehen sich auf die oben erläuterte Modellperson, nicht auf den beobachteten Durchschnitt der Stichprobe.",
    example$Thema_A, format_model_number(probability_a, 1), outcome_label,
    example$Thema_B, format_model_number(probability_b, 1),
    format_model_number(probability_a - probability_b, 1),
    nrow(topic_model_contrasts), format_model_p(example$p_Holm))
} else character()

model_probability_plot <- topic_model_probabilities %>%
  mutate(Thema = factor(Thema, levels = rev(levels(topic_model_data$Thema))),
    Definition = recode(Definition, Zufaellig = "Zufällig entdeckt",
      Ungeplant = "Ungeplant insgesamt")) %>%
  ggplot(aes(x = Wahrscheinlichkeit, y = Thema)) +
  geom_segment(aes(x = KI_unten, xend = KI_oben, yend = Thema),
    colour = "#7195A5", linewidth = .6) +
  geom_point(colour = "#276B87", size = 2.4) +
  facet_wrap(~ Definition, nrow = 1) +
  scale_y_discrete(labels = function(x) str_wrap(x, 35)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(title = "Themenunterschiede im Mehrebenenmodell",
    x = "Geschätzte Wahrscheinlichkeit (%)", y = NULL,
    caption = "Personen-Interzeptabweichung = 0; Linien: punktweise 95%-Konfidenzintervalle, ohne Mehrfachkorrektur.")
model_probability_figure <- save_plot(model_probability_plot,
  "themenmodell_wahrscheinlichkeiten", "Modellwahrscheinlichkeiten mit 95%-Konfidenzintervallen",
  height = 8, width = 11)

topic_model_report <- c(
  "### Statistische Prüfung: logistisches Mehrebenenmodell", "",
  "**Was wird erklärt?** Im primären Modell bedeutet Ja: zufällig entdeckt (Code 3); Nein: gezielte Suche oder gefolgter Account ohne gezielte Suche (Codes 1 und 2). Im ergänzenden Modell bedeutet Ja: ungeplant (Codes 2 und 3); Nein: gezielte Suche (Code 1). Die Themenkategorie ist in beiden Modellen die erklärende Variable.", "",
  sprintf("**Warum ein Mehrebenenmodell?** Die %s Screenshots stammen von %s Personen. Ein zufälliger Interzept je Person berücksichtigt, dass manche Personen grundsätzlich häufiger zufällige beziehungsweise ungeplante Begegnungen berichten und ihre Screenshots zusammengehören. Die Modellformel lautet jeweils Begegnungsart ~ Thema + (1 | Person), mit binomialer Verteilung und Logit-Link. Die Modelle sind explorativ und wurden nicht präregistriert.",
    nrow(topic_model_data), n_distinct(topic_model_data$participant)), "",
  "**Gesamttest:** Für jede Definition wird das Modell mit Themenkategorien mit einem Modell ohne Thema verglichen; beide enthalten den Personen-Interzept und dieselben Screenshots. Der Likelihood-Ratio-Test prüft, ob die Themen gemeinsam zur Erklärung beitragen. Die beiden p-Werte werden mit Holm korrigiert.", "",
  markdown_table(model_test_table), "", global_interpretation, "",
  "### Geschätzte Wahrscheinlichkeiten je Thema", "",
  "Die Tabelle übersetzt die Modellschätzungen in Prozent. Sie gilt für eine Person mit einer Interzeptabweichung von null, also einer durchschnittlichen Grundneigung auf der Logit-Skala. Sie zeigt weder die beobachteten Rohanteile noch über die gesamte Personenverteilung gemittelte Wahrscheinlichkeiten. Die Klammern enthalten punktweise 95%-Wald-Konfidenzintervalle; sie sind nicht für Mehrfachvergleiche korrigiert. Überschneidungen der Intervalle ersetzen keinen direkten Test zweier Themen.", "",
  markdown_table(model_probability_table), "", model_probability_figure, "",
  "**Warum weichen Modellwerte von den Rohanteilen ab?** Die Rohanteile mischen Personen mit unterschiedlichen Grundneigungen und unterschiedlich vielen Uploads. Die Modellwerte setzen die persönliche Interzeptabweichung dagegen auf null. Wegen der nichtlinearen Logit-Transformation ist dies kein Stichprobendurchschnitt; gerade bei ungeplanten Begegnungen können die Werte deshalb deutlich höher liegen. Verglichen werden sollten jeweils Themen innerhalb derselben Darstellung.", "",
  "### Welche Themen unterscheiden sich voneinander?", "",
  sprintf("Alle %s Themenpaare werden je Definition direkt verglichen. Die Wald-Tests verwenden die gemeinsame Kovarianzmatrix des Modells. Holm korrigiert gemeinsam über alle %s Tests beider Definitionen; dadurch soll die Vielzahl an Vergleichen nicht zu vorschnellen Einzelbefunden führen.",
    choose(nlevels(topic_model_data$Thema), 2), nrow(topic_model_contrasts)), "",
  pair_interpretation, "",
  if (nrow(significant_pairs) > 0L) markdown_table(significant_pair_table) else character(), "",
  pair_example, "",
  "**Was die Modelle nicht klären:** Die Themenunterschiede werden nicht zusätzlich um Plattform, Quelle oder Studientag bereinigt. Der Personen-Interzept trennt Unterschiede innerhalb derselben Person nicht vollständig von Unterschieden zwischen Personen. Die Modelle erlauben deshalb keine Aussage, dass das Thema selbst eine zufällige Begegnung verursacht. Auch die Auswahl hochgeladener Beiträge bleibt bestehen.", "",
  "**Modellprüfung:** Beide vollständigen Modelle und beide Nullmodelle sind ohne Konvergenzmeldungen geschätzt; die vollständigen Modelle sind nicht singulär und haben positiv definite Hesse-Matrizen. Schätzung mit lme4::glmer, neun adaptiven Quadraturpunkten; Konfidenzintervalle und Paarvergleiche sind asymptotische Näherungen. Die vollständigen Modellobjekte, alle Paarvergleiche und die Diagnostik werden unter Themenmodell_* beziehungsweise Themenmodelle.rds gespeichert.", "",
  "Methodendokumentation: [lme4::glmer](https://lme4.github.io/lme4/reference/glmer.html)", ""
)

topic_encounter_report <- c(topic_encounter_report, topic_model_report)
topic_encounter_highlight <- c(topic_encounter_highlight,
  sprintf("- Mehrebenenmodelle zu Themenunterschieden: Zufall p %s; ungeplant insgesamt p %s (jeweils Holm-korrigiert); %s abgesicherte Themenpaare nach gemeinsamer Korrektur aller Paarvergleiche",
    format_model_p(topic_model_tests$p_Holm[topic_model_tests$Definition == "Zufaellig"]),
    format_model_p(topic_model_tests$p_Holm[topic_model_tests$Definition == "Ungeplant"]),
    nrow(significant_pairs))
)
