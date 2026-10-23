# Berichtstabellen, Abbildungen und Erläuterungen zur Exploration.
# Die statistischen Ergebnisse werden in 02_Paper_Analysen.R berechnet.
# Keine neue Modellschätzung; save_plot() kommt aus 02_Exploration.R.

write.csv(topic_encounter_counts, "03_Output/Themen_und_Begegnungsart.csv",
  row.names = FALSE, fileEncoding = "UTF-8")
write.csv(encounter_topic_profile, "03_Output/Themen_innerhalb_Begegnungsarten.csv",
  row.names = FALSE, fileEncoding = "UTF-8")

# 02 Beide Definitionen mit derselben Themenreihenfolge zeigen -------------------

topic_encounter_plot_data <- topic_encounter_table %>%
  mutate(Thema = factor(Thema, levels = rev(as.character(Thema)))) %>%
  pivot_longer(c(`Zufällig (%)`, `Ungeplant insgesamt (%)`),
    names_to = "Definition", values_to = "Prozent"
  )
topic_encounter_plot <- ggplot(topic_encounter_plot_data,
  aes(x = Prozent, y = Thema, colour = Definition, shape = Definition)
) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c("Zufällig (%)" = "#276B87",
    "Ungeplant insgesamt (%)" = "#B06A2B")) +
  scale_shape_manual(values = c("Zufällig (%)" = 16,
    "Ungeplant insgesamt (%)" = 17)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_y_discrete(labels = function(x) str_wrap(x, 37)) +
  labs(title = "Welche Themen werden zufällig entdeckt?",
    x = "Anteil innerhalb des jeweiligen Themas (%)", y = NULL,
    colour = NULL, shape = NULL,
    caption = "Jeder Screenshot zählt gleich. Ungeplant umfasst Zufall und gefolgte Accounts ohne gezielte Suche."
  ) +
  theme(legend.position = "bottom")
topic_encounter_figure <- save_plot(topic_encounter_plot, "themen_begegnungsart",
  "Zufällige und ungeplante Begegnungen nach Thema", height = 8, width = 10
)

# Dieselbe Themenreihenfolge und Prozentskala in allen Facetten erlauben den
# direkten Vergleich. Dunkel hervorgehoben werden die drei häufigsten Themen
topic_order <- topic_encounters %>%
  count(topic_coded_label, sort = TRUE) %>%
  pull(topic_coded_label) %>% as.character()
facet_labels <- setNames(
  paste0(c("Gezielt gesucht", "Gefolgter Account,\nohne gezielte Suche", "Zufällig entdeckt"),
    "\nn = ", profile_totals$Gesamt_Begegnungsart),
  as.character(profile_totals$Begegnungsart)
)
profile_plot_data <- encounter_topic_profile %>%
  mutate(Thema = factor(as.character(Thema), levels = rev(topic_order)),
    Beschriftung = paste0(formatC(Prozent, digits = 1, format = "f",
      decimal.mark = ","), " %"))
profile_plot <- ggplot(profile_plot_data,
  aes(x = Prozent, y = Thema, fill = Top_3)
) +
  geom_col(width = .7) +
  geom_text(aes(label = Beschriftung), hjust = -.12, size = 3.2,
    colour = "#263747") +
  facet_wrap(~ Begegnungsart, nrow = 1,
    labeller = as_labeller(facet_labels)) +
  scale_fill_manual(values = c("TRUE" = "#276B87", "FALSE" = "#C6D8E0"),
    guide = "none") +
  scale_y_discrete(labels = function(x) str_wrap(x, 36)) +
  scale_x_continuous(limits = c(0, max(profile_plot_data$Prozent) + 4),
    breaks = seq(0, 100, 5), expand = expansion(mult = c(0, 0))) +
  labs(title = "Welche Themen dominieren je Begegnungsart?",
    x = "Themenanteil innerhalb der jeweiligen Begegnungsart (%)", y = NULL,
    caption = "Je Facette ergeben alle Themen zusammen 100 %. Dunkel: die drei häufigsten Themen (einschließlich Gleichständen)."
  )
encounter_topic_profile_figure <- save_plot(profile_plot,
  "themen_innerhalb_begegnungsarten", "Themenverteilung innerhalb der drei Begegnungsarten",
  height = 8, width = 14)

profile_top_topics <- encounter_topic_profile %>%
  filter(Top_3) %>% arrange(incidentality_code, desc(Screenshots), Thema)
profile_interpretation <- vapply(1:3, function(code) {
  top_topics <- profile_top_topics %>% filter(incidentality_code == code)
  examples <- paste0(as.character(top_topics$Thema), " (",
    formatC(top_topics$Prozent, digits = 1, format = "f", decimal.mark = ","),
    " %; n = ", top_topics$Screenshots, ")")
  sprintf("**%s (n = %s):** Am häufigsten vertreten sind %s.",
    profile_totals$Begegnungsart[code], profile_totals$Gesamt_Begegnungsart[code],
    paste(examples, collapse = "; "))
}, character(1))
profile_table <- encounter_topic_profile %>%
  mutate(Anteil = paste0(formatC(Prozent, digits = 1, format = "f", decimal.mark = ","),
    " % (", Screenshots, ")")) %>%
  select(Thema, Begegnungsart, Anteil) %>%
  pivot_wider(names_from = Begegnungsart, values_from = Anteil)

# 04 Berichtstext: Variablen, Bezugsbasis und Grenzen explizit nennen ------------

# Kennwerte aus der Tabelle beziehen, damit spätere Datenänderungen auch die
# Zusammenfassung und die beispielhafte Interpretation aktualisieren
topic_percent <- function(topic_code, column = "Zufaellig_Prozent") {
  value <- topic_encounter_counts[[column]][
    as.character(topic_encounter_counts$Thema) ==
      as.character(daily_public$topic_coded_label[match(topic_code, daily_public$topic_coded)])
  ]
  if (length(value) != 1L) stop("Themenkategorie für Berichtstext fehlt: ", topic_code)
  formatC(value, format = "f", digits = 1, decimal.mark = ",")
}
topic_encounter_highlight <- sprintf(
  "- Zufällige Entdeckung nach Thema: Verkehr, Infrastruktur und Wohnen %s %%, Gesundheit und Pflege %s %% und Internationales, Krieg und Sicherheit %s %%; Veranstaltungen und öffentlicher Service %s %% – beobachtete Anteile innerhalb der Themen; statistische Prüfung in Abschnitt 3",
  topic_percent(9), topic_percent(6), topic_percent(2), topic_percent(14)
)

topic_encounter_report <- c(
  "## 3. Themen und zufällige Begegnungen", "",
  "**Fragestellung:** Werden Beiträge zu bestimmten Themen häufiger zufällig entdeckt als Beiträge zu anderen Themen?", "",
  "**Zweck und Status:** Nachträgliche explorative Verbindung von RQ1 und RQ2b. Gegenübergestellt werden die codierte Themenkategorie (topic_coded) und die von Teilnehmenden berichtete Begegnungsart (incidentality_code). Auf die beobachteten Anteile folgen logistische Mehrebenenmodelle zur statistischen Prüfung.", "",
  sprintf("**Datenbasis:** %s öffentlich relevante Screenshots von %s Personen mit gültigem Thema und gültiger Begegnungsart. %s öffentlich relevante Screenshots entfallen wegen fehlender Angaben zu mindestens einer dieser beiden Variablen.",
    nrow(topic_encounters), n_distinct(topic_encounters$participant),
    nrow(daily_public) - nrow(topic_encounters)), "",
  "### Welche Themen dominieren innerhalb der Begegnungsarten?", "",
  "**Diese Grafik beantwortet die Frage nach der Zusammensetzung:** Unter allen gezielt gesuchten beziehungsweise zufällig entdeckten Beiträgen – welche Themen kommen am häufigsten vor? Die mittlere Facette ergänzt Beiträge gefolgter Accounts, nach denen nicht gezielt gesucht wurde. Pro Facette ist der Nenner die Gesamtzahl der Screenshots dieser Begegnungsart; alle Themenanteile ergeben dort zusammen 100 %. Die drei häufigsten Themen sind dunkel hervorgehoben. Die Themenreihenfolge und die Achsenskalierung sind in allen Facetten gleich.", "",
  encounter_topic_profile_figure, "", profile_interpretation, "",
  "Die Tabelle zeigt je Begegnungsart den Themenanteil und in Klammern die Anzahl der Screenshots. Die Daten sind screenshotgewichtet; die Grafik stellt die Zusammensetzung der eingereichten Beiträge dar und prüft keine statistischen Unterschiede.", "",
  markdown_table(profile_table), "",
  "### Wie häufig wird ein bestimmtes Thema zufällig entdeckt?", "",
  "Hier wechselt die Bezugsbasis: Gefragt wird jetzt, wie sich die Begegnungsarten innerhalb eines bestimmten Themas verteilen. Ein Thema kann viele zufällige Begegnungen ausmachen, weil es insgesamt häufig dokumentiert wurde; das bedeutet noch nicht, dass sein Anteil zufälliger Begegnungen besonders hoch ist.", "",
  "**So ist die Tabelle zu lesen:** Der Nenner ist jeweils die Zahl aller gültigen Screenshots eines Themas. Zufällig meint ausschließlich Code 3. Ungeplant insgesamt umfasst Code 2 (gefolgter Account ohne gezielte Suche nach dem Beitrag) und Code 3. Die beiden Prozentspalten überlappen und dürfen nicht addiert werden. Personen zählt unterschiedliche Beitragende je Thema; dieselbe Person kann in mehreren Themen vorkommen.", "",
  markdown_table(topic_encounter_table), "", topic_encounter_figure, "",
  sprintf("**Einordnung:** Verkehr, Infrastruktur und Wohnen (%s %%), Gesundheit und Pflege (%s %%) sowie Internationales, Krieg und Sicherheit (%s %%) weisen relativ hohe Anteile zufälliger Begegnungen auf. Veranstaltungen und öffentlicher Service liegen bei %s %%. Gemeint ist jeweils der Anteil zufälliger Begegnungen innerhalb des Themas, nicht der Themenanteil unter allen zufälligen Begegnungen.",
    topic_percent(9), topic_percent(6), topic_percent(2), topic_percent(14)), "",
  sprintf("Die Definition macht einen Unterschied: Klima, Umwelt und Energie werden zu %s %% zufällig und zu %s %% ungeplant insgesamt gesehen. Bei Wetter und Naturereignissen sind es %s %% beziehungsweise %s %%. Der verbleibende Anteil entfällt auf gezielte Suche.",
    topic_percent(7), topic_percent(7, "Ungeplant_Prozent"),
    topic_percent(10), topic_percent(10, "Ungeplant_Prozent")), "",
  "**Grenzen der Rohanteile:** Dies sind screenshotgewichtete Beschreibungen der eingereichten Beiträge. Die Themen unterscheiden sich auch in den beteiligten Personen und möglicherweise den Plattformen und Quellen. Mehrere Screenshots derselben Person sind nicht unabhängig; besonders aktive Personen haben mehr Gewicht. Die Rohanteile allein belegen deshalb weder statistisch abgesicherte Themenunterschiede noch eine Wirkung des Themas. Die folgende Modellprüfung berücksichtigt die Gruppierung der Screenshots innerhalb von Personen.", "",
  "Die vollständigen Häufigkeiten aller drei Begegnungsarten je Thema stehen zusätzlich in Themen_und_Begegnungsart.csv.", ""
)

format_model_number <- function(value, digits = 2) {
  formatC(value, format = "f", digits = digits, decimal.mark = ",")
}
format_model_p <- function(value) {
  ifelse(value < .001, "< ,001", paste0("= ", format_model_number(value, 3)))
}

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

write.csv(profile_comparison, "03_Output/Themenprofile_vs_Gesamtsample.csv",
  row.names = FALSE, fileEncoding = "UTF-8"
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
  "Nach der Korrektur von −1 = Nichts davon sind im aktuellen Korpus alle drei Praxisangaben vollständig. Die folgende technische Prüfung bestätigt dies; Unter- und Obergrenzen fallen deshalb zusammen. Die frühere Interpretation als selektive Antwortausfälle entfällt.", "",
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
