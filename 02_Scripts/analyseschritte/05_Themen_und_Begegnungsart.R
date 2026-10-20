# ==============================================================================
# Themen und Begegnungsart: Anteile innerhalb der jeweiligen Themenkategorie
# ==============================================================================
# Aufgerufen durch 03_Explorative_Auswertung.R
# Benötigt daily_public, save_plot() und markdown_table()
# Deskriptive Grundlage; die inferenzstatistische Prüfung folgt in Schritt 06

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
write.csv(topic_encounter_counts, "03_Output/Themen_und_Begegnungsart.csv",
  row.names = FALSE, fileEncoding = "UTF-8"
)

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

# 03 Themenzusammensetzung innerhalb der drei Begegnungsarten -------------------
# Hier wird die umgekehrte Frage beantwortet: Welche Themen machen eine
# Begegnungsart aus? Der Nenner ist jetzt die Begegnungsart, nicht das Thema

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
write.csv(encounter_topic_profile, "03_Output/Themen_innerhalb_Begegnungsarten.csv",
  row.names = FALSE, fileEncoding = "UTF-8"
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
