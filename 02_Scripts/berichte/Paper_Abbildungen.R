# Abbildungen und Erläuterungen aus den Ergebnissen der Paper-Analyse.
# Keine neuen Analysen. Benötigt ggplot2, knitr und die Ergebnisobjekte.
paper_dir <- "03_Output/Paper_Grafiken"
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

# Auch nach Verkleinerung auf Manuskriptbreite lesbare Beschriftungen.
paper_labels <- theme(axis.text = element_text(size = 13),
  axis.title = element_text(size = 13), strip.text = element_text(size = 13),
  plot.caption = element_text(size = 12))

paper_topic_source_report <- c("### Themen und Quellen gemeinsam", "",
  "**Zweck und Status:** Explorative Vertiefung von RQ1: Welche Quellentypen liefern die dokumentierten Beiträge zu einem Thema? Zellen enthalten n und den Anteil innerhalb des Themas. Beide Tabellen zusammen ergeben je Themenzeile 100 Prozent, abgesehen von Rundung. Keine Signifikanztests mit Screenshots als unabhängigen Fällen.", "",
  markdown_table(paper_source_a1), "", markdown_table(paper_source_a2), "",
  "Vollständige Häufigkeiten und Prozentwerte stehen im gemeinsamen Paper_Tabellen.xlsx. Restkategorien sind enthalten; Beiträge mit fehlendem Themen- oder Quellencode entfallen für diese Kreuztabelle.", "")

paper_context_report <- c("### Typischer Kontext und Tagebuch im Vergleich", "",
  "**Zweck und Status:** Nachträgliche Messdiagnostik zu RQ2a. Prüft die Übereinstimmung des typischen Kontextes im Intro mit dem individuellen Anteil häuslicher beziehungsweise alleiniger Nutzung im Tagebuch. Kein Theorietest. Pro Person ein Wertepaar.", "",
  "Intro: überwiegend zu Hause/allein = 1, gleich häufig = 0,5, überwiegend unterwegs/gemeinsam = 0. Die Zwischenstufe stellt die Rangordnung her und behauptet keinen gemessenen 50-Prozent-Anteil. Im Tagebuch entfallen fehlende Antworten und Weiß nicht mehr. Spearman mit Rangbindungen, p-Werte asymptotisch und zweiseitig, ohne Mehrfachkorrektur; N ist die Zahl gültiger Personenpaare.", "",
  markdown_table(context_correlations %>% mutate(p_raw = if_else(p_raw < .001, "< 0,001", formatC(p_raw, format = "f", digits = 3, decimal.mark = ",")))), "")

practice_code_report <- c("### Praxisangaben: Exportcodes statt leerer Zellen", "",
  "**Zweck und Status:** Technische Herkunftsprüfung und Korrektur der zuvor fälschlich als fehlend behandelten Praxisangaben. Der Abgleich erfolgt über Person und Originaldateiname direkt mit dem rohen Tagesfragebogen. Der Studienleiter hat am 20.10.2026 die Antwortdefinition bestätigt: −1 = Nichts davon. Bei drei −1-Codes werden alle drei Praktiken deshalb mit 0 ausgewertet; Originalcodes bleiben erhalten. Tatsächlich leere oder unbekannte Codes bleiben fehlend. Die allgemeine Funktion clean_binary wird nicht geändert.", "",
  markdown_table(practice_code_summary), "",
  sprintf("Bei %s von %s öffentlich relevanten Beiträgen wurde Nichts davon gewählt. Diese Antworten zählen jetzt zum gültigen Nenner. Originalcodes und Screenshot-IDs stehen im Tabellenblatt Praxis_Exportcodes von Paper_Tabellen.xlsx. Die GESIS-Dokumentation erlaubt frei beschriftete Ausweichoptionen mit negativen Codes; die konkrete Bedeutung −1 folgt unserer Fragebogenkonfiguration, nicht einer universellen Missing-Konvention.", nrow(practice_export_codes), nrow(practice_matches)), "",
  "[GESIS AppKit User Manual](https://appkit.gesis.org/manual/)", "")

content_plot <- bind_rows(lapply(names(paper_distributions), function(variable) {
  paper_distributions[[variable]] %>% mutate(Category = as.character(Category), Dimension = variable)
})) %>% mutate(Dimension = recode(Dimension, topic_coded = "Topics", source_coded = "Sources", media_format = "Formats")) %>%
  ggplot(aes(Percent, reorder(Category, Percent))) + geom_col(fill = "#276B87") +
  facet_wrap(~ Dimension, ncol = 1, scales = "free_y") +
  labs(x = "Valid posts (%)", y = NULL, title = "Topics, source types and formats")
ggsave(file.path(paper_dir, "Figure_1_Content.png"), content_plot, width = 12, height = 16, dpi = 220, bg = "white")

# Gleiche Themenfolge in allen Panels; Top drei je Begegnungsart hervorheben.
topic_order <- paper_distributions$topic_coded %>% arrange(Percent) %>% pull(Category) %>% as.character()
encounter_plot_data <- encounter_topics %>%
  group_by(Encounter) %>%
  mutate(Top_three = min_rank(desc(Percent)) <= 3,
    Topic = factor(as.character(Topic), levels = topic_order),
    Encounter = factor(Encounter, levels = c("Deliberate search", "Followed, no search", "Chance"))) %>%
  ungroup()
encounter_plot <- ggplot(encounter_plot_data, aes(Percent, Topic, fill = Top_three)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), hjust = -0.15, size = 3.5,
    colour = "#34424A") +
  facet_wrap(~ Encounter, nrow = 1) +
  scale_fill_manual(values = c(`FALSE` = "#C9DDE3", `TRUE` = "#276B87"), guide = "none") +
  scale_x_continuous(breaks = seq(0, 15, 5), expand = expansion(mult = c(0, 0.16))) +
  labs(x = "Posts within encounter type (%)", y = NULL) +
  theme_minimal(base_size = 13) + paper_labels +
  theme(axis.text.y = element_text(size = 12), panel.grid.major.y = element_blank(),
    axis.ticks.y = element_blank(), panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#EAF2F4", colour = NA),
    strip.text = element_text(colour = "#1F4658", face = "bold"))
ggsave(file.path(paper_dir, "Figure_1_Encounter_topics.png"), encounter_plot,
  width = 14, height = 8, dpi = 220, bg = "white")

# Allgemeiner Index und beitragsbezogene Berichte: eine Beobachtung je Person.
calibration_plot_data <- calibration %>% mutate(Definition = factor(Definition,
  levels = c("Zufall", "Ungeplant"), labels = c("Chance encounters", "Encounters without targeted search")))
calibration_labels <- correlation_table %>% mutate(Definition = factor(Auswertung,
  levels = c("Zufall", "Ungeplant"), labels = levels(calibration_plot_data$Definition)),
  Label = sprintf("Spearman rho = %.3f", Spearman_rho))
survey_diary_plot <- ggplot(calibration_plot_data, aes(incidentality_index, Anteil)) +
  geom_point(colour = "#276B87", alpha = .55, size = 2) +
  facet_wrap(~ Definition, nrow = 1,
    labeller = as_labeller(setNames(paste(calibration_labels$Definition,
      calibration_labels$Label, sep = "\n"), calibration_labels$Definition))) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = "General incidentalness index (initial survey)", y = "Diary share per participant (%)",
    caption = "Each point represents one participant (N = 105). Diary shares refer to submitted posts.") + paper_labels
ggsave(file.path(paper_dir, "Figure_2_Survey_Diary.png"), survey_diary_plot,
  width = 10, height = 5, dpi = 220, bg = "white")

# Drei Praktiken und drei Routen als kompakte Matrix mit direkten Prozentwerten.
practice_plot_data <- paper_practices %>% mutate(
  Practice = factor(Practice, levels = c("Interaction", "Research", "Viewing"),
    labels = c("Visible interaction", "Further information seeking", "Thorough reading / viewing")),
  Encounter = factor(Encounter, levels = c("Deliberate search", "Followed, no search", "Chance")))
practice_plot <- ggplot(practice_plot_data, aes(Encounter, Practice, fill = Percent)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.1f%%", Percent), colour = Percent > 55),
    fontface = "bold", size = 4.5) +
  scale_fill_gradient(low = "#EDF3F5", high = "#276B87", limits = c(0, 100),
    breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%"),
    name = "Mean share (%)") +
  scale_colour_manual(values = c(`FALSE` = "#243746", `TRUE` = "white"), guide = "none") +
  scale_x_discrete(labels = c("Deliberate search" = "Deliberate\nsearch",
    "Followed, no search" = "Followed account,\nno targeted search", "Chance" = "Chance")) +
  labs(x = NULL, y = NULL) + theme_minimal(base_size = 13) + paper_labels +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11), legend.position = "right")
ggsave(file.path(paper_dir, "Figure_3_Practices.png"), practice_plot,
  width = 10, height = 4.5, dpi = 220, bg = "white")
