# Explorationsbericht aus separat berechneten Zusatzanalysen.
# Aus dem Projekt-Hauptverzeichnis ausführen. Kein Überschreiben der Paper-Tabellen.
source("02_Scripts/03_Zusatzanalysen.R", encoding = "UTF-8", local = TRUE)

pacman::p_load(ggplot2, knitr)
figure_dir <- "03_Output/Exploration_Grafiken"
report_file <- "03_Output/Exploration.md"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
source("02_Scripts/berichte/Gestaltung.R", encoding = "UTF-8", local = TRUE)
theme_set(report_theme())
save_plot <- function(plot, filename, title, height = 5, width = 10) {
  ggsave(file.path(figure_dir, paste0(filename, ".png")), plot,
    width = width, height = height, dpi = 220, bg = "white"
  )
  paste0("![", title, "](Exploration_Grafiken/", filename, ".png)")
}

person_plot <- ggplot(person_long, aes(x = Anteil, y = Merkmal)) +
  geom_boxplot(outlier.shape = NA, width = .45, fill = "grey93", na.rm = TRUE) +
  geom_point(
    position = position_jitter(width = 0, height = .12, seed = 42),
    colour = "#276B87", alpha = .45, size = 1.6, na.rm = TRUE
  ) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    title = "Wie stark unterscheiden sich die Personen?", x = "Anteil je Person (%)", y = NULL,
    caption = "Jeder Punkt ist eine Person mit gültigen Angaben. Box: mittlere 50 %, Linie: Median."
  )
person_figure <- save_plot(person_plot, "personen", "Verteilungen der Personenanteile")

calibration_plot <- ggplot(calibration, aes(x = incidentality_index, y = Anteil)) +
  geom_point(aes(size = Begegnung_N), colour = "#276B87", alpha = .55, na.rm = TRUE) +
  facet_wrap(~Definition) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_y_continuous(limits = c(
    0,
    100
  )) +
  scale_size_area(
    max_size = 5,
    name = "Gültige Beiträge"
  ) +
  labs(
    title = "Passen Selbstbericht und Tagebuch zusammen?", x = "Incidentality-Index (Intro)",
    y = "Anteil je Person (%)"
  )
calibration_figure <- save_plot(calibration_plot, "intro_daily", "Intro und Tagebuch")

profile_figures <- list()
for (variable in names(profile_tables)) {
  plot_data <- profile_tables[[variable]] %>%
    mutate(Kategorie = factor(Kategorie, levels = rev(Kategorie)))
  title <- if (variable == "topic_coded_label") "Themen bei denselben Personen" else "Quellen bei denselben Personen"
  plot <- ggplot(plot_data, aes(y = Kategorie)) +
    geom_segment(aes(x = Facebook, xend = Instagram, yend = Kategorie), colour = "grey70") +
    geom_point(aes(x = Facebook, colour = "Facebook"), size = 2.8) +
    geom_point(aes(x = Instagram, colour = "Instagram"), shape = 17, size = 2.8) +
    scale_colour_manual(values = c(Facebook = "#555555", Instagram = "#276B87"), name = NULL) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_discrete(labels = function(x) str_wrap(x, 43)) +
    labs(
      title = title, x = "Mittlerer Anteil je Person und Plattform (%)", y = NULL,
      caption = paste(
        "Vergleich innerhalb derselben",
        unique(plot_data$Personen),
        "Personen. Keine Plattformeffekte ableitbar."
      )
    )
  profile_figures[[variable]] <- save_plot(plot,
    variable,
    title,
    height = .38 * nrow(plot_data) + 2
  )
}

practice_figures <- list()
for (variable in names(practice_tables)) {
  plot_data <- practice_tables[[variable]] %>%
    mutate(
      Anzeige = if_else(Personen >= 10, Personen_Prozent, NA_real_),
      Praxis = factor(Praxis, levels = c("Lesen", "Recherche", "Interaktion"))
    )
  title <- if (variable == "encounter") "Begegnungsart und Umgang mit Beiträgen" else "Format und Umgang mit Beiträgen"
  plot <- ggplot(plot_data, aes(x = Praxis, y = Gruppe, fill = Anzeige)) +
    geom_tile(colour = "white") +
    geom_text(aes(
      label = ifelse(is.na(Anzeige), "n < 10", sprintf("%.1f %%", Anzeige)),
      colour = Anzeige > 65
    ), size = 4) +
    scale_colour_manual(values = c("FALSE" = "#202020", "TRUE" = "white"), guide = "none") +
    scale_fill_gradient(
      low = "#F1F6F8",
      high = "#174D65",
      limits = c(
        0,
        100
      ),
      na.value = "grey85",
      name = "%"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, 28)) +
    labs(
      title = title, x = NULL, y = NULL,
      caption = "Mittlere Anteile je Person innerhalb der Gruppe. Die Personenzusammensetzung kann variieren."
    )
  practice_figures[[variable]] <- save_plot(plot, variable, title, height = 4)
}

day_plot <- ggplot(day_table, aes(x = study_day, y = Prozent, colour = Basis, linetype = Basis)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~Merkmal) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_colour_manual(values = c(
    "Alle verfügbaren Personen" = "grey60",
    "Angaben an allen 7 Tagen" = "#276B87"
  )) +
  labs(
    title = "Verlauf mit wechselnder und konstanter Personenbasis", x = "Studientag",
    y = "Mittlerer Anteil je Person (%)", colour = NULL, linetype = NULL
  )
day_figure <- save_plot(day_plot, "verlauf", "Verlauf über sieben Tage", height = 6)

source("02_Scripts/berichte/Exploration_Darstellung.R", encoding = "UTF-8", local = TRUE)

source("02_Scripts/berichte/Methoden_Text.R", encoding = "UTF-8", local = TRUE)

source("02_Scripts/berichte/Paper_Abbildungen.R", encoding = "UTF-8", local = TRUE)
# 09 Bericht: Agenda, Ergebnisse und Abbildungen ---------------------------------

largest_topic_difference <- profile_tables$topic_coded_label %>%
  slice_max(abs(Differenz_IG_minus_FB), n = 1, with_ties = FALSE)
largest_source_difference <- profile_tables$source_coded_label %>%
  slice_max(abs(Differenz_IG_minus_FB), n = 1, with_ties = FALSE)
research_by_encounter <- practice_tables$encounter %>% filter(Praxis == "Recherche")

report <- c(
  "# Exploration der Tagebuchstudie",
  "",
  paste(
    "Erstellt:",
    format(Sys.time(),
      "%Y-%m-%d",
      tz = "Europe/Berlin"
    )
  ),
  "",
  "## Agenda", "",
  "1. Unterschiede zwischen Personen sichtbar machen.",
  "2. Incidentality-Selbstbericht mit dem Tagebuch vergleichen.",
  "3. Themen mit zufälligen und ungeplanten Begegnungen gegenüberstellen",
  "4. Themen und Quellen auf Facebook und Instagram bei denselben Personen vergleichen",
  "5. Begegnungsart und Format mit Lesen, Recherche und Interaktion verbinden.",
  "6. Verlauf, fehlende Angaben und studienbedingte Suche prüfen.",
  "7. Bedürfnisse, Nutzungskontexte und Verarbeitungsmuster theoriegeleitet erkunden.",
  "8. Messqualität, Codierabdeckung und Machbarkeit prüfen.", "",
  "**Leseschlüssel:** Deskription beschreibt die Daten. Theoriegeleitete Exploration untersucht theoretisch begründete Zusammenhänge ohne vorab gerichtete Hypothese. Messdiagnostik und Robustheitsanalysen prüfen die Belastbarkeit der Befunde. Die Präregistrierung enthält keine gerichteten Hypothesen; die folgenden Analysen werden daher nicht als konfirmatorische Theorietests ausgewiesen.",
  "",
  "## Datenbasis", "", markdown_table(sample_counts), "",
  "Die Stichprobe und Codierungen entsprechen der Essentials-Auswertung: erfülltes Screening, abgeschlossenes Intro und Outro sowie mindestens sieben unterschiedliche Uploads. Alle Codierphasen werden berücksichtigt. Analysiert werden öffentlich relevante Uploads; fehlende Antworten zählen nicht als Nein.",
  "",
  "Die Analysen beschreiben Zusammenhänge. Sie schätzen keine kausalen Effekte. Personenanteile gewichten jede Person gleich; ihre Nenner können unterschiedlich groß sein.",
  "",
  "Der Intro-Daily-Vergleich ist in der Präregistrierung vorgesehen. Die konkreten Plattformvergleiche, Themenmodelle, Darstellungen und Sensitivitätsregeln wurden nach Sichtung der Daten festgelegt. Abschnitt 3 enthält explorative Signifikanztests mit Korrektur für Mehrfachvergleiche; die Kontextkorrelationen werden zusätzlich mit unbereinigten explorativen p-Werten berichtet.",
  "",
  "## Erste Befunde", "",
  topic_encounter_highlight,
  sprintf(
    "- Selbstbericht und Tagebuch: Spearman-rho = %.2f für Zufall und %.2f für Ungeplant. Ohne Zustimmung zur studienbedingten Suche: %.2f bzw. %.2f.",
    correlation_table$Spearman_rho[1], correlation_table$Spearman_rho[2],
    sensitivity_table$Spearman_rho[1], sensitivity_table$Spearman_rho[2]
  ),
  sprintf(
    "- Größter Themenunterschied im gepaarten Vergleich: %s (Instagram minus Facebook: %+.1f Prozentpunkte).",
    largest_topic_difference$Kategorie, largest_topic_difference$Differenz_IG_minus_FB
  ),
  sprintf(
    "- Größter Quellenunterschied: %s (Instagram minus Facebook: %+.1f Prozentpunkte).",
    largest_source_difference$Kategorie, largest_source_difference$Differenz_IG_minus_FB
  ),
  sprintf(
    "- Weiterführende Recherche: im Personenmittel %.1f %% bei gezielten und %.1f %% bei zufälligen Begegnungen. Die Gruppen umfassen unterschiedliche Personen.",
    research_by_encounter$Personen_Prozent[research_by_encounter$Gruppe == "Gezielt"],
    research_by_encounter$Personen_Prozent[research_by_encounter$Gruppe == "Zufällig"]
  ), "",
  "## 1. Unterschiede zwischen Personen", "",
  "**Zweck und Status:** Deskriptive Vertiefung von RQ2. Personenverteilungen zeigen, ob Gesamtmittelwerte große individuelle Unterschiede verdecken. Die konkrete Darstellung wurde nach Datensichtung gewählt.",
  "",
  "Zufall umfasst nur zufällige Begegnungen (Code 3). Ungeplant umfasst zusätzlich Beiträge gefolgter Accounts ohne gezielte Suche (Codes 2 und 3).",
  "",
  markdown_table(person_summary), "", person_figure, "",
  "## 2. Selbstbericht und Tagebuch", "",
  "**Zweck und Status:** Präregistrierter Abgleich zwischen allgemeinem Incidentality-Selbstbericht und protokollierten Begegnungen. Dies prüft die Übereinstimmung der Messzugänge, keine kausale Theorie. Die zwei Operationalisierungen und Spearman-Auswertung sind konkretisierte Analyseentscheidungen.",
  "",
  "Spearman-Korrelationen verwenden eine Beobachtung je Person. Die Anteile sind begrenzt und haben viele gleiche Werte. Beide Definitionen werden nebeneinander berichtet.",
  "",
  "Der Intro-Index enthält alle sechs Items; Item 5 ist umgepolt. Die grenznahe Omega-Lösung aus der Essentials-Auswertung bleibt bei der Interpretation zu berücksichtigen.",
  "",
  markdown_table(correlation_table), "", calibration_figure, "",
  topic_encounter_report,
  "## 4. Plattformprofile bei denselben Personen", "",
  "**Zweck und Status:** Explorative Vertiefung von RQ1: Themen- und Quellenprofile bei gleicher Personenbasis vergleichen. Der konkrete gepaarte Vergleich wurde nach Datensichtung festgelegt; er ist kein Theorietest und isoliert keinen Plattformeffekt.",
  "",
  markdown_table(platform_coverage),
  "",
  paste(
    "Vergleichsbasis:", length(paired_ids), "Personen mit mindestens", minimum_platform_posts,
    "öffentlich relevanten Beiträgen auf jeder der beiden Plattformen."
  ), "",
  "Je Person und Plattform werden Kategorieanteile berechnet. Anschließend werden diese über dieselben Personen gemittelt. Nicht beobachtete Kategorien zählen als null, fehlende Codes entfallen. Die Differenz ist Instagram minus Facebook in Prozentpunkten.",
  "",
  "Die Auswahl beschreibt Personen mit Uploads auf beiden Plattformen. Sie ist nicht auf alle Teilnehmenden übertragbar. TikTok und X haben zu wenige Personen für eigene Vergleiche.",
  ""
)
for (variable in names(profile_tables)) {
  report <- c(
    report, if (variable == "topic_coded_label") "### Themen" else "### Quellen", "",
    markdown_table(profile_tables[[variable]]), "", profile_figures[[variable]], ""
  )
}
report <- c(
  report, "## 5. Umgang mit Beiträgen", "",
  "**Zweck und Status:** RQ2b/c explorativ verbinden. Die Unterscheidung von Auswahlpraktiken und sichtbarer Beteiligung folgt der Präregistrierung (Groot Kormelink; Kümpel; Oeldorf-Hirsch). Begegnungsart und Format dienen als Vergleichsmerkmale. Die konkreten Kreuztabellen sind nachträglich; sie testen keine gerichteten Hypothesen.",
  "",
  "Die Tabellen zeigen beide Gewichtungen. Die Grafiken zeigen mittlere Personenanteile je Gruppe. Gruppen mit weniger als zehn Personen bleiben in der Grafik ohne Prozentwert. Die Gruppen enthalten nicht zwingend dieselben Personen.",
  "",
  "Mehrfachantworten sind möglich: Lesen, Recherche und Interaktion ergeben zusammen nicht 100 Prozent.",
  ""
)
for (variable in names(practice_tables)) {
  report <- c(
    report, if (variable == "encounter") "### Begegnungsart" else "### Format", "",
    markdown_table(practice_tables[[variable]]), "", practice_figures[[variable]], ""
  )
}
report <- c(
  report,
  "## 6. Verlauf und Robustheit", "",
  "**Zweck und Status:** Nachträgliche Methodenprüfung auf Veränderungen im Studienverlauf und wechselnde Personenbasis. Zeitverläufe allein belegen weder Lernen noch Reaktivität. Die Teilstichprobe mit sieben Tagen ist nur ein Vergleich, kein Einschlusskriterium.",
  "",
  "Die blaue Linie verwendet je Merkmal nur Personen mit gültigen Angaben an allen sieben Tagen. Die graue Linie verwendet alle am jeweiligen Tag verfügbaren Personen. Änderungen können auch auf die Auswahl der Uploads zurückgehen.",
  "",
  day_figure, "", markdown_table(day_table), "",
  "### Fehlende Angaben zu Interaktionspraktiken", "",
  "**Zweck und Status:** Nachträgliche Methodenprüfung, ob Antwortausfälle im Studienverlauf zunehmen und Praxisverläufe mitprägen könnten. Die Tabelle verwendet fehlende Leseangaben als Indikator; die drei Praktiken werden in Abschnitt 10 getrennt geprüft.",
  "",
  markdown_table(missing_by_day), "",
  "### Sensitivität gegenüber studienbedingter Suche", "",
  "**Zweck und Status:** Prüfen, ob der Intro-Daily-Abgleich anders ausfällt, wenn Personen mit berichteter studienbedingter Suche entfallen. Nachträgliche Robustheitsanalyse zum präregistrierten Reaktivitätsmaß; die Ausschlussgrenze wurde nach Datensichtung festgelegt.",
  "",
  "Das Outro-Einzelitem erfasst gezieltes Suchen nach Beiträgen für die Studie. Die folgende Auswertung lässt Zustimmung (Antwort 4 oder 5) und fehlende Antworten weg. Sie ist eine Sensitivitätsanalyse; die Hauptstichprobe bleibt unverändert. Der Reaktivitätsindex wird wegen seiner unzulässigen Omega-Lösung nicht als Filter verwendet.",
  "",
  markdown_table(sensitivity_table), "",
  paper_context_report, practice_code_report, theory_report, quality_report,
  "## Verbleibende Arbeit", "",
  "Die laufend ausgewiesene Coding-Abdeckung bei Änderungen erneut prüfen. Die qualitative Erstcodierung fachlich prüfen. Schulung und Konfliktlösung der Inhaltscodierung dokumentieren. Schlussfolgerungen bleiben auf eingereichte und codierte Beiträge begrenzt.",
  "",
  "## Anhang: R-Version und Pakete", "", "```", capture.output(sessionInfo()), "```", ""
)
append_at <- which(grepl("^## Anhang", report))[1]
report <- append(report,
  upload_sensitivity_report,
  after = if (is.na(append_at)) length(report) else append_at - 1L
)
source("02_Scripts/berichte/Erlaeuterungen.R", encoding = "UTF-8", local = TRUE)
report <- explain_report(report, "exploration")
report <- style_report(report, "exploration")
writeLines(enc2utf8(report), report_file, useBytes = TRUE)
source("02_Scripts/berichte/PDF_erstellen.R", encoding = "UTF-8", local = TRUE)
render_report_pdf(report_file)
message("Explorationsbericht gespeichert: ", report_file)
