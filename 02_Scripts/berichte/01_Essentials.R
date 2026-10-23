# Essentials-Bericht aus Paper-Ergebnissen und Methodenprüfungen.
# Aus dem Projekt-Hauptverzeichnis ausführen. Erst Analyse, dann Darstellung.
source("02_Scripts/02_Paper_Analysen.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/zusatz/01_Messqualitaet.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/zusatz/02_Datenqualitaet.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/zusatz/03_Uploadschwelle.R", encoding = "UTF-8", local = TRUE)

pacman::p_load(ggplot2, knitr)
source("02_Scripts/berichte/Gestaltung.R", encoding = "UTF-8", local = TRUE)
report_file <- "03_Output/Analyse_Essentials.md"
figure_dir <- "03_Output/Analyse_Essentials_Grafiken"
source("02_Scripts/berichte/Methoden_Text.R", encoding = "UTF-8", local = TRUE)

# 12 Grafiken für den Bericht --------------------------------------------------

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
theme_set(report_theme())

# Relative Bildpfade halten Bericht und Grafikordner gemeinsam verschiebbar.
save_figure <- function(plot, filename, title, height = 5) {
  ggsave(file.path(figure_dir, paste0(filename, ".png")), plot,
    width = 10, height = height, units = "in", dpi = 220, bg = "white"
  )
  paste0("![", title, "](Analyse_Essentials_Grafiken/", filename, ".png)")
}

plot_bars <- function(data, category, value, filename, title, axis_label,
                      percent = FALSE) {
  plot_data <- data %>%
    transmute(category = as.character(.data[[category]]), value = .data[[value]]) %>%
    mutate(category = factor(category, levels = rev(unique(category))))

  plot <- ggplot(plot_data, aes(x = value, y = category)) +
    geom_col(fill = "#276B87", width = .7) +
    scale_y_discrete(labels = function(x) str_wrap(x, 42)) +
    labs(title = title, x = axis_label, y = NULL)

  if (percent) plot <- plot + scale_x_continuous(limits = c(0, NA))
  save_figure(plot, filename, title, height = max(3, .38 * nrow(data) + 1.5))
}

# Zwei Gewichtungen in einer Grafik; Kategorien folgen der Screenshot-Häufigkeit.
daily_plot_variables <- c("Thema", "Quelle", "Format", "Plattform", "Begegnungsart")
daily_figures <- list()

for (label in daily_plot_variables) {
  comparison <- daily_screenshot_tables[[label]] %>%
    transmute(Kategorie = as.character(Kategorie), Screenshot = Prozent) %>%
    left_join(daily_person_tables[[label]] %>%
      transmute(
        Kategorie = as.character(Kategorie),
        Person = Mittlerer_Anteil_Prozent
      ), by = "Kategorie") %>%
    arrange(Screenshot) %>%
    mutate(Kategorie = factor(Kategorie, levels = Kategorie))

  comparison_long <- comparison %>%
    pivot_longer(c(Screenshot, Person), names_to = "Gewichtung", values_to = "Prozent")
  comparison_plot <- ggplot(comparison_long, aes(x = Prozent, y = Kategorie)) +
    geom_segment(
      data = comparison, aes(x = Screenshot, xend = Person, yend = Kategorie),
      colour = "grey70"
    ) +
    geom_point(aes(colour = Gewichtung, shape = Gewichtung), size = 3) +
    scale_colour_manual(values = c(Screenshot = "#276B87", Person = "#555555")) +
    scale_shape_manual(values = c(Screenshot = 16, Person = 1)) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_discrete(labels = function(x) str_wrap(x, 42)) +
    labs(
      title = label, x = "Anteil (%)", y = NULL,
      caption = "Screenshot: Anteil aller gültigen Beiträge. Person: mittlerer Anteil je Person."
    )
  daily_figures[[label]] <- save_figure(
    comparison_plot, paste0("daily_", label, "_vergleich"),
    label, max(3.5, .4 * nrow(comparison) + 2)
  )
}

# Gleiche Farbskala für alle Antwortverteilungen; leere Zellen sind nicht definiert.
plot_responses <- function(data, item, filename, title) {
  item_labels <- unique(as.character(data[[item]])) %>%
    str_replace("^intro_freq_", "") %>%
    str_replace("^intro_intensity$", "Nutzungsintensität") %>%
    str_replace("^intro_ib_", "Bedürfnis: ") %>%
    str_replace("^intro_incidentality_", "Incidentality ") %>%
    str_replace("^outro_reactivity_", "Reaktivität ") %>%
    str_replace("^outro_ease_", "Benutzerfreundlichkeit ")
  names(item_labels) <- unique(as.character(data[[item]]))
  plot_data <- data %>%
    mutate(
      Itemname = factor(.data[[item]], levels = rev(unique(.data[[item]]))),
      Antwort = factor(Kategorie, levels = unique(Kategorie))
    )
  plot <- ggplot(plot_data, aes(x = Antwort, y = Itemname, fill = Prozent)) +
    geom_tile(colour = "white") +
    geom_text(aes(
      label = ifelse(is.na(Prozent), "–", sprintf("%.1f", Prozent)),
      colour = Prozent >= 70
    ), size = 3.3) +
    scale_colour_manual(
      values = c("FALSE" = "#202020", "TRUE" = "white"),
      guide = "none"
    ) +
    scale_fill_gradient(
      low = "#F1F6F8", high = "#174D65", limits = c(0, 100),
      na.value = "grey85", name = "%"
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, 13)) +
    scale_y_discrete(labels = item_labels) +
    labs(
      title = title, x = "Antwortkategorie", y = NULL,
      caption = "Prozent der gültigen Antworten je Item. Nicht vorgesehene Skalenstufen bleiben leer."
    )
  save_figure(plot, filename, title, height = max(4, .4 * n_distinct(plot_data$Itemname) + 2))
}

age_plot <- ggplot(intro, aes(x = age)) +
  geom_histogram(
    binwidth = 1, boundary = .5, fill = "#276B87",
    colour = "white", na.rm = TRUE
  ) +
  labs(title = "Altersverteilung", x = "Alter (Jahre)", y = "Personen")
age_figure <- save_figure(age_plot, "alter", "Altersverteilung")

incidentality_plot <- ggplot(intro, aes(x = incidentality_index)) +
  geom_histogram(
    binwidth = 1 / 6, boundary = 1 - 1 / 12,
    fill = "#276B87", colour = "white", na.rm = TRUE
  ) +
  scale_x_continuous(breaks = 1:5) +
  coord_cartesian(xlim = c(1 - 1 / 12, 5 + 1 / 12)) +
  labs(
    title = "Incidentality: Mittelwertindex", x = "Indexwert (1–5)", y = "Personen",
    caption = "Sechs Items; Item 5 umgepolt. Hohe Werte: stärkere Incidentality."
  )
incidentality_figure <- save_figure(
  incidentality_plot, "incidentality_index",
  "Incidentality: Mittelwertindex"
)

participation_long <- participation %>%
  pivot_longer(c(n_screenshots, n_active_days), names_to = "Mass", values_to = "Wert") %>%
  mutate(Mass = recode(Mass,
    n_screenshots = "Screenshots je Person",
    n_active_days = "Aktive Tage je Person"
  ))
participation_plot <- ggplot(participation_long, aes(x = Wert)) +
  geom_histogram(
    data = filter(participation_long, Mass == "Aktive Tage je Person"),
    binwidth = 1, boundary = .5, fill = "#276B87", colour = "white"
  ) +
  geom_histogram(
    data = filter(participation_long, Mass == "Screenshots je Person"),
    binwidth = 5, boundary = -.5, fill = "#276B87", colour = "white"
  ) +
  facet_wrap(~Mass, scales = "free", ncol = 1) +
  labs(
    title = "Teilnahmeumfang", x = NULL, y = "Personen",
    caption = "Klassenbreite: 1 Tag bzw. 5 Screenshots. Die Achsen sind je Teilgrafik skaliert."
  )
participation_figure <- save_figure(participation_plot, "teilnahme", "Teilnahmeumfang", 7)

day_plot <- participation_by_day %>%
  pivot_longer(c(Screenshots, Personen), names_to = "Mass", values_to = "Anzahl") %>%
  ggplot(aes(x = factor(study_day), y = Anzahl)) +
  geom_col(fill = "#276B87") +
  facet_wrap(~Mass, scales = "free_y", ncol = 1) +
  labs(title = "Teilnahme nach Studientag", x = "Studientag", y = "Anzahl")
day_figure <- save_figure(day_plot, "studientage", "Teilnahme nach Studientag", 7)

scale_plot <- outro %>%
  select(reactivity_index, ease_index) %>%
  pivot_longer(everything(), names_to = "Skala", values_to = "Wert") %>%
  mutate(Skala = recode(Skala,
    reactivity_index = "Reaktivität",
    ease_index = "Benutzerfreundlichkeit"
  )) %>%
  ggplot(aes(x = Wert)) +
  geom_histogram(
    binwidth = .25, boundary = .875, fill = "#276B87",
    colour = "white", na.rm = TRUE
  ) +
  facet_wrap(~Skala, ncol = 1) +
  scale_x_continuous(breaks = 1:5) +
  coord_cartesian(xlim = c(.875, 5.125)) +
  labs(title = "Verteilung der Skalenwerte", x = "Skalenwert (1–5)", y = "Personen")
scale_figure <- save_figure(scale_plot, "skalen", "Verteilung der Skalenwerte", 7)

source("02_Scripts/berichte/Paper_Abbildungen.R", encoding = "UTF-8", local = TRUE)

# 13 Markdown-Bericht zusammenstellen ------------------------------------------

input_files <- c(intro_file, daily_file, outro_file, diary_file, pretest_file)
data_sources <- tibble(Datei = input_files, MD5 = unname(tools::md5sum(input_files)))

report <- c(
  "# Deskriptive Analyse der Tagebuchstudie", "",
  paste("Erstellt:", format(Sys.time(), "%d.%m.%Y", tz = "Europe/Berlin")), "",
  "## 1. Datenbasis und Einschluss", "",
  "**Zweck und Status:** Die Analysestichprobe nachvollziehbar abgrenzen. Methodenprüfung anhand der präregistrierten Screening-Kriterien und Uploadgrenze; Abweichungen werden im Abschnitt zur Machbarkeit benannt.",
  "",
  "Die Analysen lesen die Rohdaten direkt. Einschluss: Screening-Kriterien erfüllt, Intro und Outro jeweils abgeschlossen (committed vorhanden) und mindestens sieben unterschiedliche Upload-Dateien nach Abzug bekannter Duplikate und vier ausdrücklich ausgeschlossener, historisch ungecodeter Dateien. Eine Mindestzahl an Uploadtagen wird nicht verlangt. Die Uploadgrenze zählt vor der Aufteilung auf Codierphasen und unabhängig von öffentlicher Relevanz.",
  "",
  paste0(
    "Die Analyse schließt Personen mit mindestens ", minimum_screenshots,
    " Uploads ein. Die Uploadzahl wird vor der Auswahl der codierten Beiträge bestimmt."
  ), "",
  "Die Einschlussliste ist gemeinsam; die verfügbare Personenbasis unterscheidet sich zwischen Intro, Daily und Outro. Fehlende Codes oder Antworten entfallen nur in der jeweiligen Auswertung. Die Abdeckung wird im Abschnitt zur Machbarkeit dokumentiert.",
  "",
  markdown_table(sample_flow), "",
  "### Eingabedateien", "", markdown_table(data_sources), "",
  "## 2. Stichprobe und Teilnahme", "",
  "**Zweck und Status:** Beschreiben, wer teilgenommen hat und wie umfangreich die Tagebücher sind. Präregistrierte Stichprobenbeschreibung und Grundlage zur Beurteilung der Machbarkeit und Übertragbarkeit.",
  "",
  "### Alter", "", markdown_table(age_summary), "",
  age_figure, "",
  "### Geschlecht", "", markdown_table(gender_distribution), "",
  "### Bildung", "", markdown_table(education_distribution), "",
  plot_bars(
    education_distribution, "Kategorie", "Prozent", "bildung", "Bildungsabschluss",
    "Gültige Antworten (%)", TRUE
  ), "",
  "### Teilnahmeumfang", "", markdown_table(participation_summary), "",
  "Der Teilnahmeumfang zählt Uploads aller eingeschlossenen Personen. Uploadtage folgen dem geplanten Datum des Tagesfragebogens. Die folgende Tabelle nach Studientag verwendet die kleinere Basis der codierten Screenshots.",
  "",
  participation_figure, "",
  markdown_table(participation_by_day), "",
  day_figure, "",
  "## 3. Intro-Fragebogen", "",
  "**Zweck und Status:** Nutzungshäufigkeit, Informationsbedürfnisse und allgemeine Incidentality als Ausgangslage beschreiben. Diese präregistrierten Merkmale dienen zur Einordnung der Tagebuchbefunde; Zusammenhänge werden im Explorationsbericht untersucht.",
  "",
  "Prozentwerte beziehen sich auf gültige Antworten. Die Antwortcodes entsprechen dem Fragebogen. Die Tabellen zeigen jedes Item einzeln.",
  "",
  "### Plattformnutzung", "", markdown_table(intro_platform_distribution), "",
  plot_responses(
    intro_platform_distribution, "Plattform", "plattformnutzung",
    "Häufigkeit der Plattformnutzung"
  ), "",
  "### Nutzungsintensität, Bedürfnisse und Incidentality", "",
  markdown_table(intro_item_summary), "",
  markdown_table(intro_item_distribution), "",
  plot_responses(
    intro_item_distribution,
    "Item",
    "intro_antworten",
    "Intro: Antwortverteilungen"
  ),
  "",
  "### Incidentality: Mittelwertindex", "",
  "**Zweck:** Präregistrierte Indexbildung und Prüfung der Messqualität. Die Reliabilitätsprüfung ist kein Theorietest; die Wahl des Omega-Modells erfolgte nach Datensichtung.",
  "",
  "Der Index ist der Mittelwert aus sechs Items. Item 5 wird mit 6 − x umgepolt. Der Index reicht von 1 bis 5. Hohe Werte bedeuten stärkere Incidentality. Der Index wird nur bei sechs gültigen Antworten berechnet.",
  "",
  markdown_table(incidentality_summary), "",
  incidentality_figure, "",
  markdown_table(incidentality_reliability_table %>%
    rename(
      N = Vollstaendige_Personen, Alpha = Cronbach_Alpha, Faktoren = Gruppenfaktoren,
      `Omega H` = Omega_Hierarchisch, `Omega total` = Omega_Total
    )), "",
  "Omega hierarchisch und Omega total beruhen auf drei Gruppenfaktoren. Der Modellstatus kennzeichnet unzulässige oder grenznahe Varianzen. Alle sechs Items bleiben im Index. Die Modellbeschreibung steht im Anhang.",
  "",
  "### Räumlicher Kontext", "", markdown_table(intro_local_context), "",
  "### Sozialer Kontext", "", markdown_table(intro_social_context), "",
  "## 4. Daily Coding", "",
  "**Zweck und Status:** Die präregistrierten Forschungsfragen deskriptiv beantworten: RQ1 betrifft Themen, Quellen und Formate; RQ2 betrifft Nutzungskontext, Begegnungsart sowie Auswahl- und Interaktionspraktiken. Werbung und Plattform ergänzen das Inhaltsprofil. Die beiden Gewichtungen zeigen, wie die Ergebnisse bei gleichem Gewicht je Beitrag bzw. je Person ausfallen.",
  "",
  "Die Ergebnisse beziehen sich auf die hochgeladenen Screenshots. Die Relevanzverteilung umfasst alle eingeschlossenen Screenshots. Die Inhaltsauswertung umfasst nur öffentlich relevante Beiträge (public_rel_coded = 1).",
  "",
  "**Screenshotgewichtung:** Jeder gültige Beitrag zählt einmal.", "",
  "**Personengewichtung:** Zuerst wird der Kategorieanteil je Person berechnet. Die Tabelle zeigt den Mittelwert dieser Anteile. Jede Person zählt gleich viel. Personen ohne gültige Angaben zum jeweiligen Merkmal entfallen.",
  "",
  "### Öffentliche Relevanz", "", markdown_table(relevance_distribution), "",
  markdown_table(relevance_rate), "",
  "### Datenqualität", "",
  "**Zweck und Status:** Die Anwendung der Relevanzregel und fehlende Inhaltscodes prüfen. Technische Methodenprüfung als Voraussetzung für die Inhaltsauswertung.",
  "",
  paste(
    "Beiträge mit Inhaltscodes trotz Relevanzcode ungleich 1:",
    nrow(content_outside_gate)
  ), "",
  "Diese Inhaltscodes gehen nicht in die Auswertung ein.", "",
  "Fehlende Inhaltscodes unter öffentlich relevanten Beiträgen:", "",
  markdown_table(daily_missing_codes), ""
)

for (label in names(daily_variables)) {
  report <- c(
    report,
    paste0("### ", gsub("_", " ", label)), "",
    "**Screenshotgewichtung**", "",
    markdown_table(daily_screenshot_tables[[label]]), "",
    "**Personengewichtung**", "",
    markdown_table(daily_person_tables[[label]]), "",
    if (label %in% daily_plot_variables) c(daily_figures[[label]], "") else NULL
  )
}

report <- c(
  report,
  paper_topic_source_report, paper_context_report, practice_code_report,
  "## 5. Outro-Fragebogen", "",
  "**Zweck und Status:** Das präregistrierte Machbarkeitsziel anhand der App-Bewertung und berichteter Reaktivität beurteilen. Reaktivitätsantworten erfassen wahrgenommene Veränderungen; sie messen keinen kausalen Studieneffekt.",
  "",
  "### Itemverteilungen", "",
  "Die Itemtabellen zeigen die ursprünglichen Antwortcodes.", "",
  markdown_table(outro_item_summary), "",
  markdown_table(outro_item_distribution), "",
  plot_responses(
    outro_item_distribution,
    "Item",
    "outro_antworten",
    "Outro: Antwortverteilungen"
  ),
  "",
  "### Vollständige Skalen", "",
  "Für die Reaktivitätsskala werden die Items 2, 3 und 5 mit 6 − x invertiert. Hohe Werte bedeuten stärkere Reaktivität. Bei der Benutzerfreundlichkeit bedeuten hohe Werte eine bessere Bewertung.",
  "",
  "Der Skalenwert ist der Mittelwert aller Items der jeweiligen Skala. Er wird nur bei vollständigen Antworten berechnet.",
  "",
  markdown_table(outro_scale_summary), "",
  scale_figure, "",
  "### Interne Konsistenz", "",
  "**Zweck:** Präregistrierte Prüfung der Messqualität von Reaktivität und Benutzerfreundlichkeit. Die Modellentscheidungen sind nachträglich dokumentiert; es werden keine Theoriehypothesen getestet.",
  "",
  "Omega hierarchisch und Omega total beziehen sich auf standardisierte Items. Cronbachs Alpha ist ergänzend angegeben. NA kennzeichnet eine unzulässige Omega-Lösung. Die Indizes enthalten weiterhin alle fünf bzw. acht Items.",
  "",
  markdown_table(outro_reliability %>%
    rename(
      N = Vollstaendige_Personen, Alpha = Cronbach_Alpha, Faktoren = Gruppenfaktoren,
      `Omega H` = Omega_Hierarchisch, `Omega total` = Omega_Total
    )), "",
  "### Offene Antworten", "",
  "Anzahl der Personen mit einer Freitextantwort:", "",
  markdown_table(open_response_counts), "",
  quality_report,
  "## Anhang: Omega-Modelle", "",
  "Die Präregistrierung nennt hierarchisches Omega, legt aber kein Faktorenmodell fest. Die folgende Modellwahl wurde nach Kenntnis der Daten getroffen.",
  "",
  "Berechnung mit psych::omega: vollständige Fälle, Pearson-Korrelationen, Minres-Schätzung und Oblimin-Rotation. Die festgelegte Itempolung bleibt erhalten. Incidentality und Benutzerfreundlichkeit verwenden drei Gruppenfaktoren.",
  "",
  "Bei fünf Reaktivitätsitems hat ein Modell mit drei Gruppenfaktoren negative Freiheitsgrade. Verwendet werden deshalb zwei Gruppenfaktoren mit gleichgesetzten Ladungen auf dem Generalfaktor (option = equal). Diese Zusatzannahme schränkt die Interpretation ein. Eine negative Restvarianz macht die Lösung unzulässig.",
  "",
  "Omega hierarchisch beschreibt den Generalfaktoranteil; Omega total berücksichtigt zusätzlich die Gruppenfaktoren. Beide Werte beziehen sich hier auf standardisierte Items. Die Mittelwertindizes verwenden die ursprüngliche Antwortskala.",
  "",
  "Die Präregistrierung erlaubt eine Itemlöschung bei Omega hierarchisch unter 0,70, sofern der Wert danach über 0,70 liegt. Sie verlangt keine Löschung. Hier werden alle Items beibehalten; unzulässige Schätzungen begründen keine Itemauswahl.",
  "",
  "[Methodendokumentation: psych::omega](https://www.personality-project.org/r/psych/help/omega.html)",
  "",
  "### Generalfaktorladungen und Restvarianzen", "",
  markdown_table(omega_diagnostics), "",
  "## Anhang: R-Version und Pakete", "",
  "```", capture.output(sessionInfo()), "```", ""
)

# 14 Bericht speichern ---------------------------------------------------------

dir.create("03_Output", showWarnings = FALSE)
append_at <- which(grepl("^## Anhang", report))[1]
report <- append(report,
  upload_sensitivity_report,
  after = if (is.na(append_at)) length(report) else append_at - 1L
)
source("02_Scripts/berichte/Erlaeuterungen.R", encoding = "UTF-8", local = TRUE)
report <- explain_report(report, "essentials")
report <- style_report(report, "essentials")
writeLines(enc2utf8(report), report_file, useBytes = TRUE)
source("02_Scripts/berichte/PDF_erstellen.R", encoding = "UTF-8", local = TRUE)
render_report_pdf(report_file)

message(
  "Bericht gespeichert: ", report_file,
  "\nPersonen: ", length(included_ids),
  "\nScreenshots: ", nrow(daily),
  "\nÖffentlich relevante Beiträge: ", nrow(daily_public)
)
