# ==============================================================================
# Tagebuchstudie: fokussierte Exploration
# ==============================================================================
# Aus dem Projekt-Hauptverzeichnis ausführen. Keine Änderung der Essentials.
# Agenda: Personen -> Selbstbericht -> Plattformen -> Praktiken -> Verlauf.

# 01 Pakete und Einstellungen ---------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, openxlsx, ggplot2, knitr, readr, lme4, nnet, boot)
source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8")

# Einschluss zentral in analyseschritte/01_Stichprobe.R: abgeschlossenes Intro und Outro sowie mindestens sieben Uploads
minimum_platform_posts <- 3
figure_dir <- "03_Output/Exploration_Grafiken"
report_file <- "03_Output/Exploration.md"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(20261013)

# 02 Daten und gemeinsame Stichprobe --------------------------------------------

source("02_Scripts/analyseschritte/01_Stichprobe.R", encoding = "UTF-8")
intro <- intro_raw
outro <- outro_answered
daily <- read.xlsx("06_Coding/coding_sheet.xlsx", sheet = "Coding")
codebook <- read.xlsx("06_Coding/coding_sheet.xlsx", sheet = "Codebook")

intro <- intro %>% mutate(participant = clean_text(personalParticipantCode))
outro <- outro %>% mutate(participant = clean_text(personalParticipantCode))
daily <- daily %>% mutate(participant = clean_text(participant))
stopifnot(
  !anyDuplicated(intro$participant), !anyDuplicated(outro$participant),
  !anyDuplicated(daily$screenshot_id), !anyNA(daily$participant)
)
if (any(is_marked_screenshot_duplicate(daily$notes), na.rm = TRUE)) {
  stop("Das Coding Sheet enthält markierte Duplikate.")
}

included_ids <- study_ids
intro <- intro %>% filter(participant %in% included_ids)
outro <- outro %>% filter(participant %in% included_ids)
daily <- daily %>% filter(participant %in% included_ids)
sample_counts <- tibble(
  Eingeschlossen = length(included_ids),
  Daily_Personen = n_distinct(daily$participant),
  Screenshots = nrow(daily),
  Oeffentlich_relevant = sum(daily$public_rel_coded == 1, na.rm = TRUE)
)

# Gleiche Polung und Vollständigkeitsregel wie in der deskriptiven Auswertung.
incidentality_items <- paste0("intro_incidentality_", 1:6)
intro <- intro %>% mutate(across(all_of(incidentality_items), ~ na_if(clean_numeric(.x), -1)))
scored_items <- incidentality_scores(intro)
intro$incidentality_index <- rowMeans(scored_items, na.rm = FALSE)
outro <- outro %>% mutate(study_search = na_if(clean_numeric(outro_reactivity_1), -1))

daily_public <- daily %>%
  filter(public_rel_coded == 1) %>%
  mutate(
    platform = coalesce(clean_text(platform_coded), clean_text(platform_reported)),
    encounter = factor(incidentality_code,
      levels = 1:3,
      labels = c("Gezielt", "Gefolgt, ungeplant", "Zufällig")
    ),
    incidental_strict = if_else(incidentality_code %in% 1:3,
      as.numeric(incidentality_code == 3), NA_real_
    ),
    incidental_broad = if_else(incidentality_code %in% 1:3,
      as.numeric(incidentality_code %in% 2:3), NA_real_
    ),
    read = clean_binary(interaction_read_code),
    research = clean_binary(interaction_research_code),
    engage = clean_binary(interaction_engagement_code)
  )

for (variable in c("topic_coded", "source_coded", "media_format")) {
  labels <- codebook %>% filter(Variable == variable, str_detect(as.character(Code), "^[0-9]+$"))
  stopifnot(
    !anyDuplicated(labels$Code),
    all(na.omit(daily_public[[variable]]) %in% labels$Code)
  )
  daily_public[[paste0(variable, "_label")]] <- factor(
    daily_public[[variable]],
    levels = labels$Code, labels = str_trim(labels$Category)
  )
}

# 03 Kleine Hilfsfunktionen für Bericht und Grafiken -----------------------------

source("02_Scripts/berichte/Gestaltung.R", encoding = "UTF-8")
theme_set(report_theme())
save_plot <- function(plot, filename, title, height = 5, width = 10) {
  ggsave(file.path(figure_dir, paste0(filename, ".png")), plot,
    width = width, height = height, dpi = 220, bg = "white"
  )
  paste0("![", title, "](Exploration_Grafiken/", filename, ".png)")
}
spearman_summary <- function(data, label) {
  complete <- data %>% filter(is.finite(incidentality_index), is.finite(Anteil))
  tibble(
    Auswertung = label, Personen = nrow(complete),
    Spearman_rho = safe_spearman(complete$incidentality_index, complete$Anteil)
  )
}

# 04 Personen: Verteilungen statt nur Gesamtmittelwerte -------------------------

person_data <- daily_public %>%
  group_by(participant) %>%
  summarise(
    Posts = n(), Begegnung_N = sum(!is.na(incidental_strict)),
    Zufall = safe_mean(incidental_strict), Ungeplant = safe_mean(incidental_broad),
    Lesen = safe_mean(read), Recherche = safe_mean(research),
    Interaktion = safe_mean(engage), .groups = "drop"
  ) %>%
  left_join(select(intro, participant, incidentality_index), by = "participant") %>%
  left_join(select(outro, participant, study_search), by = "participant")

person_long <- person_data %>%
  pivot_longer(c(Zufall, Ungeplant, Lesen, Recherche, Interaktion),
    names_to = "Merkmal", values_to = "Anteil"
  ) %>%
  mutate(Anteil = Anteil * 100)
person_summary <- person_long %>%
  group_by(Merkmal) %>%
  summarise(
    Personen = sum(!is.na(Anteil)), Mittelwert = safe_mean(Anteil),
    Median = safe_median(Anteil),
    Q1 = quantile(Anteil, .25, na.rm = TRUE), Q3 = quantile(Anteil, .75, na.rm = TRUE),
    .groups = "drop"
  )
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

# 05 Intro und Daily: präregistrierter Vergleich, zwei Definitionen --------------

calibration <- person_data %>%
  select(participant, incidentality_index, Begegnung_N, study_search, Zufall, Ungeplant) %>%
  pivot_longer(c(Zufall, Ungeplant), names_to = "Definition", values_to = "Anteil") %>%
  mutate(Anteil = 100 * Anteil)
correlation_table <- bind_rows(lapply(c("Zufall", "Ungeplant"), function(definition) {
  spearman_summary(filter(calibration, Definition == definition), definition)
}))
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

# 06 Plattformprofile: dieselben Personen auf Facebook und Instagram ------------

platform_coverage <- daily_public %>%
  group_by(platform) %>%
  summarise(Posts = n(), Personen = n_distinct(participant), .groups = "drop")
paired_ids <- daily_public %>%
  filter(platform %in% c("Facebook", "Instagram")) %>%
  count(participant, platform) %>%
  filter(n >= minimum_platform_posts) %>%
  count(participant) %>%
  filter(n == 2) %>%
  pull(participant)
paired_posts <- daily_public %>%
  filter(participant %in% paired_ids, platform %in% c("Facebook", "Instagram"))

profile_tables <- list()
profile_figures <- list()
for (variable in c("topic_coded_label", "source_coded_label")) {
  valid <- paired_posts %>% filter(!is.na(.data[[variable]]))
  # Je Merkmal nur Personen mit gültigen Codes auf beiden Plattformen.
  valid_ids <- valid %>%
    distinct(participant, platform) %>%
    count(participant) %>%
    filter(n == 2) %>%
    pull(participant)
  valid <- valid %>% filter(participant %in% valid_ids)
  categories <- levels(daily_public[[variable]])
  denominators <- valid %>% count(participant, platform, name = "N")
  counts <- valid %>%
    transmute(participant,
      platform,
      Kategorie = as.character(.data[[variable]])
    ) %>%
    count(participant, platform, Kategorie, name = "n")
  shares <- crossing(
    participant = valid_ids, platform = c("Facebook", "Instagram"),
    Kategorie = categories
  ) %>%
    left_join(denominators, by = c("participant", "platform")) %>%
    left_join(counts, by = c("participant", "platform", "Kategorie")) %>%
    mutate(Anteil = 100 * replace_na(n, 0) / N)
  profile_tables[[variable]] <- shares %>%
    group_by(Kategorie, platform) %>%
    summarise(Personen = n(), Anteil = mean(Anteil), .groups = "drop") %>%
    pivot_wider(names_from = platform, values_from = Anteil) %>%
    mutate(Differenz_IG_minus_FB = Instagram - Facebook) %>%
    arrange(desc(Differenz_IG_minus_FB))
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
        length(valid_ids),
        "Personen. Keine Plattformeffekte ableitbar."
      )
    )
  profile_figures[[variable]] <- save_plot(plot,
    variable,
    title,
    height = .38 * nrow(plot_data) + 2
  )
}

# 07 Begegnung und Format: Lesen, Recherche, Interaktion -------------------------

practice_tables <- list()
practice_figures <- list()
for (variable in c("encounter", "media_format_label")) {
  practice_long <- daily_public %>%
    transmute(participant,
      Gruppe = .data[[variable]], Lesen = read,
      Recherche = research, Interaktion = engage
    ) %>%
    filter(!is.na(Gruppe)) %>%
    pivot_longer(c(Lesen, Recherche, Interaktion), names_to = "Praxis", values_to = "Wert")
  pooled <- practice_long %>%
    group_by(Gruppe, Praxis) %>%
    summarise(
      Gueltige_Posts = sum(!is.na(Wert)), Fehlend = sum(is.na(Wert)),
      Screenshot_Prozent = 100 * safe_mean(Wert), .groups = "drop"
    )
  person_rates <- practice_long %>%
    filter(!is.na(Wert)) %>%
    group_by(participant, Gruppe, Praxis) %>%
    summarise(Anteil = mean(Wert), .groups = "drop")
  practice_tables[[variable]] <- person_rates %>%
    group_by(Gruppe, Praxis) %>%
    summarise(Personen = n(), Personen_Prozent = 100 * mean(Anteil), .groups = "drop") %>%
    left_join(pooled, by = c("Gruppe", "Praxis"))
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

# 08 Verlauf: gleiche Personenbasis über sieben Tage ----------------------------

day_person <- daily_public %>%
  group_by(participant, study_day) %>%
  summarise(
    Zufall = safe_mean(incidental_strict), Lesen = safe_mean(read),
    Recherche = safe_mean(research), Interaktion = safe_mean(engage),
    Fehlende_Interaktion = mean(is.na(read)), Posts = n(), .groups = "drop"
  ) %>%
  pivot_longer(c(Zufall, Lesen, Recherche, Interaktion), names_to = "Merkmal", values_to = "Anteil")
complete_week <- day_person %>%
  filter(!is.na(Anteil), study_day %in% 1:7) %>%
  count(participant, Merkmal) %>%
  filter(n == 7)
day_all <- day_person %>%
  group_by(study_day, Merkmal) %>%
  summarise(Personen = sum(!is.na(Anteil)), Prozent = 100 * safe_mean(Anteil), .groups = "drop") %>%
  mutate(Basis = "Alle verfügbaren Personen")
day_balanced <- day_person %>%
  semi_join(complete_week, by = c("participant", "Merkmal")) %>%
  group_by(study_day, Merkmal) %>%
  summarise(Personen = n(), Prozent = 100 * mean(Anteil), .groups = "drop") %>%
  mutate(Basis = "Angaben an allen 7 Tagen")
day_table <- bind_rows(day_all, day_balanced)
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
missing_by_day <- daily_public %>%
  group_by(study_day) %>%
  summarise(
    Posts = n(), Fehlend = sum(is.na(read)),
    Fehlend_Prozent = 100 * mean(is.na(read)), .groups = "drop"
  )

# Einzelitem statt des psychometrisch problematischen Reaktivitätsindex.
sensitivity_table <- bind_rows(lapply(c("Zufall", "Ungeplant"), function(definition) {
  spearman_summary(
    filter(calibration, Definition == definition, study_search <= 3),
    paste(definition, "ohne Zustimmung zur studienbedingten Suche")
  )
}))

source("02_Scripts/analyseschritte/05_Themen_und_Begegnungsart.R", encoding = "UTF-8")
source("02_Scripts/analyseschritte/06_Themen_und_Begegnungsart_Modelle.R", encoding = "UTF-8")
source("02_Scripts/analyseschritte/07_Themenprofile_vs_Gesamtsample.R", encoding = "UTF-8")
source("02_Scripts/analyseschritte/04_Theoriebezogene_Exploration.R", encoding = "UTF-8")
source("02_Scripts/analyseschritte/02_Datenabdeckung_und_Codierqualitaet.R", encoding = "UTF-8")

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
  "Der Intro-Daily-Vergleich ist in der Präregistrierung vorgesehen. Die konkreten Plattformvergleiche, Themenmodelle, Darstellungen und Sensitivitätsregeln wurden nach Sichtung der Daten festgelegt. Abschnitt 3 enthält explorative Signifikanztests mit Korrektur für Mehrfachvergleiche; die übrigen Auswertungen bleiben deskriptiv.",
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
  theory_report, quality_report,
  "## Verbleibende Arbeit", "",
  "Die fehlende Coding-Abdeckung klären und bei verfügbaren Codes beide Berichte erneut erzeugen. Die qualitative Erstcodierung fachlich prüfen. Schulung und Konfliktlösung der Inhaltscodierung dokumentieren. Schlussfolgerungen bleiben auf eingereichte und codierte Beiträge begrenzt.",
  "",
  "## Anhang: R-Version und Pakete", "", "```", capture.output(sessionInfo()), "```", ""
)
source("02_Scripts/analyseschritte/03_Uploadschwelle_Sensitivitaet.R", encoding = "UTF-8")
append_at <- which(grepl("^## Anhang", report))[1]
report <- append(report,
  upload_sensitivity_report,
  after = if (is.na(append_at)) length(report) else append_at - 1L
)
source("02_Scripts/berichte/Erlaeuterungen.R", encoding = "UTF-8")
report <- explain_report(report, "exploration")
report <- style_report(report, "exploration")
writeLines(enc2utf8(report), report_file, useBytes = TRUE)
source("02_Scripts/berichte/PDF_erstellen.R", encoding = "UTF-8")
render_report_pdf(report_file)
message("Explorationsbericht gespeichert: ", report_file)
