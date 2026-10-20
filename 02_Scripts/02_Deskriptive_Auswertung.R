# ==============================================================================
# Tagebuchstudie: deskriptive Auswertung
# ==============================================================================
# Stichprobe -> Intro -> Daily -> Outro -> Markdown-Bericht
# Aus dem Projekt-Hauptverzeichnis ausführen. Abschnitte einzeln ausführbar.
# Voraussetzung: rohe CSV-Exporte und bereinigtes Coding Sheet.


# 01 Pakete und Einstellungen ---------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(dplyr, tidyr, stringr, openxlsx, psych, knitr, ggplot2, GPArotation, readr)

source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8")

# Einschluss zentral in analyseschritte/01_Stichprobe.R: abgeschlossenes Intro und Outro sowie mindestens sieben Uploads

daily_file <- "06_Coding/coding_sheet.xlsx"
report_file <- "03_Output/Analyse_Essentials.md"
figure_dir <- "03_Output/Analyse_Essentials_Grafiken"


# 02 Daten einlesen und Schlüssel prüfen ----------------------------------------

source("02_Scripts/analyseschritte/01_Stichprobe.R", encoding = "UTF-8")
daily_raw <- read.xlsx(daily_file, sheet = "Coding")
codebook <- read.xlsx(daily_file, sheet = "Codebook")

intro_raw <- intro_raw %>%
  mutate(participant = clean_text(personalParticipantCode))

outro_raw <- outro_raw %>%
  mutate(participant = clean_text(personalParticipantCode))

daily_raw <- daily_raw %>%
  mutate(
    participant = clean_text(participant),
    screenshot_id = clean_text(screenshot_id),
    across(
      c(
        public_rel_coded, advertisement_coded, topic_coded,
        source_coded, media_format
      ),
      clean_numeric
    )
  )

# Intro und Outro: eine Zeile je Person. Daily: eine Zeile je Screenshot.
stopifnot(
  !anyNA(intro_raw$participant),
  !anyNA(outro_raw$participant),
  !anyNA(daily_raw$participant),
  !anyNA(daily_raw$screenshot_id),
  !anyDuplicated(intro_raw$participant),
  !anyDuplicated(outro_raw$participant),
  !anyDuplicated(daily_raw$screenshot_id)
)

marked_duplicates <- daily_raw %>%
  filter(is_marked_screenshot_duplicate(notes))

if (nrow(marked_duplicates) > 0) {
  stop("Das Coding Sheet enthält noch markierte Duplikate.")
}


# 03 Kategoriensystem prüfen ----------------------------------------------------

coding_categories <- codebook %>%
  transmute(
    variable = Variable,
    code = as.character(Code),
    category = str_trim(Category)
  ) %>%
  filter(str_detect(code, "^[0-9]+$"))

expected_codes <- list(
  public_rel_coded = c(1, 0, 99, 98, 97),
  advertisement_coded = c(1, 0, 99),
  topic_coded = c(1:14, 99),
  source_coded = c(1:10, 99),
  media_format = c(1:3, 99)
)

for (variable in names(expected_codes)) {
  categories <- coding_categories %>%
    filter(.data$variable == .env$variable)

  if (anyDuplicated(categories$code) || anyNA(categories$category) ||
    !setequal(categories$code, as.character(expected_codes[[variable]]))) {
    stop("Codebook prüfen: ", variable)
  }

  observed_codes <- na.omit(daily_raw[[variable]])

  if (any(!observed_codes %in% expected_codes[[variable]])) {
    stop("Ungültige Codes: ", variable)
  }
}


# 04 Gemeinsame Analysestichprobe -----------------------------------------------

participant_selection <- sample_audit %>%
  rename(n_screenshots = n_uploads) %>%
  mutate(intro_available = TRUE)

included_ids <- participant_selection %>%
  filter(included) %>%
  pull(participant)

if (length(included_ids) == 0) {
  stop("Leere Analysestichprobe.")
}

intro <- intro_raw %>% filter(participant %in% included_ids)
daily <- daily_raw %>% filter(participant %in% included_ids)
outro <- outro_answered %>% filter(participant %in% included_ids)
daily_public <- daily %>% filter(public_rel_coded == 1)

if (nrow(daily_public) == 0) {
  stop("Keine öffentlich relevanten Beiträge vorhanden.")
}

sample_flow <- tibble(
  Stufe = c(
    "Eingeschlossene Personen (Rohdaten)", "Intro vorhanden", "Daily: Personen mit Coding",
    "Daily: codierte Screenshots",
    "Outro: Personen mit Skalenantworten",
    "Öffentlich relevante Screenshots"
  ),
  N = c(
    length(included_ids),
    nrow(intro),
    n_distinct(daily$participant),
    nrow(daily),
    nrow(outro),
    nrow(daily_public)
  )
)
source("02_Scripts/analyseschritte/02_Datenabdeckung_und_Codierqualitaet.R", encoding = "UTF-8")

sample_flow


# 05 Funktionen für wiederkehrende deskriptive Tabellen -------------------------

# Die Funktionen geben Tabellen zurück und verändern keine Datensätze.
describe_variable <- function(x, label) {
  valid_values <- x[!is.na(x)]

  tibble(
    Variable = label,
    N = length(valid_values),
    Fehlend = sum(is.na(x)),
    Mittelwert = safe_mean(x),
    SD = safe_sd(x),
    Median = safe_median(x),
    Q1 = if (length(valid_values) > 0) unname(quantile(valid_values, .25)) else NA_real_,
    Q3 = if (length(valid_values) > 0) unname(quantile(valid_values, .75)) else NA_real_,
    Minimum = safe_min(x),
    Maximum = safe_max(x)
  )
}

frequency_table <- function(x) {
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))

  # Bei Faktoren bleiben auch Kategorien mit null Beobachtungen sichtbar.
  tibble(Kategorie = x) %>%
    filter(!is.na(Kategorie)) %>%
    count(Kategorie, name = "n", .drop = FALSE) %>%
    mutate(
      Gueltig_N = n_valid,
      Fehlend_N = n_missing,
      Prozent = 100 * safe_divide(n, n_valid)
    )
}

participant_distribution <- function(data, variable) {
  valid_data <- data %>%
    select(participant, Kategorie = all_of(variable)) %>%
    filter(!is.na(Kategorie))

  categories <- if (is.factor(data[[variable]])) {
    levels(data[[variable]])
  } else {
    sort(unique(valid_data$Kategorie))
  }

  denominators <- valid_data %>% count(participant, name = "n_valid")
  category_counts <- valid_data %>% count(participant, Kategorie, name = "n")

  # Nicht beobachtete Kategorien zählen innerhalb einer Person als null.
  expand_grid(participant = denominators$participant, Kategorie = categories) %>%
    left_join(category_counts, by = c("participant", "Kategorie")) %>%
    left_join(denominators, by = "participant") %>%
    mutate(share = replace_na(n, 0) / n_valid) %>%
    group_by(Kategorie) %>%
    summarise(
      Personen_N = n(),
      Mittlerer_Anteil_Prozent = 100 * mean(share),
      SD_Prozentpunkte = 100 * sd(share),
      .groups = "drop"
    )
}


# 06 Stichprobenbeschreibung und Teilnahme --------------------------------------

intro <- intro %>%
  mutate(
    age = na_if(clean_numeric(intro_age_num), -1),
    gender_code = recode_gender_numeric(intro_gender),
    education_code = na_if(clean_numeric(intro_education), -1),
    gender = factor(
      gender_code,
      levels = 1:3, labels = c("Weiblich", "Männlich", "Divers")
    ),
    education = factor(
      education_code,
      levels = 1:8,
      labels = c(
        "Ohne Abschluss", "Hauptschule", "Mittlere Reife",
        "Polytechnische Oberschule", "Fachhochschulreife", "Abitur",
        "Hochschulabschluss", "Anderer Abschluss"
      )
    )
  )

stopifnot(
  all(is.na(intro$age) | intro$age >= 60),
  all(is.na(intro$gender_code) | intro$gender_code %in% 1:3),
  all(is.na(intro$education_code) | intro$education_code %in% 1:8)
)

age_summary <- describe_variable(intro$age, "Alter (Jahre)")
gender_distribution <- frequency_table(intro$gender)
education_distribution <- frequency_table(intro$education)

participation <- upload_records %>%
  filter(participant %in% included_ids) %>%
  group_by(participant) %>%
  summarise(
    n_screenshots = n(),
    n_active_days = n_distinct(substr(scheduled, 1, 10), na.rm = TRUE),
    .groups = "drop"
  )

participation_summary <- bind_rows(
  describe_variable(participation$n_screenshots, "Screenshots je Person"),
  describe_variable(participation$n_active_days, "Aktive Tage je Person")
)

participation_by_day <- daily %>%
  group_by(study_day) %>%
  summarise(Screenshots = n(), Personen = n_distinct(participant), .groups = "drop")


# 07 Intro: Nutzung, Bedürfnisse und Incidentality ------------------------------

platform_items <- c(
  "intro_freq_facebook", "intro_freq_instagram", "intro_freq_tiktok", "intro_freq_x"
)
need_items <- c(
  "intro_ib_undirected", "intro_ib_thematic", "intro_ib_social", "intro_ib_problem"
)
incidentality_items <- paste0("intro_incidentality_", 1:6)
context_items <- c("intro_context_local", "intro_context_situation")
intro_items <- c(platform_items, "intro_intensity", need_items, incidentality_items, context_items)

intro <- intro %>%
  mutate(across(all_of(intro_items), ~ na_if(clean_numeric(.x), -1)))

intro_ranges <- tibble(
  variable = intro_items,
  maximum = c(rep(8, 4), 7, rep(5, 10), rep(3, 2))
)

for (i in seq_len(nrow(intro_ranges))) {
  variable <- intro_ranges$variable[i]
  values <- intro[[variable]]

  if (any(values < 1 | values > intro_ranges$maximum[i], na.rm = TRUE)) {
    stop("Intro-Wertebereich prüfen: ", variable)
  }
}

frequency_labels <- c(
  "Nie", "Seltener als monatlich", "Monatlich", "2–3-mal monatlich",
  "Wöchentlich", "Mehrmals wöchentlich", "Täglich", "Mehrmals täglich"
)

platform_tables <- list()

for (variable in platform_items) {
  responses <- factor(intro[[variable]], levels = 1:8, labels = frequency_labels)
  platform_tables[[variable]] <- frequency_table(responses) %>%
    mutate(Plattform = variable, .before = 1)
}

intro_platform_distribution <- bind_rows(platform_tables)
intro_item_summaries <- list()
intro_item_distributions <- list()

# Itemtabellen behalten die ursprüngliche Polung zur Prüfung der Antworten.
for (variable in c("intro_intensity", need_items, incidentality_items)) {
  response_levels <- if (variable == "intro_intensity") 1:7 else 1:5

  intro_item_summaries[[variable]] <- describe_variable(intro[[variable]], variable)
  intro_item_distributions[[variable]] <- frequency_table(
    factor(intro[[variable]], levels = response_levels)
  ) %>%
    mutate(Item = variable, .before = 1)
}

intro_item_summary <- bind_rows(intro_item_summaries)
intro_item_distribution <- bind_rows(intro_item_distributions)

# Präregistrierter Mittelwertindex: sechs Items, Item 5 umpolen.
# Wie bei den Outro-Indizes werden vollständige Antworten vorausgesetzt.
intro <- intro %>%
  mutate(incidentality_5_reversed = incidentality_scores(intro)$intro_incidentality_5)

incidentality_scored_items <- c(
  paste0("intro_incidentality_", 1:4), "incidentality_5_reversed",
  "intro_incidentality_6"
)
intro$incidentality_index <- rowMeans(intro[incidentality_scored_items], na.rm = FALSE)
incidentality_summary <- describe_variable(intro$incidentality_index, "Incidentality")

incidentality_complete <- intro %>%
  select(all_of(incidentality_scored_items)) %>%
  drop_na()
incidentality_alpha <- NA_real_

if (nrow(incidentality_complete) >= 3 && all(sapply(incidentality_complete, sd) > 0)) {
  incidentality_reliability <- psych::alpha(
    incidentality_complete,
    check.keys = FALSE, warnings = FALSE
  )
  incidentality_alpha <- incidentality_reliability$total$raw_alpha
}

incidentality_reliability_table <- tibble(
  Skala = "Incidentality",
  Items = length(incidentality_scored_items),
  Vollstaendige_Personen = nrow(incidentality_complete),
  Cronbach_Alpha = incidentality_alpha
)

intro_local_context <- frequency_table(factor(
  intro$intro_context_local,
  levels = 1:3, labels = c("Zu Hause", "Unterwegs", "Etwa gleich häufig")
))

intro_social_context <- frequency_table(factor(
  intro$intro_context_situation,
  levels = 1:3, labels = c("Überwiegend allein", "Überwiegend gemeinsam", "Etwa gleich häufig")
))


# 08 Daily: öffentliche Relevanz und Datenqualität -------------------------------

relevance_labels <- coding_categories %>%
  filter(variable == "public_rel_coded") %>%
  arrange(match(code, c("1", "0", "99", "98", "97")))

daily <- daily %>%
  mutate(public_relevance = factor(
    public_rel_coded,
    levels = relevance_labels$code,
    labels = relevance_labels$category
  ))

relevance_distribution <- frequency_table(daily$public_relevance)

relevance_rate <- tibble(
  Mass = "Relevanzquote unter eindeutig beurteilbaren Uploads",
  Zaehler = sum(daily$public_rel_coded == 1, na.rm = TRUE),
  Nenner = sum(daily$public_rel_coded %in% c(0, 1)),
  Prozent = 100 * safe_divide(Zaehler, Nenner)
)

content_variables <- c("advertisement_coded", "topic_coded", "source_coded", "media_format")

content_outside_gate <- daily %>%
  filter(public_rel_coded != 1) %>%
  filter(if_any(all_of(content_variables), ~ !is.na(.x)))

daily_missing_codes <- daily_public %>%
  summarise(across(all_of(content_variables), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Fehlend")


# 09 Daily: Inhalte und Nutzungssituationen -------------------------------------

# Neue Faktoren für die Tabellen; Originalcodes bleiben erhalten.
for (variable in c("incidentality_code", "locality_code", "situation_code")) {
  values <- daily_public[[variable]]
  if (any(!is.na(values) & !values %in% 1:3)) {
    stop("Daily-Wertebereich prüfen: ", variable)
  }
}

daily_public <- daily_public %>%
  mutate(
    platform = coalesce(clean_text(platform_coded), clean_text(platform_reported)),
    incidentality = factor(
      incidentality_code,
      levels = 1:3,
      labels = c("Gezielt gesucht", "Gefolgt, nicht gezielt gesucht", "Zufällig begegnet")
    ),
    locality = factor(
      locality_code,
      levels = 1:3, labels = c("Zu Hause", "Unterwegs", "Weiß nicht mehr")
    ),
    situation = factor(
      situation_code,
      levels = 1:3, labels = c("Allein", "Gemeinsam", "Weiß nicht mehr")
    ),
    read_thoroughly = factor(clean_binary(interaction_read_code),
      levels = 0:1, labels = c("Nein", "Ja")
    ),
    researched = factor(clean_binary(interaction_research_code),
      levels = 0:1, labels = c("Nein", "Ja")
    ),
    engaged = factor(clean_binary(interaction_engagement_code),
      levels = 0:1, labels = c("Nein", "Ja")
    )
  )

for (variable in content_variables) {
  categories <- coding_categories %>%
    filter(.data$variable == .env$variable) %>%
    arrange(match(code, as.character(expected_codes[[.env$variable]])))

  daily_public[[paste0(variable, "_label")]] <- factor(
    daily_public[[variable]],
    levels = categories$code,
    labels = categories$category
  )
}

daily_variables <- c(
  Werbung = "advertisement_coded_label",
  Thema = "topic_coded_label",
  Quelle = "source_coded_label",
  Format = "media_format_label",
  Plattform = "platform",
  Begegnungsart = "incidentality",
  Ort = "locality",
  Sozialer_Kontext = "situation",
  Gruendlich_angesehen = "read_thoroughly",
  Weiter_recherchiert = "researched",
  Interagiert = "engaged"
)

daily_screenshot_tables <- list()
daily_person_tables <- list()

for (label in names(daily_variables)) {
  variable <- daily_variables[[label]]

  # Jeder gültige Screenshot zählt einmal.
  daily_screenshot_tables[[label]] <- frequency_table(daily_public[[variable]])

  # Jede Person mit gültigen Angaben zählt gleich viel.
  daily_person_tables[[label]] <- participant_distribution(daily_public, variable)
}


# 10 Outro: Itemverteilungen ----------------------------------------------------

reactivity_items <- paste0("outro_reactivity_", 1:5)
ease_items <- paste0("outro_ease_", 1:8)
outro_items <- c(reactivity_items, ease_items)

outro <- outro %>%
  mutate(across(all_of(outro_items), ~ na_if(clean_numeric(.x), -1)))

for (variable in outro_items) {
  if (any(outro[[variable]] < 1 | outro[[variable]] > 5, na.rm = TRUE)) {
    stop("Outro-Wertebereich prüfen: ", variable)
  }
}

outro_item_summaries <- list()
outro_item_distributions <- list()

for (variable in outro_items) {
  outro_item_summaries[[variable]] <- describe_variable(outro[[variable]], variable)
  outro_item_distributions[[variable]] <- frequency_table(
    factor(outro[[variable]], levels = 1:5)
  ) %>%
    mutate(Item = variable, .before = 1)
}

outro_item_summary <- bind_rows(outro_item_summaries)
outro_item_distribution <- bind_rows(outro_item_distributions)


# 11 Outro: vollständige Skalen und interne Konsistenz ---------------------------

# Hohe Werte: stärkere Reaktivität bzw. höhere Benutzerfreundlichkeit.
outro <- outro %>%
  mutate(
    reactivity_2_reversed = 6 - outro_reactivity_2,
    reactivity_3_reversed = 6 - outro_reactivity_3,
    reactivity_5_reversed = 6 - outro_reactivity_5
  )

reactivity_scored_items <- c(
  "outro_reactivity_1", "reactivity_2_reversed", "reactivity_3_reversed",
  "outro_reactivity_4", "reactivity_5_reversed"
)

# Mittelwert nur bei vollständigen Antworten auf die jeweilige Skala.
outro$reactivity_index <- rowMeans(outro[reactivity_scored_items], na.rm = FALSE)
outro$ease_index <- rowMeans(outro[ease_items], na.rm = FALSE)

outro_scale_summary <- bind_rows(
  describe_variable(outro$reactivity_index, "Reaktivität"),
  describe_variable(outro$ease_index, "Benutzerfreundlichkeit")
)

reactivity_complete <- outro %>%
  select(all_of(reactivity_scored_items)) %>%
  drop_na()

ease_complete <- outro %>%
  select(all_of(ease_items)) %>%
  drop_na()

# Kein automatisches Umpolen und keine Itemlöschung zur Optimierung von Alpha.
reactivity_alpha <- NA_real_
ease_alpha <- NA_real_

if (nrow(reactivity_complete) >= 3 && all(sapply(reactivity_complete, sd) > 0)) {
  reactivity_reliability <- psych::alpha(
    reactivity_complete,
    check.keys = FALSE, warnings = FALSE
  )
  reactivity_alpha <- reactivity_reliability$total$raw_alpha
}

if (nrow(ease_complete) >= 3 && all(sapply(ease_complete, sd) > 0)) {
  ease_reliability <- psych::alpha(
    ease_complete,
    check.keys = FALSE, warnings = FALSE
  )
  ease_alpha <- ease_reliability$total$raw_alpha
}

outro_reliability <- tibble(
  Skala = c("Reaktivität", "Benutzerfreundlichkeit"),
  Items = c(length(reactivity_scored_items), length(ease_items)),
  Vollstaendige_Personen = c(nrow(reactivity_complete), nrow(ease_complete)),
  Cronbach_Alpha = c(reactivity_alpha, ease_alpha)
)

# Hierarchisches Omega: Modellwahl nach Kenntnis der Daten, nicht präregistriert.
# Drei Gruppenfaktoren; bei fünf Items zwei mit gleichen Generalfaktorladungen.
# Pearson-Korrelationen: Omega bezieht sich auf standardisierte Items.
estimate_omega <- function(items, scale_name, n_factors) {
  fit <- psych::omega(
    items,
    nfactors = n_factors, fm = "minres", rotate = "oblimin",
    option = "equal", flip = FALSE, poly = FALSE, covar = FALSE, plot = FALSE
  )
  loadings <- as.data.frame(fit$schmid$sl)
  invalid <- any(!is.finite(as.matrix(loadings[c("g", "h2", "u2")])) |
    loadings$u2 < 0 | loadings$h2 > 1) ||
    !is.finite(fit$omega_h) || !is.finite(fit$omega.tot) ||
    fit$omega_h < 0 || fit$omega.tot > 1 || fit$omega_h > fit$omega.tot

  status <- if (invalid) {
    "Unzulässige Lösung; Omega nicht interpretieren"
  } else if (any(loadings$u2 < .01)) {
    "Grenznahe Lösung (Restvarianz < 0,01)"
  } else {
    "Keine unzulässigen Varianzen"
  }

  list(
    model = fit,
    summary = tibble(
      Skala = scale_name,
      Gruppenfaktoren = n_factors,
      Omega_Hierarchisch = if (invalid) NA_real_ else fit$omega_h,
      Omega_Total = if (invalid) NA_real_ else fit$omega.tot,
      Modellstatus = status
    ),
    diagnostics = tibble(
      Skala = scale_name, Item = rownames(loadings),
      Generalfaktorladung = loadings$g,
      Kommunalitaet = loadings$h2, Restvarianz = loadings$u2
    )
  )
}

incidentality_omega <- estimate_omega(incidentality_complete, "Incidentality", 3)
reactivity_omega <- estimate_omega(reactivity_complete, "Reaktivität", 2)
ease_omega <- estimate_omega(ease_complete, "Benutzerfreundlichkeit", 3)

incidentality_reliability_table <- incidentality_reliability_table %>%
  left_join(incidentality_omega$summary, by = "Skala")
outro_reliability <- outro_reliability %>%
  left_join(bind_rows(reactivity_omega$summary, ease_omega$summary), by = "Skala")

omega_diagnostics <- bind_rows(
  incidentality_omega$diagnostics, reactivity_omega$diagnostics, ease_omega$diagnostics
)

outro_scale_summary
outro_reliability

open_response_counts <- tibble(Feld = character(), Antworten = integer())

for (variable in intersect(c("outro_problems_free", "outro_suggestions_free"), names(outro))) {
  open_response_counts <- bind_rows(
    open_response_counts,
    tibble(Feld = variable, Antworten = sum(!is.na(clean_text(outro[[variable]]))))
  )
}


# 12 Grafiken für den Bericht --------------------------------------------------

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
source("02_Scripts/berichte/Gestaltung.R", encoding = "UTF-8")
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
source("02_Scripts/analyseschritte/03_Uploadschwelle_Sensitivitaet.R", encoding = "UTF-8")
append_at <- which(grepl("^## Anhang", report))[1]
report <- append(report,
  upload_sensitivity_report,
  after = if (is.na(append_at)) length(report) else append_at - 1L
)
source("02_Scripts/berichte/Erlaeuterungen.R", encoding = "UTF-8")
report <- explain_report(report, "essentials")
report <- style_report(report, "essentials")
writeLines(enc2utf8(report), report_file, useBytes = TRUE)
source("02_Scripts/berichte/PDF_erstellen.R", encoding = "UTF-8")
render_report_pdf(report_file)

message(
  "Bericht gespeichert: ", report_file,
  "\nPersonen: ", length(included_ids),
  "\nScreenshots: ", nrow(daily),
  "\nÖffentlich relevante Beiträge: ", nrow(daily_public)
)
