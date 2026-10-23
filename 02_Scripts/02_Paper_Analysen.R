# ==============================================================================
# Paper-Analysen der Tagebuchstudie
# ==============================================================================
# Einstieg für die Übergabe: dieses Skript im geöffneten R-Projekt ausführen.
# Ablauf: Stichprobe -> Intro -> Daily -> Outro -> Vergleiche -> Ergebnistabellen.
# Ergebnisse stehen als R-Objekte bereit und in 03_Output/Paper_Tabellen.xlsx.
# Grafiken, Markdown und PDFs werden nur über berichte/ erstellt.

# 01 Pakete und Datenbasis ------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, readr, openxlsx, psych,
  lme4, nnet, boot, writexl)

source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/funktionen/Deskriptive_Tabellen.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/paper/01_Datenbasis.R", encoding = "UTF-8", local = TRUE)

# 02 Stichprobenbeschreibung und Teilnahme --------------------------------------

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

# 03 Intro: Nutzung, Bedürfnisse und Incidentality ------------------------------

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

# 04 Daily: öffentliche Relevanz und Datenqualität -------------------------------

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

# 05 Daily: Inhalte und Nutzungssituationen -------------------------------------

# Neue Faktoren für die Tabellen; Originalcodes bleiben erhalten.
for (variable in c("incidentality_code", "locality_code", "situation_code")) {
  values <- daily_public[[variable]]
  if (any(!is.na(values) & !values %in% 1:3)) {
    stop("Daily-Wertebereich prüfen: ", variable)
  }
}

daily_public <- daily_public %>%
  decode_practices() %>%
  mutate(
    platform = coalesce(clean_text(platform_coded), clean_text(platform_reported)),
    encounter = factor(incidentality_code, levels = 1:3,
      labels = c("Gezielt", "Gefolgt, ungeplant", "Zufällig")),
    incidental_strict = if_else(incidentality_code %in% 1:3,
      as.numeric(incidentality_code == 3), NA_real_),
    incidental_broad = if_else(incidentality_code %in% 1:3,
      as.numeric(incidentality_code %in% 2:3), NA_real_),
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
    read_thoroughly = factor(read,
      levels = 0:1, labels = c("Nein", "Ja")
    ),
    researched = factor(research,
      levels = 0:1, labels = c("Nein", "Ja")
    ),
    engaged = factor(engage,
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

# 06 Outro: Itemverteilungen ----------------------------------------------------

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

# 07 Outro: vollständige Skalen und interne Konsistenz ---------------------------

# Hohe Werte: stärkere Reaktivität bzw. höhere Benutzerfreundlichkeit.
# Reaktivität nur deskriptiv verwenden; die ergänzende Omega-Prüfung zeigt eine
# unzulässige Lösung. Sensitivitäten verwenden das Einzelitem zur gezielten Suche.
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

# 08 Begegnungsarten und Praktiken je Person ------------------------------------
# Beide Gewichtungen behalten: jeder Screenshot bzw. jede Person zählt gleich.

outro <- outro %>% mutate(study_search = na_if(clean_numeric(outro_reactivity_1), -1))
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

# 09 Selbstbericht und Tagebuch vergleichen -------------------------------------
# Präregistrierter Messvergleich, kein kausaler Theorietest.
# Zufall: Code 3. Ungeplant: Codes 2 und 3. Eine Beobachtung je Person.

spearman_summary <- function(data, label) {
  complete <- data %>% filter(is.finite(incidentality_index), is.finite(Anteil))
  tibble(
    Auswertung = label, Personen = nrow(complete),
    Spearman_rho = safe_spearman(complete$incidentality_index, complete$Anteil)
  )
}

calibration <- person_data %>%
  select(participant, incidentality_index, Begegnung_N, study_search, Zufall, Ungeplant) %>%
  pivot_longer(c(Zufall, Ungeplant), names_to = "Definition", values_to = "Anteil") %>%
  mutate(Anteil = 100 * Anteil)
correlation_table <- bind_rows(lapply(c("Zufall", "Ungeplant"), function(definition) {
  spearman_summary(filter(calibration, Definition == definition), definition)
}))

# 10 Praktiken nach Begegnungsart und Format ------------------------------------
# Explorative Verbindung von RQ2b/c. Mehrfachantworten sind möglich.
# Die Personenzusammensetzung kann zwischen den Gruppen variieren.

practice_tables <- list()
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
}

# 11 Themen und Begegnungsart ---------------------------------------------------
# Explorative Verbindung von RQ1 und RQ2b; berücksichtigt mehrere Posts je Person.
# Die längeren Modell- und Bootstrapberechnungen stehen in eigenen Skripten.

source("02_Scripts/paper/02_Themen_und_Begegnungsart.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/paper/03_Themenprofile.R", encoding = "UTF-8", local = TRUE)

# 12 Paper-Tabellen speichern ---------------------------------------------------
# Enthält Themen x Quellen, Kontextabgleich, Praxis-Nenner und Modellbefunde.

source("02_Scripts/paper/04_Ergebnistabellen.R", encoding = "UTF-8", local = TRUE)

print(sample_flow)
print(correlation_table)
print(topic_model_tests)
message("Paper-Analysen abgeschlossen: 03_Output/Paper_Tabellen.xlsx")
