# ==============================================================================
# Ergänzende Kennzahlen für das Manuskript
# ==============================================================================
# Aus dem Projektverzeichnis ausführen. Keine neuen Modelle oder Ausschlussregeln.
# Die Reaktivitätsprüfung bleibt ergänzend; die Hauptstichprobe wird nicht geändert.

# 01 Vorhandenen Datenimport und Funktionen verwenden ---------------------------

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, readr, openxlsx, writexl)
source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/funktionen/Deskriptive_Tabellen.R", encoding = "UTF-8", local = TRUE)
source("02_Scripts/paper/01_Datenbasis.R", encoding = "UTF-8", local = TRUE)

daily_public <- daily_public %>%
  decode_practices() %>%
  mutate(platform = coalesce(clean_text(platform_coded), clean_text(platform_reported)))

# 02 Teilnahmeumfang und beitragende Personen je Plattform -----------------------
# scheduled bezeichnet den vorgesehenen Fragebogentag, nicht den Begegnungszeitpunkt.

coverage <- upload_records %>%
  filter(participant %in% included_ids) %>%
  group_by(participant) %>%
  summarise(Uploads = n(), Questionnaire_days = n_distinct(substr(scheduled, 1, 10), na.rm = TRUE),
    .groups = "drop")

coverage_summary <- bind_rows(
  describe_variable(coverage$Uploads, "Distinct uploads per person"),
  describe_variable(coverage$Questionnaire_days, "Scheduled questionnaire days with uploads")
)
platform_coverage <- daily_public %>%
  group_by(platform) %>%
  summarise(Posts = n(), Participants = n_distinct(participant), .groups = "drop")

# Messvergleich für die Abbildung: dieselbe Indexbildung wie in der Paper-Analyse.
manuscript_calibration <- daily_public %>% group_by(participant) %>%
  summarise(Chance = mean(incidentality_code == 3),
    Without_search = mean(incidentality_code %in% 2:3), Posts = n(), .groups = "drop") %>%
  left_join(tibble(participant = intro$participant,
    Index = rowMeans(incidentality_scores(intro), na.rm = FALSE)), by = "participant")

# 03 Kleine Sensitivitätsprüfung für die berichteten Kernverteilungen ------------
# Ausschluss nur hier: Zustimmung (4 oder 5) zu gezielter Suche für die Studie.

search_ids <- outro %>%
  filter(clean_numeric(outro_reactivity_1) %in% 4:5) %>% pull(participant)

sensitivity <- bind_rows(lapply(c("All participants", "Without reported study search"), function(sample) {
  selected <- if (sample == "All participants") daily_public else
    filter(daily_public, !participant %in% search_ids)
  encounters <- selected %>%
    transmute(Measure = factor(incidentality_code, levels = 1:3,
      labels = c("Deliberate search", "Followed, no targeted search", "Chance encounter"))) %>%
    count(Measure, name = "Yes", .drop = FALSE) %>%
    mutate(Measure = as.character(Measure), Valid_N = sum(Yes), Percent = 100 * Yes / Valid_N)
  practices <- bind_rows(lapply(c("All posts", "Chance encounters"), function(scope) {
    posts <- if (scope == "All posts") selected else filter(selected, incidentality_code == 3)
    posts %>% select(read, research, engage) %>%
      pivot_longer(everything(), names_to = "Practice", values_to = "Value") %>%
      group_by(Practice) %>%
      summarise(Yes = sum(Value == 1, na.rm = TRUE), Valid_N = sum(!is.na(Value)),
        Percent = 100 * mean(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Measure = paste(scope, Practice, sep = ": ")) %>% select(-Practice)
  }))
  bind_rows(encounters, practices) %>%
    mutate(Sample = sample, Participants = n_distinct(selected$participant), .before = 1)
}))

sensitivity_comparison <- sensitivity %>%
  select(Measure, Sample, Percent) %>% pivot_wider(names_from = Sample, values_from = Percent) %>%
  mutate(Difference_pp = `Without reported study search` - `All participants`)

stopifnot(nrow(coverage) == 105, sum(coverage$Uploads) == 3288,
  nrow(daily_public) == 3074, length(search_ids) == 20,
  nrow(manuscript_calibration) == 105, all(is.finite(manuscript_calibration$Index)),
  !anyNA(coverage), all(coverage$Questionnaire_days %in% 1:7),
  all(is.finite(sensitivity$Percent)), nrow(sensitivity_comparison) == 9)

# 04 Ein ergänzendes Arbeitsbuch statt einzelner CSV-Dateien ---------------------

writexl::write_xlsx(list(Coverage = coverage_summary, Platforms = platform_coverage,
  Sensitivity = sensitivity, Comparison = sensitivity_comparison, Calibration = manuscript_calibration),
  "03_Output/Manuskript_Ergaenzungen.xlsx")
print(coverage_summary)
print(platform_coverage)
print(sensitivity_comparison)
