# Ergänzende Datenabdeckung, Pretest-Vergleich und offene Rückmeldungen.
# Benötigt die Datenbasis der Paper-Analyse; verändert keine Codierungen.
# Die qualitative Erstzuordnung muss fachlich geprüft werden.

# 01 Uploads und vorhandene Hauptcodierung abgleichen ----------------------------

coverage_table <- sample_audit %>%
  filter(included) %>%
  left_join(count(daily, participant, name = "Codiert"), by = "participant") %>%
  mutate(Codiert = replace_na(Codiert, 0L))
readr::write_csv(coverage_table, "03_Output/Coding_Abdeckung.csv")
uncoded_uploads <- upload_records %>%
  filter(participant %in% included_ids) %>%
  anti_join(daily %>% transmute(participant, filename = clean_text(original_filename)),
    by = c("participant", "filename")
  )
if (nrow(uncoded_uploads) > 0) {
  readr::write_csv(uncoded_uploads, "03_Output/Uploads_ohne_Hauptcoding.csv")
}
# Dateibezogene Erklärung der verbleibenden Abdeckungslücke

# 02 Dokumentierte Ursachen fehlender Hauptcodierung zuordnen --------------------

pretest_files <- readr::read_csv("06_Coding/pretest_excluded_original_files.csv",
  show_col_types = FALSE
)
completed_outro_ids <- outro_raw %>%
  filter(
    !is.na(clean_text(firstOpened)),
    !is.na(clean_text(committed))
  ) %>%
  pull(participant)
missing_open_files <- upload_raw %>%
  filter(is.na(clean_text(firstOpened))) %>%
  select(participant, matches("^daily_[0-9]+_screenshot$")) %>%
  pivot_longer(-participant, values_to = "filename") %>%
  filter(!is.na(clean_text(filename))) %>%
  distinct(participant, filename) %>%
  mutate(Fehlender_Startzeitpunkt = TRUE)
upload_day_counts <- upload_records %>%
  group_by(participant) %>%
  summarise(Uploadtage = n_distinct(substr(scheduled, 1, 10)), .groups = "drop")
coverage_reasons <- uncoded_uploads %>%
  left_join(
    pretest_files %>% transmute(participant,
      filename = original_filename,
      Pretest = TRUE
    ),
    by = c(
      "participant",
      "filename"
    )
  ) %>%
  left_join(missing_open_files, by = c("participant", "filename")) %>%
  left_join(upload_day_counts, by = "participant") %>%
  mutate(Grund = case_when(
    Pretest %in% TRUE ~ "Separat ausgewählte Pretest-Datei",
    !participant %in% completed_outro_ids ~ "Früherer Ausschluss ohne abgeschlossenes Outro",
    Fehlender_Startzeitpunkt %in% TRUE ~ "Früherer Ausschluss bei fehlendem firstOpened",
    Uploadtage < 3 ~ "Früherer Ausschluss bei weniger als drei Uploadtagen",
    TRUE ~ "Weiterer Abgleich erforderlich"
  ))
coverage_reason_summary <- coverage_reasons %>%
  group_by(Grund) %>%
  summarise(Uploads = n(), Personen = n_distinct(participant), .groups = "drop")
if (nrow(coverage_reasons) > 0) {
  readr::write_csv(coverage_reasons, "03_Output/Coding_Abdeckung_Ursachen.csv")
  readr::write_csv(
    coverage_reasons %>% filter(!Pretest %in% TRUE) %>%
      select(participant, original_filename = filename),
    "03_Output/Zusaetzliche_Uploads_zur_Codierung.csv"
  )
}

# 03 Abdeckung und Unterschiede zwischen den Personengruppen beschreiben ---------

coverage_summary <- coverage_table %>% summarise(
  Eingeschlossene_Personen = n(), Mit_Coding = sum(Codiert > 0),
  Ohne_Coding = sum(Codiert == 0), Uploads = sum(n_uploads), Coding_Zeilen = sum(Codiert)
)
selection_summary <- intro_raw %>%
  filter(participant %in% included_ids) %>%
  left_join(select(coverage_table, participant, Codiert), by = "participant") %>%
  mutate(
    Gruppe = if_else(Codiert > 0, "Mit Hauptcoding", "Ohne Hauptcoding"),
    Alter = na_if(clean_numeric(intro_age_num), -1),
    Intensitaet = na_if(clean_numeric(intro_intensity), -1)
  ) %>%
  group_by(Gruppe) %>%
  summarise(
    Personen = n(), Alter_N = sum(!is.na(Alter)),
    Alter_M = safe_mean(Alter), Intensitaet_N = sum(!is.na(Intensitaet)),
    Intensitaet_M = safe_mean(Intensitaet), .groups = "drop"
  )

# 04 Gespeicherte Reliabilität und Unterschiede der Codebooks dokumentieren -------

# Die gespeicherte Pretest-Reliabilität liegt bereits als pretest_table vor.
version_table <- bind_rows(lapply(c("LA", "LG", "MF"), function(coder_name) {
  path <- paste0("07_Pretest/coding_sheet_", coder_name, ".xlsx")
  pretest_codebook <- openxlsx::read.xlsx(path, sheet = "Codebook") %>%
    transmute(Variable, Code = as.character(Code), Category = stringr::str_squish(Category)) %>%
    tidyr::fill(Variable) %>%
    filter(!is.na(Code))
  final_codebook <- codebook %>%
    transmute(Variable, Code = as.character(Code), Category = stringr::str_squish(Category)) %>%
    tidyr::fill(Variable) %>%
    filter(!is.na(Code))
  tibble(
    Coder = coder_name, Abweichende_Codebook_Zeilen =
      nrow(anti_join(pretest_codebook,
        final_codebook,
        by = names(pretest_codebook)
      )) + nrow(anti_join(final_codebook,
        pretest_codebook,
        by = names(pretest_codebook)
      ))
  )
}))

# Inhaltliche Erstzuordnung nach Lesen der vollständigen Antworten, KI-gestützt.
# Mehrfachzuordnungen sind erlaubt. Neue Antworten bleiben sichtbar unzugeordnet.

# 05 Vorhandene Erstzuordnung der offenen Rückmeldungen ---------------------------

feedback_codes <- readr::read_csv("06_Coding/feedback_codes.csv",
  col_types = readr::cols(.default = "c"), show_col_types = FALSE)
feedback <- outro_raw %>%
  filter(participant %in% included_ids) %>%
  select(participant, problems = outro_problems_free, suggestions = outro_suggestions_free) %>%
  pivot_longer(-participant, names_to = "Feld", values_to = "Antwort") %>%
  mutate(Antwort = clean_text(Antwort)) %>%
  filter(!is.na(Antwort)) %>%
  left_join(feedback_codes, by = c("participant", "Feld")) %>%
  mutate(Themen = replace_na(Themen, "Noch nicht zugeordnet"))
readr::write_csv(feedback, "03_Output/Feedback_Erstcodierung.csv")
feedback_summary <- feedback %>%
  separate_rows(Themen, sep = ";") %>%
  group_by(Thema = Themen) %>%
  summarise(Personen = n_distinct(participant), .groups = "drop") %>%
  arrange(desc(Personen))
