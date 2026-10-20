# 02 Datenabdeckung, Codierqualität und offene Rückmeldungen ----------------------
# Aufgerufen durch: beide Auswertungen nach Auswahl der Analysestichprobe
# Benötigt: sample_audit, daily, included_ids, upload_records, upload_raw,
#          intro_raw, outro_raw, raw_flow und codebook sowie gemeinsame Funktionen
# Liest zusätzlich: Pretest-Dateizuordnung, gespeicherte Reliabilität, Codebooks
# Liefert: Abdeckungs-, Selektions-, Pretest- und Feedbacktabellen; quality_report
# Schreibt: fünf CSV-Prüftabellen nach 03_Output
# Die Abdeckungsdiagnostik ergänzt keine fehlenden Inhaltscodes

# Studienablauf und Codierqualität; gemeinsame Berichtsteile.
# Ausführen, nachdem daily und included_ids vorliegen.


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
readr::write_csv(uncoded_uploads, "03_Output/Uploads_ohne_Hauptcoding.csv")
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
readr::write_csv(coverage_reasons, "03_Output/Coding_Abdeckung_Ursachen.csv")
readr::write_csv(
  coverage_reasons %>% filter(!Pretest %in% TRUE) %>%
    select(participant,
      original_filename = filename
    ),
  "03_Output/Zusaetzliche_Uploads_zur_Codierung.csv"
)


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

pretest_file <- "07_Pretest/Reli_Pretest_Ergebnisse_icr.xlsx"
pretest_table <- openxlsx::read.xlsx(pretest_file, sheet = "Reliability") %>%
  transmute(
    Variable = variable, Beurteilbar = n_pairable_units,
    Vollstaendig = n_complete_units, Alpha = kripp_alpha_nominal,
    KI_unten = ci95_low, KI_oben = ci95_high
  )
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

feedback_codes <- tibble::tribble(
  ~participant, ~Feld, ~Themen,
  "weval6jc", "problems", "Bildansicht und Bearbeitung",
  "pw6za3l8", "problems", "Instruktionen und Inhaltsauswahl",
  "pdus4hpc", "problems", "Bildansicht und Bearbeitung;Übertragung und Speicherung",
  "4a45imez", "problems", "Bildansicht und Bearbeitung",
  "bdh3un45", "problems", "Unspezifisches Screenshotproblem",
  "it5klml9", "problems", "Uploadablauf und Einstieg",
  "deexhb69", "problems", "Uploadablauf und Einstieg",
  "ankdmy5q", "problems", "Bildansicht und Bearbeitung",
  "bw0qd30f", "problems", "Uploadablauf und Einstieg",
  "lstkj97s", "problems", "Übertragung und Speicherung",
  "du94csba", "problems", "Uploadablauf und Einstieg",
  "siy45bnf", "problems", "Uploadablauf und Einstieg",
  "t2mz057i", "problems", "Bildansicht und Bearbeitung",
  "4gldfcyj", "problems", "Übertragung und Speicherung;Zeitfenster und Erinnerungen",
  "02hc7dvz", "problems", "Uploadablauf und Einstieg",
  "xz4x508z", "problems", "Instruktionen und Inhaltsauswahl",
  "i307pmik", "suggestions", "Positive Rückmeldung",
  "pw6za3l8", "suggestions", "Instruktionen und Inhaltsauswahl",
  "pdus4hpc", "suggestions", "Positive Rückmeldung",
  "1px0s1da", "suggestions", "Positive Rückmeldung;Zeitfenster und Erinnerungen",
  "8unf4xdd", "suggestions", "Positive Rückmeldung",
  "p03zl58f", "suggestions", "Positive Rückmeldung",
  "wcxsrrk4", "suggestions", "Antwortoptionen und Aufwand",
  "4fkmpl0t", "suggestions", "Antwortoptionen und Aufwand",
  "j3oxyxnq", "suggestions", "Lesbarkeit und Navigation",
  "bw0qd30f", "suggestions", "Positive Rückmeldung",
  "u7kjr0xs", "suggestions", "Positive Rückmeldung",
  "du94csba", "suggestions", "Positive Rückmeldung",
  "uqk7jup4", "suggestions", "Zeitfenster und Erinnerungen",
  "t2mz057i", "suggestions", "Positive Rückmeldung",
  "lyj7657n", "suggestions", "Uploadablauf und Einstieg",
  "re1buasq", "suggestions", "Zeitfenster und Erinnerungen",
  "jtih5moz", "suggestions", "Positive Rückmeldung",
  "h6csyig3", "suggestions", "Positive Rückmeldung",
  "v9r3ewpq", "suggestions", "Positive Rückmeldung",
  "k401d9b5", "suggestions", "Instruktionen und Inhaltsauswahl",
  "02hc7dvz", "suggestions", "Instruktionen und Inhaltsauswahl",
  "mkuybnhn", "suggestions", "Positive Rückmeldung;Lesbarkeit und Navigation",
  "kgfkzwyp", "suggestions", "Bildansicht und Bearbeitung;Instruktionen und Inhaltsauswahl"
)
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


# 06 Tabellen und Erläuterungen für den Bericht zusammenstellen -------------------

quality_report <- c(
  "## Einschluss, Abdeckung und Machbarkeit", "",
  "**Zweck:** Rekrutierungsverlauf und Datenausfälle prüfen. Dies folgt dem Machbarkeitsziel der Präregistrierung; es ist kein Theorietest.",
  "",
  "Grundlage sind die rohen CSV-Exporte. Beide Screening-Kriterien, abgeschlossenes Intro und Outro (jeweils committed vorhanden) und mindestens sieben unterschiedliche Screenshot-Dateien sind verpflichtend. Bekannte markierte Duplikate sind abgezogen. Es gibt keine Mindestzahl an Uploadtagen. Die zusätzliche historische Drei-Tage-Regel wird nicht übernommen. Die konkrete Outro-Abschlussanforderung wird gegenüber der Präregistrierung transparent dokumentiert.",
  "",
  as.character(knitr::kable(raw_flow, format = "pipe", row.names = FALSE)), "",
  as.character(knitr::kable(coverage_summary, format = "pipe", row.names = FALSE)), "",
  "Die Einschlussgrenze bezieht sich auf Uploads, nicht auf die Teilmenge im Coding Sheet. Die Inhaltsanalysen verwenden nur vorhandene Codes. Nicht codierte Uploads sind keine irrelevanten Beiträge. Die Ursachen der unterschiedlichen Abdeckung werden unten dateibezogen ausgewiesen. Uploadauswahl und fehlende Codierung können die Befunde verzerren. Coding_Abdeckung.csv dokumentiert die Abdeckung je Person.",
  "",
  "Die Analyse beschränkt sich auf Personen mit abgeschlossenem Intro und Outro sowie mindestens sieben Uploads. Vier historisch wegen fehlendem firstOpened ungecodete Dateien sind gemäß Entscheidung vom 16.10.2026 ausgeschlossen (06_Coding/upload_exclusions.csv). Ein pauschaler firstOpened-Filter auf weitere Diary-Zeilen wird nicht angewandt. Das Coding Sheet umfasst Hauptcodierung, gemeinsame Codierung und Reliabilitäts-Pretest. Die folgende Tabelle beschreibt nur noch Uploads eingeschlossener Personen ohne vorhandene Codierzeile; die Uploadzahlen sind disjunkt.",
  "",
  as.character(knitr::kable(coverage_reason_summary, format = "pipe", row.names = FALSE)), "",
  "Zusaetzliche_Uploads_zur_Codierung.csv enthält die verbleibenden Dateien ohne Codierung. Die Pretest-Codierungen wurden bereits übernommen, bei Abweichungen mit LA-Vorrang. Ergänzungen und Anpassungen an das finale Schema sind im Coding Sheet protokolliert. Diese Übernahme ersetzt keine erneute unabhängige Reliabilitätsprüfung.",
  "",
  "**Zweck:** Nachträgliche Selektionsdiagnostik anhand von Alter und Nutzungsintensität. Die beiden Mittelwertvergleiche beschreiben die verfügbare Coding-Basis; sie belegen weder zufällige Ausfälle noch Repräsentativität.",
  "",
  as.character(knitr::kable(selection_summary, format = "pipe", digits = 2, row.names = FALSE)), "",
  "### Codierqualität", "",
  "**Zweck:** Messqualität der Inhaltsanalyse dokumentieren. Dies ist eine Methodenprüfung, kein Theorietest. Die Tabelle übernimmt den vorhandenen Pretest mit drei Codierenden. Konfidenzintervalle stammen aus der gespeicherten Auswertung und wurden hier nicht neu geschätzt.",
  "",
  as.character(knitr::kable(pretest_table, format = "pipe", digits = 3, row.names = FALSE)), "",
  "Krippendorffs Alpha ist nominal. Öffentliche Relevanz und Plattform beziehen sich auf 140 Pretest-Beiträge. Nachgelagerte Variablen verwenden 117 Beiträge, die alle drei Codierenden als öffentlich relevant einstuften. Die Unsicherheit ist insbesondere bei Werbung groß. Die Werte belegen keine Reliabilität der vollständigen Hauptcodierung.",
  "",
  "Vergleich der Codebook-Zeilen (Variable, Code, Bezeichnung) zwischen Pretest-Dateien und aktuellem Coding Sheet:",
  "",
  as.character(knitr::kable(version_table, format = "pipe", row.names = FALSE)), "",
  "Die Codebooks weichen voneinander ab. Der Pretest enthält beispielsweise Mixed media format (Code 4) und Not determinable (−1); das aktuelle Format-Schema verwendet 1 bis 3 sowie 99. Die Pretest-Werte dürfen deshalb nicht als Reliabilität des aktuellen Kategoriensystems ausgegeben werden. Eine erneute unabhängige Codierung mit dem aktuellen System ist für diese Aussage nötig. Eine datierte Dokumentation zu Schulung, Regeländerungen und Konfliktlösung muss das Forschungsteam ergänzen. Die Dateiprüfsumme im Essentials-Bericht identifiziert den analysierten Stand.",
  "",
  "### Offene Rückmeldungen", "",
  "**Zweck:** Hürden und Verbesserungen für digitale Tagebücher mit älteren Personen identifizieren. Qualitative Exploration zum präregistrierten Machbarkeitsziel; die Themen wurden nach Datensichtung gebildet.",
  "",
  sprintf(
    "Basis: %s Antworten von %s eingeschlossenen Personen. Die inhaltliche Erstcodierung ist KI-gestützt und noch nicht unabhängig geprüft. Mehrfachzuordnungen sind möglich. Die prüfbare Zuordnung mit Originaltext steht in Feedback_Erstcodierung.csv. Häufigkeiten beschreiben die Antwortenden, nicht die Verbreitung in der gesamten Stichprobe.",
    nrow(feedback),
    n_distinct(feedback$participant)
  ),
  "",
  as.character(knitr::kable(feedback_summary, format = "pipe", row.names = FALSE)), "",
  "Konkrete Ansatzpunkte sind eine Vorschau hochgeladener Bilder, Korrektur- und Speicherfunktionen sowie klarere Hinweise zum Ablauf und zur Inhaltsauswahl. Positive App-Bewertungen schließen einzelne Bedienprobleme nicht aus.",
  ""
)
