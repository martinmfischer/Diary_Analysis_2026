# Ergebnistabellen für das Paper; keine Grafiken oder Berichtstexte.
# Benötigt: Ergebnisse aus 02_Paper_Analysen.R und den vorherigen Paper-Schritten.
# Nenner: gültige Codes, Themen x Quellen nur vollständige Codepaare.
# Kontextkorrelationen: nachträglicher Messvergleich zu RQ2a, keine Kausalaussage.
# Praxisprüfung: Originalexport mit Coding Sheet abgleichen; -1 = Nichts davon.

topic_names <- c("Politics, state and elections", "International affairs, war and security",
  "Economy, work, finance and consumers", "Society, social affairs, migration and religion",
  "Education, science and technology", "Health and care", "Climate, environment and energy",
  "Crime, accidents and justice", "Transport, infrastructure and housing", "Weather and natural events",
  "Culture, media and entertainment", "History and remembrance", "Sport", "Events and public service",
  "Other, mixed or unclear topic")
names(topic_names) <- c(1:14, 99)
source_names <- c("Journalistic media outlet", "Alternative or partisan media outlet",
  "Political party or politician", "State or public institution", "NGO, association, initiative or movement",
  "Science, expert or fact-checker", "Company or brand", "Individual journalist",
  "Creator, influencer or other public figure", "Private individual", "Other or unidentifiable account")
names(source_names) <- c(1:10, 99)
format_names <- c("Text/link-based", "Static visual", "Moving audiovisual", "Not determinable")
names(format_names) <- c(1:3, 99)
paper_distributions <- list()
for (variable in c("topic_coded", "source_coded", "media_format")) {
  labels <- switch(variable, topic_coded = topic_names, source_coded = source_names,
                   media_format = format_names)
  values <- as.character(daily_public[[variable]])
  stopifnot(all(na.omit(values) %in% names(labels)))
  paper_distributions[[variable]] <- tibble(Category = factor(values,
    levels = names(labels), labels = labels)) %>% filter(!is.na(Category)) %>%
    count(Category, name = "n", .drop = FALSE) %>%
    mutate(Percent = 100 * n / sum(n), Valid_N = sum(n), Missing = sum(is.na(values)))
}

# Prozentwerte innerhalb eines Themas; Nullzellen ausdrücklich beibehalten.
topic_source_long <- daily_public %>%
  filter(!is.na(topic_coded), !is.na(source_coded)) %>%
  transmute(Topic = factor(as.character(topic_coded), levels = names(topic_names), labels = topic_names),
            Source = factor(as.character(source_coded), levels = names(source_names), labels = source_names)) %>%
  count(Topic, Source, name = "n", .drop = FALSE) %>% group_by(Topic) %>%
  mutate(Topic_N = sum(n), Percent = if_else(Topic_N > 0, 100 * n / Topic_N, NA_real_)) %>% ungroup()
topic_source_wide <- topic_source_long %>%
  mutate(Cell = sprintf("%s (%.1f%%)", n, Percent)) %>% select(Topic, Topic_N, Source, Cell) %>%
  pivot_wider(names_from = Source, values_from = Cell)
paper_source_a1 <- topic_source_wide %>% select(Topic, Topic_N, all_of(unname(source_names[1:5])))
paper_source_a2 <- topic_source_wide %>% select(Topic, Topic_N, all_of(unname(source_names[6:11])))
stopifnot(sum(topic_source_long$n) == sum(complete.cases(daily_public[c("topic_coded", "source_coded")])) )

context_person <- daily_public %>% group_by(participant) %>% summarise(
  Home_share = safe_mean(if_else(locality_code %in% 1:2, as.numeric(locality_code == 1), NA_real_)),
  Alone_share = safe_mean(if_else(situation_code %in% 1:2, as.numeric(situation_code == 1), NA_real_)),
  .groups = "drop") %>%
  left_join(intro %>% transmute(participant,
    Typical_home = recode(clean_numeric(intro_context_local), `1` = 1, `2` = 0, `3` = .5, .default = NA_real_),
    Typical_alone = recode(clean_numeric(intro_context_situation), `1` = 1, `2` = 0, `3` = .5, .default = NA_real_)),
    by = "participant")
context_correlations <- bind_rows(lapply(c("Home", "Alone"), function(context) {
  x <- context_person[[paste0("Typical_", tolower(context))]]
  y <- context_person[[paste0(context, "_share")]]
  valid <- is.finite(x) & is.finite(y)
  rho <- safe_spearman(x, y)
  p <- if (is.finite(rho)) cor.test(x[valid], y[valid], method = "spearman", exact = FALSE)$p.value else NA_real_
  tibble(Context = context, N = sum(valid), Spearman_rho = rho, p_raw = p)
}))

practice_raw <- upload_raw %>%
  select(participant, matches("^daily_[0-9]+_(screenshot|interaction_[123])$")) %>%
  pivot_longer(-participant, names_to = c("Slot", ".value"), names_pattern = "^daily_([0-9]+)_(.+)$") %>%
  filter(!is.na(clean_text(screenshot))) %>% distinct(participant, screenshot, .keep_all = TRUE)
practice_matches <- daily_public %>%
  select(participant, screenshot_id, original_filename, interaction_read_code,
         interaction_research_code, interaction_engagement_code) %>%
  left_join(practice_raw, by = c("participant", "original_filename" = "screenshot"))
stopifnot(!anyNA(practice_matches$Slot))
for (i in 1:3) {
  coded <- practice_matches[[c("interaction_read_code", "interaction_research_code", "interaction_engagement_code")[i]]]
  raw <- clean_numeric(practice_matches[[paste0("interaction_", i)]])
  stopifnot(isTRUE(all.equal(clean_binary(coded), clean_binary(raw))))
}
practice_export_codes <- practice_matches %>%
  decode_practices() %>% filter(practice_none)
practice_code_summary <- practice_matches %>% count(interaction_1, interaction_2, interaction_3, name = "Posts")

encounter_topics <- daily_public %>%
  transmute(Topic = factor(as.character(topic_coded), levels = names(topic_names), labels = topic_names),
    Encounter = factor(incidentality_code, levels = 1:3,
                       labels = c("Deliberate search", "Followed, no search", "Chance"))) %>%
  filter(!is.na(Topic), !is.na(Encounter)) %>% count(Encounter, Topic, .drop = FALSE) %>%
  group_by(Encounter) %>% mutate(Percent = 100 * n / sum(n)) %>% ungroup()

paper_practices <- daily_public %>%
  transmute(participant, Encounter = factor(incidentality_code, levels = 1:3,
    labels = c("Deliberate search", "Followed, no search", "Chance")),
    Viewing = read, Research = research, Interaction = engage) %>%
  pivot_longer(c(Viewing, Research, Interaction), names_to = "Practice", values_to = "Value") %>%
  filter(!is.na(Encounter), !is.na(Value)) %>% group_by(participant, Encounter, Practice) %>%
  summarise(Percent = 100 * mean(Value), .groups = "drop") %>% group_by(Encounter, Practice) %>%
  summarise(N = n(), Percent = mean(Percent), .groups = "drop")

# Kennzahlen für Text und Abbildungen mit expliziten Nennern.
paper_practice_summary <- daily_public %>% select(participant, read, research, engage, practice_none) %>%
  pivot_longer(-participant, names_to = "Practice", values_to = "Value") %>%
  group_by(Practice) %>% summarise(Yes = sum(Value == 1, na.rm = TRUE),
    Valid_N = sum(!is.na(Value)), Percent = 100 * safe_mean(as.numeric(Value)), .groups = "drop")
paper_corpus <- tibble(Measure = c("Included participants", "Coded uploads", "Publicly relevant posts"),
  N = c(length(included_ids), nrow(daily), nrow(daily_public)))

# Ein Arbeitsbuch statt zusätzlicher einzelner CSV-Tabellen.
paper_tables <- list(T1_Topics = paper_distributions$topic_coded,
  T2_Sources = paper_distributions$source_coded, T3_Formats = paper_distributions$media_format,
  A1_Topic_Source = paper_source_a1, A2_Topic_Source = paper_source_a2,
  Topic_Source_Long = topic_source_long, Context_Correlations = context_correlations,
  Context_Persons = context_person, Corpus = paper_corpus, Practice_Summary = paper_practice_summary,
  Practice_Codes = practice_code_summary,
  Praxis_Exportcodes = practice_export_codes, Figure2_Data = encounter_topics,
  Figure3_Data = paper_practices,
  Readme = data.frame(Note = c("Created from the active analysis sample; original data unchanged.",
    "T1-T3: valid-code denominators. A1-A2: valid topic-and-source pairs; percentages within topic.",
    "Context: exploratory Spearman tests, unadjusted asymptotic two-sided p-values.",
    "Practices: three raw -1 codes mean None of the above and are decoded as three zeros (confirmed by study lead, 2026-10-20).",
    "Run 02_Scripts/berichte/01_Essentials.R or 02_Exploration.R for figures and reports. Additional analyses have a separate workbook.")))

# Ein vollständiger Export pro Paper-Lauf; Reports überschreiben ihn nicht.
# Gespeicherte Pretest-Reliabilität für die Methodendokumentation übernehmen.
# Der Bootstrap zur Ermittlung dieser Werte ist ein separater Vorbereitungslauf.
pretest_file <- "07_Pretest/Reli_Pretest_Ergebnisse_icr.xlsx"
pretest_table <- openxlsx::read.xlsx(pretest_file, sheet = "Reliability") %>%
  transmute(
    Variable = variable, Beurteilbar = n_pairable_units,
    Vollstaendig = n_complete_units, Alpha = kripp_alpha_nominal,
    KI_unten = ci95_low, KI_oben = ci95_high
  )

paper_tables <- c(paper_tables, list(
  Stichprobenfluss = raw_flow, Alter = age_summary,
  Geschlecht = gender_distribution, Bildung = education_distribution,
  Teilnahme = participation_summary, Intro_Items = intro_item_summary,
  Incidentality = incidentality_summary, Incidentality_Alpha = incidentality_reliability_table,
  Intro_Ort = intro_local_context, Intro_Sozial = intro_social_context,
  Relevanz = relevance_distribution, Personenanteile = person_summary,
  Outro_Items = outro_item_summary, Outro_Skalen = outro_scale_summary,
  Outro_Alpha = outro_reliability, Survey_Diary = correlation_table,
  Practices_Encounter = practice_tables$encounter,
  Practices_Format = practice_tables$media_format_label,
  Topic_Encounter = topic_encounter_counts,
  Model_Tests = topic_model_tests, Model_Probabilities = topic_model_probabilities,
  Model_Contrasts = topic_model_contrasts, Model_Diagnostics = topic_model_diagnostics,
  Topic_Profile_Bootstrap = profile_comparison, Pretest_Reliabilitaet = pretest_table
))
writexl::write_xlsx(paper_tables, "03_Output/Paper_Tabellen.xlsx")
saveRDS(topic_models, "03_Output/Themenmodelle.rds")
saveRDS(list(model = profile_multinomial_model, bootstrap = profile_bootstrap,
  critical_value = simultaneous_critical_value, results = profile_comparison),
  "03_Output/Themenprofile_vs_Gesamtsample.rds")
