################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    04a_Screening_Analysis.R
#
# Zweck:
#   Bereitet das Screening knapp auf, erstellt den Personen-Datensatz für
#   Daily/Outro und berichtet die wichtigsten deskriptiven und explorativen
#   Ergebnisse.
#
# Outputs:
#   03_Output/Screening_Results.xlsx
#   03_Output/screening_prepared.rds
#   optional: vier Screening-Grafiken in 04_Figures/
################################################################################

rm(list = ls())


#===============================================================================
# 01 Settings, Packages, Paths
#===============================================================================
# Hier stehen nur Einstellungen, die den Output tatsächlich verändern.

overwrite_outputs <- TRUE
create_figures <- TRUE
reliability_threshold <- 0.70

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, psych, openxlsx, fs, scales)

helper_script <- file.path("02_Scripts", "00_Helpers.R")
data_file <- file.path("01_Data", "screening-befragung_tagebuchstudie.rds")
output_excel <- file.path("03_Output", "Screening_Results.xlsx")
output_rds <- file.path("03_Output", "screening_prepared.rds")
figure_folder <- "04_Figures"

if (!file.exists(helper_script)) stop("Helper-Script fehlt: ", helper_script)
if (!file.exists(data_file)) stop("Screening-Datei fehlt: ", data_file)
source(helper_script)

fs::dir_create("03_Output")
if (create_figures) fs::dir_create(figure_folder)

if (!overwrite_outputs && any(file.exists(c(output_excel, output_rds)))) {
  stop("Screening-Output existiert bereits; overwrite_outputs = FALSE.")
}


#===============================================================================
# 02 Load + minimale Datenchecks
#===============================================================================
# Geprüft werden nur Fehler, die die Analyse oder Verknüpfung mit Daily/Outro
# tatsächlich unbrauchbar machen: fehlende Variablen, IDs, Duplikate und
# unmögliche Skalenwerte.

screening_raw <- readRDS(data_file)

required_variables <- c(
  "personalParticipantCode", "intro_stop_age", "intro_stop_usage",
  "intro_age_num", "intro_gender", "intro_education",
  "intro_freq_facebook", "intro_freq_instagram", "intro_freq_tiktok", "intro_freq_x",
  "intro_intensity",
  "intro_ib_undirected", "intro_ib_thematic", "intro_ib_social", "intro_ib_problem",
  paste0("intro_incidentality_", 1:6),
  "intro_context_local", "intro_context_situation"
)

missing_variables <- setdiff(required_variables, names(screening_raw))
if (length(missing_variables) > 0) {
  stop("Fehlende Screening-Variablen: ", paste(missing_variables, collapse = ", "))
}

screening_all <- screening_raw %>%
  mutate(
    participant = clean_text(personalParticipantCode),
    intro_stop_age_logical = as_logical_safe(intro_stop_age),
    intro_stop_usage_logical = as_logical_safe(intro_stop_usage),
    eligible_screening = intro_stop_age_logical %in% TRUE &
      intro_stop_usage_logical %in% TRUE
  )

n_missing_codes <- sum(is.na(screening_all$participant))
if (n_missing_codes > 0) {
  warning(n_missing_codes, " Screening-Zeilen ohne Participant Code werden ausgeschlossen.")
}

duplicate_codes <- screening_all %>%
  filter(!is.na(participant)) %>%
  count(participant) %>%
  filter(n > 1)

if (nrow(duplicate_codes) > 0) {
  stop("Doppelte Participant Codes: ", paste(duplicate_codes$participant, collapse = ", "))
}

screening <- screening_all %>%
  filter(eligible_screening, !is.na(participant))

numeric_variables <- c(
  "intro_age_num", "intro_gender", "intro_education",
  "intro_freq_facebook", "intro_freq_instagram", "intro_freq_tiktok", "intro_freq_x",
  "intro_intensity",
  "intro_ib_undirected", "intro_ib_thematic", "intro_ib_social", "intro_ib_problem",
  paste0("intro_incidentality_", 1:6),
  "intro_context_local", "intro_context_situation"
)

screening <- screening %>%
  mutate(
    intro_gender = recode_gender_numeric(intro_gender),
    across(all_of(setdiff(numeric_variables, "intro_gender")), clean_numeric),
    across(all_of(numeric_variables), ~ na_if(.x, -1))
  )

range_limits <- tribble(
  ~Variable, ~Minimum, ~Maximum,
  "intro_age_num", 60, Inf,
  "intro_gender", 1, 3,
  "intro_education", 1, 8,
  "intro_freq_facebook", 1, 8,
  "intro_freq_instagram", 1, 8,
  "intro_freq_tiktok", 1, 8,
  "intro_freq_x", 1, 8,
  "intro_intensity", 1, 7,
  "intro_ib_undirected", 1, 5,
  "intro_ib_thematic", 1, 5,
  "intro_ib_social", 1, 5,
  "intro_ib_problem", 1, 5,
  "intro_incidentality_1", 1, 5,
  "intro_incidentality_2", 1, 5,
  "intro_incidentality_3", 1, 5,
  "intro_incidentality_4", 1, 5,
  "intro_incidentality_5", 1, 5,
  "intro_incidentality_6", 1, 5,
  "intro_context_local", 1, 3,
  "intro_context_situation", 1, 3
)

range_issues <- purrr::pmap_dfr(
  range_limits,
  function(Variable, Minimum, Maximum) {
    screening %>%
      transmute(participant, Variable, Value = .data[[Variable]], Minimum, Maximum) %>%
      filter(!is.na(Value), Value < Minimum | Value > Maximum)
  }
)

if (nrow(range_issues) > 0) {
  stop(
    "Werte außerhalb des erwarteten Bereichs: ",
    paste(unique(range_issues$Variable), collapse = ", ")
  )
}


#===============================================================================
# 03 Labels + zentrale Personenmerkmale
#===============================================================================
# Hier werden nur Variablen erzeugt, die wir beschreiben oder später in
# Daily/Outro wiederverwenden.

frequency_levels <- c(
  "Nie", "Seltener als einmal im Monat", "Einmal im Monat",
  "Zwei- bis dreimal im Monat", "Einmal pro Woche", "Mehrmals pro Woche",
  "Einmal täglich", "Mehrmals täglich"
)

screening <- screening %>%
  mutate(
    gender = factor(intro_gender, 1:3, c("Weiblich", "Männlich", "Divers")),
    education = factor(
      intro_education, 1:8,
      c(
        "Schule ohne Abschluss beendet", "Haupt-/Volksschulabschluss",
        "Realschulabschluss/Mittlere Reife", "Polytechnische Oberschule",
        "Fachhochschulreife", "Abitur/Hochschulreife",
        "Hochschulabschluss", "Anderer Abschluss"
      )
    ),
    education_three_level = factor(
      case_when(
        intro_education %in% 1:2 ~ "Niedrig",
        intro_education %in% 3:4 ~ "Mittel",
        intro_education %in% 5:7 ~ "Hoch",
        TRUE ~ NA_character_
      ),
      levels = c("Niedrig", "Mittel", "Hoch")
    ),
    context_local = factor(
      intro_context_local, 1:3,
      c("Zu Hause", "Unterwegs", "An beiden Orten ähnlich häufig")
    ),
    context_social = factor(
      intro_context_situation, 1:3,
      c("Überwiegend allein", "Überwiegend gemeinsam", "Beides ähnlich häufig")
    ),
    age_group = cut(
      intro_age_num,
      breaks = c(59, 64, 69, 74, Inf),
      labels = c("60–64", "65–69", "70–74", "75+"),
      ordered_result = TRUE
    )
  )


#===============================================================================
# 04 Plattformnutzung
#===============================================================================
# Beschreibt die Plattformökologie der Personen. Für spätere Analysen bleiben
# Frequenzen, wöchentlich/täglich genutzte Plattformen und Primärplattform erhalten.

platform_long <- screening %>%
  select(
    participant,
    Facebook = intro_freq_facebook,
    Instagram = intro_freq_instagram,
    TikTok = intro_freq_tiktok,
    X = intro_freq_x
  ) %>%
  pivot_longer(-participant, names_to = "Platform", values_to = "Usage_Frequency") %>%
  mutate(
    Weekly_Use = Usage_Frequency >= 5,
    Daily_Use = Usage_Frequency >= 7,
    Used_At_All = Usage_Frequency > 1
  )

platform_profile <- platform_long %>%
  group_by(participant) %>%
  summarise(
    N_Platforms_Used = if_else(all(is.na(Used_At_All)), NA_integer_, sum(Used_At_All, na.rm = TRUE)),
    N_Platforms_Weekly = if_else(all(is.na(Weekly_Use)), NA_integer_, sum(Weekly_Use, na.rm = TRUE)),
    N_Platforms_Daily = if_else(all(is.na(Daily_Use)), NA_integer_, sum(Daily_Use, na.rm = TRUE)),
    Primary_Platform = {
      if (all(is.na(Usage_Frequency))) {
        NA_character_
      } else {
        m <- max(Usage_Frequency, na.rm = TRUE)
        if (m <= 1) "Keine Plattform genutzt" else
          paste(Platform[!is.na(Usage_Frequency) & Usage_Frequency == m], collapse = " / ")
      }
    },
    .groups = "drop"
  )

screening <- screening %>%
  left_join(platform_profile, by = "participant") %>%
  mutate(
    facebook_weekly = intro_freq_facebook >= 5,
    instagram_weekly = intro_freq_instagram >= 5,
    tiktok_weekly = intro_freq_tiktok >= 5,
    x_weekly = intro_freq_x >= 5,
    facebook_daily = intro_freq_facebook >= 7,
    instagram_daily = intro_freq_instagram >= 7,
    tiktok_daily = intro_freq_tiktok >= 7,
    x_daily = intro_freq_x >= 7,
    Platform_Repertoire = factor(
      case_when(
        is.na(N_Platforms_Weekly) ~ NA_character_,
        N_Platforms_Weekly == 0 ~ "Keine Plattform wöchentlich",
        N_Platforms_Weekly == 1 ~ "Eine Plattform wöchentlich",
        TRUE ~ "Mehrere Plattformen wöchentlich"
      ),
      levels = c(
        "Keine Plattform wöchentlich",
        "Eine Plattform wöchentlich",
        "Mehrere Plattformen wöchentlich"
      )
    ),
    freq_facebook_label = factor(intro_freq_facebook, 1:8, frequency_levels, ordered = TRUE),
    freq_instagram_label = factor(intro_freq_instagram, 1:8, frequency_levels, ordered = TRUE),
    freq_tiktok_label = factor(intro_freq_tiktok, 1:8, frequency_levels, ordered = TRUE),
    freq_x_label = factor(intro_freq_x, 1:8, frequency_levels, ordered = TRUE)
  )

# Plausibilitätscheck: Das Stop-Item sollte durch mindestens eine wöchentlich
# genutzte Plattform gestützt werden. Fälle werden nur markiert, nicht entfernt.
screening <- screening %>%
  mutate(
    platform_eligibility_consistent = case_when(
      is.na(N_Platforms_Weekly) ~ NA,
      intro_stop_usage_logical %in% TRUE & N_Platforms_Weekly >= 1 ~ TRUE,
      intro_stop_usage_logical %in% TRUE & N_Platforms_Weekly == 0 ~ FALSE,
      TRUE ~ NA
    )
  )

n_eligibility_inconsistencies <- sum(screening$platform_eligibility_consistent %in% FALSE, na.rm = TRUE)
if (n_eligibility_inconsistencies > 0) {
  warning(n_eligibility_inconsistencies, " Fälle: Stop-Item und Plattformfrequenzen sind inkonsistent.")
}


#===============================================================================
# 05 Informationsbedürfnisse
#===============================================================================
# Neben den vier Einzelmaßen werden zwei knappe Profilmerkmale gebildet:
# durchschnittliche Wichtigkeit und Differenzierung zwischen stärkstem/schwächstem Bedürfnis.

need_labels <- c(
  intro_ib_undirected = "Ungerichtet",
  intro_ib_thematic = "Thematisch",
  intro_ib_social = "Sozial",
  intro_ib_problem = "Problembezogen"
)

needs_long <- screening %>%
  select(participant, all_of(names(need_labels))) %>%
  pivot_longer(-participant, names_to = "Need_Variable", values_to = "Importance") %>%
  mutate(Information_Need = unname(need_labels[Need_Variable]))

need_profiles <- needs_long %>%
  group_by(participant) %>%
  summarise(
    Mean_Importance = safe_mean(Importance),
    Need_Differentiation = safe_max(Importance) - safe_min(Importance),
    Number_of_High_Needs = if_else(all(is.na(Importance)), NA_integer_, sum(Importance >= 4, na.rm = TRUE)),
    Dominant_Information_Need = {
      if (all(is.na(Importance))) {
        NA_character_
      } else {
        max_value <- max(Importance, na.rm = TRUE)
        top <- Information_Need[!is.na(Importance) & Importance == max_value]
        if (length(top) == 1) top else "Kein eindeutiges dominantes Bedürfnis"
      }
    },
    .groups = "drop"
  )

screening <- screening %>%
  left_join(need_profiles, by = "participant")


#===============================================================================
# 06 Incidentality-Index + Reliabilität
#===============================================================================
# Item 5 wird invertiert. Der Index ist der Mittelwert aller sechs Items und wird
# nur bei vollständigen Antworten gebildet. Berichtet werden Alpha und Omega total.

screening <- screening %>%
  mutate(intro_incidentality_5_reversed = 6 - intro_incidentality_5)

incidentality_items <- c(
  "intro_incidentality_1", "intro_incidentality_2",
  "intro_incidentality_3", "intro_incidentality_4",
  "intro_incidentality_5_reversed", "intro_incidentality_6"
)

screening <- screening %>%
  mutate(
    incidentality_index = if_else(
      if_all(all_of(incidentality_items), ~ !is.na(.x)),
      rowMeans(across(all_of(incidentality_items))),
      NA_real_
    )
  )

incidentality_complete <- screening %>%
  select(all_of(incidentality_items)) %>%
  drop_na()

incidentality_alpha <- tryCatch(
  psych::alpha(incidentality_complete, check.keys = FALSE, warnings = FALSE),
  error = function(e) NULL
)

incidentality_omega <- tryCatch(
  {
    result <- NULL
    invisible(capture.output(
      result <- suppressWarnings(suppressMessages(
        psych::omega(incidentality_complete, nfactors = 1, plot = FALSE)
      ))
    ))
    result
  },
  error = function(e) NULL
)

reliability_summary <- tibble(
  N_Complete = nrow(incidentality_complete),
  Cronbach_Alpha = if (is.null(incidentality_alpha)) NA_real_ else incidentality_alpha$total$raw_alpha,
  Omega_Total = if (is.null(incidentality_omega)) NA_real_ else incidentality_omega$omega.tot,
  Threshold = reliability_threshold
)

# Itemdiagnostik bleibt drin, weil sie für die präregistrierte Entscheidung über
# einen möglichen Itemausschluss relevant ist; es wird nichts automatisch entfernt.
alpha_item_stats <- if (is.null(incidentality_alpha)) {
  tibble(
    Item = incidentality_items,
    Corrected_Item_Total_R = NA_real_,
    Item_Mean = NA_real_,
    Item_SD = NA_real_,
    Alpha_If_Deleted = NA_real_
  )
} else {
  incidentality_alpha$item.stats %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Item") %>%
    transmute(
      Item,
      Corrected_Item_Total_R = r.drop,
      Item_Mean = mean,
      Item_SD = sd
    ) %>%
    left_join(
      incidentality_alpha$alpha.drop %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Item") %>%
        transmute(Item, Alpha_If_Deleted = raw_alpha),
      by = "Item"
    )
}

omega_item_deleted <- purrr::map_dfr(
  incidentality_items,
  function(item_removed) {
    reduced <- incidentality_complete %>% select(-all_of(item_removed))
    omega_reduced <- tryCatch(
      {
        result <- NULL
        invisible(capture.output(
          result <- suppressWarnings(suppressMessages(
            psych::omega(reduced, nfactors = 1, plot = FALSE)
          ))
        ))
        result
      },
      error = function(e) NULL
    )
    
    tibble(
      Item_Removed = item_removed,
      Omega_Total_If_Deleted = if (is.null(omega_reduced)) NA_real_ else omega_reduced$omega.tot
    )
  }
)


#===============================================================================
# 07 Deskriptive Ergebnisse
#===============================================================================
# Diese Tabellen beantworten nur die Kernfragen zur Stichprobe, Nutzung,
# Informationsbedürfnissen und Incidentality; Detailtabellen werden vermieden.

age_stats <- continuous_summary(screening, "intro_age_num", "Alter")
usage_stats <- continuous_summary(screening, "intro_intensity", "Nutzungsintensität")
incidentality_stats <- continuous_summary(screening, "incidentality_index", "Incidentality-Index")

overview_output <- bind_rows(
  tibble(
    Section = "Sample",
    Measure = c(
      "Rohzeilen", "Teilnahmeberechtigte Personen", "Fehlende Participant Codes",
      "Inkonsistenz Stop-Item / Plattformfrequenz"
    ),
    Category = NA_character_,
    N = c(nrow(screening_raw), nrow(screening), n_missing_codes, n_eligibility_inconsistencies),
    Percent = NA_real_, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  ),
  age_stats %>% transmute(
    Section = "Soziodemografie", Measure = Variable, Category = NA_character_,
    N = N_Valid, Percent = NA_real_, Mean, SD, Median, Minimum, Maximum
  ),
  usage_stats %>% transmute(
    Section = "Nutzung", Measure = Variable, Category = NA_character_,
    N = N_Valid, Percent = NA_real_, Mean, SD, Median, Minimum, Maximum
  ),
  frequency_summary(screening, "gender", "Geschlecht") %>% transmute(
    Section = "Soziodemografie", Measure = Variable, Category = Level,
    N, Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  ),
  frequency_summary(screening, "education", "Bildungsabschluss") %>% transmute(
    Section = "Soziodemografie", Measure = Variable, Category = Level,
    N, Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  ),
  frequency_summary(screening, "context_local", "Typischer räumlicher Kontext") %>% transmute(
    Section = "Kontext", Measure = Variable, Category = Level,
    N, Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  ),
  frequency_summary(screening, "context_social", "Typischer sozialer Kontext") %>% transmute(
    Section = "Kontext", Measure = Variable, Category = Level,
    N, Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  )
)

platform_distribution <- platform_long %>%
  filter(!is.na(Usage_Frequency)) %>%
  count(Platform, Usage_Frequency, name = "N") %>%
  complete(Platform, Usage_Frequency = 1:8, fill = list(N = 0)) %>%
  group_by(Platform) %>%
  mutate(Percent = safe_percent(N, sum(N))) %>%
  ungroup()

platform_weekly <- platform_long %>%
  group_by(Platform) %>%
  summarise(
    N = sum(!is.na(Weekly_Use)),
    N_Weekly = sum(Weekly_Use %in% TRUE, na.rm = TRUE),
    Percent_Weekly = safe_percent(N_Weekly, N),
    .groups = "drop"
  )

platform_output <- bind_rows(
  platform_weekly %>% transmute(
    Section = "Mindestens wöchentlich", Platform, Category = NA_character_,
    N = N_Weekly, Percent = Percent_Weekly, Value = NA_real_
  ),
  platform_distribution %>% transmute(
    Section = "Frequenzverteilung", Platform, Category = frequency_levels[Usage_Frequency],
    N, Percent, Value = Usage_Frequency
  ),
  screening %>% count(Platform_Repertoire, name = "N") %>% mutate(Percent = safe_percent(N, sum(N))) %>%
    transmute(
      Section = "Plattformrepertoire", Platform = NA_character_, Category = as.character(Platform_Repertoire),
      N, Percent, Value = NA_real_
    ),
  screening %>% count(Primary_Platform, name = "N") %>% mutate(Percent = safe_percent(N, sum(N))) %>%
    transmute(
      Section = "Primärplattform", Platform = NA_character_, Category = Primary_Platform,
      N, Percent, Value = NA_real_
    )
)

need_descriptives <- needs_long %>%
  group_by(Information_Need) %>%
  summarise(
    N = sum(!is.na(Importance)),
    Mean = safe_mean(Importance), SD = safe_sd(Importance), Median = safe_median(Importance),
    CI95_Lower = if_else(N > 1, Mean - qt(.975, N - 1) * SD / sqrt(N), NA_real_),
    CI95_Upper = if_else(N > 1, Mean + qt(.975, N - 1) * SD / sqrt(N), NA_real_),
    .groups = "drop"
  )

need_distribution <- needs_long %>%
  filter(!is.na(Importance)) %>%
  count(Information_Need, Importance, name = "N") %>%
  complete(Information_Need, Importance = 1:5, fill = list(N = 0)) %>%
  group_by(Information_Need) %>%
  mutate(Percent = safe_percent(N, sum(N))) %>%
  ungroup()

information_needs_output <- bind_rows(
  need_descriptives %>% transmute(
    Section = "Deskriptiv", Information_Need, Category = NA_character_, N,
    Percent = NA_real_, Mean, SD, Median, CI95_Lower, CI95_Upper
  ),
  need_distribution %>% transmute(
    Section = "Antwortverteilung", Information_Need, Category = as.character(Importance), N,
    Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, CI95_Lower = NA_real_, CI95_Upper = NA_real_
  ),
  screening %>% count(Dominant_Information_Need, name = "N") %>% mutate(Percent = safe_percent(N, sum(N))) %>%
    transmute(
      Section = "Dominantes Bedürfnis", Information_Need = NA_character_,
      Category = Dominant_Information_Need, N, Percent,
      Mean = NA_real_, SD = NA_real_, Median = NA_real_, CI95_Lower = NA_real_, CI95_Upper = NA_real_
    )
)

incidentality_distribution <- screening %>%
  select(all_of(incidentality_items)) %>%
  pivot_longer(everything(), names_to = "Item", values_to = "Response") %>%
  filter(!is.na(Response)) %>%
  count(Item, Response, name = "N") %>%
  complete(Item, Response = 1:5, fill = list(N = 0)) %>%
  group_by(Item) %>%
  mutate(Percent = safe_percent(N, sum(N))) %>%
  ungroup()

incidentality_output <- bind_rows(
  incidentality_stats %>% transmute(
    Section = "Index", Item = NA_character_, Category = NA_character_, N = N_Valid,
    Percent = NA_real_, Mean, SD, Median, Value = NA_real_, Value_2 = NA_real_
  ),
  reliability_summary %>% transmute(
    Section = "Reliabilität", Item = NA_character_, Category = "Gesamtskala", N = N_Complete,
    Percent = NA_real_, Mean = Cronbach_Alpha, SD = Omega_Total, Median = NA_real_,
    Value = Threshold, Value_2 = NA_real_
  ),
  alpha_item_stats %>% transmute(
    Section = "Itemdiagnostik", Item, Category = NA_character_, N = NA_integer_, Percent = NA_real_,
    Mean = Item_Mean, SD = Item_SD, Median = NA_real_,
    Value = Corrected_Item_Total_R, Value_2 = Alpha_If_Deleted
  ),
  omega_item_deleted %>% transmute(
    Section = "Omega bei Itemausschluss", Item = Item_Removed, Category = NA_character_,
    N = NA_integer_, Percent = NA_real_, Mean = NA_real_, SD = NA_real_, Median = NA_real_,
    Value = Omega_Total_If_Deleted, Value_2 = NA_real_
  ),
  incidentality_distribution %>% transmute(
    Section = "Itemverteilung", Item, Category = as.character(Response), N, Percent,
    Mean = NA_real_, SD = NA_real_, Median = NA_real_, Value = Response, Value_2 = NA_real_
  )
)


#===============================================================================
# 08 Explorativ: Incidentality und Heterogenität der Stichprobe
#===============================================================================
# Diese Analysen dienen der Hypothesengenerierung und als Brücke zur Diary-
# Analyse. Wir betrachten nur theoretisch anschlussfähige Zusammenhänge.

incidentality_predictors <- c(
  intro_age_num = "Alter",
  intro_intensity = "Nutzungsintensität",
  N_Platforms_Weekly = "Wöchentlich genutzte Plattformen",
  intro_ib_undirected = "Ungerichtetes Informationsbedürfnis",
  intro_ib_thematic = "Thematisches Informationsbedürfnis",
  intro_ib_social = "Soziales Informationsbedürfnis",
  intro_ib_problem = "Problembezogenes Informationsbedürfnis",
  Mean_Importance = "Mittlere Wichtigkeit der Informationsbedürfnisse",
  Need_Differentiation = "Differenzierung der Informationsbedürfnisse"
)

incidentality_correlations <- purrr::imap_dfr(
  incidentality_predictors,
  ~ spearman_test(
    screening,
    x_variable = .y,
    y_variable = "incidentality_index",
    x_label = .x,
    y_label = "Incidentality"
  )
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(P_Value, method = "BH"),
    Analysis = "Incidentality-Korrelation"
  )

# Alter wird zusätzlich kontinuierlich mit zentralen Nutzungs- und Bedürfnismaßen
# verbunden. Das beschreibt Heterogenität innerhalb der 60+-Stichprobe, ohne
# ältere Personen als Defizitgruppe zu behandeln.
age_markers <- c(
  intro_intensity = "Nutzungsintensität",
  N_Platforms_Weekly = "Wöchentlich genutzte Plattformen",
  intro_ib_undirected = "Ungerichtetes Informationsbedürfnis",
  intro_ib_thematic = "Thematisches Informationsbedürfnis",
  intro_ib_social = "Soziales Informationsbedürfnis",
  intro_ib_problem = "Problembezogenes Informationsbedürfnis",
  incidentality_index = "Incidentality"
)

age_correlations <- purrr::imap_dfr(
  age_markers,
  ~ spearman_test(
    screening,
    x_variable = "intro_age_num",
    y_variable = .y,
    x_label = "Alter",
    y_label = .x
  )
) %>%
  mutate(
    P_Adjusted_BH = p.adjust(P_Value, method = "BH"),
    Analysis = "Alters-Korrelation"
  )

# Subgruppen sind rein deskriptiv: Sie zeigen, ob Screening-Incidentality je nach
# typischem Kontext oder Plattformökologie sichtbar unterschiedlich ausfällt.
subgroup_variables <- c(
  context_local = "Räumlicher Kontext",
  context_social = "Sozialer Kontext",
  Platform_Repertoire = "Plattformrepertoire",
  Primary_Platform = "Primärplattform",
  age_group = "Altersgruppe",
  education_three_level = "Bildung"
)

incidentality_subgroups <- purrr::imap_dfr(
  subgroup_variables,
  function(label, variable) {
    screening %>%
      transmute(Group = as.character(.data[[variable]]), Incidentality = incidentality_index) %>%
      filter(!is.na(Group)) %>%
      group_by(Group) %>%
      summarise(
        N = sum(!is.na(Incidentality)),
        Mean = safe_mean(Incidentality),
        SD = safe_sd(Incidentality),
        Median = safe_median(Incidentality),
        .groups = "drop"
      ) %>%
      mutate(Grouping = label, .before = 1)
  }
)

exploratory_output <- bind_rows(
  bind_rows(incidentality_correlations, age_correlations) %>%
    transmute(
      Analysis, Grouping = NA_character_, Group = NA_character_,
      Variable_1, Variable_2, N,
      Mean = NA_real_, SD = NA_real_, Median = NA_real_,
      Spearman_Rho, P_Value, P_Adjusted_BH
    ),
  incidentality_subgroups %>%
    transmute(
      Analysis = "Incidentality-Subgruppe", Grouping, Group,
      Variable_1 = NA_character_, Variable_2 = NA_character_, N,
      Mean, SD, Median,
      Spearman_Rho = NA_real_, P_Value = NA_real_, P_Adjusted_BH = NA_real_
    )
)


#===============================================================================
# 09 Save prepared participant data
#===============================================================================
# Dieses RDS ist der eigentliche Übergabedatensatz für Daily und Outro. Deshalb
# bleiben die Originalvariablen plus zentrale abgeleitete Merkmale erhalten.

saveRDS(screening, output_rds)


#===============================================================================
# 10 Excel: eine Datei, fünf übersichtliche Blätter
#===============================================================================
# Die Excel-Datei fasst nur interpretierbare Ergebnisse zusammen; reine
# technische Zwischenobjekte werden nicht exportiert.

workbook <- openxlsx::createWorkbook()
header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF", fgFill = "#315F6B", textDecoration = "bold",
  halign = "center", valign = "center"
)

add_excel_sheet(workbook, "Overview", overview_output, header_style)
add_excel_sheet(workbook, "Platforms", platform_output, header_style)
add_excel_sheet(workbook, "Information_Needs", information_needs_output, header_style)
add_excel_sheet(workbook, "Incidentality", incidentality_output, header_style)
add_excel_sheet(workbook, "Exploratory", exploratory_output, header_style)

openxlsx::saveWorkbook(workbook, output_excel, overwrite = overwrite_outputs)


#===============================================================================
# 11 Figures
#===============================================================================
# Vier zentrale Grafiken mit demselben Projekttheme. Die Visualisierung soll
# vor allem schnell lesbar sein: klare Typohierarchie, direkte Wertebeschriftung
# und eine zurückhaltende gemeinsame Farbpalette aus 00_Helpers.R.

if (create_figures) {
  
  # Plattformnutzung: absolute Häufigkeiten bleiben die Hauptinformation;
  # Prozentwerte stehen nur ergänzend direkt am Balken.
  figure_platforms <- platform_weekly %>%
    mutate(
      Label = paste0(N_Weekly, "  (", round(Percent_Weekly, 1), " %)")
    ) %>%
    ggplot(aes(
      x = N_Weekly,
      y = forcats::fct_reorder(Platform, N_Weekly)
    )) +
    geom_col(
      width = 0.64,
      fill = unname(project_colors["primary"])
    ) +
    geom_text(
      aes(label = Label),
      hjust = -0.12,
      size = 3.4,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.22))
    ) +
    labs(
      title = "Mindestens wöchentliche Plattformnutzung",
      subtitle = paste0("Absolute Häufigkeiten; N = ", nrow(screening)),
      x = "Personen",
      y = NULL
    ) +
    theme_project(base_size = 12, legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank()
    )
  
  save_project_plot(
    figure_platforms,
    file.path(figure_folder, "Screening_Platform_Weekly.png"),
    width = 7.4,
    height = 4.6
  )
  
  # Informationsbedürfnisse: Punkt + Konfidenzintervall betont Unterschiede,
  # ohne die ordinalen 1–5-Skalen als exakte metrische Balken zu inszenieren.
  figure_needs <- need_descriptives %>%
    mutate(
      Label = sprintf("%.2f", Mean),
      Need = forcats::fct_reorder(Information_Need, Mean)
    ) %>%
    ggplot(aes(
      x = Mean,
      y = Need
    )) +
    geom_segment(
      aes(
        x = CI95_Lower,
        xend = CI95_Upper,
        y = Need,
        yend = Need
      ),
      linewidth = 0.9,
      lineend = "round",
      colour = unname(project_colors["secondary"])
    ) +
    geom_point(
      size = 3.4,
      colour = unname(project_colors["primary"])
    ) +
    geom_text(
      aes(label = Label),
      nudge_y = 0.18,
      hjust = 0,
      size = 3.2,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_x_continuous(
      limits = c(1, 5),
      breaks = 1:5,
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(
      title = "Informationsbedürfnisse",
      subtitle = "Mittelwerte und 95%-Konfidenzintervalle",
      x = "Mittelwert (1–5)",
      y = NULL
    ) +
    theme_project(base_size = 12, legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank()
    )
  
  save_project_plot(
    figure_needs,
    file.path(figure_folder, "Screening_Information_Needs.png"),
    width = 7.6,
    height = 4.7
  )
  
  # Incidentality: Verteilung plus Mittelwert als Orientierung. Die gestrichelte
  # Linie ist deskriptiv und kein Schwellenwert.
  incidentality_mean <- safe_mean(screening$incidentality_index)
  
  figure_incidentality <- screening %>%
    filter(!is.na(incidentality_index)) %>%
    ggplot(aes(x = incidentality_index)) +
    geom_histogram(
      binwidth = 0.25,
      boundary = 1,
      colour = unname(project_colors["white"]),
      linewidth = 0.45,
      fill = unname(project_colors["primary"])
    ) +
    geom_vline(
      xintercept = incidentality_mean,
      linewidth = 0.8,
      linetype = "dashed",
      colour = unname(project_colors["accent"])
    ) +
    annotate(
      "text",
      x = incidentality_mean,
      y = Inf,
      label = paste0("M = ", round(incidentality_mean, 2)),
      vjust = 1.6,
      hjust = -0.08,
      size = 3.3,
      fontface = "bold",
      colour = unname(project_colors["accent"])
    ) +
    scale_x_continuous(
      limits = c(1, 5),
      breaks = 1:5,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = "Screening-Incidentalität",
      subtitle = "Verteilung des sechs Items umfassenden Index",
      x = "Incidentality-Index (1–5)",
      y = "Personen"
    ) +
    theme_project(base_size = 12, legend_position = "none") +
    theme(
      panel.grid.major.x = element_blank()
    )
  
  save_project_plot(
    figure_incidentality,
    file.path(figure_folder, "Screening_Incidentality_Index.png"),
    width = 7.4,
    height = 4.7
  )
  
  # Explorative Korrelationen: Richtung wird farblich getrennt, die Effektgröße
  # steht direkt am Balken. Signifikanz wird bewusst nicht visuell überhöht.
  figure_exploratory <- incidentality_correlations %>%
    filter(!is.na(Spearman_Rho)) %>%
    mutate(
      Direction = if_else(Spearman_Rho >= 0, "Positiv", "Negativ"),
      Label = sprintf("%.2f", Spearman_Rho)
    ) %>%
    ggplot(aes(
      x = Spearman_Rho,
      y = forcats::fct_reorder(Variable_1, Spearman_Rho),
      fill = Direction
    )) +
    geom_col(width = 0.62) +
    geom_vline(
      xintercept = 0,
      linewidth = 0.6,
      colour = unname(project_colors["dark"])
    ) +
    geom_text(
      aes(
        label = Label,
        hjust = if_else(Spearman_Rho >= 0, -0.15, 1.15)
      ),
      size = 3.2,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_fill_manual(
      values = c(
        "Positiv" = unname(project_colors["primary"]),
        "Negativ" = unname(project_colors["accent"])
      )
    ) +
    scale_x_continuous(
      limits = c(-1, 1),
      breaks = seq(-1, 1, by = 0.25),
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    labs(
      title = "Explorative Zusammenhänge mit Incidentality",
      subtitle = "Spearman-Korrelationen; rohe und BH-adjustierte p-Werte im Excel-Output",
      x = "Spearman ρ",
      y = NULL,
      fill = NULL
    ) +
    theme_project(base_size = 11.5, legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank()
    )
  
  save_project_plot(
    figure_exploratory,
    file.path(figure_folder, "Screening_Incidentality_Correlations.png"),
    width = 8.7,
    height = 5.8
  )
}


#===============================================================================
# 12 Console report
#===============================================================================
# Kurzer Abschlusscheck für den laufenden Workflow.

cat(
  "\nSCREENING ANALYSIS COMPLETED\n",
  "Eligible participants: ", nrow(screening), "\n",
  "Mean age: ", round(age_stats$Mean, 2), "\n",
  "Incidentality: M = ", round(incidentality_stats$Mean, 2),
  ", SD = ", round(incidentality_stats$SD, 2), "\n",
  "Cronbach alpha: ", round(reliability_summary$Cronbach_Alpha, 3), "\n",
  "Omega total: ", round(reliability_summary$Omega_Total, 3), "\n",
  "Excel: ", output_excel, "\n",
  "Prepared RDS: ", output_rds, "\n",
  sep = ""
)
