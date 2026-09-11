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
#   optional: sieben Screening-Grafiken in 04_Figures/
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
tables_folder <- file.path("03_Output", "Tables")

if (!file.exists(helper_script)) stop("Helper-Script fehlt: ", helper_script)
if (!file.exists(data_file)) stop("Screening-Datei fehlt: ", data_file)
source(helper_script)

fs::dir_create("03_Output")
fs::dir_create(tables_folder)
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
  stop("Missing screening variables: ", paste(missing_variables, collapse = ", "))
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
  warning(n_missing_codes, " screening rows without a participant code are excluded.")
}

duplicate_codes <- screening_all %>%
  filter(!is.na(participant)) %>%
  count(participant) %>%
  filter(n > 1)

if (nrow(duplicate_codes) > 0) {
  stop("Duplicate participant codes: ", paste(duplicate_codes$participant, collapse = ", "))
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
    "Values outside the expected range: ",
    paste(unique(range_issues$Variable), collapse = ", ")
  )
}


#===============================================================================
# 03 Labels and core participant characteristics
#===============================================================================
# Only variables that are reported here or reused later in Daily/Outro.

frequency_levels <- c(
  "Never", "Less than once a month", "Once a month",
  "Two to three times a month", "Once a week", "Several times a week",
  "Once a day", "Several times a day"
)

screening <- screening %>%
  mutate(
    gender = factor(intro_gender, 1:3, c("Female", "Male", "Diverse")),
    education = factor(
      intro_education, 1:8,
      c(
        "Left school without a degree", "Lower secondary degree",
        "Intermediate secondary degree", "Polytechnic secondary school",
        "University of applied sciences entrance qualification",
        "Higher education entrance qualification (Abitur)",
        "University degree", "Other qualification"
      )
    ),
    education_three_level = factor(
      case_when(
        intro_education %in% 1:2 ~ "Low",
        intro_education %in% 3:4 ~ "Medium",
        intro_education %in% 5:7 ~ "High",
        TRUE ~ NA_character_
      ),
      levels = c("Low", "Medium", "High")
    ),
    context_local = factor(
      intro_context_local, 1:3,
      c("At home", "Out and about", "About equally in both places")
    ),
    context_social = factor(
      intro_context_situation, 1:3,
      c("Mostly alone", "Mostly with others", "About equally alone and with others")
    ),
    age_group = cut(
      intro_age_num,
      breaks = c(59, 64, 69, 74, Inf),
      labels = c("60–64", "65–69", "70–74", "75+"),
      ordered_result = TRUE
    )
  )


#===============================================================================
# 03a Detailed sample description: console
#===============================================================================
# Wird bewusst vor den inhaltlichen Screening-Analysen ausgegeben. Prozentwerte
# kategorialer Variablen beziehen sich jeweils auf die gültigen Antworten.

print_sample_frequency <- function(data, variable, label) {
  n_valid <- sum(!is.na(data[[variable]]))
  n_missing <- sum(is.na(data[[variable]]))
  
  cat(
    "\n", label,
    " (valid n = ", n_valid,
    "; missing = ", n_missing, "):\n",
    sep = ""
  )
  
  data %>%
    filter(!is.na(.data[[variable]])) %>%
    count(Category = .data[[variable]], .drop = FALSE) %>%
    mutate(
      Percent = round(100 * n / n_valid, 1),
      `n (%)` = paste0(n, " (", Percent, " %)")
    ) %>%
    select(Category, `n (%)`) %>%
    print(n = Inf)
}

age_valid <- screening$intro_age_num[!is.na(screening$intro_age_num)]
usage_valid <- screening$intro_intensity[!is.na(screening$intro_intensity)]

platform_sample <- purrr::imap_dfr(
  c(
    Facebook = "intro_freq_facebook",
    Instagram = "intro_freq_instagram",
    TikTok = "intro_freq_tiktok",
    X = "intro_freq_x"
  ),
  function(variable, platform) {
    x <- screening[[variable]]
    n_valid <- sum(!is.na(x))
    n_weekly <- sum(x >= 5, na.rm = TRUE)
    n_daily <- sum(x >= 7, na.rm = TRUE)
    
    tibble(
      Platform = platform,
      Valid_N = n_valid,
      Weekly = paste0(n_weekly, " (", round(100 * n_weekly / n_valid, 1), " %)"),
      Daily = paste0(n_daily, " (", round(100 * n_daily / n_valid, 1), " %)")
    )
  }
)

cat(
  "\n============================================================\n",
  "SCREENING SAMPLE DESCRIPTION\n",
  "============================================================\n",
  "Participants: ", nrow(screening), "\n",
  "Age: valid n = ", length(age_valid),
  "; missing = ", sum(is.na(screening$intro_age_num)),
  "; M = ", round(mean(age_valid), 2),
  "; SD = ", round(sd(age_valid), 2),
  "; Median = ", round(median(age_valid), 2),
  "; IQR = ", round(IQR(age_valid), 2),
  "; Range = ", min(age_valid), "–", max(age_valid), "\n",
  "Usage intensity (1–7): valid n = ", length(usage_valid),
  "; missing = ", sum(is.na(screening$intro_intensity)),
  "; M = ", round(mean(usage_valid), 2),
  "; SD = ", round(sd(usage_valid), 2),
  "; Median = ", round(median(usage_valid), 2), "\n",
  sep = ""
)

print_sample_frequency(screening, "age_group", "Age groups")
print_sample_frequency(screening, "gender", "Gender")
print_sample_frequency(screening, "education", "Education")
print_sample_frequency(screening, "education_three_level", "Education (3 levels)")
print_sample_frequency(screening, "context_local", "Typical spatial context")
print_sample_frequency(screening, "context_social", "Typical social context")

cat("\nPlatform use (weekly/daily):\n")
print(platform_sample, n = Inf)

cat("============================================================\n\n")


#===============================================================================
# 03b Figures: sample characteristics
#===============================================================================
# Die bisherigen Screening-Grafiken enthalten keine Soziodemografie. Ergänzt
# werden daher Alter, Geschlecht und Bildung; vorhandene Analyseplots bleiben.

if (create_figures) {
  
  age_mean <- mean(age_valid)
  
  figure_sample_age <- screening %>%
    filter(!is.na(intro_age_num)) %>%
    ggplot(aes(x = intro_age_num)) +
    geom_histogram(
      binwidth = 2,
      boundary = 60,
      fill = unname(project_colors["primary"]),
      colour = unname(project_colors["white"]),
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = age_mean,
      linetype = "dashed",
      linewidth = 0.8,
      colour = unname(project_colors["accent"])
    ) +
    annotate(
      "text",
      x = age_mean,
      y = Inf,
      label = paste0("M = ", round(age_mean, 1)),
      vjust = 1.5,
      hjust = -0.1,
      fontface = "bold",
      colour = unname(project_colors["accent"])
    ) +
    labs(
      title = "Age distribution",
      subtitle = paste0("Screening analysis sample; N = ", nrow(screening)),
      x = "Age in years",
      y = "Participants"
    ) +
    theme_project(base_size = 12, legend_position = "none")
  
  save_project_plot(
    figure_sample_age,
    file.path(figure_folder, "Screening_Sample_Age.png"),
    width = 7.4,
    height = 4.7
  )
  
  
  gender_plot_data <- screening %>%
    filter(!is.na(gender)) %>%
    count(gender, name = "N") %>%
    mutate(
      Percent = 100 * N / sum(N),
      Label = paste0(N, " (", round(Percent, 1), " %)")
    )
  
  figure_sample_gender <- gender_plot_data %>%
    ggplot(aes(x = gender, y = N)) +
    geom_col(
      width = 0.64,
      fill = unname(project_colors["primary"])
    ) +
    geom_text(
      aes(label = Label),
      vjust = -0.45,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Gender distribution",
      subtitle = paste0("Valid responses; n = ", sum(gender_plot_data$N)),
      x = NULL,
      y = "Participants"
    ) +
    theme_project(base_size = 12, legend_position = "none") +
    theme(panel.grid.major.x = element_blank())
  
  save_project_plot(
    figure_sample_gender,
    file.path(figure_folder, "Screening_Sample_Gender.png"),
    width = 6.8,
    height = 4.7
  )
  
  
  education_plot_data <- screening %>%
    filter(!is.na(education)) %>%
    count(education, name = "N") %>%
    mutate(
      Percent = 100 * N / sum(N),
      Label = paste0(N, " (", round(Percent, 1), " %)")
    )
  
  figure_sample_education <- education_plot_data %>%
    ggplot(aes(
      x = N,
      y = forcats::fct_reorder(education, N)
    )) +
    geom_col(
      width = 0.64,
      fill = unname(project_colors["primary"])
    ) +
    geom_text(
      aes(label = Label),
      hjust = -0.12,
      fontface = "bold",
      colour = unname(project_colors["dark"])
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.28))) +
    labs(
      title = "Educational attainment",
      subtitle = paste0("Valid responses; n = ", sum(education_plot_data$N)),
      x = "Participants",
      y = NULL
    ) +
    theme_project(base_size = 11.5, legend_position = "none") +
    theme(panel.grid.major.y = element_blank())
  
  save_project_plot(
    figure_sample_education,
    file.path(figure_folder, "Screening_Sample_Education.png"),
    width = 9.2,
    height = 5.8
  )
}


#===============================================================================
# 04 Platform use
#===============================================================================
# Describes participants' platform ecology. Frequencies, weekly/daily platforms
# and the primary platform are retained for later analyses.

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
        if (m <= 1) "No platform used" else
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
        N_Platforms_Weekly == 0 ~ "No platform weekly",
        N_Platforms_Weekly == 1 ~ "One platform weekly",
        TRUE ~ "Multiple platforms weekly"
      ),
      levels = c(
        "No platform weekly",
        "One platform weekly",
        "Multiple platforms weekly"
      )
    ),
    freq_facebook_label = factor(intro_freq_facebook, 1:8, frequency_levels, ordered = TRUE),
    freq_instagram_label = factor(intro_freq_instagram, 1:8, frequency_levels, ordered = TRUE),
    freq_tiktok_label = factor(intro_freq_tiktok, 1:8, frequency_levels, ordered = TRUE),
    freq_x_label = factor(intro_freq_x, 1:8, frequency_levels, ordered = TRUE)
  )

# Plausibility check: the stop item should be backed by at least one platform
# used weekly. Cases are only flagged, not removed.
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
  warning(n_eligibility_inconsistencies, " cases: stop item and platform frequencies are inconsistent.")
}


#===============================================================================
# 05 Information needs
#===============================================================================
# Besides the four single measures, two compact profile features are built:
# average importance and differentiation between strongest/weakest need.

need_labels <- c(
  intro_ib_undirected = "Undirected",
  intro_ib_thematic = "Thematic",
  intro_ib_social = "Social",
  intro_ib_problem = "Problem-related"
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
        if (length(top) == 1) top else "No single dominant need"
      }
    },
    .groups = "drop"
  )

screening <- screening %>%
  left_join(need_profiles, by = "participant")


#===============================================================================
# 06 Incidental exposure index and reliability
#===============================================================================
# Item 5 is reverse-coded. The index is the mean of all six items, formed only
# for complete responses. Alpha and omega (total and hierarchical) are reported.

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
  Omega_Hierarchical = if (is.null(incidentality_omega)) NA_real_ else incidentality_omega$omega_h,
  Threshold = reliability_threshold,
  Note = paste0(
    "Praereg.: hierarchisches Omega. Da die Skala eindimensional modelliert ist ",
    "(nfactors = 1), ist Omega total die geeignetere Reliabilitaetsschaetzung; ",
    "die Ausschlussentscheidung folgt daher Omega total. Abweichung dokumentiert."
  )
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
# 07 Descriptive results
#===============================================================================
# These tables address only the core questions on sample, use, information needs
# and incidental exposure; detailed sub-tables are avoided.

age_stats <- continuous_summary(screening, "intro_age_num", "Age")
usage_stats <- continuous_summary(screening, "intro_intensity", "Usage intensity")
incidentality_stats <- continuous_summary(screening, "incidentality_index", "Incidental exposure index")

overview_output <- bind_rows(
  tibble(
    Section = "Sample",
    Measure = c(
      "Raw rows", "Eligible participants", "Missing participant codes",
      "Stop-item / platform-frequency inconsistency"
    ),
    Category = NA_character_,
    N = c(nrow(screening_raw), nrow(screening), n_missing_codes, n_eligibility_inconsistencies),
    Percent = NA_real_, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  ),
  age_stats %>% transmute(
    Section = "Sociodemographics", Measure = Variable, Category = NA_character_,
    N = N_Valid, Percent = NA_real_, Mean, SD, Median, Minimum, Maximum
  ),
  usage_stats %>% transmute(
    Section = "Use", Measure = Variable, Category = NA_character_,
    N = N_Valid, Percent = NA_real_, Mean, SD, Median, Minimum, Maximum
  ),
  frequency_summary(screening, "gender", "Gender") %>% transmute(
    Section = "Sociodemographics", Measure = Variable, Category = Level,
    N, Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  ),
  frequency_summary(screening, "education", "Education") %>% transmute(
    Section = "Sociodemographics", Measure = Variable, Category = Level,
    N, Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  ),
  frequency_summary(screening, "context_local", "Typical spatial context") %>% transmute(
    Section = "Context", Measure = Variable, Category = Level,
    N, Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, Minimum = NA_real_, Maximum = NA_real_
  ),
  frequency_summary(screening, "context_social", "Typical social context") %>% transmute(
    Section = "Context", Measure = Variable, Category = Level,
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
    Section = "At least weekly", Platform, Category = NA_character_,
    N = N_Weekly, Percent = Percent_Weekly, Value = NA_real_
  ),
  platform_distribution %>% transmute(
    Section = "Frequency distribution", Platform, Category = frequency_levels[Usage_Frequency],
    N, Percent, Value = Usage_Frequency
  ),
  screening %>% count(Platform_Repertoire, name = "N") %>% mutate(Percent = safe_percent(N, sum(N))) %>%
    transmute(
      Section = "Platform repertoire", Platform = NA_character_, Category = as.character(Platform_Repertoire),
      N, Percent, Value = NA_real_
    ),
  screening %>% count(Primary_Platform, name = "N") %>% mutate(Percent = safe_percent(N, sum(N))) %>%
    transmute(
      Section = "Primary platform", Platform = NA_character_, Category = Primary_Platform,
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
    Section = "Descriptive", Information_Need, Category = NA_character_, N,
    Percent = NA_real_, Mean, SD, Median, CI95_Lower, CI95_Upper
  ),
  need_distribution %>% transmute(
    Section = "Response distribution", Information_Need, Category = as.character(Importance), N,
    Percent, Mean = NA_real_, SD = NA_real_, Median = NA_real_, CI95_Lower = NA_real_, CI95_Upper = NA_real_
  ),
  screening %>% count(Dominant_Information_Need, name = "N") %>% mutate(Percent = safe_percent(N, sum(N))) %>%
    transmute(
      Section = "Dominant need", Information_Need = NA_character_,
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
    Section = "Reliability", Item = NA_character_, Category = "Full scale", N = N_Complete,
    Percent = NA_real_, Mean = Cronbach_Alpha, SD = Omega_Total, Median = NA_real_,
    Value = Threshold, Value_2 = NA_real_
  ),
  alpha_item_stats %>% transmute(
    Section = "Item diagnostics", Item, Category = NA_character_, N = NA_integer_, Percent = NA_real_,
    Mean = Item_Mean, SD = Item_SD, Median = NA_real_,
    Value = Corrected_Item_Total_R, Value_2 = Alpha_If_Deleted
  ),
  omega_item_deleted %>% transmute(
    Section = "Omega if item deleted", Item = Item_Removed, Category = NA_character_,
    N = NA_integer_, Percent = NA_real_, Mean = NA_real_, SD = NA_real_, Median = NA_real_,
    Value = Omega_Total_If_Deleted, Value_2 = NA_real_
  ),
  incidentality_distribution %>% transmute(
    Section = "Item distribution", Item, Category = as.character(Response), N, Percent,
    Mean = NA_real_, SD = NA_real_, Median = NA_real_, Value = Response, Value_2 = NA_real_
  )
)


#===============================================================================
# 08 Exploratory: incidental exposure and sample heterogeneity
#===============================================================================
# These analyses are exploratory (hypothesis-generating) and bridge to the diary
# analysis. Only theoretically meaningful associations are considered.

incidentality_predictors <- c(
  intro_age_num = "Age",
  intro_intensity = "Usage intensity",
  N_Platforms_Weekly = "Platforms used weekly",
  intro_ib_undirected = "Undirected information need",
  intro_ib_thematic = "Thematic information need",
  intro_ib_social = "Social information need",
  intro_ib_problem = "Problem-related information need",
  Mean_Importance = "Mean importance of information needs",
  Need_Differentiation = "Differentiation of information needs"
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
    Analysis = "Incidental exposure correlation"
  )

# Age is additionally related to central use and need measures. This describes
# heterogeneity within the 60+ sample without treating older persons as a
# deficit group.
age_markers <- c(
  intro_intensity = "Usage intensity",
  N_Platforms_Weekly = "Platforms used weekly",
  intro_ib_undirected = "Undirected information need",
  intro_ib_thematic = "Thematic information need",
  intro_ib_social = "Social information need",
  intro_ib_problem = "Problem-related information need",
  incidentality_index = "Incidental exposure"
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
    Analysis = "Age correlation"
  )

# Subgroups are purely descriptive: they show whether screening incidental
# exposure differs visibly by typical context or platform ecology.
subgroup_variables <- c(
  context_local = "Spatial context",
  context_social = "Social context",
  Platform_Repertoire = "Platform repertoire",
  Primary_Platform = "Primary platform",
  age_group = "Age group",
  education_three_level = "Education"
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
      Analysis = "Incidental exposure subgroup", Grouping, Group,
      Variable_1 = NA_character_, Variable_2 = NA_character_, N,
      Mean, SD, Median,
      Spearman_Rho = NA_real_, P_Value = NA_real_, P_Adjusted_BH = NA_real_
    )
)


#===============================================================================
# 09 Save prepared participant data
#===============================================================================
# This RDS is the hand-off dataset for Daily and Outro, so the original
# variables plus central derived features are retained.

saveRDS(screening, output_rds)


#===============================================================================
# 10 Excel: one file, five compact sheets
#===============================================================================
# The Excel file summarizes only interpretable results; purely technical
# intermediate objects are not exported.

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

# Publication-ready tables (.docx) for the manuscript, APA-style. Excel remains
# the working format; these are the clean, interpretable core tables.
n_screening <- nrow(screening)

# Sub-rows of "n (%)" for a categorical variable, indented under a header row.
pct_rows <- function(var) {
  screening %>%
    filter(!is.na(.data[[var]])) %>%
    count(Level = .data[[var]], .drop = FALSE) %>%
    transmute(
      Characteristic = paste0("   ", as.character(Level)),
      Value = paste0(n, " (", fmt_num(100 * n / n_screening, 1), ")")
    )
}

screening_table1 <- bind_rows(
  tibble(Characteristic = "Age in years, M (SD)",
         Value = m_sd(age_stats$Mean, age_stats$SD)),
  tibble(Characteristic = "Usage intensity (1-7), M (SD)",
         Value = m_sd(usage_stats$Mean, usage_stats$SD)),
  tibble(Characteristic = "Platforms used weekly (0-4), M (SD)",
         Value = m_sd(safe_mean(screening$N_Platforms_Weekly),
                      safe_sd(screening$N_Platforms_Weekly))),
  tibble(Characteristic = "Gender, n (%)", Value = ""),
  pct_rows("gender"),
  tibble(Characteristic = "Education, n (%)", Value = ""),
  pct_rows("education_three_level"),
  tibble(Characteristic = "Typical spatial context, n (%)", Value = ""),
  pct_rows("context_local"),
  tibble(Characteristic = "Typical social context, n (%)", Value = ""),
  pct_rows("context_social")
)

save_pub_table(
  screening_table1,
  file.path(tables_folder, "Tab_Screening_Sample.docx"),
  table_number = 1,
  title = "Sample characteristics (screening survey)",
  note = paste0("N = ", n_screening,
                ". M (SD) for continuous measures; n (%) for categorical measures.")
)

save_pub_table(
  need_descriptives %>% transmute(
    `Information need` = Information_Need,
    `M (SD)` = m_sd(Mean, SD),
    `95% CI` = fmt_ci(CI95_Lower, CI95_Upper)
  ),
  file.path(tables_folder, "Tab_Screening_Information_Needs.docx"),
  table_number = 2,
  title = "Information needs (importance ratings)",
  note = "Importance rated on a 5-point scale from 1 (not at all important) to 5 (very important)."
)

save_pub_table(
  tibble(
    Scale = "Incidental exposure",
    k = length(incidentality_items),
    `M (SD)` = m_sd(incidentality_stats$Mean, incidentality_stats$SD),
    `95% CI` = fmt_ci(incidentality_stats$CI95_Lower, incidentality_stats$CI95_Upper),
    `Cronbach's alpha` = fmt_num(reliability_summary$Cronbach_Alpha, 2),
    `Omega total` = fmt_num(reliability_summary$Omega_Total, 2),
    `Omega hierarchical` = fmt_num(reliability_summary$Omega_Hierarchical, 2)
  ),
  file.path(tables_folder, "Tab_Screening_Incidental_Exposure.docx"),
  table_number = 3,
  title = "Incidental exposure index: descriptives and reliability",
  note = paste0(
    "Six-item scale (Ahmadi & Wohn, 2018); item 5 reverse-coded. ",
    "The preregistration specified hierarchical omega; because the scale is ",
    "modeled unidimensionally, omega total is the more appropriate estimate and ",
    "the basis for the single-item-exclusion rule (deviation documented)."
  )
)


#===============================================================================
# 11 Figures
#===============================================================================
# Four core figures using the shared project theme. The goal is quick
# readability: clear typographic hierarchy, direct value labels and the shared
# muted palette from 00_Helpers.R.

if (create_figures) {
  
  # Platform use: absolute counts are the main information; percentages appear
  # only as supplementary labels on the bars.
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
      title = "At least weekly platform use",
      subtitle = paste0("Absolute counts; N = ", nrow(screening)),
      x = "Participants",
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
  
  # Information needs: point + confidence interval emphasizes differences without
  # staging the ordinal 1–5 scales as exact metric bars.
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
      title = "Information needs",
      subtitle = "Means and 95% confidence intervals",
      x = "Mean (1–5)",
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
  
  # Incidental exposure: distribution plus mean for orientation. The dashed line
  # is descriptive, not a threshold.
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
      title = "Incidental exposure (screening)",
      subtitle = "Distribution of the six-item index",
      x = "Incidental exposure index (1–5)",
      y = "Participants"
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
  
  # Exploratory correlations: direction is colour-coded and the effect size is
  # labelled directly on the bar. Significance is deliberately not emphasized.
  figure_exploratory <- incidentality_correlations %>%
    filter(!is.na(Spearman_Rho)) %>%
    mutate(
      Direction = if_else(Spearman_Rho >= 0, "Positive", "Negative"),
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
        "Positive" = unname(project_colors["primary"]),
        "Negative" = unname(project_colors["accent"])
      )
    ) +
    scale_x_continuous(
      limits = c(-1, 1),
      breaks = seq(-1, 1, by = 0.25),
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    labs(
      title = "Exploratory associations with incidental exposure",
      subtitle = "Spearman correlations; raw and BH-adjusted p-values in the Excel output",
      x = "Spearman rho",
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
# Short closing check for the running workflow.

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
