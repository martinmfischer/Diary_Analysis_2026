################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    05_Advanced_Exploration.R
#
# Zweck:
#   Compact, exploratory integration of the three survey levels (screening,
#   diary, outro). Deliberately lean and focused on preregistered constructs
#   plus established theory. ALL results are exploratory and serve hypothesis
#   generation, not confirmation (n ~ 100).
#
# Contents:
#   1) Integrated sample table (Table 1) across the three levels.
#   2) Co-occurrence: selection/engagement practices by content type, context
#      and incidental exposure (screenshot level, publicly relevant posts).
#   3) Focused participant-level associations (needs / use -> diary measures),
#      Spearman with BH correction per family, effect-size focus.
#   4) Extension: ICC (random intercept) to justify aggregation.
#   5) Extension: one exploratory typology (k-means) with a stability check.
#
# Inputs (aus 04a/04b/04c):
#   03_Output/screening_prepared.rds
#   03_Output/daily_screenshot_level.rds
#   03_Output/daily_participant_level.rds
#   03_Output/outro_prepared.rds
#
# Outputs:
#   03_Output/Advanced_Exploration.xlsx
#   03_Output/Tables/Tab_Adv_*.docx
#   optional: 04_Figures/Advanced/*.png
################################################################################

rm(list = ls())


#===============================================================================
# 01 Settings
#===============================================================================

create_figures <- TRUE
overwrite_outputs <- TRUE
run_icc <- TRUE            # benötigt lme4
run_typology <- TRUE       # benötigt cluster
n_clusters <- 3            # explorative Typologie
exploration_seed <- 4172

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, openxlsx, fs)

helper_script <- file.path("02_Scripts", "00_Helpers.R")
output_folder <- "03_Output"
tables_folder <- file.path(output_folder, "Tables")
figure_folder <- file.path("04_Figures", "Advanced")

if (!file.exists(helper_script)) stop("Helper-Script fehlt: ", helper_script)
source(helper_script)

fs::dir_create(tables_folder)
if (create_figures) fs::dir_create(figure_folder)

output_excel <- file.path(output_folder, "Advanced_Exploration.xlsx")


#===============================================================================
# 02 Load and harmonise the three levels
#===============================================================================

screening   <- readRDS(file.path(output_folder, "screening_prepared.rds"))
daily_posts <- readRDS(file.path(output_folder, "daily_screenshot_level.rds"))
participant <- readRDS(file.path(output_folder, "daily_participant_level.rds"))
outro       <- readRDS(file.path(output_folder, "outro_prepared.rds"))

# Personen-Mastertabelle: Diary-Personenebene + Reaktivität/Ease aus dem Outro.
master <- participant %>%
  left_join(
    outro %>% select(participant, reactivity_index, ease_index),
    by = "participant"
  )

# Öffentlich relevante, codierte Beiträge für die Ko-Okkurrenz-Analysen.
public_posts <- daily_posts %>%
  filter(public_relevance == 1L, coding_completed_binary %in% TRUE)


#===============================================================================
# 03 Local helpers
#===============================================================================

# Praxis-Raten (in %) je Ausprägung einer Gruppierungsvariable, Screenshot-
# gewichtet. Die Personen-Nesting-Struktur wird über die ICC (Abschnitt 06)
# eingeordnet; für die deskriptive Ko-Okkurrenz sind Raten mit N transparent.
practice_vars <- c(
  interaction_read = "Read thoroughly",
  interaction_research = "Sought further info",
  interaction_engagement = "Engaged",
  interaction_any = "Any interaction"
)

cooccurrence <- function(data, group_var, group_label) {
  data %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(Level = as.character(.data[[group_var]])) %>%
    summarise(
      N_Posts = n(),
      across(all_of(names(practice_vars)), ~ 100 * safe_mean(.x)),
      .groups = "drop"
    ) %>%
    rename(!!!setNames(names(practice_vars), unname(practice_vars))) %>%
    mutate(Grouping = group_label, .before = 1)
}

# Ein Spearman-Zusammenhang plus Kennzeichnung der Familie.
assoc <- function(x_var, y_var, x_label, y_label, family) {
  spearman_test(master, x_var, y_var, x_label, y_label) %>%
    mutate(Family = family, .before = 1)
}


#===============================================================================
# 04 Integrated sample table (Table 1)
#===============================================================================

table1_continuous <- purrr::map_dfr(
  c(
    "intro_age_num", "intro_intensity", "incidentality_index",
    "intro_ib_undirected", "intro_ib_thematic", "intro_ib_social", "intro_ib_problem",
    "N_Screenshots", "N_Active_Days", "Share_Publicly_Relevant",
    "Share_Incidental_Broad", "Share_Read_Thoroughly", "Share_Any_Interaction",
    "Share_Home", "Share_Alone", "Topic_Shannon", "Share_Current_Affairs",
    "Share_Video", "reactivity_index", "ease_index"
  ),
  ~ descriptive_summary(master[[.x]], .x)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

table1_categorical <- bind_rows(
  frequency_summary(master, "gender", "Gender"),
  frequency_summary(master, "education_three_level", "Education (3 levels)"),
  frequency_summary(master, "Platform_Repertoire", "Platform repertoire"),
  frequency_summary(master, "Primary_Platform", "Primary platform")
) %>%
  select(Variable, Level, N, Percent = Percent_Valid) %>%
  mutate(Percent = round(Percent, 1))


#===============================================================================
# 05 Co-occurrence: practices x content / context / incidentality
#===============================================================================

cooccurrence_table <- bind_rows(
  cooccurrence(public_posts, "incidentality", "Incidental exposure"),
  cooccurrence(public_posts, "topic_macro", "Topic area"),
  cooccurrence(public_posts, "source_macro", "Source type"),
  cooccurrence(public_posts, "locality", "Spatial context"),
  cooccurrence(public_posts, "situation", "Social context")
) %>%
  mutate(across(all_of(unname(practice_vars)), ~ round(.x, 1)))


#===============================================================================
# 06 Focused participant-level associations (Spearman, BH per family)
#===============================================================================

association_specs <- tribble(
  ~x_var,                 ~y_var,                    ~x_label,                 ~y_label,                       ~family,
  "intro_ib_undirected",  "Share_Current_Affairs",    "Undirected need",         "Share current affairs",    "Need -> content",
  "intro_ib_thematic",    "Share_Knowledge_Interests","Thematic need",           "Share knowledge/culture",  "Need -> content",
  "intro_ib_problem",     "Share_Practical_Service",  "Problem-related need",    "Share practical",          "Need -> content",
  "intro_ib_social",      "Share_Peer_Sources",       "Social need",             "Share peer sources",       "Need -> content",
  "intro_intensity",      "Share_Read_Thoroughly",    "Usage intensity",         "Share read thoroughly",    "Use -> processing",
  "intro_intensity",      "Share_Any_Interaction",    "Usage intensity",         "Share any interaction",    "Use -> processing",
  "incidentality_index",  "Share_Incidental_Broad",   "Screening incidental",    "Diary incidental (broad)", "Calibration",
  "incidentality_index",  "Share_Incidental_Strict",  "Screening incidental",    "Diary incidental (strict)","Calibration",
  "intro_age_num",        "Share_Publicly_Relevant",  "Age",                     "Share publicly relevant",  "Age -> use",
  "intro_age_num",        "Share_Video",              "Age",                     "Share video",              "Age -> use",
  "intro_age_num",        "Topic_Shannon",            "Age",                     "Topic diversity (Shannon)","Age -> use",
  "reactivity_index",     "targeted_post_day_slope",  "Reactivity",              "Trend targeted posts",     "Reactivity (Outro)",
  "reactivity_index",     "upload_count_day_slope",   "Reactivity",              "Trend upload count",       "Reactivity (Outro)"
)

associations <- purrr::pmap_dfr(
  association_specs,
  function(x_var, y_var, x_label, y_label, family) {
    assoc(x_var, y_var, x_label, y_label, family)
  }
) %>%
  group_by(Family) %>%
  mutate(P_BH = p.adjust(P_Value, method = "BH")) %>%
  ungroup() %>%
  transmute(
    Family, Variable_1, Variable_2, N,
    Spearman_Rho = round(Spearman_Rho, 3),
    P_Value = round(P_Value, 4),
    P_BH = round(P_BH, 4)
  )


#===============================================================================
# 07 ICC: justify participant-level aggregation (extension, exploratory)
#===============================================================================

icc_results <- tibble(
  Outcome = character(), ICC = double(), N_Posts = integer(), Note = character()
)

if (run_icc && requireNamespace("lme4", quietly = TRUE)) {
  compute_icc <- function(data, outcome, label) {
    d <- data %>% transmute(participant, y = .data[[outcome]]) %>% drop_na()
    if (n_distinct(d$y) < 2 || n_distinct(d$participant) < 10) {
      return(tibble(Outcome = label, ICC = NA_real_, N_Posts = nrow(d),
                    Note = "Zu wenig Varianz oder Cluster"))
    }
    model <- tryCatch(
      suppressMessages(lme4::glmer(y ~ 1 + (1 | participant), data = d, family = binomial)),
      error = function(e) NULL
    )
    if (is.null(model)) {
      return(tibble(Outcome = label, ICC = NA_real_, N_Posts = nrow(d),
                    Note = "Modell nicht konvergiert"))
    }
    var_between <- as.numeric(lme4::VarCorr(model)$participant)
    tibble(
      Outcome = label,
      ICC = round(var_between / (var_between + pi^2 / 3), 3),
      N_Posts = nrow(d),
      Note = NA_character_
    )
  }

  icc_results <- bind_rows(
    compute_icc(public_posts, "public_relevance", "Public relevance"),
    compute_icc(public_posts, "incidental_broad", "Incidental (broad)"),
    compute_icc(public_posts, "interaction_any", "Any interaction"),
    compute_icc(public_posts, "interaction_read", "Read thoroughly")
  )
} else if (run_icc) {
  message("lme4 not available; ICC section skipped.")
}


#===============================================================================
# 08 Exploratory typology (extension, exploratory)
#===============================================================================

typology_profiles <- tibble()
typology_note <- "nicht berechnet"
cluster_assignment <- tibble()

if (run_typology && requireNamespace("cluster", quietly = TRUE)) {
  typology_vars <- c(
    "Share_Incidental_Broad", "Share_Read_Thoroughly", "Share_Any_Interaction",
    "Topic_Shannon", "Share_Current_Affairs", "Share_Video"
  )

  typology_data <- master %>%
    select(participant, all_of(typology_vars)) %>%
    drop_na()

  # Konstante Merkmale (keine Varianz) würden beim z-Standardisieren NaN
  # erzeugen und werden daher vor dem Clustering entfernt.
  keep_vars <- typology_vars[
    vapply(typology_data[typology_vars], function(z) sd(z) > 0, logical(1))
  ]

  if (nrow(typology_data) >= 30 && length(keep_vars) >= 2) {
    x_scaled <- scale(as.matrix(typology_data[keep_vars]))
    set.seed(exploration_seed)
    km <- kmeans(x_scaled, centers = n_clusters, nstart = 25)

    sil <- cluster::silhouette(km$cluster, dist(x_scaled))
    avg_sil <- round(mean(sil[, "sil_width"]), 2)
    dropped <- setdiff(typology_vars, keep_vars)
    typology_note <- paste0(
      "Exploratory; k-means, k = ", n_clusters, "; N = ", nrow(typology_data),
      "; mean silhouette = ", avg_sil,
      " (< .25 = weakly separated clusters).",
      if (length(dropped) > 0) paste0(" Dropped (no variance): ",
                                      paste(dropped, collapse = ", "), ".") else ""
    )

    cluster_assignment <- typology_data %>%
      transmute(participant, Cluster = km$cluster)

    typology_profiles <- as_tibble(x_scaled) %>%
      mutate(Cluster = km$cluster) %>%
      group_by(Cluster) %>%
      summarise(N = n(), across(all_of(keep_vars), ~ round(mean(.x), 2)),
                .groups = "drop")
  } else {
    typology_note <- "Too few complete cases (< 30) or too little variance."
  }
} else if (run_typology) {
  message("cluster not available; typology section skipped.")
}


#===============================================================================
# 09 Excel workbook and publication tables
#===============================================================================

workbook <- openxlsx::createWorkbook()
header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF", fgFill = "#315F6B", textDecoration = "bold",
  halign = "center", valign = "center"
)

add_excel_sheet(workbook, "Table1_Continuous", table1_continuous, header_style)
add_excel_sheet(workbook, "Table1_Categorical", table1_categorical, header_style)
add_excel_sheet(workbook, "Cooccurrence", cooccurrence_table, header_style)
add_excel_sheet(workbook, "Associations", associations, header_style)
add_excel_sheet(workbook, "ICC", icc_results, header_style)
if (nrow(typology_profiles) > 0) {
  add_excel_sheet(workbook, "Typology_Profiles", typology_profiles, header_style)
}

openxlsx::saveWorkbook(workbook, output_excel, overwrite = overwrite_outputs)

save_pub_table(
  table1_continuous,
  file.path(tables_folder, "Tab_Adv_Table1_Continuous.docx"),
  table_number = 10,
  title = "Integrated sample (continuous measures)"
)
save_pub_table(
  cooccurrence_table,
  file.path(tables_folder, "Tab_Adv_Cooccurrence.docx"),
  table_number = 11,
  title = "Exploratory: practices by content, context and incidental exposure",
  note = "Post-weighted rates (%). For nesting, see the ICC table.",
  digits = 1
)
save_pub_table(
  associations,
  file.path(tables_folder, "Tab_Adv_Associations.docx"),
  table_number = 12,
  title = "Exploratory: participant-level associations",
  note = "Spearman correlations; P_BH = Benjamini-Hochberg within each family.",
  digits = 3
)


#===============================================================================
# 10 Figures
#===============================================================================

if (create_figures) {

  practice_long <- cooccurrence_table %>%
    pivot_longer(all_of(unname(practice_vars)), names_to = "Practice", values_to = "Rate")

  # Practices by incidental exposure.
  fig_incidentality <- practice_long %>%
    filter(Grouping == "Incidental exposure") %>%
    ggplot(aes(x = Rate, y = Practice, fill = Level)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    scale_fill_project() +
    labs(
      title = "Practices by discovery mode",
      subtitle = "Exploratory; share of posts with the respective practice (%)",
      x = "Share (%)", y = NULL, fill = NULL
    ) +
    theme_project()
  save_project_plot(fig_incidentality,
    file.path(figure_folder, "01_Practices_By_Incidentality.png"), width = 8, height = 5)

  # Practices by topic area.
  fig_topic <- practice_long %>%
    filter(Grouping == "Topic area") %>%
    ggplot(aes(x = Rate, y = Level, fill = Practice)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    scale_fill_project() +
    labs(
      title = "Practices by topic area",
      subtitle = "Exploratory; share of posts with the respective practice (%)",
      x = "Share (%)", y = NULL, fill = NULL
    ) +
    theme_project()
  save_project_plot(fig_topic,
    file.path(figure_folder, "02_Practices_By_Topic.png"), width = 8.5, height = 5)

  # Zusammenhangsatlas (Effektstärke im Fokus).
  fig_assoc <- associations %>%
    filter(!is.na(Spearman_Rho)) %>%
    mutate(
      Pair = paste0(Variable_1, " ~ ", Variable_2),
      Pair = fct_reorder(Pair, Spearman_Rho)
    ) %>%
    ggplot(aes(x = Spearman_Rho, y = Pair)) +
    geom_vline(xintercept = 0, colour = unname(project_colors["medium"])) +
    geom_segment(aes(x = 0, xend = Spearman_Rho, yend = Pair),
                 colour = unname(project_colors["secondary"]), linewidth = 0.8) +
    geom_point(size = 3, colour = unname(project_colors["primary"])) +
    facet_grid(rows = vars(Family), scales = "free_y", space = "free_y") +
    scale_x_continuous(limits = c(-1, 1)) +
    labs(
      title = "Exploratory participant-level associations",
      subtitle = "Spearman rho; no confirmatory interpretation",
      x = "Spearman rho", y = NULL
    ) +
    theme_project(legend_position = "none")
  save_project_plot(fig_assoc,
    file.path(figure_folder, "03_Association_Atlas.png"), width = 8.5, height = 7)

  # ICC.
  if (nrow(icc_results) > 0 && any(!is.na(icc_results$ICC))) {
    fig_icc <- icc_results %>%
      filter(!is.na(ICC)) %>%
      ggplot(aes(x = ICC, y = fct_reorder(Outcome, ICC))) +
      geom_col(width = 0.64, fill = unname(project_colors["primary"])) +
      geom_text(aes(label = sprintf("%.2f", ICC)), hjust = -0.15,
                size = 3.3, colour = unname(project_colors["dark"])) +
      scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Intraclass correlation of diary measures",
        subtitle = "Share of variance between participants (justifies aggregation)",
        x = "ICC", y = NULL
      ) +
      theme_project(legend_position = "none")
    save_project_plot(fig_icc,
      file.path(figure_folder, "04_Diary_ICC.png"), width = 7.5, height = 4.6)
  }

  # Typologie-Profile.
  if (nrow(typology_profiles) > 0) {
    fig_typology <- typology_profiles %>%
      pivot_longer(-c(Cluster, N), names_to = "Feature", values_to = "Z") %>%
      mutate(Cluster = paste0("Cluster ", Cluster, " (n = ", N, ")")) %>%
      ggplot(aes(x = Feature, y = Cluster, fill = Z)) +
      geom_tile(colour = "white", linewidth = 0.4) +
      geom_text(aes(label = sprintf("%.1f", Z)), size = 3,
                colour = unname(project_colors["dark"])) +
      scale_fill_gradient2(
        low = unname(project_colors["blue"]), mid = "white",
        high = unname(project_colors["accent"]), midpoint = 0
      ) +
      labs(
        title = "Exploratory typology (z-standardized profiles)",
        subtitle = typology_note,
        x = NULL, y = NULL, fill = "z"
      ) +
      theme_project() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    save_project_plot(fig_typology,
      file.path(figure_folder, "05_Typology_Profiles.png"), width = 9, height = 4.6)
  }
}


#===============================================================================
# 11 Console report
#===============================================================================

cat(
  "\nADVANCED EXPLORATION COMPLETED (exploratory)\n",
  "Participants (master):     ", nrow(master), "\n",
  "Publicly relevant posts:   ", nrow(public_posts), "\n",
  "Associations tested:       ", nrow(associations), "\n",
  "ICCs computed:             ", sum(!is.na(icc_results$ICC)), "\n",
  "Typology:                  ", typology_note, "\n",
  "Excel:                     ", output_excel, "\n",
  sep = ""
)
