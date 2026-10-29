# Checks for the paper analysis ------------------------------------------------
# Run after 01_paper_analysis.R in the same session, before loading report scripts.

# 1. Syntax -------------------------------------------------------------------

script_files <- list.files(
  "02_Scripts",
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
)

invisible(lapply(script_files, parse, encoding = "UTF-8"))

# 2. Sample and denominators ---------------------------------------------------

stopifnot(
  all(sample_audit$n_uploads[sample_audit$included] >= 7),
  nrow(intro) == length(included_ids),
  nrow(outro) == length(included_ids),
  !anyDuplicated(daily$screenshot_id),
  all(daily_public$public_rel_coded == 1),
  sum(topics$n) == sum(!is.na(daily_public$topic)),
  sum(topic_source$n) == sum(complete.cases(daily_public[c("topic", "source")])),
  all(survey_diary$N <= length(included_ids))
)

# 3. Model checks and separation from reporting --------------------------------

stopifnot(
  nrow(topic_model_tests) == 2,
  all(topic_model_tests$p_Holm >= topic_model_tests$p_raw),
  all(topic_model_tests$Converged),
  !any(topic_model_tests$Singular),
  !exists("report", inherits = FALSE),
  !exists("render_report_pdf", inherits = FALSE)
)

# 4. "None of the above" versus genuinely missing answers -----------------------

stopifnot(
  all(daily_public$read[daily_public$practice_none] == 0),
  all(daily_public$research[daily_public$practice_none] == 0),
  all(daily_public$engage[daily_public$practice_none] == 0)
)

example <- tibble(
  interaction_read_code = c(-1, NA, 1),
  interaction_research_code = c(-1, NA, 0),
  interaction_engagement_code = c(-1, NA, 0)
)

decoded <- decode_practices(example)

stopifnot(
  decoded$practice_none[1],
  decoded$read[1] == 0,
  is.na(decoded$read[2]),
  decoded$read[3] == 1
)

# 5. Survey scoring and complete output without report software ----------------

score_example <- tibble(a = c("1", "-1", "5"), b = c("1", "5", "NA"))
score_result <- score_survey_items(score_example, c("a", "b"), reversed = "b")
score_means <- rowMeans(score_result, na.rm = FALSE)

stopifnot(
  score_means[1] == 3,
  all(is.na(score_means[2:3])),
  identical(score_example$b, c("1", "5", "NA")),
  inherits(try(score_survey_items(tibble(a = 6), "a"), silent = TRUE), "try-error"),
  setequal(survey_indices$Variable, c("incidentality_index", "reactivity_index", "ease_index")),
  all(survey_indices$N + survey_indices$Missing == length(included_ids)),
  identical(survey_indices$Items, c(6L, 5L, 8L)),
  nrow(survey_intro_items) == 15L,
  nrow(survey_outro_items) == 13L,
  all(survey_indices$Minimum >= 1, na.rm = TRUE),
  all(survey_indices$Maximum <= 5, na.rm = TRUE),
  identical(survey_indices$Mean[1], incidentalness_summary$Mean),
  identical(survey_indices$Alpha[1], incidentalness_reliability$Alpha),
  identical(survey_indices$Omega_total[1], incidentalness_reliability$Omega_total)
)

invalid_omega <- grepl("Inadmissible|Not estimable", survey_indices$Omega_status)
stopifnot(
  all(is.na(survey_indices$Omega_hierarchical[invalid_omega])),
  all(is.na(survey_indices$Omega_total[invalid_omega]))
)

# A valid omega estimate must not survive an inadmissible loading solution.
omega_example <- list(
  omega_h = 0.7,
  omega.tot = 0.8,
  schmid = list(sl = cbind(g = 0.5, h2 = 0.5, u2 = 0.5))
)
stopifnot(describe_omega(omega_example)$Omega_total == 0.8)
omega_example$schmid$sl[, "u2"] <- -0.1
stopifnot(is.na(describe_omega(omega_example)$Omega_total))

paper_output_files <- c(
  "outputs/paper_tables.xlsx", "outputs/paper_results.rds",
  "outputs/sample_audit.csv", "outputs/session_info.txt",
  paste0("outputs/figures/", c(
    "figure_1_topic_routes.png", "figure_2_survey_diary.png", "figure_3_practices.png"
  ))
)

stopifnot(
  all(file.exists(paper_output_files)),
  all(file.size(paper_output_files) > 0),
  all(c("Survey_Indices", "Survey_Intro_items", "Survey_Outro_items",
        "Survey_Index_scoring", "Figure2_Survey_diary") %in%
      getSheetNames("outputs/paper_tables.xlsx"))
)

exported_indices <- read.xlsx(
  "outputs/paper_tables.xlsx", sheet = "Survey_Indices", check.names = FALSE
)
stopifnot(isTRUE(all.equal(
  exported_indices, as.data.frame(survey_indices),
  tolerance = 1e-12, check.attributes = FALSE
)))

# 6. Package bootstrap without a configured CRAN mirror ------------------------
# Substitute the download and attach calls so this check needs no network.
package_check <- new.env()
sys.source("02_Scripts/packages.R", envir = package_check)
package_check$requireNamespace <- function(package, quietly) package == "stats"
package_check$install.packages <- function(packages, repos) {
  stopifnot(identical(packages, "missing_package"), repos == "https://cloud.r-project.org")
  package_check$installed <- TRUE
}
package_check$library <- function(package, character.only) invisible(NULL)
package_check$load_packages(c("stats", "missing_package"))
stopifnot(isTRUE(package_check$installed))

message(
  "Syntax, sample, denominators, practice decoding, ",
  "models, survey scoring and outputs without reporting checked."
)
