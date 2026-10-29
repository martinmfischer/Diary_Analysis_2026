# Diary study: analyses in manuscript order -------------------------------------
# Start in the project root with a fresh R session.
# Outputs are tables, saved results and figures. Report software is not needed.

# 1. Packages and shared functions ---------------------------------------------

source("02_Scripts/packages.R", encoding = "UTF-8", local = TRUE)
load_packages(c(
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "readr",
  "openxlsx",
  "psych",
  "GPArotation",
  "lme4",
  "boot",
  "ggplot2"
))

source("02_Scripts/analysis/helpers.R", encoding = "UTF-8", local = TRUE)

# 2. Methods: eligibility, coding and sample description -------------------------

source(
  "02_Scripts/analysis/01_data_and_sample.R",
  encoding = "UTF-8",
  local = TRUE
)

# 3. RQ1: topics, sources, formats and platforms ---------------------------------

source("02_Scripts/analysis/02_content.R", encoding = "UTF-8", local = TRUE)

# 4. RQ2a: contexts and comparison with typical use ------------------------------

source("02_Scripts/analysis/03_contexts.R", encoding = "UTF-8", local = TRUE)

# 5. RQ2b: encounter routes and general incidentalness ---------------------------

source(
  "02_Scripts/analysis/04_incidentalness.R",
  encoding = "UTF-8",
  local = TRUE
)

# 6. RQ2c: practices per post and with equal weight per person -------------------

source("02_Scripts/analysis/05_practices.R", encoding = "UTF-8", local = TRUE)

# 7. Describe all survey indices and intro/outro items ---------------------------

source("02_Scripts/analysis/06_survey_measures.R", encoding = "UTF-8", local = TRUE)

# 8. Export tables and save results for optional reporting -----------------------

source("02_Scripts/analysis/07_export.R", encoding = "UTF-8", local = TRUE)

# 9. Draw figures without Markdown, Pandoc or LaTeX ------------------------------

source("02_Scripts/figures/paper_figures.R", encoding = "UTF-8", local = TRUE)
draw_paper_figures(paper_results)

message(
  "Paper analysis complete. Files in ", normalizePath("outputs", winslash = "/"),
  "\n  paper_tables.xlsx: manuscript tables, all survey indices/items, figure data",
  "\n  paper_results.rds: saved tables and model/figure results",
  "\n  sample_audit.csv: inclusion decisions; session_info.txt: software versions",
  "\n  figures/figure_1_topic_routes.png, figure_2_survey_diary.png, ",
  "figure_3_practices.png: manuscript figures"
)
