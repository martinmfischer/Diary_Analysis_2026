# Diary study: analyses in manuscript order -------------------------------------
# Start in the project root with a fresh R session.
# Outputs are tables and saved results. Report software is not needed here.

# 1. Packages and shared functions ---------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  dplyr,
  tidyr,
  purrr,
  stringr,
  readr,
  openxlsx,
  writexl,
  psych,
  GPArotation,
  lme4,
  boot
)

source("01_Scripts/analysis/helpers.R", encoding = "UTF-8", local = TRUE)

# 2. Methods: eligibility, coding and sample description -------------------------

source(
  "01_Scripts/analysis/01_data_and_sample.R",
  encoding = "UTF-8",
  local = TRUE
)

# 3. RQ1: topics, sources, formats and platforms ---------------------------------

source("01_Scripts/analysis/02_content.R", encoding = "UTF-8", local = TRUE)

# 4. RQ2a: contexts and comparison with typical use ------------------------------

source("01_Scripts/analysis/03_contexts.R", encoding = "UTF-8", local = TRUE)

# 5. RQ2b: encounter routes and general incidentalness ---------------------------

source(
  "01_Scripts/analysis/04_incidentalness.R",
  encoding = "UTF-8",
  local = TRUE
)

# 6. RQ2c: practices per post and with equal weight per person -------------------

source("01_Scripts/analysis/05_practices.R", encoding = "UTF-8", local = TRUE)

# 7. Export tables and save results for optional reporting -----------------------

source("01_Scripts/analysis/06_export.R", encoding = "UTF-8", local = TRUE)

message("Paper analysis complete: outputs/paper_tables.xlsx")
