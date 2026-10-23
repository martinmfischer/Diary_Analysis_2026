# Diary study: analyses in manuscript order -------------------------------------
# Run from the project root in a fresh R session.
# Outputs: tables, figure data and session information. No report software needed.

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, purrr, stringr, readr, openxlsx, writexl,
  psych, GPArotation, lme4, boot)

source("01_Scripts/analysis/helpers.R", encoding = "UTF-8", local = TRUE)

# 1. Methods: eligibility, coding and sample description
source("01_Scripts/analysis/01_data_and_sample.R", encoding = "UTF-8", local = TRUE)

# 2. RQ1: topics, sources, formats and platforms
source("01_Scripts/analysis/02_content.R", encoding = "UTF-8", local = TRUE)

# 3. RQ2a: contexts and comparison with typical use
source("01_Scripts/analysis/03_contexts.R", encoding = "UTF-8", local = TRUE)

# 4. RQ2b: encounter routes and general incidentalness
source("01_Scripts/analysis/04_incidentalness.R", encoding = "UTF-8", local = TRUE)

# 5. RQ2c: practices, both per post and with equal weight per person
source("01_Scripts/analysis/05_practices.R", encoding = "UTF-8", local = TRUE)

# 6. Export in the same order as the manuscript
source("01_Scripts/analysis/06_export.R", encoding = "UTF-8", local = TRUE)
message("Paper analysis complete: outputs/paper_tables.xlsx")
