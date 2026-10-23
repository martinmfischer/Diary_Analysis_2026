# Optional presentation layer. First run 01_Scripts/01_paper_analysis.R.
# To extend: add a report_section() call using a table saved by the analysis.
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, purrr, ggplot2, knitr, rmarkdown)
results <- readRDS("outputs/paper_results.rds")
tables <- results$tables
source("01_Scripts/reporting/render_pdf.R", encoding = "UTF-8", local = TRUE)
source("01_Scripts/reporting/paper_figures.R", encoding = "UTF-8", local = TRUE)

report <- c("# Diary study: paper results", "",
  "Results follow the manuscript order. Percentages refer to participant-selected submissions.", "",
  report_section("Sample", "Preregistered inclusion: completed initial and closing surveys and at least seven distinct uploads.", tables$Methods_Sample_flow),
  report_section("Age", "Sample description.", tables$Methods_Age),
  report_section("Gender", "Sample description.", tables$Methods_Gender),
  report_section("Education", "Sample description.", tables$Methods_Education),
  report_section("Participation", "Scheduled questionnaire coverage does not measure the encounter date.", tables$Methods_Participation),
  report_section("Coding reliability", "Stored pretest agreement; no screenshot recoding.", tables$Methods_Pretest),
  report_section("Index reliability", "Six-item mean; item 5 reversed. Omega uses the exploratory three-factor specification reported in the manuscript.", tables$Methods_Index_reliability),
  report_section("Diary corpus", "Descriptive relevance assessment of all included uploads.", tables$Corpus_Relevance),
  report_section("Topics (Table 1)", "RQ1: percentages among valid topic codes, including residual categories.", tables$T1_Topics),
  report_section("Sources (Table 2)", "RQ1: publishing account types, not original producers or information quality.", tables$T2_Sources),
  report_section("Topics by source (Table A1)", "Exploratory RQ1 description. Each topic is the denominator; both appendix tables together sum to 100% per topic.", tables$A1_Topic_Source),
  report_section("Topics by source (Table A2)", "Continuation of Table A1.", tables$A2_Topic_Source),
  report_section("Formats (Table 3)", "RQ1: valid format codes.", tables$T3_Formats),
  report_section("Platforms", "RQ1: participants can contribute on multiple platforms.", tables$RQ1_Platforms),
  report_section("Advertising", "RQ1: missing advertising codes are excluded from the percentage denominator.", tables$RQ1_Advertising),
  report_section("Spatial context", "RQ2a: post-level description, including cannot-remember responses.", tables$RQ2a_Spatial),
  report_section("Social context", "RQ2a: post-level description.", tables$RQ2a_Social),
  report_section("Typical spatial context", "Initial-survey context description.", tables$RQ2a_Typical_spatial),
  report_section("Typical social context", "Initial-survey context description.", tables$RQ2a_Typical_social),
  report_section("Typical use and diary contexts", "Exploratory measurement comparison; one pair per person. Two-sided, unadjusted Spearman p-values.", tables$RQ2a_Context_comparison),
  report_section("General incidentalness", "Six-item initial-survey index.", tables$RQ2b_Index),
  report_section("Encounter routes", "RQ2b: each post receives equal weight.", tables$RQ2b_Encounter_posts, "figures/figure_1_topic_routes.png"),
  report_section("Encounter routes with equal person weights", "Each participant contributes one share per route, including zero shares.", tables$RQ2b_Encounter_persons),
  report_section("Topic differences in encounter routes", "Exploratory random-intercept logistic models; Holm correction across the two omnibus tests. No causal interpretation.", tables$RQ2b_Topic_models),
  report_section("General and encounter-specific incidentalness", "Preregistered measurement comparison. Exploratory 95% percentile intervals resample participants 9,999 times. Both measures are self-reported.", tables$RQ2b_Survey_diary, "figures/figure_2_survey_diary.png"),
  report_section("Practices", "RQ2c: multiple responses. Three -1 codes denote None of the above; genuine missing answers stay missing.", tables$RQ2c_Practice_posts),
  report_section("Practices with equal person weights", "Each person contributes their own practice share across valid submitted posts.", tables$RQ2c_Practice_persons),
  report_section("Practices by encounter route", "Exploratory connection of RQ2b/c. Person_percent weights people equally within each route. Participants can occur in several groups; a person without a route contributes no value for that route.", tables$Figure3_Practice_routes,
    "figures/figure_3_practices.png"))
writeLines(enc2utf8(report), "outputs/paper_report.md")
render_report_pdf("outputs/paper_report.md")
