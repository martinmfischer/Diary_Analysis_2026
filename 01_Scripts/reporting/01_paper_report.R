# Paper report -----------------------------------------------------------------
# Run 01_Scripts/01_paper_analysis.R first. This script only presents saved results.

# 1. Read results and load the presentation tools ------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(dplyr, tidyr, purrr, ggplot2, knitr, rmarkdown)

results <- readRDS("outputs/paper_results.rds")
tables <- results$tables

source("01_Scripts/reporting/render_pdf.R", encoding = "UTF-8", local = TRUE)
source("01_Scripts/reporting/paper_figures.R", encoding = "UTF-8", local = TRUE)

# 2. Assemble sections in manuscript order -------------------------------------
# To add a result, add a report_section() call with its table and purpose.

report <- c(
  "# Diary study: paper results",
  "",
  paste(
    "Results follow the manuscript order.",
    "Percentages refer to participant-selected submissions."
  ),
  "",

  # Methods and sample

  report_section(
    title = "Sample",
    purpose = paste(
      "Preregistered inclusion: completed initial and closing surveys and",
      "at least seven distinct uploads."
    ),
    table = tables$Methods_Sample_flow
  ),
  report_section(
    title = "Age",
    purpose = "Sample description.",
    table = tables$Methods_Age
  ),
  report_section(
    title = "Gender",
    purpose = "Sample description.",
    table = tables$Methods_Gender
  ),
  report_section(
    title = "Education",
    purpose = "Sample description.",
    table = tables$Methods_Education
  ),
  report_section(
    title = "Participation",
    purpose = paste(
      "Scheduled questionnaire coverage does not measure the encounter",
      "date."
    ),
    table = tables$Methods_Participation
  ),
  report_section(
    title = "Coding reliability",
    purpose = "Stored pretest agreement; no screenshot recoding.",
    table = tables$Methods_Pretest
  ),
  report_section(
    title = "Index reliability",
    purpose = paste(
      "Six-item mean; item 5 reversed. Omega uses the exploratory",
      "three-factor specification reported in the manuscript."
    ),
    table = tables$Methods_Index_reliability
  ),

  # Diary corpus and RQ1

  report_section(
    title = "Diary corpus",
    purpose = "Descriptive relevance assessment of all included uploads.",
    table = tables$Corpus_Relevance
  ),
  report_section(
    title = "Topics (Table 1)",
    purpose = paste(
      "RQ1: percentages among valid topic codes, including residual",
      "categories."
    ),
    table = tables$T1_Topics
  ),
  report_section(
    title = "Sources (Table 2)",
    purpose = paste(
      "RQ1: publishing account types, not original producers or information",
      "quality."
    ),
    table = tables$T2_Sources
  ),
  report_section(
    title = "Topics by source (Table A1)",
    purpose = paste(
      "Exploratory RQ1 description. Each topic is the denominator; both",
      "appendix tables together sum to 100% per topic."
    ),
    table = tables$A1_Topic_Source
  ),
  report_section(
    title = "Topics by source (Table A2)",
    purpose = "Continuation of Table A1.",
    table = tables$A2_Topic_Source
  ),
  report_section(
    title = "Formats (Table 3)",
    purpose = "RQ1: valid format codes.",
    table = tables$T3_Formats
  ),
  report_section(
    title = "Platforms",
    purpose = "RQ1: participants can contribute on multiple platforms.",
    table = tables$RQ1_Platforms
  ),
  report_section(
    title = "Advertising",
    purpose = paste(
      "RQ1: missing advertising codes are excluded from the percentage",
      "denominator."
    ),
    table = tables$RQ1_Advertising
  ),

  # RQ2a: contexts

  report_section(
    title = "Spatial context",
    purpose = "RQ2a: post-level description, including cannot-remember responses.",
    table = tables$RQ2a_Spatial
  ),
  report_section(
    title = "Social context",
    purpose = "RQ2a: post-level description.",
    table = tables$RQ2a_Social
  ),
  report_section(
    title = "Typical spatial context",
    purpose = "Initial-survey context description.",
    table = tables$RQ2a_Typical_spatial
  ),
  report_section(
    title = "Typical social context",
    purpose = "Initial-survey context description.",
    table = tables$RQ2a_Typical_social
  ),
  report_section(
    title = "Typical use and diary contexts",
    purpose = paste(
      "Exploratory measurement comparison; one pair per person. Two-sided,",
      "unadjusted Spearman p-values."
    ),
    table = tables$RQ2a_Context_comparison
  ),

  # RQ2b: incidentalness

  report_section(
    title = "General incidentalness",
    purpose = "Six-item initial-survey index.",
    table = tables$RQ2b_Index
  ),
  report_section(
    title = "Encounter routes",
    purpose = "RQ2b: each post receives equal weight.",
    table = tables$RQ2b_Encounter_posts
  ),
  report_section(
    title = "Encounter routes with equal person weights",
    purpose = paste(
      "Each participant contributes one share per route, including zero",
      "shares."
    ),
    table = tables$RQ2b_Encounter_persons
  ),
  "## Topic composition by encounter route",
  "",
  paste(
    "Exploratory description linking RQ1 and RQ2b. Percentages refer to",
    "valid topic codes within each route. The three most frequent topics",
    "are highlighted. Each post receives equal weight."
  ),
  "",
  "![Topic composition by encounter route](figures/figure_1_topic_routes.png)",
  "",
  report_section(
    title = "Topic differences in encounter routes",
    purpose = paste(
      "Exploratory random-intercept logistic models; Holm correction across",
      "the two omnibus tests. No causal interpretation."
    ),
    table = tables$RQ2b_Topic_models
  ),
  report_section(
    title = "General and encounter-specific incidentalness",
    purpose = paste(
      "Preregistered measurement comparison. Exploratory 95% percentile",
      "intervals resample participants 9,999 times. Both measures are",
      "self-reported."
    ),
    table = tables$RQ2b_Survey_diary,
    figure = "figures/figure_2_survey_diary.png"
  ),

  # RQ2c: practices

  report_section(
    title = "Practices",
    purpose = paste(
      "RQ2c: multiple responses. Three -1 codes denote None of the above;",
      "genuine missing answers stay missing."
    ),
    table = tables$RQ2c_Practice_posts
  ),
  report_section(
    title = "Practices with equal person weights",
    purpose = paste(
      "Each person contributes their own practice share across valid",
      "submitted posts."
    ),
    table = tables$RQ2c_Practice_persons
  ),
  report_section(
    title = "Practices by encounter route",
    purpose = paste(
      "Exploratory connection of RQ2b/c. Person_percent weights people",
      "equally within each route. Participants can occur in several groups;",
      "a person without a route contributes no value for that route."
    ),
    table = tables$Figure3_Practice_routes,
    figure = "figures/figure_3_practices.png"
  )
)

# 3. Write Markdown and render the PDF -----------------------------------------

writeLines(enc2utf8(report), "outputs/paper_report.md")
render_report_pdf("outputs/paper_report.md")
