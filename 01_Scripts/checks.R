# Checks for the paper analysis ------------------------------------------------
# Run after 01_paper_analysis.R in the same session, before loading report scripts.

# 1. Syntax -------------------------------------------------------------------

script_files <- list.files(
  "01_Scripts",
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

message(
  "Syntax, sample, denominators, practice decoding, ",
  "models and reporting separation checked."
)
