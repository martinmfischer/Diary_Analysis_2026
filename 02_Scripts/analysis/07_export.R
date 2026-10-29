# Export the results -----------------------------------------------------------

# 1. Assemble workbook sheets in manuscript order ------------------------------

paper_tables <- list(
  Methods_Sample_flow = sample_flow,
  Methods_Age = sample_age,
  Methods_Gender = sample_gender,
  Methods_Education = sample_education,
  Methods_Participation = participation_summary,
  Methods_Pretest = pretest_reliability,
  Methods_Index_reliability = incidentalness_reliability,
  Survey_Indices = survey_indices,
  Survey_Intro_items = survey_intro_items,
  Survey_Outro_items = survey_outro_items,
  Survey_Index_scoring = survey_index_scoring,
  Corpus_Relevance = relevance,
  T1_Topics = topics,
  T2_Sources = sources,
  A1_Topic_Source = table_a1,
  A2_Topic_Source = table_a2,
  T3_Formats = formats,
  RQ1_Platforms = platforms,
  RQ1_Advertising = advertising,
  RQ2a_Spatial = spatial_context,
  RQ2a_Social = social_context,
  RQ2a_Typical_spatial = typical_spatial_context,
  RQ2a_Typical_social = typical_social_context,
  RQ2a_Context_comparison = context_correlations,
  RQ2b_Index = incidentalness_summary,
  RQ2b_Encounter_posts = encounters,
  RQ2b_Encounter_persons = encounter_person_summary,
  Figure1_Topic_routes = encounter_topics,
  RQ2b_Topic_models = topic_model_tests,
  RQ2b_Survey_diary = survey_diary,
  Figure2_Survey_diary = calibration,
  RQ2c_Practice_posts = practice_summary,
  RQ2c_Practice_persons = practice_person_summary,
  Figure3_Practice_routes = practices_by_route
)

practice_labels <- c(
  read = "Thorough reading / viewing",
  research = "Further information seeking",
  engage = "Visible interaction",
  practice_none = "None of the above"
)

practice_table_names <- c(
  "RQ2c_Practice_posts",
  "RQ2c_Practice_persons",
  "Figure3_Practice_routes"
)

# Labels change only in the exported tables; raw practice codes remain available.
paper_tables[practice_table_names] <- map(
  paper_tables[practice_table_names],
  ~ mutate(.x, Practice = recode(Practice, !!!practice_labels))
)

# 2. Write tables and the sample audit -----------------------------------------

dir.create("outputs", showWarnings = FALSE)
write_result_tables(
  paper_tables, "outputs/paper_tables.xlsx",
  c("Survey_Indices", "Survey_Intro_items", "Survey_Outro_items",
    "Survey_Index_scoring", "Figure2_Survey_diary")
)
write_csv(sample_audit, "outputs/sample_audit.csv")

# 3. Save the inputs for optional reporting ------------------------------------
# Reports read these results instead of rerunning the analyses.

paper_results <- list(
  tables = paper_tables,
  topic_source = topic_source,
  calibration = calibration,
  topic_models = topic_models,
  figure1 = encounter_topics,
  figure3 = practices_by_route
)

saveRDS(paper_results, "outputs/paper_results.rds")
writeLines(capture.output(sessionInfo()), "outputs/session_info.txt")
