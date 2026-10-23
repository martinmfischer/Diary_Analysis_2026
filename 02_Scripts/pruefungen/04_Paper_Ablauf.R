# Paper-Ablauf einschließlich zentraler fachlicher Regeln prüfen.
# Im Projekt-Hauptverzeichnis in einer frischen R-Sitzung ausführen.
# Berechnet die Paper-Ergebnisse, erzeugt aber keine Reports oder Grafiken.

paper_check <- new.env(parent = globalenv())
source("02_Scripts/02_Paper_Analysen.R", local = paper_check, encoding = "UTF-8")

with(paper_check, {
  stopifnot(
    setequal(included_ids, study_ids),
    all(sample_audit$n_uploads[sample_audit$included] >= 7L),
    nrow(intro) == length(included_ids),
    all(daily_public$public_rel_coded == 1),
    !anyDuplicated(daily$screenshot_id),
    all(daily_public$read[daily_public$practice_none] == 0L),
    all(daily_public$research[daily_public$practice_none] == 0L),
    all(daily_public$engage[daily_public$practice_none] == 0L),
    isTRUE(all.equal(intro$incidentality_index,
      rowMeans(incidentality_scores(intro), na.rm = FALSE))),
    sum(topic_encounter_counts$Screenshots) == nrow(topic_encounters),
    nrow(topic_model_tests) == 2L,
    nrow(profile_comparison) == 3L * nlevels(topic_model_data$Thema),
    all(topic_model_tests$p_Holm >= topic_model_tests$p_roh),
    all(topic_model_diagnostics$Konvergiert),
    all(topic_model_diagnostics$Hessian_positiv),
    !any(topic_model_diagnostics$Singulaer),
    nrow(context_correlations) == 2L,
    all(c("T1_Topics", "A1_Topic_Source", "Model_Tests", "Survey_Diary") %in% names(paper_tables))
  )
})

stopifnot(
  !exists("report", envir = paper_check, inherits = FALSE),
  !exists("save_plot", envir = paper_check, inherits = FALSE),
  !exists("markdown_table", envir = paper_check, inherits = FALSE),
  !exists("render_report_pdf", envir = paper_check, inherits = FALSE)
)
message("Paper-Ablauf: Stichprobe, Nenner, Index, Praxisantworten und Modelle geprüft; Reporting getrennt")
