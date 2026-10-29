# Report for the selected exploratory analyses ----------------------------------
# Run 02_Scripts/02_selected_exploration.R first; no analysis is repeated here.

# 1. Read saved results --------------------------------------------------------

source("02_Scripts/packages.R", encoding = "UTF-8", local = TRUE)
load_packages(c(
  "dplyr",
  "ggplot2",
  "knitr",
  "rmarkdown",
  "tinytex"
))

results <- readRDS("outputs/selected_exploration.rds")
source("02_Scripts/reporting/render_pdf.R", encoding = "UTF-8", local = TRUE)

source("02_Scripts/figures/exploration_figures.R", encoding = "UTF-8", local = TRUE)
draw_exploration_figures(results)

# 2. Assemble the report -------------------------------------------------------

report <- c(
  "# Selected exploratory analyses",
  "",
  report_section(
    title = "Within-person platform profiles",
    purpose = paste(
      "Descriptive comparison among people with at least three submitted",
      "posts on each platform. Categories with zero posts count as zero",
      "within an observed person-platform combination.",
      "This does not identify a platform effect."
    ),
    table = results$platform_profiles,
    figure = "figures/exploration_platforms.png"
  ),
  report_section(
    title = "Information needs and practices",
    purpose = paste(
      "Theory-guided exploration based on information needs,",
      "not a test of need fulfilment. One pair per person;",
      "Holm correction across three two-sided Spearman tests."
    ),
    table = results$needs_correlations,
    figure = "figures/exploration_needs.png"
  )
)

writeLines(enc2utf8(report), "outputs/selected_exploration.md")
render_report_pdf("outputs/selected_exploration.md")
