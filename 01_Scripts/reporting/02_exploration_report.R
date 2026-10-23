# Optional presentation layer. First run 01_Scripts/02_selected_exploration.R.
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, knitr, rmarkdown)
results <- readRDS("outputs/selected_exploration.rds")
source("01_Scripts/reporting/render_pdf.R", encoding = "UTF-8", local = TRUE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
platform_plot <- ggplot(results$platform_profiles,
  aes(Difference_pp, reorder(Category, Difference_pp))) +
  geom_vline(xintercept = 0, colour = "grey80") + geom_point(colour = "#276B87") +
  facet_wrap(~ Dimension, scales = "free_y", ncol = 1) + theme_minimal(base_size = 11) +
  labs(x = "Instagram minus Facebook (percentage points)", y = NULL)
ggsave("outputs/figures/exploration_platforms.png", platform_plot, width = 10, height = 10, bg = "white")
needs_plot <- ggplot(results$needs_pairs, aes(Need, 100 * Share)) +
  geom_point(alpha = .4, colour = "#276B87", position = position_jitter(width = .08, height = 0, seed = 1)) +
  facet_wrap(~ Rationale, ncol = 1) + theme_minimal(base_size = 11) +
  scale_x_continuous(breaks = 1:5) + labs(x = "Information need (1-5)", y = "Participant practice share (%)")
ggsave("outputs/figures/exploration_needs.png", needs_plot, width = 9, height = 8, bg = "white")
report <- c("# Selected exploratory analyses", "",
  report_section("Within-person platform profiles",
    "Descriptive comparison among people with at least three submitted posts on each platform. Categories with zero posts count as zero within an observed person-platform combination. This does not identify a platform effect.",
    results$platform_profiles, "figures/exploration_platforms.png"),
  report_section("Information needs and practices",
    "Theory-guided exploration based on information needs, not a test of need fulfilment. One pair per person; Holm correction across three two-sided Spearman tests.",
    results$needs_correlations, "figures/exploration_needs.png"))
writeLines(enc2utf8(report), "outputs/selected_exploration.md")
render_report_pdf("outputs/selected_exploration.md")
