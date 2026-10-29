# Figures from the saved exploratory results; no report software required.
draw_exploration_figures <- function(results) {
  dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

  platform_plot <- ggplot(
    results$platform_profiles,
    aes(Difference_pp, reorder(Category, Difference_pp))
  ) +
    geom_vline(xintercept = 0, colour = "grey80") +
    geom_point(colour = "#276B87") +
    facet_wrap(~Dimension, scales = "free_y", ncol = 1) +
    theme_minimal(base_size = 11) +
    labs(x = "Instagram minus Facebook (percentage points)", y = NULL)

  ggsave(
    "outputs/figures/exploration_platforms.png", platform_plot,
    width = 10, height = 10, bg = "white"
  )

  # Jitter reveals tied responses without changing the y-values.
  needs_plot <- ggplot(results$needs_pairs, aes(Need, 100 * Share)) +
    geom_point(
      alpha = 0.4, colour = "#276B87",
      position = position_jitter(width = 0.08, height = 0, seed = 1)
    ) +
    facet_wrap(~Rationale, ncol = 1) +
    theme_minimal(base_size = 11) +
    scale_x_continuous(breaks = 1:5) +
    labs(x = "Information need (1-5)", y = "Participant practice share (%)")

  ggsave(
    "outputs/figures/exploration_needs.png", needs_plot,
    width = 9, height = 8, bg = "white"
  )
}
