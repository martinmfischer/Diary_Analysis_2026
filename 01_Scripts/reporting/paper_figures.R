# Figure data were calculated by the paper analysis; this file only draws them.
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
paper_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

# Figure 1: same topic order across routes; highlight the top three in each route.
topic_order <- tables$T1_Topics %>% arrange(Percent) %>% pull(Category) %>% as.character()
topic_plot_data <- results$figure1 %>% group_by(Encounter) %>%
  mutate(Top_three = min_rank(desc(Percent)) <= 3,
    Topic = factor(as.character(Topic), levels = topic_order)) %>% ungroup()
topic_plot <- ggplot(topic_plot_data, aes(Percent, Topic, fill = Top_three)) +
  geom_col() + geom_text(aes(label = sprintf("%.1f%%", Percent)), hjust = -.15, size = 3) +
  facet_wrap(~ Encounter, nrow = 1) +
  scale_fill_manual(values = c(`FALSE` = "#C9DDE3", `TRUE` = "#276B87"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .18))) + paper_theme +
  labs(x = "Posts within encounter route (%)", y = NULL)
ggsave("outputs/figures/figure_1_topic_routes.png", topic_plot,
  width = 14, height = 8, dpi = 220, bg = "white")

# Figure 2: one point per participant; both measures use self-reports.
calibration_labels <- tables$RQ2b_Survey_diary %>%
  mutate(Label = paste0(if_else(Definition == "Chance", "Chance encounters",
    "Encounters without targeted search"), sprintf("\nSpearman rho = %.3f", Spearman_rho)))
calibration_plot_data <- results$calibration %>%
  left_join(select(calibration_labels, Definition, Label), by = "Definition")
calibration_plot <- ggplot(calibration_plot_data, aes(incidentality_index, 100 * Share)) +
  geom_point(colour = "#276B87", alpha = .55, size = 2) + facet_wrap(~ Label, nrow = 1) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) + paper_theme +
  labs(x = "General incidentalness index (initial survey)", y = "Diary share per participant (%)")
ggsave("outputs/figures/figure_2_survey_diary.png", calibration_plot,
  width = 10, height = 5, dpi = 220, bg = "white")

# Figure 3: equal-person practice shares within each route; groups overlap.
practice_plot_data <- results$figure3 %>% filter(Practice != "practice_none") %>%
  mutate(Practice = factor(Practice, levels = c("engage", "research", "read"),
    labels = c("Visible interaction", "Further information seeking", "Thorough reading / viewing")))
route_sizes <- practice_plot_data %>% distinct(encounter, Participants) %>%
  mutate(Label = paste0(encounter, "\n(n = ", Participants, ")"))
practice_plot <- ggplot(practice_plot_data, aes(encounter, Practice, fill = Person_percent)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.1f%%", Person_percent), colour = Person_percent > 55),
    fontface = "bold", size = 4.5) +
  scale_fill_gradient(low = "#EDF3F5", high = "#276B87", limits = c(0, 100), name = "Mean share (%)") +
  scale_colour_manual(values = c(`FALSE` = "#243746", `TRUE` = "white"), guide = "none") +
  scale_x_discrete(labels = setNames(route_sizes$Label, route_sizes$encounter)) +
  labs(x = NULL, y = NULL) + paper_theme + theme(panel.grid = element_blank())
ggsave("outputs/figures/figure_3_practices.png", practice_plot,
  width = 10, height = 4.5, dpi = 220, bg = "white")
