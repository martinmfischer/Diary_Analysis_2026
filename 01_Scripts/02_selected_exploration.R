# Two optional extensions; neither is required to reproduce the paper.
# Run the paper analysis first. No additional exclusions affect its sample.
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, purrr, stringr, readr, openxlsx, writexl)
source("01_Scripts/analysis/helpers.R", encoding = "UTF-8", local = TRUE)
source("01_Scripts/analysis/01_data_and_sample.R", encoding = "UTF-8", local = TRUE)
source("01_Scripts/analysis/02_content.R", encoding = "UTF-8", local = TRUE)

# 1. Content on Facebook and Instagram within the same people.
# At least three posts on each platform. Equal person weights, descriptive only.
# This separates platform composition from differences in who uses each platform.
paired_ids <- daily_public %>% filter(platform %in% c("Facebook", "Instagram")) %>%
  count(participant, platform) %>% filter(n >= 3) %>% count(participant) %>%
  filter(n == 2) %>% pull(participant)
platform_profiles <- map(c("topic", "source"), function(dimension) {
  valid <- daily_public %>% filter(participant %in% paired_ids,
    platform %in% c("Facebook", "Instagram"), !is.na(.data[[dimension]])) %>%
    transmute(participant, platform, Category = .data[[dimension]])
  valid_ids <- valid %>% distinct(participant, platform) %>% count(participant) %>%
    filter(n == 2) %>% pull(participant)
  valid %>% filter(participant %in% valid_ids) %>% count(participant, platform, Category) %>%
    group_by(participant, platform) %>% complete(Category, fill = list(n = 0)) %>%
    mutate(Percent = 100 * n / sum(n)) %>% ungroup() %>% group_by(Category, platform) %>%
    summarise(Participants = n(), Percent = mean(Percent), .groups = "drop") %>%
    pivot_wider(names_from = platform, values_from = Percent) %>%
    mutate(Dimension = dimension, Difference_pp = Instagram - Facebook, .before = 1)
}) %>% list_rbind()

# 2. Information needs and reported practices (Hasebrink and Domeyer).
# Three theory-guided associations; one pair per person. No need-fulfilment claim.
needs <- intro %>% select(participant, intro_ib_problem, intro_ib_thematic, intro_ib_undirected) %>%
  mutate(across(-participant, ~ na_if(as.numeric(.x), -1))) %>%
  left_join(daily_public %>% group_by(participant) %>% summarise(
    Research = mean(research, na.rm = TRUE), Viewing = mean(read, na.rm = TRUE),
    Deliberate_search = mean(if_else(incidentality_code %in% 1:3,
      as.numeric(incidentality_code == 1), NA_real_), na.rm = TRUE), .groups = "drop"), by = "participant")
needs_plan <- tribble(~Predictor, ~Outcome, ~Rationale,
  "intro_ib_problem", "Research", "Problem-solving needs and further research",
  "intro_ib_thematic", "Deliberate_search", "Thematic interests and deliberate search",
  "intro_ib_undirected", "Viewing", "General orientation and thorough viewing")
needs_pairs <- pmap(needs_plan, function(Predictor, Outcome, Rationale) {
  needs %>% transmute(participant, Need = .data[[Predictor]], Share = .data[[Outcome]], Rationale)
}) %>% list_rbind() %>% filter(is.finite(Need), is.finite(Share))
needs_correlations <- map(needs_plan$Rationale, function(rationale) {
  pairs <- filter(needs_pairs, Rationale == rationale)
  fit <- if (nrow(pairs) >= 3 && n_distinct(pairs$Need) > 1 && n_distinct(pairs$Share) > 1)
    cor.test(pairs$Need, pairs$Share, method = "spearman", exact = FALSE) else NULL
  tibble(Rationale = rationale, N = nrow(pairs),
    Spearman_rho = if (is.null(fit)) NA_real_ else unname(fit$estimate),
    p_raw = if (is.null(fit)) NA_real_ else fit$p.value)
}) %>% list_rbind() %>% mutate(p_Holm = p.adjust(p_raw, method = "holm"))

dir.create("outputs", showWarnings = FALSE)
write_xlsx(list(Paired_platform_profiles = platform_profiles, Needs = needs_correlations),
  "outputs/selected_exploration.xlsx")
saveRDS(list(platform_profiles = platform_profiles, needs_correlations = needs_correlations,
  needs_pairs = needs_pairs), "outputs/selected_exploration.rds")
message("Selected exploration complete: outputs/selected_exploration.xlsx")
