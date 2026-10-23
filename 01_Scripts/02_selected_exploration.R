# Selected exploratory analyses ------------------------------------------------
# Independent entry point: read the shared data, without rerunning paper models.
# These comparisons do not change the sample or the paper results.

# 1. Packages and shared data --------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(dplyr, tidyr, purrr, stringr, readr, openxlsx, writexl)

source("01_Scripts/analysis/helpers.R", encoding = "UTF-8", local = TRUE)
source("01_Scripts/analysis/01_data_and_sample.R", encoding = "UTF-8", local = TRUE)
source("01_Scripts/analysis/02_content.R", encoding = "UTF-8", local = TRUE)

# 2. Compare Facebook and Instagram within the same people ---------------------
# At least three submitted posts per platform. Each person receives equal weight.
# This describes platform composition; it does not estimate a platform effect.

paired_ids <- daily_public %>%
  filter(platform %in% c("Facebook", "Instagram")) %>%
  count(participant, platform) %>%
  filter(n >= 3) %>%
  count(participant) %>%
  filter(n == 2) %>%
  pull(participant)

platform_profiles <- map(c("topic", "source"), function(dimension) {
  valid_posts <- daily_public %>%
    filter(
      participant %in% paired_ids,
      platform %in% c("Facebook", "Instagram"),
      !is.na(.data[[dimension]])
    ) %>%
    transmute(participant, platform, Category = .data[[dimension]])

  # A person needs valid codes on both platforms for this particular dimension.
  valid_ids <- valid_posts %>%
    distinct(participant, platform) %>%
    count(participant) %>%
    filter(n == 2) %>%
    pull(participant)

  # Complete categories within observed person-platform pairs, not absent users.
  person_profiles <- valid_posts %>%
    filter(participant %in% valid_ids) %>%
    count(participant, platform, Category) %>%
    group_by(participant, platform) %>%
    complete(Category, fill = list(n = 0)) %>%
    mutate(Percent = 100 * n / sum(n)) %>%
    ungroup()

  person_profiles %>%
    group_by(Category, platform) %>%
    summarise(
      Participants = n(),
      Percent = mean(Percent),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = platform, values_from = Percent) %>%
    mutate(
      Dimension = dimension,
      Difference_pp = Instagram - Facebook,
      .before = 1
    )
}) %>%
  list_rbind()

# 3. Link information needs with practices -------------------------------------
# Three theory-guided associations based on Hasebrink and Domeyer.
# One pair per person; the measures do not establish need fulfilment.

person_practices <- daily_public %>%
  group_by(participant) %>%
  summarise(
    Research = mean(research, na.rm = TRUE),
    Viewing = mean(read, na.rm = TRUE),
    Deliberate_search = mean(
      if_else(
        incidentality_code %in% 1:3,
        as.numeric(incidentality_code == 1),
        NA_real_
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

needs <- intro %>%
  select(
    participant,
    intro_ib_problem,
    intro_ib_thematic,
    intro_ib_undirected
  ) %>%
  mutate(across(-participant, ~ na_if(as.numeric(.x), -1))) %>%
  left_join(person_practices, by = "participant")

needs_plan <- tribble(
  ~Predictor, ~Outcome, ~Rationale,
  "intro_ib_problem", "Research", "Problem-solving needs and further research",
  "intro_ib_thematic", "Deliberate_search", "Thematic interests and deliberate search",
  "intro_ib_undirected", "Viewing", "General orientation and thorough viewing"
)

needs_pairs <- pmap(needs_plan, function(Predictor, Outcome, Rationale) {
  needs %>%
    transmute(
      participant,
      Need = .data[[Predictor]],
      Share = .data[[Outcome]],
      Rationale
    )
}) %>%
  list_rbind() %>%
  filter(is.finite(Need), is.finite(Share))

needs_correlations <- map(needs_plan$Rationale, function(rationale) {
  pairs <- needs_pairs %>%
    filter(Rationale == rationale)

  has_variation <- nrow(pairs) >= 3 &&
    n_distinct(pairs$Need) > 1 &&
    n_distinct(pairs$Share) > 1

  rho <- NA_real_
  p_value <- NA_real_

  if (has_variation) {
    correlation <- cor.test(
      pairs$Need,
      pairs$Share,
      method = "spearman",
      exact = FALSE
    )
    rho <- unname(correlation$estimate)
    p_value <- correlation$p.value
  }

  tibble(
    Rationale = rationale,
    N = nrow(pairs),
    Spearman_rho = rho,
    p_raw = p_value
  )
}) %>%
  list_rbind() %>%
  mutate(p_Holm = p.adjust(p_raw, method = "holm"))

# 4. Save the two comparisons for inspection and optional reporting -------------

dir.create("outputs", showWarnings = FALSE)

write_xlsx(
  list(
    Paired_platform_profiles = platform_profiles,
    Needs = needs_correlations
  ),
  "outputs/selected_exploration.xlsx"
)

saveRDS(
  list(
    platform_profiles = platform_profiles,
    needs_correlations = needs_correlations,
    needs_pairs = needs_pairs
  ),
  "outputs/selected_exploration.rds"
)

message("Selected exploration complete: outputs/selected_exploration.xlsx")
