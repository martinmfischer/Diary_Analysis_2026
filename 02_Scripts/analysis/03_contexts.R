# RQ2a: contexts and typical use ------------------------------------------------

# 1. Describe reported and typical contexts ------------------------------------
# "Cannot remember" stays in these descriptive tables.

spatial_context <- frequency_table(factor(
  daily_public$locality_code,
  levels = 1:3,
  labels = c("At home", "Out and about", "Cannot remember")
))

social_context <- frequency_table(factor(
  daily_public$situation_code,
  levels = 1:3,
  labels = c("Alone", "Together with someone", "Cannot remember")
))

intro <- intro %>%
  mutate(across(
    c(intro_context_local, intro_context_situation),
    ~ na_if(as.numeric(.x), -1)
  ))

typical_spatial_context <- frequency_table(factor(
  intro$intro_context_local,
  levels = 1:3,
  labels = c("Mainly at home", "Mainly elsewhere", "Equally often")
))

typical_social_context <- frequency_table(factor(
  intro$intro_context_situation,
  levels = 1:3,
  labels = c("Mainly alone", "Mainly together", "Equally often")
))

# 2. Calculate one context share per person ------------------------------------
# Exclude unknown contexts from each share rather than counting them as "no".

known_contexts <- daily_public %>%
  mutate(
    home = if_else(
      locality_code %in% 1:2,
      as.numeric(locality_code == 1),
      NA_real_
    ),
    alone = if_else(
      situation_code %in% 1:2,
      as.numeric(situation_code == 1),
      NA_real_
    )
  )

context_shares <- known_contexts %>%
  group_by(participant) %>%
  summarise(
    Home_share = mean(home, na.rm = TRUE),
    Alone_share = mean(alone, na.rm = TRUE),
    .groups = "drop"
  )

# The middle category supplies a rank, not a measured 50% share.
typical_contexts <- intro %>%
  transmute(
    participant,
    Typical_home = recode(
      intro_context_local,
      `1` = 1, `2` = 0, `3` = 0.5,
      .default = NA_real_
    ),
    Typical_alone = recode(
      intro_context_situation,
      `1` = 1, `2` = 0, `3` = 0.5,
      .default = NA_real_
    )
  )

context_person <- context_shares %>%
  left_join(typical_contexts, by = "participant")

# 3. Compare typical use with diary shares -------------------------------------
# Exploratory measurement comparison: one pair per person, no causal test.
# Two-sided Spearman p-values are unadjusted, as reported in the manuscript.

context_correlations <- map(c("Home", "Alone"), function(context) {
  pairs <- context_person %>%
    transmute(
      x = .data[[paste0("Typical_", tolower(context))]],
      y = .data[[paste0(context, "_share")]]
    ) %>%
    filter(is.finite(x), is.finite(y))

  has_variation <- nrow(pairs) >= 3 &&
    n_distinct(pairs$x) > 1 &&
    n_distinct(pairs$y) > 1

  rho <- NA_real_
  p_value <- NA_real_

  if (has_variation) {
    correlation <- cor.test(
      pairs$x,
      pairs$y,
      method = "spearman",
      exact = FALSE
    )
    rho <- unname(correlation$estimate)
    p_value <- correlation$p.value
  }

  tibble(
    Context = context,
    N = nrow(pairs),
    Spearman_rho = rho,
    p_raw = p_value
  )
}) %>%
  list_rbind()
