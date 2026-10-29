# RQ2b: incidentalness and encounter routes -------------------------------------

# 1. Score the six-item initial-survey index ------------------------------------
# A mean requires all six responses. Item 5 is reversed before scoring.

item_names <- paste0("intro_incidentality_", 1:6)

scored_items <- score_survey_items(
  intro,
  item_names,
  reversed = "intro_incidentality_5"
)

intro$incidentality_index <- rowMeans(scored_items, na.rm = FALSE)

incidentalness_summary <- describe_numeric(
  intro$incidentality_index,
  "General incidentalness index"
)

# 2. Estimate the reliability reported in Methods ------------------------------
# Omega retains the exploratory three-group-factor specification in the paper.

complete_items <- drop_na(scored_items)

alpha_fit <- psych::alpha(
  complete_items,
  check.keys = FALSE,
  warnings = FALSE
)

omega_fit <- psych::omega(
  complete_items,
  nfactors = 3,
  fm = "minres",
  rotate = "oblimin",
  option = "equal",
  flip = FALSE,
  poly = FALSE,
  covar = FALSE,
  plot = FALSE
)

# Do not report omega if the fitted solution has inadmissible variances.
incidentalness_omega <- describe_omega(omega_fit)
stopifnot(!is.na(incidentalness_omega$Omega_total))

incidentalness_reliability <- tibble(
  N = nrow(complete_items),
  Items = 6L,
  Alpha = alpha_fit$total$raw_alpha,
  Omega_hierarchical = omega_fit$omega_h,
  Omega_total = omega_fit$omega.tot
)

# 3. Describe encounter routes with both weightings -----------------------------

encounters <- frequency_table(daily_public$encounter)

# Here every observed person contributes a share for every route, including zero.
# Route-specific practice means below use a different denominator.
encounter_person <- daily_public %>%
  filter(incidentality_code %in% 1:3) %>%
  count(participant, encounter, .drop = FALSE) %>%
  filter(participant %in% included_ids) %>%
  group_by(participant) %>%
  filter(sum(n) > 0) %>%
  mutate(Percent = 100 * n / sum(n)) %>%
  ungroup()

encounter_person_summary <- encounter_person %>%
  group_by(encounter) %>%
  summarise(
    N = n(),
    Percent = mean(Percent),
    .groups = "drop"
  )

# Chance is route 3; without targeted search combines routes 2 and 3.
# Unknown routes remain missing in both indicators.
encounter_indicators <- daily_public %>%
  mutate(
    chance_encounter = if_else(
      incidentality_code %in% 1:3,
      as.numeric(incidentality_code == 3),
      NA_real_
    ),
    without_search = if_else(
      incidentality_code %in% 1:3,
      as.numeric(incidentality_code %in% 2:3),
      NA_real_
    )
  )

person_data <- encounter_indicators %>%
  group_by(participant) %>%
  summarise(
    Posts = n(),
    Chance = mean(chance_encounter, na.rm = TRUE),
    Without_search = mean(without_search, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    select(intro, participant, incidentality_index),
    by = "participant"
  )

calibration <- person_data %>%
  pivot_longer(
    c(Chance, Without_search),
    names_to = "Definition",
    values_to = "Share"
  )

# 4. Compare general incidentalness with diary encounter shares -----------------
# Preregistered measurement comparison, with exploratory percentile intervals.
# Resample whole participants; keep the original seed and sampling sequence.

set.seed(20261020)

survey_diary <- map(c("Chance", "Without_search"), function(definition) {
  pairs <- calibration %>%
    filter(
      Definition == definition,
      is.finite(incidentality_index),
      is.finite(Share)
    )

  bootstrap <- boot::boot(
    pairs,
    statistic = function(data, indices) {
      cor(
        data$incidentality_index[indices],
        data$Share[indices],
        method = "spearman"
      )
    },
    R = 9999
  )

  stopifnot(all(is.finite(bootstrap$t)))
  interval <- boot::boot.ci(bootstrap, type = "perc")$percent[4:5]

  tibble(
    Definition = definition,
    N = nrow(pairs),
    Spearman_rho = unname(bootstrap$t0),
    CI_lower = interval[1],
    CI_upper = interval[2],
    Bootstrap_samples = bootstrap$R
  )
}) %>%
  list_rbind()

# 5. Prepare Figure 1: topic composition within each route ----------------------
# Keep zero-frequency categories. Percentages sum to 100% within a route.

encounter_topics <- daily_public %>%
  filter(!is.na(topic), !is.na(encounter)) %>%
  count(Encounter = encounter, Topic = topic, .drop = FALSE) %>%
  group_by(Encounter) %>%
  mutate(Percent = 100 * n / sum(n)) %>%
  ungroup()

# 6. Test the topic differences reported in Results -----------------------------
# Exploratory random-intercept logistic models: several posts belong to a person.
# Only the two omnibus tests are reported; Holm correction covers both tests.

topic_model_data <- daily_public %>%
  filter(!is.na(topic), incidentality_code %in% 1:3) %>%
  transmute(
    participant = factor(participant),
    topic = droplevels(topic),
    Chance = as.integer(incidentality_code == 3),
    Without_search = as.integer(incidentality_code %in% 2:3)
  )

model_control <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000)
)

topic_models <- list()
topic_model_tests <- list()

for (outcome in c("Chance", "Without_search")) {
  full_formula <- as.formula(paste(outcome, "~ topic + (1 | participant)"))
  null_formula <- as.formula(paste(outcome, "~ 1 + (1 | participant)"))

  full_model <- glmer(
    full_formula,
    data = topic_model_data,
    family = binomial(),
    nAGQ = 9,
    control = model_control
  )

  null_model <- glmer(
    null_formula,
    data = topic_model_data,
    family = binomial(),
    nAGQ = 9,
    control = model_control
  )

  # Stop before interpreting a model with convergence or identification problems.
  for (model in list(full_model, null_model)) {
    convergence_messages <- model@optinfo$conv$lme4$messages
    optimizer_status <- model@optinfo$conv$opt
    hessian_values <- eigen(
      model@optinfo$derivs$Hessian,
      symmetric = TRUE,
      only.values = TRUE
    )$values

    stopifnot(
      length(convergence_messages) == 0,
      optimizer_status == 0,
      !isSingular(model),
      min(hessian_values) > 0
    )
  }

  comparison <- anova(null_model, full_model, test = "Chisq")
  topic_models[[outcome]] <- list(full = full_model, null = null_model)

  topic_model_tests[[outcome]] <- tibble(
    Definition = outcome,
    Posts = nobs(full_model),
    Participants = n_distinct(topic_model_data$participant),
    Chi_square = comparison$Chisq[2],
    df = comparison$Df[2],
    p_raw = comparison$`Pr(>Chisq)`[2],
    Converged = TRUE,
    Singular = isSingular(full_model)
  )
}

topic_model_tests <- list_rbind(topic_model_tests) %>%
  mutate(p_Holm = p.adjust(p_raw, method = "holm"))
