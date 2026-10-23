# RQ2b: the six-item mean requires complete answers; reverse item 5.
item_names <- paste0("intro_incidentality_", 1:6)
scored_items <- intro %>% select(all_of(item_names)) %>%
  mutate(across(everything(), ~ na_if(as.numeric(.x), -1)),
    intro_incidentality_5 = 6 - intro_incidentality_5)
stopifnot(all(is.na(as.matrix(scored_items)) | as.matrix(scored_items) %in% 1:5))
intro$incidentality_index <- rowMeans(scored_items, na.rm = FALSE)
incidentalness_summary <- describe_numeric(intro$incidentality_index, "General incidentalness index")
complete_items <- drop_na(scored_items)
alpha_fit <- psych::alpha(complete_items, check.keys = FALSE, warnings = FALSE)

# Reproduce the manuscript's hierarchical omega (exploratory, three group factors).
omega_fit <- psych::omega(complete_items, nfactors = 3, fm = "minres", rotate = "oblimin",
  option = "equal", flip = FALSE, poly = FALSE, covar = FALSE, plot = FALSE)
omega_loadings <- omega_fit$schmid$sl
stopifnot(all(is.finite(omega_loadings[, c("g", "h2", "u2")])),
  all(omega_loadings[, "u2"] >= 0), all(omega_loadings[, "h2"] <= 1),
  is.finite(omega_fit$omega_h), is.finite(omega_fit$omega.tot),
  omega_fit$omega_h >= 0, omega_fit$omega_h <= omega_fit$omega.tot, omega_fit$omega.tot <= 1)
incidentalness_reliability <- tibble(N = nrow(complete_items), Items = 6L,
  Alpha = alpha_fit$total$raw_alpha, Omega_hierarchical = omega_fit$omega_h,
  Omega_total = omega_fit$omega.tot)

encounters <- frequency_table(daily_public$encounter)
# Zero shares belong to observed people with valid route answers; no-route people
# are not added to a route-specific practice mean later.
encounter_person <- daily_public %>% filter(incidentality_code %in% 1:3) %>%
  count(participant, encounter, .drop = FALSE) %>% filter(participant %in% included_ids) %>%
  group_by(participant) %>% filter(sum(n) > 0) %>%
  mutate(Percent = 100 * n / sum(n)) %>% ungroup()
encounter_person_summary <- encounter_person %>% group_by(encounter) %>%
  summarise(N = n(), Percent = mean(Percent), .groups = "drop")
person_data <- daily_public %>% group_by(participant) %>% summarise(
  Posts = n(), Chance = mean(if_else(incidentality_code %in% 1:3,
    as.numeric(incidentality_code == 3), NA_real_), na.rm = TRUE),
  Without_search = mean(if_else(incidentality_code %in% 1:3,
    as.numeric(incidentality_code %in% 2:3), NA_real_), na.rm = TRUE),
  .groups = "drop") %>% left_join(select(intro, participant, incidentality_index), by = "participant")
calibration <- person_data %>% pivot_longer(c(Chance, Without_search),
  names_to = "Definition", values_to = "Share")

# Preregistered survey-diary comparison; bootstrap intervals added exploratorily.
# Preserve the original seed and sampling sequence for manuscript reproducibility.
set.seed(20261020)
survey_diary <- map(c("Chance", "Without_search"), function(definition) {
  pairs <- calibration %>% filter(Definition == definition,
    is.finite(incidentality_index), is.finite(Share))
  fit <- boot::boot(pairs, statistic = function(data, indices) {
    cor(data$incidentality_index[indices], data$Share[indices], method = "spearman")
  }, R = 9999)
  stopifnot(all(is.finite(fit$t)))
  interval <- boot::boot.ci(fit, type = "perc")$percent[4:5]
  tibble(Definition = definition, N = nrow(pairs), Spearman_rho = unname(fit$t0),
    CI_lower = interval[1], CI_upper = interval[2], Bootstrap_samples = fit$R)
}) %>% list_rbind()

# Figure 1: percentages within each route; keep explicit zero-frequency topics.
encounter_topics <- daily_public %>% filter(!is.na(topic), !is.na(encounter)) %>%
  count(Encounter = encounter, Topic = topic, .drop = FALSE) %>%
  group_by(Encounter) %>% mutate(Percent = 100 * n / sum(n)) %>% ungroup()

# Exploratory topic differences mentioned in Results: participant random intercept.
# Retain the two omnibus tests. Unreported pairwise contrasts/profile models are omitted.
topic_model_data <- daily_public %>% filter(!is.na(topic), incidentality_code %in% 1:3) %>%
  transmute(participant = factor(participant), topic = droplevels(topic),
    Chance = as.integer(incidentality_code == 3),
    Without_search = as.integer(incidentality_code %in% 2:3))
topic_models <- list()
topic_model_tests <- list()
for (outcome in c("Chance", "Without_search")) {
  control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
  full <- glmer(as.formula(paste(outcome, "~ topic + (1 | participant)")),
    data = topic_model_data, family = binomial(), nAGQ = 9, control = control)
  null <- glmer(as.formula(paste(outcome, "~ 1 + (1 | participant)")),
    data = topic_model_data, family = binomial(), nAGQ = 9, control = control)
  for (model in list(full, null)) {
    stopifnot(length(model@optinfo$conv$lme4$messages) == 0,
      model@optinfo$conv$opt == 0, !isSingular(model),
      min(eigen(model@optinfo$derivs$Hessian, symmetric = TRUE, only.values = TRUE)$values) > 0)
  }
  comparison <- anova(null, full, test = "Chisq")
  topic_models[[outcome]] <- list(full = full, null = null)
  topic_model_tests[[outcome]] <- tibble(Definition = outcome, Posts = nobs(full), Participants = n_distinct(topic_model_data$participant),
    Chi_square = comparison$Chisq[2], df = comparison$Df[2], p_raw = comparison$`Pr(>Chisq)`[2],
    Converged = TRUE, Singular = isSingular(full))
}
topic_model_tests <- list_rbind(topic_model_tests) %>% mutate(p_Holm = p.adjust(p_raw, method = "holm"))
