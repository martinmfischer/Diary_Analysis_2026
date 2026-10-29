# Intro and outro: index and item descriptives ---------------------------------
# Same included sample as the manuscript. Scoring follows the original analysis.
# Item directions checked against the OSF preregistration qbxh8. Full batteries
# are retained; the preregistration permits, but does not require, item removal.

# 1. Score the complete item batteries -----------------------------------------

reactivity_items <- paste0("outro_reactivity_", 1:5)
ease_items <- paste0("outro_ease_", 1:8)
reactivity_reversed <- paste0("outro_reactivity_", c(2, 3, 5))

survey_scales <- list(
  incidentality_index = scored_items,
  reactivity_index = score_survey_items(
    outro, reactivity_items, reversed = reactivity_reversed
  ),
  ease_index = score_survey_items(outro, ease_items)
)

# All items must be valid; a missing response makes the person's index missing.
outro$reactivity_index <- rowMeans(survey_scales$reactivity_index, na.rm = FALSE)
outro$ease_index <- rowMeans(survey_scales$ease_index, na.rm = FALSE)

survey_index_definitions <- tribble(
  ~Survey, ~Variable, ~Measure, ~Group_factors,
  "Intro", "incidentality_index", "General incidentalness index", 3L,
  "Outro", "reactivity_index", "Reactivity index", 2L,
  "Outro", "ease_index", "Ease of use index", 3L
)

# 2. Describe each index and its reliability -----------------------------------
# Preserve the historical omega models; reuse the existing intro fits.

survey_index_tables <- list()

for (index in seq_len(nrow(survey_index_definitions))) {
  definition <- survey_index_definitions[index, ]
  variable <- definition$Variable
  items <- survey_scales[[variable]]
  complete <- drop_na(items)
  alpha <- NA_real_
  omega_summary <- tibble(
    Omega_hierarchical = NA_real_,
    Omega_total = NA_real_,
    Omega_status = "Not estimable: fewer than three complete cases or constant items"
  )

  if (nrow(complete) >= 3 && all(vapply(complete, sd, numeric(1)) > 0)) {
    if (variable == "incidentality_index") {
      # Already estimated in the incidentalness analysis.
      alpha <- alpha_fit$total$raw_alpha
      omega_summary <- incidentalness_omega
    } else {
      alpha <- psych::alpha(
        complete, check.keys = FALSE, warnings = FALSE
      )$total$raw_alpha
      omega <- psych::omega(
        complete, nfactors = definition$Group_factors,
        fm = "minres", rotate = "oblimin",
        option = "equal", flip = FALSE, poly = FALSE, covar = FALSE, plot = FALSE
      )
      omega_summary <- describe_omega(omega)
    }
  }

  survey_index_tables[[variable]] <- describe_numeric(
    rowMeans(items, na.rm = FALSE), definition$Measure
  ) %>%
    mutate(
      Survey = definition$Survey,
      Variable = variable,
      Items = ncol(items),
      Alpha = alpha,
      Omega_hierarchical = omega_summary$Omega_hierarchical,
      Omega_total = omega_summary$Omega_total,
      Group_factors = definition$Group_factors,
      Omega_status = omega_summary$Omega_status
    ) %>%
    select(Survey, Variable, Measure, Items, N:Maximum, everything())
}

survey_indices <- bind_rows(survey_index_tables)

# 3. Describe the original item responses --------------------------------------

# Single-item summaries retain original directions, including reversed items.
intro_item_names <- grep(
  "^intro_(freq_|intensity$|ib_|incidentality_)", names(intro), value = TRUE
)
outro_item_names <- c(reactivity_items, ease_items)

survey_intro_items <- map_dfr(intro_item_names, function(variable) {
  describe_numeric(as.numeric(clean_text(intro[[variable]])), variable) %>%
    mutate(Reverse_for_index = variable == "intro_incidentality_5")
})

survey_outro_items <- map_dfr(outro_item_names, function(variable) {
  describe_numeric(as.numeric(clean_text(outro[[variable]])), variable) %>%
    mutate(Reverse_for_index = variable %in% reactivity_reversed)
})

# 4. Document scoring in the workbook -----------------------------------------

survey_index_scoring <- survey_index_definitions %>%
  select(Survey, Variable, Measure) %>%
  mutate(
    Item_variables = map_chr(survey_scales, ~ paste(names(.x), collapse = ", ")),
    Reversed_items = c(
      "intro_incidentality_5", paste(reactivity_reversed, collapse = ", "), "None"
    ),
    Range_min = 1L, Range_max = 5L,
    Rule = "Mean of all items; complete responses required; reversal = 6 - response",
    Higher_values = c(
      "Greater incidentalness", "Greater study reactivity", "Greater ease of use"
    )
  )
