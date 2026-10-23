# RQ2a: include "Cannot remember" in descriptive tables, exclude it from shares.
spatial_context <- frequency_table(factor(daily_public$locality_code, 1:3,
  c("At home", "Out and about", "Cannot remember")))
social_context <- frequency_table(factor(daily_public$situation_code, 1:3,
  c("Alone", "Together with someone", "Cannot remember")))
intro <- intro %>% mutate(across(c(intro_context_local, intro_context_situation),
  ~ na_if(as.numeric(.x), -1)))
typical_spatial_context <- frequency_table(factor(intro$intro_context_local, 1:3,
  c("Mainly at home", "Mainly elsewhere", "Equally often")))
typical_social_context <- frequency_table(factor(intro$intro_context_situation, 1:3,
  c("Mainly alone", "Mainly together", "Equally often")))

# Exploratory measurement comparison: one valid pair per person, not a causal test.
# The middle category supplies a rank, not an observed 50% share.
context_person <- daily_public %>% group_by(participant) %>% summarise(
  Home_share = mean(if_else(locality_code %in% 1:2, as.numeric(locality_code == 1), NA_real_), na.rm = TRUE),
  Alone_share = mean(if_else(situation_code %in% 1:2, as.numeric(situation_code == 1), NA_real_), na.rm = TRUE),
  .groups = "drop") %>% left_join(intro %>% transmute(participant,
    Typical_home = recode(intro_context_local, `1` = 1, `2` = 0, `3` = .5, .default = NA_real_),
    Typical_alone = recode(intro_context_situation, `1` = 1, `2` = 0, `3` = .5, .default = NA_real_)),
    by = "participant")
context_correlations <- map(c("Home", "Alone"), function(context) {
  pairs <- context_person %>% transmute(x = .data[[paste0("Typical_", tolower(context))]],
    y = .data[[paste0(context, "_share")]]) %>% filter(is.finite(x), is.finite(y))
  fit <- if (nrow(pairs) >= 3 && n_distinct(pairs$x) > 1 && n_distinct(pairs$y) > 1)
    cor.test(pairs$x, pairs$y, method = "spearman", exact = FALSE) else NULL
  tibble(Context = context, N = nrow(pairs),
    Spearman_rho = if (is.null(fit)) NA_real_ else unname(fit$estimate),
    p_raw = if (is.null(fit)) NA_real_ else fit$p.value)
}) %>% list_rbind()
