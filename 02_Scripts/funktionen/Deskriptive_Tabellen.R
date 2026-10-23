# 05 Funktionen für wiederkehrende deskriptive Tabellen -------------------------

# Die Funktionen geben Tabellen zurück und verändern keine Datensätze.
describe_variable <- function(x, label) {
  valid_values <- x[!is.na(x)]

  tibble(
    Variable = label,
    N = length(valid_values),
    Fehlend = sum(is.na(x)),
    Mittelwert = safe_mean(x),
    SD = safe_sd(x),
    Median = safe_median(x),
    Q1 = if (length(valid_values) > 0) unname(quantile(valid_values, .25)) else NA_real_,
    Q3 = if (length(valid_values) > 0) unname(quantile(valid_values, .75)) else NA_real_,
    Minimum = safe_min(x),
    Maximum = safe_max(x)
  )
}

frequency_table <- function(x) {
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))

  # Bei Faktoren bleiben auch Kategorien mit null Beobachtungen sichtbar.
  tibble(Kategorie = x) %>%
    filter(!is.na(Kategorie)) %>%
    count(Kategorie, name = "n", .drop = FALSE) %>%
    mutate(
      Gueltig_N = n_valid,
      Fehlend_N = n_missing,
      Prozent = 100 * safe_divide(n, n_valid)
    )
}

participant_distribution <- function(data, variable) {
  valid_data <- data %>%
    select(participant, Kategorie = all_of(variable)) %>%
    filter(!is.na(Kategorie))

  categories <- if (is.factor(data[[variable]])) {
    levels(data[[variable]])
  } else {
    sort(unique(valid_data$Kategorie))
  }

  denominators <- valid_data %>% count(participant, name = "n_valid")
  category_counts <- valid_data %>% count(participant, Kategorie, name = "n")

  # Nicht beobachtete Kategorien zählen innerhalb einer Person als null.
  expand_grid(participant = denominators$participant, Kategorie = categories) %>%
    left_join(category_counts, by = c("participant", "Kategorie")) %>%
    left_join(denominators, by = "participant") %>%
    mutate(share = replace_na(n, 0) / n_valid) %>%
    group_by(Kategorie) %>%
    summarise(
      Personen_N = n(),
      Mittlerer_Anteil_Prozent = 100 * mean(share),
      SD_Prozentpunkte = 100 * sd(share),
      .groups = "drop"
    )
}
