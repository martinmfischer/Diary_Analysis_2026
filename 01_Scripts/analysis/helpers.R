# Study-specific missing tokens; stringr handles whitespace, base R handles NA.
clean_text <- function(x) {
  x <- str_squish(as.character(x))
  x[str_to_upper(x) %in% c("", "-1", "NA", "N/A", "NULL")] <- NA_character_
  x
}

# Repeated table layouts, not alternative statistical estimators.
frequency_table <- function(x) {
  tibble(Category = x) %>%
    filter(!is.na(Category)) %>%
    count(Category, name = "n", .drop = FALSE) %>%
    mutate(Valid_N = sum(!is.na(x)), Missing = sum(is.na(x)),
      Percent = if_else(Valid_N > 0, 100 * n / Valid_N, NA_real_))
}

describe_numeric <- function(x, label) {
  valid <- x[is.finite(x)]
  if (!length(valid)) {
    return(tibble(Measure = label, N = 0L, Missing = length(x), Mean = NA_real_,
      SD = NA_real_, Median = NA_real_, Q1 = NA_real_, Q3 = NA_real_,
      Minimum = NA_real_, Maximum = NA_real_))
  }
  summary <- psych::describe(valid, quant = c(.25, .75), skew = FALSE)
  tibble(Measure = label, N = summary$n, Missing = length(x) - summary$n,
    Mean = summary$mean, SD = summary$sd, Median = summary$median,
    Q1 = summary$Q0.25, Q3 = summary$Q0.75,
    Minimum = summary$min, Maximum = summary$max)
}

# This questionnaire's three -1 codes mean the explicit "None of the above".
# A single missing/unknown code stays missing. Do not apply this to other items.
decode_practices <- function(data) {
  columns <- c("interaction_read_code", "interaction_research_code",
    "interaction_engagement_code")
  data %>%
    mutate(across(all_of(columns), as.numeric),
      practice_none = if_all(all_of(columns), ~ !is.na(.x) & .x == -1),
      read = if_else(practice_none, 0, if_else(interaction_read_code %in% 0:1,
        interaction_read_code, NA_real_)),
      research = if_else(practice_none, 0, if_else(interaction_research_code %in% 0:1,
        interaction_research_code, NA_real_)),
      engage = if_else(practice_none, 0, if_else(interaction_engagement_code %in% 0:1,
        interaction_engagement_code, NA_real_)))
}
