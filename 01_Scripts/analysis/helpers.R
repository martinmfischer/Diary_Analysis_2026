# Shared data rules and table layouts -------------------------------------------

# Export-specific missing tokens; do not apply to numeric practice codes.
clean_text <- function(x) {
  x <- str_squish(as.character(x))
  missing_tokens <- c("", "-1", "NA", "N/A", "NULL")
  x[str_to_upper(x) %in% missing_tokens] <- NA_character_

  x
}

# Keep zero-frequency factor levels; percentages exclude missing responses.
frequency_table <- function(x) {
  tibble(Category = x) %>%
    filter(!is.na(Category)) %>%
    count(Category, name = "n", .drop = FALSE) %>%
    mutate(
      Valid_N = sum(!is.na(x)),
      Missing = sum(is.na(x)),
      Percent = if_else(Valid_N > 0, 100 * n / Valid_N, NA_real_)
    )
}

# Use psych for the statistics; return the same columns for empty inputs.
describe_numeric <- function(x, label) {
  valid <- x[is.finite(x)]

  if (length(valid) == 0) {
    return(tibble(
      Measure = label,
      N = 0L,
      Missing = length(x),
      Mean = NA_real_,
      SD = NA_real_,
      Median = NA_real_,
      Q1 = NA_real_,
      Q3 = NA_real_,
      Minimum = NA_real_,
      Maximum = NA_real_
    ))
  }

  summary <- psych::describe(valid, quant = c(0.25, 0.75), skew = FALSE)

  tibble(
    Measure = label,
    N = summary$n,
    Missing = length(x) - summary$n,
    Mean = summary$mean,
    SD = summary$sd,
    Median = summary$median,
    Q1 = summary$Q0.25,
    Q3 = summary$Q0.75,
    Minimum = summary$min,
    Maximum = summary$max
  )
}

# Three -1 codes mean the exclusive "None of the above" response in this item.
# Other unknown or missing responses stay missing, not zero.
decode_practices <- function(data) {
  practice_columns <- c(
    "interaction_read_code",
    "interaction_research_code",
    "interaction_engagement_code"
  )

  data %>%
    mutate(
      across(all_of(practice_columns), as.numeric),
      practice_none = if_all(
        all_of(practice_columns),
        ~ !is.na(.x) & .x == -1
      ),
      read = case_when(
        practice_none ~ 0,
        interaction_read_code %in% 0:1 ~ interaction_read_code,
        TRUE ~ NA_real_
      ),
      research = case_when(
        practice_none ~ 0,
        interaction_research_code %in% 0:1 ~ interaction_research_code,
        TRUE ~ NA_real_
      ),
      engage = case_when(
        practice_none ~ 0,
        interaction_engagement_code %in% 0:1 ~ interaction_engagement_code,
        TRUE ~ NA_real_
      )
    )
}
