# Shared data rules and table layouts -------------------------------------------

# 1. Missing values ------------------------------------------------------------

# Export-specific missing tokens; do not apply to numeric practice codes.
clean_text <- function(x) {
  x <- str_squish(as.character(x))
  missing_tokens <- c("", "-1", "NA", "N/A", "NULL")
  x[str_to_upper(x) %in% missing_tokens] <- NA_character_

  x
}

# 2. Descriptive tables --------------------------------------------------------

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

# 3. Survey scoring and reliability --------------------------------------------

# Score five-point survey items without changing the original responses.
score_survey_items <- function(data, item_names, reversed = character()) {
  items <- data %>%
    select(all_of(item_names)) %>%
    mutate(across(everything(), clean_text))
  numeric_items <- mutate(items, across(everything(), as.numeric))

  stopifnot(
    all(reversed %in% item_names),
    !any(!is.na(as.matrix(items)) & is.na(as.matrix(numeric_items))),
    all(is.na(as.matrix(numeric_items)) | as.matrix(numeric_items) %in% 1:5)
  )

  numeric_items %>% mutate(across(all_of(reversed), ~ 6 - .x))
}

describe_omega <- function(fit) {
  loadings <- fit$schmid$sl
  admissible <-
    all(is.finite(loadings[, c("g", "h2", "u2")])) &&
    all(loadings[, "u2"] >= 0) && all(loadings[, "h2"] <= 1) &&
    is.finite(fit$omega_h) && is.finite(fit$omega.tot) &&
    fit$omega_h >= 0 && fit$omega_h <= fit$omega.tot && fit$omega.tot <= 1

  if (!admissible) {
    return(tibble(
      Omega_hierarchical = NA_real_,
      Omega_total = NA_real_,
      Omega_status = "Inadmissible solution; omega not interpretable"
    ))
  }

  status <- "No inadmissible variances"
  if (any(loadings[, "u2"] < 0.01)) {
    status <- "Near-boundary solution: residual variance < 0.01"
  }

  tibble(
    Omega_hierarchical = fit$omega_h,
    Omega_total = fit$omega.tot,
    Omega_status = status
  )
}

# 4. Excel export --------------------------------------------------------------

# Keep existing sheets unchanged; make newly added output tables readable.
write_result_tables <- function(tables, path, readable_sheets) {
  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center")
  for (sheet in names(tables)) {
    openxlsx::addWorksheet(workbook, sheet)
    openxlsx::writeData(workbook, sheet, tables[[sheet]], headerStyle = header_style)
  }
  for (sheet in readable_sheets) {
    table <- tables[[sheet]]
    numeric_columns <- which(vapply(table, is.numeric, logical(1)))
    integer_columns <- which(names(table) %in% c(
      "N", "Missing", "Items", "Group_factors", "Range_min", "Range_max", "Posts", "Need"
    ))
    column_widths <- vapply(seq_along(table), function(column) {
      column_text <- c(names(table)[column], as.character(table[[column]]))
      min(50, max(nchar(column_text), na.rm = TRUE) + 2)
    }, numeric(1))
    openxlsx::setColWidths(workbook, sheet, cols = seq_along(table), widths = column_widths)
    openxlsx::setColWidths(
      workbook, sheet, cols = numeric_columns,
      widths = pmax(12, nchar(names(table)[numeric_columns]) + 2)
    )
    openxlsx::addStyle(
      workbook, sheet, openxlsx::createStyle(numFmt = "0.000"),
      rows = seq_len(nrow(table)) + 1, cols = setdiff(numeric_columns, integer_columns),
      gridExpand = TRUE, stack = TRUE
    )
    text_columns <- which(names(table) %in% c(
      "Item_variables", "Reversed_items", "Rule", "Higher_values", "Omega_status"
    ))
    if (length(text_columns)) {
      openxlsx::setColWidths(workbook, sheet, cols = text_columns, widths = 50)
      openxlsx::addStyle(
        workbook, sheet, openxlsx::createStyle(wrapText = TRUE, valign = "center"),
        rows = seq_len(nrow(table)) + 1, cols = text_columns, gridExpand = TRUE, stack = TRUE
      )
      openxlsx::setRowHeights(
        workbook, sheet, rows = seq_len(nrow(table)) + 1,
        heights = if ("Item_variables" %in% names(table)) 90 else 32
      )
    }
  }
  # These workbooks contain tables only. Drop openxlsx's unused drawing targets.
  workbook$worksheets_rels <- lapply(workbook$worksheets_rels, function(rels) {
    rels[!grepl("/relationships/(drawing|vmlDrawing)", rels)]
  })
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
}

# 5. Practice coding -----------------------------------------------------------

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
