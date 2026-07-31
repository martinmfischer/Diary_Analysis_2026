################################################################################
# Project: Tagebuchstudie
# File:    00_Helpers.R
#
# Purpose:
#   Gemeinsame Hilfsfunktionen für Screening-, Daily-, Outro- und integrierte
#   Analyseskripte. Die Datei enthält ausschließlich wiederverwendbare
#   Funktionen und soll nach dem jeweiligen Package-Block via source() geladen
#   werden.
#
# Usage:
#   helper_script <- file.path("02_Scripts", "00_Helpers.R")
#   source(helper_script)
#
# Required packages:
#   tidyverse, psych, openxlsx, stringr
#
# Design principles:
#   - Funktionsnamen bleiben stabil.
#   - Fehlende Werte werden explizit und defensiv behandelt.
#   - Divisionen durch 0 liefern NA statt Inf/NaN.
#   - Excel-Blattnamen > 31 Zeichen und doppelte Namen führen zum Abbruch.
#   - clean_numeric() konvertiert nur den Datentyp; sentinel values wie -1
#     werden nicht pauschal entfernt. Dies geschieht in den Analyseskripten
#     beziehungsweise in spezifischen Cleaning-Funktionen.
#   - clean_binary() liefert Integerwerte 0/1/NA.
################################################################################


#===============================================================================
# 01 Basic operators and cleaning
#===============================================================================

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\u00a0", " ")
  x <- stringr::str_squish(x)
  
  missing_tokens <- c(
    "",
    "-1",
    "NA",
    "N/A",
    "NULL"
  )
  
  x[
    is.na(x) |
      stringr::str_to_upper(x) %in% missing_tokens
  ] <- NA_character_
  
  x
}


clean_numeric <- function(x) {
  suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
}


as_logical_safe <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  
  x_character <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(x)
    )
  )
  
  dplyr::case_when(
    x_character %in% c(
      "true",
      "t",
      "1",
      "yes",
      "y",
      "ja",
      "j"
    ) ~ TRUE,
    
    x_character %in% c(
      "false",
      "f",
      "0",
      "no",
      "n",
      "nein"
    ) ~ FALSE,
    
    TRUE ~ NA
  )
}


clean_binary <- function(x) {
  raw <- stringr::str_to_lower(
    clean_text(x)
  )
  
  numeric_value <- clean_numeric(raw)
  
  dplyr::case_when(
    raw %in% c(
      "true",
      "t",
      "yes",
      "y",
      "ja",
      "j",
      "completed",
      "complete"
    ) ~ 1L,
    
    raw %in% c(
      "false",
      "f",
      "no",
      "n",
      "nein",
      "incomplete"
    ) ~ 0L,
    
    numeric_value == 1 ~ 1L,
    numeric_value == 0 ~ 0L,
    numeric_value == -1 ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}


recode_gender_numeric <- function(x) {
  x_character <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(x)
    )
  )
  
  numeric_candidate <- clean_numeric(x_character)
  
  dplyr::case_when(
    numeric_candidate %in% 1:3 ~ numeric_candidate,
    x_character %in% c("w", "weiblich", "female", "frau") ~ 1,
    x_character %in% c("m", "männlich", "maennlich", "male", "mann") ~ 2,
    x_character %in% c("d", "divers", "diverse", "other") ~ 3,
    TRUE ~ NA_real_
  )
}


#===============================================================================
# 02 Safe numeric operations
#===============================================================================

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}


safe_sd <- function(x) {
  if (sum(!is.na(x)) < 2) {
    return(NA_real_)
  }
  
  stats::sd(x, na.rm = TRUE)
}


safe_median <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  stats::median(x, na.rm = TRUE)
}


safe_min <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  min(x, na.rm = TRUE)
}


safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  max(x, na.rm = TRUE)
}


safe_divide <- function(numerator, denominator) {
  result <- numerator / denominator
  
  result[
    is.na(denominator) |
      denominator == 0
  ] <- NA_real_
  
  result
}


safe_percent <- function(numerator, denominator) {
  100 * safe_divide(
    numerator,
    denominator
  )
}


safe_z <- function(x) {
  x <- clean_numeric(x)
  
  if (all(is.na(x))) {
    return(
      rep(
        NA_real_,
        length(x)
      )
    )
  }
  
  if (
    sum(!is.na(x)) < 2 ||
    isTRUE(
      all.equal(
        stats::sd(x, na.rm = TRUE),
        0
      )
    )
  ) {
    result <- rep(0, length(x))
    result[is.na(x)] <- NA_real_
    return(result)
  }
  
  as.numeric(
    scale(x)
  )
}


safe_date_min <- function(x) {
  x <- suppressWarnings(
    as.Date(x)
  )
  
  if (all(is.na(x))) {
    return(
      as.Date(NA)
    )
  }
  
  min(x, na.rm = TRUE)
}


#===============================================================================
# 03 General descriptive summaries
#===============================================================================

continuous_summary <- function(
    data,
    variable,
    label
) {
  x <- clean_numeric(
    data[[variable]]
  )
  
  n_valid <- sum(!is.na(x))
  sd_value <- safe_sd(x)
  mean_value <- safe_mean(x)
  
  if (
    n_valid > 1 &&
    !is.na(sd_value)
  ) {
    margin_error <- stats::qt(
      0.975,
      df = n_valid - 1
    ) * sd_value / sqrt(n_valid)
    
    ci_lower <- mean_value - margin_error
    ci_upper <- mean_value + margin_error
  } else {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }
  
  tibble::tibble(
    Variable = label,
    Variable_Name = variable,
    N_Total = length(x),
    N_Valid = n_valid,
    N_Missing = sum(is.na(x)),
    Mean = mean_value,
    SD = sd_value,
    Median = safe_median(x),
    Minimum = safe_min(x),
    Maximum = safe_max(x),
    CI95_Lower = ci_lower,
    CI95_Upper = ci_upper
  )
}


frequency_summary <- function(
    data,
    variable,
    label
) {
  x <- data[[variable]]
  n_total <- length(x)
  n_valid <- sum(!is.na(x))
  
  tibble::tibble(
    Level = as.character(x)
  ) %>%
    dplyr::mutate(
      Level = tidyr::replace_na(
        Level,
        "Missing"
      )
    ) %>%
    dplyr::count(
      Level,
      name = "N"
    ) %>%
    dplyr::mutate(
      Variable = label,
      Variable_Name = variable,
      N_Total = n_total,
      N_Valid = n_valid,
      Percent_Total = safe_percent(N, n_total),
      Percent_Valid = dplyr::if_else(
        Level == "Missing",
        NA_real_,
        safe_percent(N, n_valid)
      ),
      Percent = dplyr::if_else(
        Level == "Missing",
        safe_percent(N, n_total),
        safe_percent(N, n_valid)
      ),
      .before = 1
    )
}


item_distribution <- function(
    data,
    variable,
    label,
    item_labels = NULL
) {
  # Single-variable version used in the screening script.
  if (is.null(item_labels)) {
    x <- clean_numeric(
      data[[variable]]
    )
    
    n_valid <- sum(!is.na(x))
    
    return(
      tibble::tibble(
        Response = x
      ) %>%
        dplyr::filter(
          !is.na(Response)
        ) %>%
        dplyr::count(
          Response,
          name = "N"
        ) %>%
        dplyr::mutate(
          Variable = label,
          Variable_Name = variable,
          N_Valid = n_valid,
          Percent_Valid = safe_percent(N, n_valid),
          .before = 1
        )
    )
  }
  
  # Multi-item version used in the Outro script.
  variables <- variable
  scale_name <- label
  
  data %>%
    dplyr::select(
      dplyr::all_of(variables)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "Item_Variable",
      values_to = "Response"
    ) %>%
    dplyr::mutate(
      Response = clean_numeric(Response),
      Response = dplyr::if_else(
        Response %in% 1:5,
        Response,
        NA_real_
      ),
      Item = unname(
        item_labels[Item_Variable]
      ),
      Item = factor(
        Item,
        levels = unname(item_labels)
      ),
      Response_Factor = factor(
        Response,
        levels = 1:5,
        ordered = TRUE
      )
    ) %>%
    dplyr::filter(
      !is.na(Response_Factor)
    ) %>%
    dplyr::count(
      Item,
      Response_Factor,
      .drop = FALSE,
      name = "N"
    ) %>%
    dplyr::group_by(Item) %>%
    dplyr::mutate(
      N_Valid = sum(N),
      Percent_Valid = safe_percent(N, N_Valid),
      Scale = scale_name,
      .before = 1
    ) %>%
    dplyr::ungroup()
}


plot_frequency_data <- function(data, variable) {
  variable_quo <- rlang::enquo(variable)
  
  data %>%
    dplyr::filter(
      !is.na(!!variable_quo)
    ) %>%
    dplyr::count(
      !!variable_quo,
      name = "N"
    ) %>%
    dplyr::mutate(
      Percent = safe_percent(
        N,
        sum(N)
      )
    )
}


descriptive_summary <- function(x, label) {
  x <- clean_numeric(x)
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))
  
  if (n_valid == 0) {
    return(
      tibble::tibble(
        Variable = label,
        N_Valid = 0L,
        N_Missing = n_missing,
        Mean = NA_real_,
        SD = NA_real_,
        Median = NA_real_,
        Minimum = NA_real_,
        Maximum = NA_real_,
        CI95_Lower = NA_real_,
        CI95_Upper = NA_real_
      )
    )
  }
  
  mean_value <- safe_mean(x)
  sd_value <- safe_sd(x)
  
  if (
    n_valid > 1 &&
    !is.na(sd_value)
  ) {
    margin_error <- stats::qt(
      0.975,
      df = n_valid - 1
    ) * sd_value / sqrt(n_valid)
    
    ci_lower <- mean_value - margin_error
    ci_upper <- mean_value + margin_error
  } else {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }
  
  tibble::tibble(
    Variable = label,
    N_Valid = n_valid,
    N_Missing = n_missing,
    Mean = mean_value,
    SD = sd_value,
    Median = safe_median(x),
    Minimum = safe_min(x),
    Maximum = safe_max(x),
    CI95_Lower = ci_lower,
    CI95_Upper = ci_upper
  )
}


item_descriptives <- function(
    data,
    items,
    labels
) {
  purrr::map_dfr(
    items,
    function(item) {
      descriptive_summary(
        data[[item]],
        labels[[item]]
      ) %>%
        dplyr::mutate(
          Item = item,
          .before = 1
        )
    }
  )
}


item_distributions <- function(
    data,
    items,
    labels
) {
  purrr::map_dfr(
    items,
    function(item) {
      tibble::tibble(
        Response = clean_numeric(
          data[[item]]
        )
      ) %>%
        dplyr::mutate(
          Response_Label = dplyr::case_when(
            Response == 1 ~ "1 – Stimme überhaupt nicht zu",
            Response == 2 ~ "2",
            Response == 3 ~ "3",
            Response == 4 ~ "4",
            Response == 5 ~ "5 – Stimme voll und ganz zu",
            is.na(Response) ~ "Missing",
            TRUE ~ "Ungültiger Wert"
          )
        ) %>%
        dplyr::count(
          Response,
          Response_Label,
          name = "N"
        ) %>%
        dplyr::mutate(
          Percent = safe_percent(
            N,
            sum(N)
          ),
          Item = item,
          Item_Label = labels[[item]],
          .before = 1
        ) %>%
        dplyr::arrange(
          is.na(Response),
          Response
        )
    }
  )
}


#===============================================================================
# 04 Scale construction and reliability
#===============================================================================

complete_mean <- function(data, items) {
  item_data <- data %>%
    dplyr::select(
      dplyr::all_of(items)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        clean_numeric
      )
    )
  
  item_matrix <- as.matrix(item_data)
  
  n_answered <- rowSums(
    !is.na(item_matrix)
  )
  
  index <- rowMeans(
    item_matrix,
    na.rm = FALSE
  )
  
  index[
    n_answered < length(items)
  ] <- NA_real_
  
  index
}


calculate_scale_reliability <- function(
    data,
    items,
    scale_name
) {
  item_data <- data %>%
    dplyr::select(
      dplyr::all_of(items)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        clean_numeric
      )
    )
  
  complete_item_data <- item_data %>%
    tidyr::drop_na()
  
  n_complete <- nrow(complete_item_data)
  
  zero_variance_items <- names(complete_item_data)[
    purrr::map_lgl(
      complete_item_data,
      ~ dplyr::n_distinct(.x) <= 1
    )
  ]
  
  if (
    n_complete < 3 ||
    length(zero_variance_items) > 0
  ) {
    warning(
      scale_name,
      ": Reliabilität kann möglicherweise nicht stabil berechnet werden. ",
      "Vollständige Fälle: ",
      n_complete,
      "; Items ohne Varianz: ",
      paste(
        zero_variance_items,
        collapse = ", "
      )
    )
  }
  
  alpha_object <- tryCatch(
    psych::alpha(
      complete_item_data,
      check.keys = FALSE,
      warnings = FALSE
    ),
    error = function(e) {
      warning(
        scale_name,
        ": Alpha konnte nicht berechnet werden: ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  omega_object <- tryCatch(
    {
      result <- NULL
      
      invisible(
        capture.output(
          result <- suppressWarnings(
            suppressMessages(
              psych::omega(
                complete_item_data,
                nfactors = 1,
                plot = FALSE
              )
            )
          ),
          type = "output"
        )
      )
      
      result
    },
    error = function(e) {
      warning(
        scale_name,
        ": Omega konnte nicht berechnet werden: ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  summary <- tibble::tibble(
    Scale = scale_name,
    Number_of_Items = length(items),
    N_Complete = n_complete,
    
    Cronbach_Alpha = if (
      is.null(alpha_object)
    ) {
      NA_real_
    } else {
      unname(
        alpha_object$total$raw_alpha
      )
    },
    
    Standardized_Alpha = if (
      is.null(alpha_object)
    ) {
      NA_real_
    } else {
      unname(
        alpha_object$total$std.alpha
      )
    },
    
    Omega_Hierarchical = if (
      is.null(omega_object)
    ) {
      NA_real_
    } else {
      unname(
        omega_object$omega_h
      )
    },
    
    Omega_Total = if (
      is.null(omega_object)
    ) {
      NA_real_
    } else {
      unname(
        omega_object$omega.tot
      )
    }
  )
  
  leave_one_out <- purrr::map_dfr(
    items,
    function(removed_item) {
      reduced_items <- setdiff(
        items,
        removed_item
      )
      
      reduced_data <- data %>%
        dplyr::select(
          dplyr::all_of(reduced_items)
        ) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::everything(),
            clean_numeric
          )
        ) %>%
        tidyr::drop_na()
      
      reduced_alpha <- tryCatch(
        psych::alpha(
          reduced_data,
          check.keys = FALSE,
          warnings = FALSE
        ),
        error = function(e) NULL
      )
      
      reduced_omega <- tryCatch(
        {
          result <- NULL
          
          invisible(
            capture.output(
              result <- suppressWarnings(
                suppressMessages(
                  psych::omega(
                    reduced_data,
                    nfactors = 1,
                    plot = FALSE
                  )
                )
              ),
              type = "output"
            )
          )
          
          result
        },
        error = function(e) NULL
      )
      
      tibble::tibble(
        Scale = scale_name,
        Item_Removed = removed_item,
        Number_of_Items = length(reduced_items),
        N_Complete = nrow(reduced_data),
        
        Cronbach_Alpha = if (
          is.null(reduced_alpha)
        ) {
          NA_real_
        } else {
          unname(
            reduced_alpha$total$raw_alpha
          )
        },
        
        Omega_Hierarchical = if (
          is.null(reduced_omega)
        ) {
          NA_real_
        } else {
          unname(
            reduced_omega$omega_h
          )
        },
        
        Omega_Total = if (
          is.null(reduced_omega)
        ) {
          NA_real_
        } else {
          unname(
            reduced_omega$omega.tot
          )
        }
      )
    }
  )
  
  alpha_item_statistics <- if (
    is.null(alpha_object)
  ) {
    tibble::tibble(
      Scale = scale_name,
      Note = "Alpha konnte nicht berechnet werden."
    )
  } else {
    alpha_object$item.stats %>%
      as.data.frame() %>%
      tibble::rownames_to_column(
        "Item"
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        Scale = scale_name,
        .before = 1
      )
  }
  
  alpha_if_deleted <- if (
    is.null(alpha_object)
  ) {
    tibble::tibble(
      Scale = scale_name,
      Note = "Alpha konnte nicht berechnet werden."
    )
  } else {
    alpha_object$alpha.drop %>%
      as.data.frame() %>%
      tibble::rownames_to_column(
        "Item_Removed"
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        Scale = scale_name,
        .before = 1
      )
  }
  
  list(
    summary = summary,
    leave_one_out = leave_one_out,
    alpha_object = alpha_object,
    omega_object = omega_object,
    alpha_item_statistics = alpha_item_statistics,
    alpha_if_deleted = alpha_if_deleted
  )
}


select_scale_items <- function(
    reliability_result,
    original_items,
    cutoff = 0.70,
    allow_exclusion = TRUE
) {
  full_omega <- reliability_result$summary$
    Omega_Hierarchical[[1]]
  
  selected_items <- original_items
  excluded_item <- NA_character_
  decision <- "Alle Items beibehalten"
  
  if (
    allow_exclusion &&
    !is.na(full_omega) &&
    full_omega < cutoff
  ) {
    candidates <- reliability_result$leave_one_out %>%
      dplyr::filter(
        !is.na(Omega_Hierarchical),
        Omega_Hierarchical >= cutoff
      ) %>%
      dplyr::arrange(
        dplyr::desc(Omega_Hierarchical),
        dplyr::desc(Omega_Total)
      )
    
    if (nrow(candidates) > 0) {
      excluded_item <- candidates$
        Item_Removed[[1]]
      
      selected_items <- setdiff(
        original_items,
        excluded_item
      )
      
      decision <- paste0(
        "Item ausgeschlossen: ",
        excluded_item
      )
    }
  }
  
  decision_table <- tibble::tibble(
    Full_Omega_Hierarchical = full_omega,
    Cutoff = cutoff,
    Item_Excluded = excluded_item,
    Number_of_Final_Items = length(selected_items),
    Decision = decision,
    Selected_Items = paste(
      selected_items,
      collapse = "; "
    )
  )
  
  list(
    decision = decision_table,
    selected_items = selected_items,
    excluded_item = excluded_item
  )
}


#===============================================================================
# 05 Distribution, repertoire and profile helpers
#===============================================================================

share_value <- function(x, value) {
  valid <- !is.na(x)
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  mean(
    x[valid] == value
  )
}


n_distinct_valid <- function(x) {
  dplyr::n_distinct(
    x[!is.na(x)]
  )
}


shannon_entropy <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  counts <- table(
    x,
    useNA = "no"
  )
  
  # Unbeobachtete Faktorstufen entfernen, damit 0 * log(0) nicht NaN ergibt.
  counts <- counts[counts > 0]
  
  if (
    length(counts) == 0 ||
    sum(counts) == 0
  ) {
    return(NA_real_)
  }
  
  probabilities <- counts / sum(counts)
  
  -sum(
    probabilities * log(probabilities)
  )
}


shannon_evenness <- function(x) {
  richness <- n_distinct_valid(x)
  
  if (richness == 0) {
    return(NA_real_)
  }
  
  if (richness == 1) {
    return(0)
  }
  
  shannon_entropy(x) / log(richness)
}


dominant_share <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  max(
    prop.table(
      table(x)
    )
  )
}


profile_alignment <- function(
    screening_values,
    observed_values
) {
  complete <- stats::complete.cases(
    screening_values,
    observed_values
  )
  
  if (
    sum(complete) < 3 ||
    dplyr::n_distinct(
      screening_values[complete]
    ) < 2 ||
    dplyr::n_distinct(
      observed_values[complete]
    ) < 2
  ) {
    return(NA_real_)
  }
  
  suppressWarnings(
    stats::cor(
      screening_values[complete],
      observed_values[complete],
      method = "spearman"
    )
  )
}


jensen_shannon_divergence <- function(p, q) {
  p <- clean_numeric(p)
  q <- clean_numeric(q)
  
  if (
    length(p) != length(q) ||
    all(is.na(p)) ||
    all(is.na(q))
  ) {
    return(NA_real_)
  }
  
  p[is.na(p)] <- 0
  q[is.na(q)] <- 0
  
  if (
    sum(p) <= 0 ||
    sum(q) <= 0
  ) {
    return(NA_real_)
  }
  
  p <- p / sum(p)
  q <- q / sum(q)
  m <- (p + q) / 2
  
  kl_divergence <- function(a, b) {
    positive <- a > 0 & b > 0
    
    if (!any(positive)) {
      return(0)
    }
    
    sum(
      a[positive] * log2(
        a[positive] / b[positive]
      )
    )
  }
  
  0.5 * kl_divergence(p, m) +
    0.5 * kl_divergence(q, m)
}


frequency_distribution <- function(
    data,
    variable,
    variable_label,
    categories = NULL
) {
  raw_values <- clean_text(
    data[[variable]]
  )
  
  if (is.null(categories)) {
    categories <- sort(
      unique(
        raw_values[!is.na(raw_values)]
      )
    )
  }
  
  n_total <- length(raw_values)
  n_valid <- sum(!is.na(raw_values))
  
  valid_counts <- tibble::tibble(
    Category = raw_values
  ) %>%
    dplyr::filter(
      !is.na(Category)
    ) %>%
    dplyr::count(
      Category,
      name = "N"
    ) %>%
    tidyr::complete(
      Category = categories,
      fill = list(
        N = 0
      )
    )
  
  missing_count <- tibble::tibble(
    Category = "Missing",
    N = sum(is.na(raw_values))
  )
  
  dplyr::bind_rows(
    valid_counts,
    missing_count
  ) %>%
    dplyr::mutate(
      Variable = variable_label,
      N_Total = n_total,
      N_Valid = n_valid,
      Percent_Total = safe_percent(N, n_total),
      Percent_Valid = dplyr::if_else(
        Category == "Missing",
        NA_real_,
        safe_percent(N, n_valid)
      ),
      .before = 1
    )
}


make_participant_shares <- function(
    data,
    variable,
    categories,
    variable_label
) {
  participant_ids <- sort(
    unique(
      data$participant[!is.na(data$participant)]
    )
  )
  
  counts <- data %>%
    dplyr::transmute(
      participant,
      Category = clean_text(
        .data[[variable]]
      )
    ) %>%
    dplyr::filter(
      !is.na(Category)
    ) %>%
    dplyr::count(
      participant,
      Category,
      name = "N"
    )
  
  denominators <- data %>%
    dplyr::transmute(
      participant,
      Value = clean_text(
        .data[[variable]]
      )
    ) %>%
    dplyr::group_by(participant) %>%
    dplyr::summarise(
      Denominator = sum(!is.na(Value)),
      .groups = "drop"
    )
  
  tidyr::expand_grid(
    participant = participant_ids,
    Category = categories
  ) %>%
    dplyr::left_join(
      counts,
      by = c(
        "participant",
        "Category"
      )
    ) %>%
    dplyr::left_join(
      denominators,
      by = "participant"
    ) %>%
    dplyr::mutate(
      N = tidyr::replace_na(N, 0L),
      Share = safe_divide(N, Denominator),
      Variable = variable_label,
      .before = 1
    )
}


summarise_participant_shares <- function(participant_shares) {
  participant_shares %>%
    dplyr::group_by(
      Variable,
      Category
    ) %>%
    dplyr::summarise(
      N_Participants = sum(!is.na(Share)),
      Mean_Share = safe_mean(Share),
      SD_Share = safe_sd(Share),
      Median_Share = safe_median(Share),
      Minimum_Share = safe_min(Share),
      Maximum_Share = safe_max(Share),
      Mean_Percent = 100 * Mean_Share,
      .groups = "drop"
    )
}


cross_tabulation <- function(
    data,
    row_variable,
    column_variable,
    row_label,
    column_label
) {
  data %>%
    dplyr::transmute(
      Row = clean_text(
        .data[[row_variable]]
      ),
      Column = clean_text(
        .data[[column_variable]]
      )
    ) %>%
    dplyr::filter(
      !is.na(Row),
      !is.na(Column)
    ) %>%
    dplyr::count(
      Row,
      Column,
      name = "N"
    ) %>%
    dplyr::group_by(Row) %>%
    dplyr::mutate(
      Row_Percent = safe_percent(
        N,
        sum(N)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Column) %>%
    dplyr::mutate(
      Column_Percent = safe_percent(
        N,
        sum(N)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Total_Percent = safe_percent(
        N,
        sum(N)
      ),
      Row_Variable = row_label,
      Column_Variable = column_label,
      .before = 1
    )
}


binary_group_summary <- function(
    data,
    group_variable,
    outcome_variable,
    group_label,
    outcome_label
) {
  data %>%
    dplyr::transmute(
      Group = clean_text(
        .data[[group_variable]]
      ),
      Outcome = clean_binary(
        .data[[outcome_variable]]
      )
    ) %>%
    dplyr::filter(
      !is.na(Group)
    ) %>%
    dplyr::group_by(Group) %>%
    dplyr::summarise(
      N_Valid = sum(!is.na(Outcome)),
      N_Yes = sum(Outcome == 1, na.rm = TRUE),
      Percent_Yes = safe_percent(N_Yes, N_Valid),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Grouping_Variable = group_label,
      Outcome = outcome_label,
      .before = 1
    )
}


#===============================================================================
# 06 Variable, date and file helpers
#===============================================================================

parse_study_day <- function(x) {
  numeric_direct <- clean_numeric(x)
  
  extracted <- suppressWarnings(
    as.numeric(
      stringr::str_extract(
        as.character(x),
        "[0-9]+"
      )
    )
  )
  
  dplyr::coalesce(
    numeric_direct,
    extracted
  )
}


rename_first_available <- function(
    data,
    target,
    candidates,
    required = FALSE
) {
  if (target %in% names(data)) {
    return(data)
  }
  
  available <- intersect(
    candidates,
    names(data)
  )
  
  if (length(available) > 0) {
    names(data)[
      names(data) == available[[1]]
    ] <- target
  } else if (required) {
    stop(
      "Benötigte Variable '",
      target,
      "' wurde nicht gefunden. Geprüfte Alternativen: ",
      paste(
        candidates,
        collapse = ", "
      )
    )
  } else {
    data[[target]] <- NA
  }
  
  data
}


first_existing <- function(
    data,
    candidates,
    required = TRUE,
    description = "Variable"
) {
  found <- candidates[
    candidates %in% names(data)
  ]
  
  if (length(found) == 0) {
    if (required) {
      stop(
        description,
        " wurde nicht gefunden. Erwartet wurde eine der Variablen: ",
        paste(
          candidates,
          collapse = ", "
        )
      )
    }
    
    return(NA_character_)
  }
  
  found[[1]]
}


#===============================================================================
# 07 Correlation helpers
#===============================================================================

spearman_test <- function(
    data,
    x_variable,
    y_variable,
    x_label = x_variable,
    y_label = y_variable
) {
  test_data <- data %>%
    dplyr::transmute(
      x = clean_numeric(
        .data[[x_variable]]
      ),
      y = clean_numeric(
        .data[[y_variable]]
      )
    ) %>%
    tidyr::drop_na()
  
  if (
    nrow(test_data) < 3 ||
    dplyr::n_distinct(test_data$x) < 2 ||
    dplyr::n_distinct(test_data$y) < 2
  ) {
    return(
      tibble::tibble(
        Variable_1 = x_label,
        Variable_2 = y_label,
        N = nrow(test_data),
        Spearman_Rho = NA_real_,
        P_Value = NA_real_
      )
    )
  }
  
  result <- suppressWarnings(
    stats::cor.test(
      test_data$x,
      test_data$y,
      method = "spearman",
      exact = FALSE
    )
  )
  
  tibble::tibble(
    Variable_1 = x_label,
    Variable_2 = y_label,
    N = nrow(test_data),
    Spearman_Rho = unname(
      result$estimate
    ),
    P_Value = result$p.value
  )
}


#===============================================================================
# 08 Excel output
#===============================================================================

add_excel_sheet <- function(
    workbook,
    sheet_name,
    data,
    header_style
) {
  if (nchar(sheet_name) > 31) {
    stop(
      "Excel-Blattname ist länger als 31 Zeichen: ",
      sheet_name
    )
  }
  
  if (sheet_name %in% names(workbook)) {
    stop(
      "Doppelter Excel-Blattname: ",
      sheet_name
    )
  }
  
  if (is.null(data)) {
    data <- tibble::tibble(
      Note = "Object is NULL"
    )
  }
  
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  if (ncol(data) == 0) {
    data <- tibble::tibble(
      Note = "No columns available"
    )
  }
  
  openxlsx::addWorksheet(
    workbook,
    sheetName = sheet_name
  )
  
  openxlsx::writeData(
    workbook,
    sheet = sheet_name,
    x = data,
    headerStyle = header_style,
    withFilter = nrow(data) > 0
  )
  
  openxlsx::freezePane(
    workbook,
    sheet = sheet_name,
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    workbook,
    sheet = sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
  
  invisible(workbook)
}
