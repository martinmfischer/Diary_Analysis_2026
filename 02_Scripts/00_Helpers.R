### Helper Functions 

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  psych,
  janitor,
  openxlsx,
  fs,
  scales
)


# Sichere Umwandlung unterschiedlicher TRUE-/FALSE-Darstellungen
as_logical_safe <- function(x) {
  
  x_character <- stringr::str_to_upper(
    stringr::str_trim(as.character(x))
  )
  
  dplyr::case_when(
    x_character %in% c("TRUE", "T", "1", "YES", "Y", "JA") ~ TRUE,
    x_character %in% c("FALSE", "F", "0", "NO", "N", "NEIN") ~ FALSE,
    TRUE ~ NA
  )
}


# Deskriptive Statistiken für metrische oder ordinale Variablen
continuous_summary <- function(data, variable, variable_label) {
  
  x <- data[[variable]]
  x <- suppressWarnings(as.numeric(x))
  
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))
  
  if (n_valid == 0) {
    
    return(
      tibble(
        Variable = variable_label,
        Variable_Name = variable,
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
  
  mean_value <- mean(x, na.rm = TRUE)
  sd_value <- if (n_valid > 1) sd(x, na.rm = TRUE) else NA_real_
  
  if (n_valid > 1 && !is.na(sd_value)) {
    
    standard_error <- sd_value / sqrt(n_valid)
    margin_error <- qt(0.975, df = n_valid - 1) * standard_error
    
    ci_lower <- mean_value - margin_error
    ci_upper <- mean_value + margin_error
    
  } else {
    
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }
  
  tibble(
    Variable = variable_label,
    Variable_Name = variable,
    N_Valid = n_valid,
    N_Missing = n_missing,
    Mean = mean_value,
    SD = sd_value,
    Median = median(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE),
    CI95_Lower = ci_lower,
    CI95_Upper = ci_upper
  )
}


# Häufigkeitstabelle einschließlich fehlender Werte
frequency_summary <- function(data, variable, variable_label) {
  
  values <- as.character(data[[variable]])
  
  values <- tidyr::replace_na(
    values,
    "Missing"
  )
  
  tibble(
    Level = values
  ) %>%
    count(
      Level,
      name = "N"
    ) %>%
    mutate(
      Percent = 100 * N / sum(N),
      Variable = variable_label,
      Variable_Name = variable
    ) %>%
    select(
      Variable,
      Variable_Name,
      Level,
      N,
      Percent
    )
}


# Antwortverteilungen numerischer Items
item_distribution <- function(data, variable, variable_label) {
  
  values <- suppressWarnings(
    as.numeric(data[[variable]])
  )
  
  tibble(
    Response = values
  ) %>%
    mutate(
      Response_Label = if_else(
        is.na(Response),
        "Missing",
        as.character(Response)
      )
    ) %>%
    count(
      Response,
      Response_Label,
      name = "N"
    ) %>%
    mutate(
      Percent = 100 * N / sum(N),
      Variable = variable_label,
      Variable_Name = variable
    ) %>%
    arrange(
      is.na(Response),
      Response
    ) %>%
    select(
      Variable,
      Variable_Name,
      Response,
      Response_Label,
      N,
      Percent
    )
}


# Tabellenblatt formatiert zu einer Excel-Arbeitsmappe hinzufügen
add_excel_sheet <- function(
    workbook,
    sheet_name,
    data,
    header_style
) {
  
  # Excel erlaubt höchstens 31 Zeichen für Blattnamen
  sheet_name <- stringr::str_sub(
    sheet_name,
    1,
    31
  )
  
  openxlsx::addWorksheet(
    workbook,
    sheetName = sheet_name
  )
  
  openxlsx::writeData(
    workbook,
    sheet = sheet_name,
    x = data,
    withFilter = TRUE
  )
  
  if (ncol(data) > 0 && nrow(data) >= 0) {
    
    openxlsx::addStyle(
      workbook,
      sheet = sheet_name,
      style = header_style,
      rows = 1,
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
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
  }
}


# Prozentangaben für Balkendiagramme vorbereiten
plot_frequency_data <- function(data, variable) {
  
  variable_quo <- rlang::enquo(variable)
  
  data %>%
    filter(
      !is.na(!!variable_quo)
    ) %>%
    count(
      !!variable_quo,
      name = "N"
    ) %>%
    mutate(
      Percent = 100 * N / sum(N)
    )
}