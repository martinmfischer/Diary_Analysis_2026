# ============================================================
# Reli-Pretest: Intercoder-Reliabilitaet
# 3 Codierende | nominale Kategorien | Krippendorffs Alpha via icr
# ============================================================


if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  "readxl", "dplyr", "tidyr", "icr", "writexl"
)


# ------------------------------------------------------------
# 1. Einstellungen
# ------------------------------------------------------------

input_dir <- "07_Pretest"

input_files <- c(
  LA = file.path(input_dir, "coding_sheet_LA.xlsx"),
  MF = file.path(input_dir, "coding_sheet_MF.xlsx"),
  LG = file.path(input_dir, "coding_sheet_LG.xlsx")
)

output_file <- file.path(input_dir, "Reli_Pretest_Ergebnisse_icr.xlsx")

vars <- c(
  "public_rel_coded",
  "advertisement_coded",
  "topic_coded",
  "source_coded",
  "platform_coded",
  "media_format"
)

# Nachgelagerte Variablen: nur analysieren, wenn alle drei public_rel == 1.
conditional_vars <- c(
  "advertisement_coded",
  "topic_coded",
  "source_coded",
  "media_format"
)

coders <- names(input_files)

# Krippendorff-Bootstrap. Ein Kern ist fuer dieses Sample ausreichend
# und maximiert die Reproduzierbarkeit ueber verschiedene Rechner hinweg.
nboot <- 20000L
cores <- 1L
seed <- rep(12345, 6)
conf_level <- 0.95


# ------------------------------------------------------------
# 2. Daten einlesen und pruefen
# ------------------------------------------------------------

if (!all(file.exists(input_files))) {
  stop(
    "Nicht alle Eingabedateien gefunden:\n",
    paste(input_files[!file.exists(input_files)], collapse = "\n")
  )
}

read_coder <- function(path, coder_name) {
  
  x <- readxl::read_excel(path, sheet = "Coding")
  needed <- c("screenshot_id", vars)
  
  if (!all(needed %in% names(x))) {
    stop(
      basename(path), ": fehlende Spalten: ",
      paste(setdiff(needed, names(x)), collapse = ", ")
    )
  }
  
  x %>%
    select(all_of(needed)) %>%
    mutate(
      across(everything(), ~ na_if(trimws(as.character(.x)), "")),
      coder = .env$coder_name
    )
}

coder_data <- Map(
  read_coder,
  unname(input_files),
  names(input_files)
)
names(coder_data) <- coders

for (i in seq_along(coder_data)) {
  
  x <- coder_data[[i]]
  
  if (anyNA(x$screenshot_id)) {
    stop("Fehlende screenshot_id bei ", coders[i])
  }
  
  if (anyDuplicated(x$screenshot_id)) {
    stop("Doppelte screenshot_id bei ", coders[i])
  }
}

id_sets <- lapply(coder_data, function(x) sort(x$screenshot_id))

if (!all(vapply(id_sets[-1], identical, logical(1), id_sets[[1]]))) {
  stop("Die drei Dateien enthalten nicht dieselben screenshot_id-Werte.")
}

# Datenlogik pruefen: Wenn ein Coder public_rel != 1 setzt, sollten seine
# nachgelagerten Codes leer sein.
hierarchy_violations <- bind_rows(coder_data) %>%
  filter(is.na(.data$public_rel_coded) | .data$public_rel_coded != "1") %>%
  filter(if_any(all_of(conditional_vars), ~ !is.na(.x)))

if (nrow(hierarchy_violations) > 0) {
  warning(
    nrow(hierarchy_violations),
    " Hierarchie-Verletzung(en): nachgelagerter Code trotz public_rel != 1."
  )
}

# Eine Zeile pro Screenshot x Variable, eine Spalte pro Codierendem.
ratings <- bind_rows(coder_data) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = c(screenshot_id, variable),
    names_from = coder,
    values_from = value
  )

public_ok <- ratings %>%
  filter(.data$variable == "public_rel_coded") %>%
  transmute(
    screenshot_id,
    all_public =
      .data$LA == "1" &
      .data$MF == "1" &
      .data$LG == "1"
  )

ratings <- ratings %>%
  left_join(public_ok, by = "screenshot_id")


# ------------------------------------------------------------
# 3. Hilfsfunktionen
# ------------------------------------------------------------

get_matrix <- function(var_name) {
  
  # .env$var_name ist wichtig: verhindert dplyr-Data-Mask-Namenskonflikte.
  x <- ratings %>%
    filter(.data$variable == .env$var_name)
  
  if (var_name %in% conditional_vars) {
    x <- x %>%
      filter(.data$all_public %in% TRUE)
  }
  
  m <- x %>%
    select(all_of(coders)) %>%
    as.matrix()
  
  rownames(m) <- x$screenshot_id
  m
}

pair_agreement <- function(m, coder_a, coder_b) {
  
  ok <- !is.na(m[, coder_a]) & !is.na(m[, coder_b])
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  mean(m[ok, coder_a] == m[ok, coder_b])
}

all_three_agreement <- function(m) {
  
  ok <- stats::complete.cases(m)
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  mean(
    apply(
      m[ok, , drop = FALSE],
      1,
      function(x) length(unique(x)) == 1
    )
  )
}

dominant_rating_share <- function(m) {
  
  x <- as.vector(m)
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  as.numeric(max(prop.table(table(x))))
}

fit_alpha <- function(m, var_name) {
  
  # Einheiten mit < 2 Ratings tragen nicht zu Krippendorffs Alpha bei.
  m_alpha <- m[
    rowSums(!is.na(m)) >= 2,
    ,
    drop = FALSE
  ]
  
  if (nrow(m_alpha) == 0) {
    return(c(
      alpha = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      observed_agreement = NA_real_
    ))
  }
  
  observed_values <- as.vector(m_alpha)
  observed_values <- observed_values[!is.na(observed_values)]
  
  # Bei nur einer Kategorie ist De = 0:
  # rohe Uebereinstimmung = 1, chance-korrigiertes Alpha nicht informativ.
  if (length(unique(observed_values)) < 2) {
    return(c(
      alpha = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      observed_agreement = 1
    ))
  }
  
  # icr: Codierende = Zeilen, Codiereinheiten = Spalten.
  fit <- icr::krippalpha(
    t(m_alpha),
    metric = "nominal",
    bootstrap = TRUE,
    bootnp = FALSE,
    nboot = nboot,
    cores = cores,
    seed = seed
  )
  
  boot <- fit$bootstraps[is.finite(fit$bootstraps)]
  
  if (length(boot) < nboot * 0.95) {
    warning(
      var_name, ": nur ", length(boot), " von ", nboot,
      " Bootstrap-Replikationen gueltig."
    )
  }
  
  if (length(boot) >= 2) {
    alpha_tail <- (1 - conf_level) / 2
    ci <- stats::quantile(
      boot,
      probs = c(alpha_tail, 1 - alpha_tail),
      names = FALSE
    )
  } else {
    ci <- c(NA_real_, NA_real_)
  }
  
  c(
    alpha = fit$alpha,
    ci_low = ci[1],
    ci_high = ci[2],
    
    # Bei nominaler Metrik: 1 - beobachtete Disagreement-Rate.
    observed_agreement = 1 - fit$D_o
  )
}

issue_type <- function(x) {
  
  observed <- x[!is.na(x)]
  disagreement <- length(observed) >= 2 && length(unique(observed)) > 1
  has_missing <- anyNA(x)
  
  if (disagreement && has_missing) return("Dissens + Missing")
  if (disagreement) return("Dissens")
  if (has_missing) return("Missing")
  
  NA_character_
}


# ------------------------------------------------------------
# 4. Analyse
# ------------------------------------------------------------

analyse_variable <- function(var_name) {
  
  m <- get_matrix(var_name)
  alpha_stats <- fit_alpha(m, var_name)
  
  summary <- tibble(
    variable = var_name,
    n_eligible_units = nrow(m),
    n_pairable_units = sum(rowSums(!is.na(m)) >= 2),
    n_complete_units = sum(stats::complete.cases(m)),
    missing_ratings = sum(is.na(m)),
    agreement_observed = unname(alpha_stats["observed_agreement"]),
    agreement_all_3_exact = all_three_agreement(m),
    agreement_LA_MF = pair_agreement(m, "LA", "MF"),
    agreement_LA_LG = pair_agreement(m, "LA", "LG"),
    agreement_MF_LG = pair_agreement(m, "MF", "LG"),
    kripp_alpha_nominal = unname(alpha_stats["alpha"]),
    ci95_low = unname(alpha_stats["ci_low"]),
    ci95_high = unname(alpha_stats["ci_high"]),
    dominant_rating_share = dominant_rating_share(m)
  )
  
  issues <- apply(m, 1, issue_type)
  keep <- !is.na(issues)
  
  if (any(keep)) {
    problems <- tibble(
      variable = var_name,
      screenshot_id = rownames(m)[keep],
      LA = m[keep, "LA"],
      MF = m[keep, "MF"],
      LG = m[keep, "LG"],
      issue = issues[keep]
    )
  } else {
    problems <- tibble(
      variable = character(),
      screenshot_id = character(),
      LA = character(),
      MF = character(),
      LG = character(),
      issue = character()
    )
  }
  
  list(summary = summary, problems = problems)
}

results <- lapply(vars, analyse_variable)

summary_table <- bind_rows(
  lapply(results, function(x) x$summary)
) %>%
  mutate(
    across(
      c(
        agreement_observed,
        agreement_all_3_exact,
        agreement_LA_MF,
        agreement_LA_LG,
        agreement_MF_LG,
        kripp_alpha_nominal,
        ci95_low,
        ci95_high,
        dominant_rating_share
      ),
      ~ round(.x, 3)
    )
  )

problem_table <- bind_rows(
  lapply(results, function(x) x$problems)
)

# ------------------------------------------------------------
# 4b. Analyse haeufig verwechselter Codes
# ------------------------------------------------------------

get_confusions <- function(var_name) {
  
  m <- get_matrix(var_name)
  
  coder_pairs <- list(
    c("LA", "MF"),
    c("LA", "LG"),
    c("MF", "LG")
  )
  
  confusion_data <- lapply(
    coder_pairs,
    function(pair) {
      
      coder_a_name <- pair[1]
      coder_b_name <- pair[2]
      
      value_a <- m[, coder_a_name]
      value_b <- m[, coder_b_name]
      
      tibble(
        screenshot_id = rownames(m),
        coder_a = coder_a_name,
        coder_b = coder_b_name,
        value_a = value_a,
        value_b = value_b
      ) %>%
        filter(
          !is.na(.data$value_a),
          !is.na(.data$value_b),
          .data$value_a != .data$value_b
        ) %>%
        mutate(
          # Richtung der Verwechslung ignorieren:
          # 3 vs. 7 und 7 vs. 3 gelten als dieselbe Kombination.
          code_1 = pmin(.data$value_a, .data$value_b),
          code_2 = pmax(.data$value_a, .data$value_b)
        )
    }
  ) %>%
    bind_rows()
  
  if (nrow(confusion_data) == 0) {
    
    return(
      tibble(
        variable = character(),
        code_1 = character(),
        code_2 = character(),
        n_pair_disagreements = integer(),
        n_units = integer(),
        share_of_disagreements = numeric()
      )
    )
  }
  
  total_disagreements <- nrow(confusion_data)
  
  confusion_data %>%
    group_by(
      .data$code_1,
      .data$code_2
    ) %>%
    summarise(
      n_pair_disagreements = n(),
      n_units = n_distinct(.data$screenshot_id),
      .groups = "drop"
    ) %>%
    mutate(
      variable = var_name,
      share_of_disagreements =
        .data$n_pair_disagreements / total_disagreements
    ) %>%
    select(
      .data$variable,
      .data$code_1,
      .data$code_2,
      .data$n_pair_disagreements,
      .data$n_units,
      .data$share_of_disagreements
    ) %>%
    arrange(
      desc(.data$n_pair_disagreements),
      desc(.data$n_units)
    )
}


confusion_table <- lapply(
  vars,
  get_confusions
) %>%
  bind_rows() %>%
  mutate(
    share_of_disagreements =
      round(.data$share_of_disagreements, 3)
  )
# ------------------------------------------------------------
# 5. Ausgabe
# ------------------------------------------------------------

print(summary_table, n = Inf, width = Inf)


writexl::write_xlsx(
  list(
    Reliability = summary_table,
    Disagreements = problem_table,
    Confusions = confusion_table
  ),
  output_file
)

message(
  "\nErgebnisdatei geschrieben: ",
  normalizePath(output_file, mustWork = FALSE)
)

# Hinweise:
# - Alle sechs Variablen werden nominal behandelt.
# - source_name_coded wird bewusst nicht ausgewertet.
# - advertisement/topic/source/media_format werden nur bei einstimmigem
#   public_rel_coded == 1 analysiert.
# - Codes 90-99 bleiben regulaere Kategorien, sofern sie laut Codebuch
#   echte Rest-/Sonderkategorien und keine Missing-Codes sind.
# - Alpha bei starker Kategorien-Schiefe immer zusammen mit
#   agreement_observed und dominant_rating_share interpretieren.

