################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    05_Advanced_Exploration.R
# Version: 2026-08-10 – professional exploratory workflow
#
# Purpose:
#   Theoriegeleitete, ausdrücklich explorative Integration von Screening,
#   7-Tage-Diary und Outro. Das Skript dient der Musterentdeckung,
#   Robustheitsprüfung und Hypothesengenerierung – nicht der nachträglichen
#   Produktion konfirmatorischer Evidenz.
#
# ANALYTISCHE GRUNDSÄTZE
# ----------------------
# 1. Drei Ebenen werden getrennt respektiert:
#    - Screening/Outro: Personenebene
#    - Diary: Posts innerhalb von Personen; zusätzlich Person-Tag-Ebene
# 2. Post-Level-Häufigkeiten beschreiben die HOCHGELADENE Stichprobe, nicht die
#    vollständige Social-Media-Exposition. Wo sinnvoll werden zusätzlich
#    teilnehmergewichtete Schätzer berichtet.
# 3. Zeitvariable Prädiktoren werden in Multilevel-Modellen in Within- und
#    Between-Person-Anteile zerlegt. Dadurch werden situative und stabile
#    Personenunterschiede nicht vermischt.
# 4. Anteile über Themen/Quellen/Plattformen sind kompositionale Daten. Zentrale
#    Need–Content-Analysen werden deshalb zusätzlich über log-ratio/CLR-Koordinaten
#    geprüft; einfache Share-Korrelationen bleiben nur deskriptive Sensitivität.
# 5. Unsicherheit wird für zentrale Personenebenen-Zusammenhänge mit
#    Bootstrap-Konfidenzintervallen ausgewiesen. Tagesverläufe verwenden
#    Cluster-Bootstrap auf Personenebene.
# 6. Multiple Explorationen werden in klaren Familien gebündelt. Breite Discovery-
#    Atlanten verwenden BH-Korrektur; Fokus liegt trotzdem auf Effektgröße,
#    Richtung, Unsicherheit und Robustheit statt auf p < .05.
# 7. Zentrale Ergebnisse werden über plausible Spezifikationen/Operationalisierungen
#    geprüft (z.B. strict vs. broad Incidentality, Mindestzahl öffentlicher Posts,
#    raw shares vs. log-ratios). Einzelne "beste" Spezifikationen werden nicht
#    nach Ergebnislage ausgewählt.
# 8. Novelty ist diary-intern und sinkt mechanisch mit zunehmender Beobachtungszeit.
#    Ein Permutations-Nullmodell trennt diesen mechanischen Anteil von auffälligen
#    zeitlichen Mustern.
# 9. Cluster/Profile sind optional und ausschließlich hypothesengenerierend.
#    Sie werden nur bei ausreichender Stichprobe, Silhouette und Subsample-
#    Stabilität als interpretierbar markiert.
# 10. Outro-Reaktivität wird nach dem Diary erhoben. Zusammenhänge mit Diary-
#     Verläufen sind deshalb methodische Plausibilitäts-/Sensitivitätsanalysen,
#     keine kausalen Reaktivitätseffekte.
# 11. Simulierte Daily-Inhalte dienen ausschließlich Pipeline-Tests und werden in
#     separaten Outputpfaden gespeichert sowie in Grafiken sichtbar markiert.
# 12. Die Uploads sind eine von den Teilnehmenden erzeugte Auswahl: Die Studie misst
#     keine vollständige Feed-Exposition. `public_rel_coded` ist deshalb primär ein
#     Gate/QC-Marker für die Befolgung der Uploadinstruktion, keine Prävalenzschätzung
#     öffentlich relevanter Inhalte im Feed. Auch zeitlich geordnete Screening→Diary-
#     Brücken werden als Assoziationen, nicht als kausale Effekte interpretiert.
# 13. Plattformunterschiede können durch unterschiedliche Nutzergruppen entstehen.
#     Deshalb werden globale Plattform-Fingerprints durch Within-Person-Vergleiche
#     bei Multi-Plattform-Nutzenden ergänzt; auch diese bleiben deskriptiv.
# 14. Ein Tag ohne Screenshot ist in den hier verwendeten Screenshot-Daten KEIN
#     verifizierter Nulltag: Es kann fehlende Teilnahme oder tatsächlich keinen
#     Upload bedeuten. Zeitverläufe von Uploadzahl/Inhalten konditionieren deshalb
#     auf beobachtete Upload-Tage; die Rate beobachteter Upload-Tage wird separat
#     als Coverage-Marker gezeigt.
# 15. Die finale Coding-Datei enthält pro Screenshot nur einen finalen Code.
#     Intercoder-Reliabilität kann daraus nicht nachträglich geschätzt werden.
#     Falls mehrere Codierende eingesetzt wurden, gehört eine separat doppelt
#     codierte Reliabilitätsstichprobe in den Methodenbericht.
# 16. Für substantielle Zusammenhänge wird nicht imputiert; jedes Ergebnis berichtet
#     sein analysierbares N. Median-Imputation wird ausschließlich für die grafische
#     Darstellung/PCA einer bereits bestimmten explorativen Clusterlösung verwendet.
# 17. Auf Lag-/AR- oder kausale Sequenzmodelle wird bewusst verzichtet: Die Uploads
#     bilden keinen vollständigen, kontinuierlich beobachteten Expositionsstrom ab.
# 18. Kategorielle Richness steigt mechanisch mit der Zahl beobachteter Posts.
#     Roh-Richness wird deshalb durch Hurlbert-Rarefaction auf eine gemeinsame
#     Postzahl ergänzt. Das standardisiert nur die OBSERVIERTE Uploadstichprobe und
#     schätzt weiterhin nicht das vollständige Informationsrepertoire einer Person.
#
# Methodische Orientierung:
#   Bolger, Davis & Rafaeli (2003), Diary methods:
#   https://doi.org/10.1146/annurev.psych.54.101601.145030
#   Stone & Shiffman (2002), EMA reporting guidelines:
#   https://doi.org/10.1207/S15324796ABM2403_09
#   Curran & Bauer (2011), Within-/Between-Person disaggregation:
#   https://doi.org/10.1146/annurev.psych.093008.100356
#   Hoffman & Stawski (2009), Persons as contexts:
#   https://doi.org/10.1080/15427600902911189
#   Aitchison (1982), compositional data:
#   https://doi.org/10.1111/j.2517-6161.1982.tb01195.x
#   Steegen et al. (2016), multiverse/sensitivity analysis:
#   https://doi.org/10.1177/1745691616658637
#   Krippendorff (2004), reliability in content analysis:
#   https://doi.org/10.1111/j.1468-2958.2004.tb00738.x
#   Hayes & Krippendorff (2007), agreement for coding data:
#   https://doi.org/10.1080/19312450709336664
#   Hurlbert (1971), expected richness under rarefaction:
#   https://doi.org/10.2307/1934145
#
# Inputs:
#   03_Output/screening_prepared.rds
#   03_Output/daily_screenshot_level.rds
#   03_Output/daily_participant_level.rds
#   03_Output/outro_prepared.rds
#
# simulate_coding = TRUE nutzt die separaten simulierten Daily-Outputs aus 04b.
# Screening und Outro bleiben real; nur manuell codierte Daily-Inhalte sind dann
# simuliert.
#
# Outputs:
#   03_Output/Advanced_Exploration.xlsx
#   bzw. Advanced_Exploration_SIMULATED.xlsx
#   04_Figures/Advanced/*.png
#   bzw. 04_Figures/Advanced_SIMULATED/*.png
################################################################################

rm(list = ls())


#===============================================================================
# 01 Settings
#===============================================================================

# Pipeline-Modus ---------------------------------------------------------------
simulate_coding <- TRUE
create_figures <- TRUE
overwrite_outputs <- TRUE

# Rechenintensive optionale Explorationen -------------------------------------
run_mixed_models <- TRUE
run_profile_typology <- TRUE
run_alluvial <- FALSE
run_compositional_analysis <- TRUE
run_sensitivity_multiverse <- TRUE
run_novelty_permutation <- TRUE

# Mindestgrößen ---------------------------------------------------------------
minimum_model_n <- 80
minimum_model_participants <- 20
minimum_model_events <- 10
minimum_typology_n <- 40
minimum_composition_posts <- 5
minimum_posts_per_platform_within <- 2
rarefaction_post_count <- 5

# Robustheit / Resampling ------------------------------------------------------
bootstrap_reps <- 1000
novelty_permutation_reps <- 300
profile_stability_reps <- 100
sensitivity_public_post_thresholds <- c(5, 10, 15)

# Typologie wird nur dann als interpretierbar markiert, wenn beide Kriterien
# erfüllt sind. Sie wird andernfalls trotzdem als Diagnose ausgegeben.
minimum_cluster_silhouette <- 0.20
minimum_profile_stability_ari <- 0.60

# Reproduzierbarkeit -----------------------------------------------------------
exploration_seed <- 20260810


#===============================================================================
# 02 Packages, helper and paths
#===============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  janitor,
  openxlsx,
  fs,
  scales,
  cluster,
  lme4,
  broom.mixed
)

helper_script <- file.path("02_Scripts", "00_Helpers.R")
if (!file.exists(helper_script)) stop("Helper-Script nicht gefunden: ", helper_script)
source(helper_script)

screening_file <- file.path("03_Output", "screening_prepared.rds")
outro_file <- file.path("03_Output", "outro_prepared.rds")

if (simulate_coding) {
  daily_screenshot_file <- file.path("03_Output", "daily_screenshot_level_SIMULATED.rds")
  daily_participant_file <- file.path("03_Output", "daily_participant_level_SIMULATED.rds")
  output_excel <- file.path("03_Output", "Advanced_Exploration_SIMULATED.xlsx")
  figure_folder <- file.path("04_Figures", "Advanced_SIMULATED")
} else {
  daily_screenshot_file <- file.path("03_Output", "daily_screenshot_level.rds")
  daily_participant_file <- file.path("03_Output", "daily_participant_level.rds")
  output_excel <- file.path("03_Output", "Advanced_Exploration.xlsx")
  figure_folder <- file.path("04_Figures", "Advanced")
}

required_files <- c(
  screening_file,
  daily_screenshot_file,
  daily_participant_file,
  outro_file
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  extra_note <- if (simulate_coding) {
    paste0(
      "\nFür simulate_coding = TRUE zuerst 04b_Daily_Analysis.R mit ",
      "simulate_coding <- TRUE ausführen."
    )
  } else {
    ""
  }
  stop("Folgende Inputs fehlen:\n- ", paste(missing_files, collapse = "\n- "), extra_note)
}

fs::dir_create(dirname(output_excel))
fs::dir_create(figure_folder)

if (!overwrite_outputs && file.exists(output_excel)) {
  stop("Output existiert bereits: ", output_excel)
}

analysis_note <- if (simulate_coding) {
  "SIMULIERTE DAILY-INHALTE – ausschließlich Pipeline-Test; Simulator kann Screening-Muster absichtlich in Daily-Codes einprägen"
} else {
  "Reale Coding-Inhalte – sämtliche Analysen dieses Skripts explorativ"
}


#===============================================================================
# 03 Script-specific methodological helpers
#===============================================================================
# Nur Funktionen, die speziell für die fortgeschrittene Exploration benötigt
# werden. Allgemeine Projektfunktionen verbleiben in 00_Helpers.R.

save_adv_plot <- function(plot, filename, width = 9, height = 6) {
  if (!create_figures) return(invisible(NULL))
  
  # Simulierte Ergebnisse werden zusätzlich im Plot selbst markiert – nicht nur
  # über Dateiname/Ordner. So kann eine exportierte Grafik nicht versehentlich als
  # reales Ergebnis weitergegeben werden.
  if (simulate_coding) {
    old_caption <- plot$labels$caption %||% ""
    simulation_warning <- "SIMULIERTE DAILY-INHALTE – NICHT SUBSTANTIV INTERPRETIEREN"
    new_caption <- paste(c(old_caption, simulation_warning)[nzchar(c(old_caption, simulation_warning))], collapse = "\n")
    plot <- plot + labs(caption = new_caption)
  }
  
  save_project_plot(
    plot = plot,
    filename = file.path(figure_folder, filename),
    width = width,
    height = height
  )
}

mean_ci <- function(x) {
  x <- clean_numeric(x)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) {
    return(tibble(N = 0L, Mean = NA_real_, SD = NA_real_, CI_Low = NA_real_, CI_High = NA_real_))
  }
  m <- mean(x)
  s <- if (n > 1) sd(x) else NA_real_
  se <- if (n > 1) s / sqrt(n) else NA_real_
  margin <- if (n > 1) qt(.975, df = n - 1) * se else NA_real_
  tibble(N = n, Mean = m, SD = s, CI_Low = m - margin, CI_High = m + margin)
}

# Nichtparametrisches Personen-Bootstrap-CI für Mittelwerte. Wird für
# Within-Person-Plattformvergleiche genutzt, nachdem pro Person bereits genau ein
# Differenz-/JSD-Wert vorliegt.
# Expected categorical richness in a random subsample of fixed size (Hurlbert-
# Rarefaction). This is useful because raw Topic/Source/Account richness rises
# mechanically with the number of observed posts. The estimate standardizes only
# the observed upload sample; it does not recover unseen feed content.
rarefied_richness <- function(x, sample_size = rarefaction_post_count) {
  x <- clean_text(x)
  x <- x[!is.na(x)]
  total <- length(x)
  
  if (total < sample_size || sample_size < 1) return(NA_real_)
  
  counts <- as.numeric(table(x))
  log_denominator <- lchoose(total, sample_size)
  
  probability_absent <- vapply(
    counts,
    function(n_i) {
      available_without_category <- total - n_i
      if (available_without_category < sample_size) {
        0
      } else {
        exp(lchoose(available_without_category, sample_size) - log_denominator)
      }
    },
    numeric(1)
  )
  
  sum(1 - probability_absent)
}


bootstrap_mean_ci <- function(x, reps = bootstrap_reps, seed = exploration_seed) {
  x <- clean_numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n == 0) {
    return(tibble(N = 0L, Mean = NA_real_, CI95_Low = NA_real_, CI95_High = NA_real_))
  }
  
  estimate <- mean(x)
  if (n < 8) {
    return(tibble(N = n, Mean = estimate, CI95_Low = NA_real_, CI95_High = NA_real_))
  }
  
  set.seed(seed + n)
  boots <- replicate(reps, mean(sample(x, size = n, replace = TRUE)))
  ci <- quantile(boots, c(.025, .975), na.rm = TRUE, type = 6)
  
  tibble(
    N = n, Mean = estimate,
    CI95_Low = unname(ci[[1]]),
    CI95_High = unname(ci[[2]])
  )
}

safe_spearman <- function(data, x, y, x_label = x, y_label = y, family = "Explorativ") {
  if (!all(c(x, y) %in% names(data))) {
    return(tibble(
      Family = family, Predictor = x_label, Outcome = y_label,
      N = 0L, Spearman_Rho = NA_real_, P_Value = NA_real_
    ))
  }
  
  d <- data %>%
    transmute(x = clean_numeric(.data[[x]]), y = clean_numeric(.data[[y]])) %>%
    drop_na()
  
  if (nrow(d) < 5 || n_distinct(d$x) < 2 || n_distinct(d$y) < 2) {
    return(tibble(
      Family = family, Predictor = x_label, Outcome = y_label,
      N = nrow(d), Spearman_Rho = NA_real_, P_Value = NA_real_
    ))
  }
  
  test <- suppressWarnings(cor.test(d$x, d$y, method = "spearman", exact = FALSE))
  
  tibble(
    Family = family,
    Predictor = x_label,
    Outcome = y_label,
    N = nrow(d),
    Spearman_Rho = unname(test$estimate),
    P_Value = test$p.value
  )
}

# Für die publication-orientierten Brücken werden Personen resampled. Das
# Bootstrap-CI ist robuster und informativer als nur ein p-Wert.
bootstrap_spearman <- function(
    data, x, y, x_label = x, y_label = y, family = "Explorativ",
    reps = bootstrap_reps, seed = exploration_seed) {
  
  if (!all(c(x, y) %in% names(data))) {
    return(tibble(
      Family = family, Predictor = x_label, Outcome = y_label,
      N = 0L, Spearman_Rho = NA_real_, CI95_Low = NA_real_, CI95_High = NA_real_,
      P_Value = NA_real_
    ))
  }
  
  d <- data %>%
    transmute(x = clean_numeric(.data[[x]]), y = clean_numeric(.data[[y]])) %>%
    drop_na()
  
  if (nrow(d) < 8 || n_distinct(d$x) < 2 || n_distinct(d$y) < 2) {
    return(tibble(
      Family = family, Predictor = x_label, Outcome = y_label,
      N = nrow(d), Spearman_Rho = NA_real_, CI95_Low = NA_real_, CI95_High = NA_real_,
      P_Value = NA_real_
    ))
  }
  
  rho <- suppressWarnings(cor(d$x, d$y, method = "spearman"))
  test <- suppressWarnings(cor.test(d$x, d$y, method = "spearman", exact = FALSE))
  
  # Variablennamen gehen in den Seed ein, sodass einzelne Analysen reproduzierbar
  # bleiben, selbst wenn die Reihenfolge der Spezifikationen verändert wird.
  local_seed <- seed + sum(utf8ToInt(paste0(x, "__", y)))
  set.seed(local_seed)
  
  boots <- replicate(reps, {
    idx <- sample.int(nrow(d), nrow(d), replace = TRUE)
    xb <- d$x[idx]
    yb <- d$y[idx]
    if (n_distinct(xb) < 2 || n_distinct(yb) < 2) return(NA_real_)
    suppressWarnings(cor(xb, yb, method = "spearman"))
  })
  boots <- boots[is.finite(boots)]
  
  ci <- if (length(boots) >= max(100, reps * .5)) {
    unname(quantile(boots, c(.025, .975), na.rm = TRUE, type = 6))
  } else {
    c(NA_real_, NA_real_)
  }
  
  tibble(
    Family = family,
    Predictor = x_label,
    Outcome = y_label,
    N = nrow(d),
    Spearman_Rho = rho,
    CI95_Low = ci[[1]],
    CI95_High = ci[[2]],
    P_Value = test$p.value
  )
}

# Leave-one-person-out (LOO) prüft bei kleinen Personenstichproben, ob eine
# zentrale Korrelation von einzelnen Fällen getragen wird. Das ist keine zweite
# Signifikanzprüfung, sondern eine Einfluss-/Robustheitsdiagnostik.
loo_spearman_summary <- function(data, x, y) {
  if (!all(c(x, y) %in% names(data))) {
    return(tibble(LOO_Min = NA_real_, LOO_Max = NA_real_,
                  LOO_Median = NA_real_, LOO_Sign_Stable = NA))
  }
  
  d <- data %>%
    transmute(x = clean_numeric(.data[[x]]), y = clean_numeric(.data[[y]])) %>%
    drop_na()
  
  if (nrow(d) < 10 || n_distinct(d$x) < 2 || n_distinct(d$y) < 2) {
    return(tibble(LOO_Min = NA_real_, LOO_Max = NA_real_,
                  LOO_Median = NA_real_, LOO_Sign_Stable = NA))
  }
  
  observed <- suppressWarnings(cor(d$x, d$y, method = "spearman"))
  loo <- map_dbl(seq_len(nrow(d)), function(i) {
    di <- d[-i, , drop = FALSE]
    if (n_distinct(di$x) < 2 || n_distinct(di$y) < 2) return(NA_real_)
    suppressWarnings(cor(di$x, di$y, method = "spearman"))
  })
  loo <- loo[is.finite(loo)]
  
  if (length(loo) == 0) {
    return(tibble(LOO_Min = NA_real_, LOO_Max = NA_real_,
                  LOO_Median = NA_real_, LOO_Sign_Stable = NA))
  }
  
  sign_stable <- if (!is.finite(observed) || observed == 0) {
    NA
  } else if (all(loo == 0)) {
    FALSE
  } else {
    all(sign(loo[loo != 0]) == sign(observed))
  }
  
  tibble(
    LOO_Min = min(loo),
    LOO_Max = max(loo),
    LOO_Median = median(loo),
    LOO_Sign_Stable = sign_stable
  )
}

# Standardisierte Mittelwertdifferenz für Selektions-/Attritionsdiagnostik.
# Sie ist deskriptiv und wird nicht als Signifikanztest missverstanden.
standardized_mean_difference <- function(x, group) {
  x <- clean_numeric(x)
  g <- as.integer(group)
  keep <- !is.na(x) & !is.na(g) & g %in% c(0L, 1L)
  x <- x[keep]
  g <- g[keep]
  
  if (length(x) < 4 || length(unique(g)) < 2) return(NA_real_)
  x0 <- x[g == 0L]
  x1 <- x[g == 1L]
  if (length(x0) < 2 || length(x1) < 2) return(NA_real_)
  
  pooled_sd <- sqrt(((length(x0) - 1) * var(x0) + (length(x1) - 1) * var(x1)) /
                      (length(x0) + length(x1) - 2))
  if (!is.finite(pooled_sd) || pooled_sd == 0) return(NA_real_)
  (mean(x1) - mean(x0)) / pooled_sd
}

# Standardisierte Residuen sind hier rein deskriptive "Fingerprints". Der
# klassische Chi²-p-Wert wird bewusst nicht exportiert, weil Posts innerhalb von
# Personen verschachtelt sind und die Unabhängigkeitsannahme verletzt wäre.
chisq_residual_table <- function(data, row_var, col_var, row_label, col_label) {
  d <- data %>% filter(!is.na(.data[[row_var]]), !is.na(.data[[col_var]]))
  tab <- table(d[[row_var]], d[[col_var]])
  
  if (nrow(tab) < 2 || ncol(tab) < 2 || sum(tab) == 0) return(tibble())
  
  test <- suppressWarnings(chisq.test(tab))
  as.data.frame(test$stdres) %>%
    as_tibble() %>%
    setNames(c(row_label, col_label, "Std_Residual")) %>%
    mutate(
      N_Cell = as.vector(tab),
      Expected_N = as.vector(test$expected),
      Sparse_Cell = Expected_N < 5,
      Note = if_else(
        Sparse_Cell,
        "Deskriptiver Fingerprint; erwartete Zellhäufigkeit < 5 – besonders vorsichtig interpretieren",
        "Deskriptiver Fingerprint; kein inferenzieller Chi²-Test wegen verschachtelter Posts"
      )
    )
}

choose_dominant_domain <- function(undirected, thematic, social, problem) {
  values <- c(
    Ungerichtet = clean_numeric(undirected),
    Thematisch = clean_numeric(thematic),
    Sozial = clean_numeric(social),
    Problembezogen = clean_numeric(problem)
  )
  if (all(is.na(values))) return(NA_character_)
  max_value <- max(values, na.rm = TRUE)
  top <- names(values)[!is.na(values) & values == max_value]
  if (length(top) == 1) top else "Kein eindeutiges dominantes Bedürfnis"
}

# Cluster-Bootstrap für Tagesmittel: Personen, nicht einzelne Posts/Tage, werden
# resampled. Dadurch bleibt die Within-Person-Abhängigkeit im Resampling erhalten.
cluster_bootstrap_day_summary <- function(data, reps = bootstrap_reps, seed = exploration_seed) {
  observed <- data %>%
    group_by(study_day, Metric) %>%
    summarise(
      N = sum(!is.na(Value)),
      Mean = safe_mean(Value),
      .groups = "drop"
    )
  
  ids <- unique(data$participant[!is.na(data$participant)])
  if (length(ids) < 8) {
    return(observed %>% mutate(CI_Low = NA_real_, CI_High = NA_real_))
  }
  
  set.seed(seed)
  boot <- map_dfr(seq_len(reps), function(b) {
    sampled <- sample(ids, length(ids), replace = TRUE)
    w <- as.data.frame(table(sampled), stringsAsFactors = FALSE)
    names(w) <- c("participant", "boot_weight")
    w$participant <- as.character(w$participant)
    w$boot_weight <- as.numeric(w$boot_weight)
    
    data %>%
      inner_join(w, by = "participant") %>%
      filter(!is.na(Value)) %>%
      group_by(study_day, Metric) %>%
      summarise(
        Boot_Mean = weighted.mean(Value, boot_weight),
        .groups = "drop"
      ) %>%
      mutate(Boot = b)
  })
  
  ci <- boot %>%
    group_by(study_day, Metric) %>%
    summarise(
      CI_Low = quantile(Boot_Mean, .025, na.rm = TRUE, type = 6),
      CI_High = quantile(Boot_Mean, .975, na.rm = TRUE, type = 6),
      .groups = "drop"
    )
  
  observed %>% left_join(ci, by = c("study_day", "Metric"))
}

# CLR-Transformation für kompositionale Repertoires. Jede CLR-Koordinate ist
# der Log-Anteil einer Kategorie relativ zum geometrischen Mittel der übrigen
# Komposition – also RELATIVE, nicht absolute Häufigkeit. Ein kleiner Pseudocount
# verhindert log(0); Ergebnisse werden deshalb über Pseudocounts und Mindestzahl
# beobachteter Posts sensitivitätsgeprüft.
make_clr_composition <- function(data, category_var, min_total = minimum_composition_posts, pseudocount = .5) {
  d <- data %>%
    filter(!is.na(participant), !is.na(.data[[category_var]])) %>%
    transmute(participant, Category = as.character(.data[[category_var]]))
  
  categories <- sort(unique(d$Category))
  if (length(categories) < 2) {
    return(list(data = tibble(participant = character()), map = tibble(), categories = categories))
  }
  
  counts <- d %>%
    count(participant, Category, name = "N") %>%
    complete(participant, Category = categories, fill = list(N = 0L)) %>%
    pivot_wider(names_from = Category, values_from = N, values_fill = 0L)
  
  mat <- as.matrix(as.data.frame(counts)[, -1, drop = FALSE])
  totals <- rowSums(mat)
  keep <- totals >= min_total
  mat <- mat[keep, , drop = FALSE]
  ids <- counts$participant[keep]
  
  if (nrow(mat) == 0) {
    return(list(data = tibble(participant = character()), map = tibble(), categories = categories))
  }
  
  mat_pc <- mat + pseudocount
  prop <- mat_pc / rowSums(mat_pc)
  log_prop <- log(prop)
  clr <- log_prop - rowMeans(log_prop)
  
  clean_names <- janitor::make_clean_names(colnames(clr))
  colnames(clr) <- paste0("clr_", clean_names)
  
  map <- tibble(
    Category = colnames(mat),
    CLR_Variable = colnames(clr)
  )
  
  out <- as_tibble(clr) %>%
    mutate(participant = ids, .before = 1)
  
  list(data = out, map = map, categories = categories)
}

get_clr_variable <- function(composition_object, category_label) {
  hit <- composition_object$map %>% filter(Category == category_label)
  if (nrow(hit) != 1) return(NA_character_)
  hit$CLR_Variable[[1]]
}

# Adjusted Rand Index für die Stabilitätsprüfung der explorativen Profile.
adjusted_rand_index <- function(a, b) {
  keep <- !is.na(a) & !is.na(b)
  a <- a[keep]
  b <- b[keep]
  n <- length(a)
  if (n < 4) return(NA_real_)
  
  tab <- table(a, b)
  choose2 <- function(x) x * (x - 1) / 2
  sum_cells <- sum(choose2(tab))
  sum_rows <- sum(choose2(rowSums(tab)))
  sum_cols <- sum(choose2(colSums(tab)))
  total_pairs <- choose2(n)
  if (total_pairs == 0) return(NA_real_)
  
  expected <- sum_rows * sum_cols / total_pairs
  max_index <- .5 * (sum_rows + sum_cols)
  denom <- max_index - expected
  if (denom == 0) return(NA_real_)
  (sum_cells - expected) / denom
}

# Logistic ICC auf Latent-Response-Skala. Nur zur Quantifizierung der
# Personenclusterung; nicht als inhaltliches Hauptergebnis.
logistic_icc <- function(data, outcome, label) {
  if (!outcome %in% names(data)) return(tibble())
  d <- data %>%
    transmute(participant, y = clean_numeric(.data[[outcome]])) %>%
    filter(y %in% c(0, 1), !is.na(participant))
  
  if (nrow(d) < minimum_model_n || n_distinct(d$participant) < minimum_model_participants ||
      sum(d$y == 1) < minimum_model_events || sum(d$y == 0) < minimum_model_events) {
    return(tibble(Outcome = label, N = nrow(d), Participants = n_distinct(d$participant), ICC = NA_real_))
  }
  
  fit <- tryCatch(
    glmer(y ~ 1 + (1 | participant), data = d, family = binomial(),
          control = glmerControl(optimizer = "bobyqa")),
    error = function(e) NULL
  )
  if (is.null(fit)) return(tibble(Outcome = label, N = nrow(d), Participants = n_distinct(d$participant), ICC = NA_real_))
  
  var_person <- as.numeric(VarCorr(fit)$participant[1])
  icc <- var_person / (var_person + (pi^2 / 3))
  tibble(Outcome = label, N = nrow(d), Participants = n_distinct(d$participant), ICC = icc)
}

# Multilevel-Modell mit sauberer Complete-Case-Zählung, Konvergenzstatus und
# Konfidenzintervallen. Fixed effects werden als OR ausgegeben.
safe_glmer <- function(data, formula, family, model_name, effect_scale = "OR") {
  vars <- unique(c(all.vars(formula), "participant"))
  vars <- intersect(vars, names(data))
  d <- data %>% select(all_of(vars)) %>% drop_na()
  
  outcome <- all.vars(formula)[1]
  n <- nrow(d)
  n_person <- n_distinct(d$participant)
  events <- if (family$family == "binomial") sum(d[[outcome]] == 1, na.rm = TRUE) else NA_integer_
  nonevents <- if (family$family == "binomial") sum(d[[outcome]] == 0, na.rm = TRUE) else NA_integer_
  
  enough <- n >= minimum_model_n && n_person >= minimum_model_participants
  if (family$family == "binomial") {
    enough <- enough && events >= minimum_model_events && nonevents >= minimum_model_events
  }
  
  if (!enough) {
    return(list(
      status = tibble(
        Model = model_name, N = n, Participants = n_person,
        Events = events, NonEvents = nonevents,
        Singular = NA, Convergence_Message = NA_character_,
        Status = "Übersprungen: Mindestgröße nicht erreicht"
      ),
      tidy = tibble()
    ))
  }
  
  fit <- tryCatch(
    glmer(
      formula, data = d, family = family,
      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(list(
      status = tibble(
        Model = model_name, N = n, Participants = n_person,
        Events = events, NonEvents = nonevents,
        Singular = NA, Convergence_Message = "Modellfehler",
        Status = "Modellfehler"
      ),
      tidy = tibble()
    ))
  }
  
  singular <- lme4::isSingular(fit, tol = 1e-4)
  conv_msg <- fit@optinfo$conv$lme4$messages
  conv_msg <- if (is.null(conv_msg) || length(conv_msg) == 0) {
    NA_character_
  } else {
    msg <- paste(conv_msg, collapse = " | ")
    if (nzchar(msg)) msg else NA_character_
  }
  status_text <- case_when(
    !is.na(conv_msg) ~ "Geschätzt; Konvergenzhinweis",
    singular ~ "Geschätzt; singulär",
    TRUE ~ "Geschätzt"
  )
  
  tidy <- broom.mixed::tidy(
    fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE
  ) %>%
    filter(term != "(Intercept)") %>%
    transmute(
      Model = model_name,
      Effect_Scale = effect_scale,
      Term = term,
      Estimate_Exp = estimate,
      CI95_Low = conf.low,
      CI95_High = conf.high,
      P_Value = p.value
    )
  
  list(
    status = tibble(
      Model = model_name, N = n, Participants = n_person,
      Events = events, NonEvents = nonevents,
      Singular = singular, Convergence_Message = conv_msg,
      Status = status_text
    ),
    tidy = tidy,
    fit = fit
  )
}


#===============================================================================
# 04 Load and harmonise the three survey levels
#===============================================================================
# Alle Variablennamen werden einmal in snake_case überführt. Die integrierte
# Personenebene verwendet Screening als Ausgangspunkt, ergänzt Daily-Metriken und
# anschließend Outro-Indizes. So bleiben die Messzeitpunkte konzeptionell sauber.

screening <- readRDS(screening_file) %>% janitor::clean_names()
daily_all <- readRDS(daily_screenshot_file) %>% janitor::clean_names()
daily_person <- readRDS(daily_participant_file) %>% janitor::clean_names()
outro <- readRDS(outro_file) %>% janitor::clean_names()

for (obj_name in c("screening", "daily_all", "daily_person", "outro")) {
  obj <- get(obj_name)
  if (!"participant" %in% names(obj)) {
    candidate <- intersect(
      c("personal_participant_code", "personalparticipantcode"),
      names(obj)
    )
    if (length(candidate) == 0) stop("Kein Participant-Identifier in ", obj_name)
    obj$participant <- obj[[candidate[[1]]]]
  }
  obj$participant <- clean_text(obj$participant)
  assign(obj_name, obj)
}

screening <- screening %>% filter(!is.na(participant))
daily_person <- daily_person %>% filter(!is.na(participant))
outro <- outro %>% filter(!is.na(participant))
daily_all <- daily_all %>% filter(!is.na(participant))

# Prepared Inputs sollten auf Personenebene bereits eindeutig sein. Doppelte IDs
# werden hier nicht stillschweigend dedupliziert, weil das eine inhaltlich
# relevante Pipeline-Entscheidung verbergen würde.
for (obj_name in c("screening", "daily_person", "outro")) {
  obj <- get(obj_name)
  dup <- obj %>% count(participant, name = "N") %>% filter(N > 1)
  if (nrow(dup) > 0) {
    stop(obj_name, " enthält doppelte Participant IDs. Bitte vorgelagerten Analyseschritt prüfen.")
  }
}

if (!"screenshot_id" %in% names(daily_all)) {
  stop("daily_screenshot_level benötigt screenshot_id für Sequenz-/Novelty-Analysen.")
}

if (anyDuplicated(daily_all$screenshot_id)) {
  stop("daily_screenshot_level enthält doppelte screenshot_id-Werte.")
}

# In Simulationen sollten die separaten Daily-Dateien tatsächlich entsprechend
# markiert sein. Der Dateiname ist die primäre Sicherung; diese Plausibilitäts-
# meldung verhindert versehentliche Interpretation als reale Inhaltsanalyse.
if (simulate_coding) {
  message("ACHTUNG: Advanced Exploration läuft mit SIMULIERTEN Daily-Coding-Inhalten.")
  if ("coding_was_simulated" %in% names(daily_all) &&
      !any(daily_all$coding_was_simulated %in% TRUE, na.rm = TRUE)) {
    warning("SIMULATED-Dateiname gewählt, aber coding_was_simulated ist nirgends TRUE.")
  }
} else {
  if ("coding_was_simulated" %in% names(daily_all) &&
      any(daily_all$coding_was_simulated %in% TRUE, na.rm = TRUE)) {
    stop("Realer Analysemodus, aber Daily-Daten enthalten simulierte Coding-Zeilen.")
  }
}


#===============================================================================
# 05 Integrated participant master table
#===============================================================================
# Für integrierte Analysen werden nur Daily-eigene und Outro-eigene Merkmale
# ergänzt; Screening bleibt die Quelle für Alter, Bedürfnisse und Baseline-Maße.

screening_keep <- c(
  "participant", "intro_age_num", "gender", "education_three_level",
  "intro_intensity", "n_platforms_weekly", "platform_repertoire",
  "primary_platform", "intro_ib_undirected", "intro_ib_thematic",
  "intro_ib_social", "intro_ib_problem", "mean_importance",
  "need_differentiation", "number_of_high_needs",
  "dominant_information_need", "incidentality_index",
  "context_local", "context_social"
)

daily_keep <- c(
  "participant", "n_screenshots", "n_active_days", "n_public_relevant",
  "n_not_public_relevant", "n_not_assessable", "share_publicly_relevant",
  "n_public_content_posts", "share_incidental_strict", "share_incidental_broad",
  "share_targeted", "share_read_thoroughly", "share_researched",
  "share_engaged", "share_any_interaction", "share_home", "share_away",
  "share_alone", "share_together", "topic_richness", "topic_shannon",
  "source_richness", "source_shannon", "platform_richness", "platform_shannon",
  "format_richness", "format_shannon", "n_unique_account_names",
  "share_current_affairs", "share_practical_service", "share_knowledge_interests",
  "share_journalistic_sources", "share_peer_sources", "share_facebook",
  "share_instagram", "share_tik_tok", "share_x", "share_video",
  "share_topic_novelty", "share_account_novelty",
  "share_productive_serendipity_strict", "share_productive_serendipity_broad",
  "mean_post_need_fit_z", "platform_report_match_rate", "daily_primary_platform",
  "primary_platform_match", "local_context_alignment", "social_context_alignment",
  "incidentality_gap_broad", "absolute_incidentality_gap_broad",
  "incidentality_gap_strict", "absolute_incidentality_gap_strict",
  "platform_profile_alignment",
  "public_relevance_day_slope", "targeted_post_day_slope",
  "thorough_reading_day_slope"
)

# janitor kann TikTok je nach Ursprung als share_tik_tok oder share_tiktok führen.
if ("share_tiktok" %in% names(daily_person) && !"share_tik_tok" %in% names(daily_person)) {
  daily_person <- daily_person %>% rename(share_tik_tok = share_tiktok)
}

outro_keep <- c(
  "participant", "reactivity_index", "ease_index",
  paste0("outro_reactivity_", 1:5),
  paste0("outro_ease_", 1:8),
  "problems_free", "suggestions_free"
)

master <- screening %>%
  select(any_of(screening_keep)) %>%
  inner_join(
    daily_person %>% select(any_of(daily_keep)),
    by = "participant"
  ) %>%
  left_join(
    outro %>% select(any_of(outro_keep)),
    by = "participant"
  ) %>%
  mutate(
    outro_available = !is.na(reactivity_index) | !is.na(ease_index),
    # Methodischer QC-Marker: nicht beurteilbare Uploads sind kein Inhaltstyp.
    share_not_assessable_uploads = safe_divide(n_not_assessable, n_screenshots),
    processing_intensity = pmap_dbl(
      list(share_read_thoroughly, share_researched, share_engaged),
      function(a, b, c) {
        x <- c(a, b, c)
        if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
      }
    ),
    context_alignment_mean = rowMeans(
      cbind(local_context_alignment, social_context_alignment),
      na.rm = TRUE
    ),
    context_alignment_mean = if_else(
      is.nan(context_alignment_mean),
      NA_real_,
      context_alignment_mean
    ),
    need_profile_alignment = pmap_dbl(
      list(
        intro_ib_undirected, intro_ib_thematic,
        intro_ib_social, intro_ib_problem,
        share_current_affairs, share_knowledge_interests,
        share_peer_sources, share_practical_service
      ),
      function(n_und, n_them, n_soc, n_prob, d_und, d_them, d_soc, d_prob) {
        profile_alignment(
          c(n_und, n_them, n_soc, n_prob),
          c(d_und, d_them, d_soc, d_prob)
        )
      }
    ),
    observed_dominant_need = pmap_chr(
      list(
        share_current_affairs,
        share_knowledge_interests,
        share_peer_sources,
        share_practical_service
      ),
      choose_dominant_domain
    ),
    dominant_need_match = case_when(
      is.na(dominant_information_need) | is.na(observed_dominant_need) ~ NA_integer_,
      str_detect(dominant_information_need, "Kein eindeutiges") ~ NA_integer_,
      str_detect(observed_dominant_need, "Kein eindeutiges") ~ NA_integer_,
      dominant_information_need == observed_dominant_need ~ 1L,
      TRUE ~ 0L
    ),
    # Reaktivität ist inhaltlich heterogen. Für gezielte Methodensensitivität
    # werden drei Items zusätzlich einzeln und richtungsgleich betrachtet.
    # Höher = mehr reaktives Verhalten / geringere Repräsentativität.
    reactivity_upload_search_item = clean_numeric(outro_reactivity_1),
    reactivity_usage_change_item = 6 - clean_numeric(outro_reactivity_3),
    reactivity_low_representativeness_item = 6 - clean_numeric(outro_reactivity_5),
    # Z-Standardisierung erfolgt auf Personenebene, damit Viel-Uploader die
    # Skalierung von Cross-Level-Prädiktoren nicht verzerren.
    age_z_adv = safe_z(intro_age_num),
    intensity_z_adv = safe_z(intro_intensity),
    repertoire_z_adv = safe_z(n_platforms_weekly),
    reactivity_z_adv = safe_z(reactivity_index),
    ease_z_adv = safe_z(ease_index)
  )

# Reactivity/Ease-Gruppen dienen nur Visualisierungen. Die kontinuierlichen
# Indizes bleiben für alle theoriegeleiteten Explorationen maßgeblich.
master <- master %>%
  mutate(
    reactivity_group = if_else(
      !is.na(reactivity_index),
      as.character(ntile(reactivity_index, 3)),
      NA_character_
    ),
    reactivity_group = recode(
      reactivity_group,
      `1` = "Niedrig",
      `2` = "Mittel",
      `3` = "Hoch"
    ),
    reactivity_group = factor(reactivity_group, levels = c("Niedrig", "Mittel", "Hoch")),
    ease_group = if_else(
      !is.na(ease_index),
      as.character(ntile(ease_index, 3)),
      NA_character_
    ),
    ease_group = recode(
      ease_group,
      `1` = "Niedrig",
      `2` = "Mittel",
      `3` = "Hoch"
    ),
    ease_group = factor(ease_group, levels = c("Niedrig", "Mittel", "Hoch"))
  )


#===============================================================================
# 06 Post-level and day-level integrated data
#===============================================================================
# Daily-Posts werden mit Screening- und Outro-Merkmalen verbunden. Diese Ebene
# erlaubt Fragen nach Informationspfaden, Need Fit, Verarbeitung und Serendipität.

post_join_keep <- master %>%
  select(
    participant,
    intro_age_num, intro_intensity, n_platforms_weekly,
    intro_ib_undirected, intro_ib_thematic, intro_ib_social, intro_ib_problem,
    incidentality_index, reactivity_index, ease_index,
    age_z_adv, intensity_z_adv, repertoire_z_adv, reactivity_z_adv, ease_z_adv,
    reactivity_group, ease_group, need_profile_alignment
  )

daily_integrated <- daily_all %>%
  select(-any_of(setdiff(names(post_join_keep), "participant"))) %>%
  left_join(post_join_keep, by = "participant") %>%
  mutate(
    # Studientag variiert auf Post-Ebene; alle Personenmerkmale wurden bereits
    # auf Personenebene standardisiert.
    study_day_z = safe_z(study_day)
  )

daily_posts <- daily_integrated %>%
  filter(public_relevance == 1L, !is.na(topic_coded), !is.na(source_coded)) %>%
  group_by(participant) %>%
  mutate(
    # Zeitvariable Prädiktoren werden in stabile Between-Person-Anteile und
    # situative Within-Person-Abweichungen zerlegt. Das verhindert, dass
    # "Personen, die generell mehr incidental sehen" mit "Posts, die für eine
    # bestimmte Person incidental sind" vermischt werden.
    incidental_strict_between = safe_mean(incidental_strict),
    incidental_strict_within = incidental_strict - incidental_strict_between,
    post_need_fit_between = safe_mean(post_need_fit_z),
    post_need_fit_within = post_need_fit_z - post_need_fit_between
  ) %>%
  ungroup()

# Bedingte Serendipity-Effizienz wird direkt aus Postzählungen gebildet. Personen
# mit weniger als drei incidental Posts erhalten keinen Effizienzwert, weil ein
# Verhältnis mit sehr kleinem Nenner extrem instabil wäre.
serendipity_efficiency_person <- daily_posts %>%
  group_by(participant) %>%
  summarise(
    N_Incidental_Strict = sum(incidental_strict == 1L, na.rm = TRUE),
    N_Productive_Strict = sum(productive_serendipity_strict == 1L, na.rm = TRUE),
    Serendipity_Efficiency_Strict = if_else(
      N_Incidental_Strict >= 3,
      N_Productive_Strict / N_Incidental_Strict,
      NA_real_
    ),
    .groups = "drop"
  )

diversity_adjusted_person <- daily_posts %>%
  group_by(participant) %>%
  summarise(
    topic_evenness_adv = shannon_evenness(topic_coded),
    source_evenness_adv = shannon_evenness(source_coded),
    platform_evenness_adv = shannon_evenness(platform),
    .groups = "drop"
  )

# Rarefied Richness vergleicht Personen bei derselben Zahl zufällig gezogener
# beobachteter Posts. Dadurch wird die rein mechanische Kopplung von Richness und
# Uploadmenge reduziert. Es bleibt eine Kennzahl des hochgeladenen Samples.
rarefied_repertoire_person <- daily_posts %>%
  group_by(participant) %>%
  summarise(
    topic_richness_rarefied = rarefied_richness(topic_coded),
    source_richness_rarefied = rarefied_richness(source_coded),
    account_richness_rarefied = rarefied_richness(source_name_coded),
    .groups = "drop"
  )

master <- master %>%
  left_join(serendipity_efficiency_person, by = "participant") %>%
  left_join(diversity_adjusted_person, by = "participant") %>%
  left_join(rarefied_repertoire_person, by = "participant")

participants <- daily_person$participant

# WICHTIG: Das Screenshot-Level-RDS enthält nur Tage, an denen mindestens ein
# Screenshot vorliegt. Aus einem fehlenden Person-Tag kann hier nicht geschlossen
# werden, dass die Person den Daily-Fragebogen vollständig bearbeitet und exakt
# null relevante Inhalte gesehen/hochgeladen hat. Deshalb bleibt `n_uploads`
# an unbeobachteten Tagen NA; `day_has_upload` ist ein separater Coverage-Marker.
participant_day <- expand_grid(participant = participants, study_day = 1:7) %>%
  left_join(
    daily_integrated %>% count(participant, study_day, name = "n_uploads"),
    by = c("participant", "study_day")
  ) %>%
  left_join(
    daily_integrated %>%
      filter(!is.na(public_relevance)) %>%
      group_by(participant, study_day) %>%
      summarise(share_public = safe_mean(public_relevance), .groups = "drop"),
    by = c("participant", "study_day")
  ) %>%
  left_join(
    daily_posts %>%
      group_by(participant, study_day) %>%
      summarise(
        share_incidental_strict = safe_mean(incidental_strict),
        share_incidental_broad = safe_mean(incidental_broad),
        share_targeted = safe_mean(targeted_exposure),
        share_read = safe_mean(interaction_read),
        share_research = safe_mean(interaction_research),
        share_engaged = safe_mean(interaction_engagement),
        share_novel = safe_mean(any_repertoire_novelty),
        share_serendipity = safe_mean(productive_serendipity_strict),
        mean_need_fit = safe_mean(post_need_fit_z),
        .groups = "drop"
      ),
    by = c("participant", "study_day")
  ) %>%
  left_join(
    master %>% select(participant, reactivity_index, reactivity_group, ease_index),
    by = "participant"
  ) %>%
  mutate(
    day_has_upload = as.integer(!is.na(n_uploads))
  )

# Personenspezifische Zeitmarker werden hier neu und methodisch sauberer gebildet.
# Die alte Daily-Variable `upload_count_day_slope` behandelte nicht beobachtete
# Screenshot-Tage als Null-Uploads. Advanced Exploration trennt stattdessen:
#   (a) ob überhaupt ein Upload-Tag beobachtet wurde und
#   (b) wie viele Uploads an beobachteten Upload-Tagen vorlagen.
advanced_day_slopes <- participant_day %>%
  group_by(participant) %>%
  summarise(
    upload_day_presence_slope = {
      d <- tibble(x = study_day, y = day_has_upload) %>% drop_na()
      if (nrow(d) < 3 || n_distinct(d$y) < 2) {
        NA_real_
      } else {
        unname(coef(lm(y ~ x, data = d))[2])
      }
    },
    upload_intensity_active_day_slope = {
      d <- tibble(x = study_day, y = n_uploads) %>% drop_na()
      if (nrow(d) < 3 || n_distinct(d$y) < 2) {
        NA_real_
      } else {
        unname(coef(lm(y ~ x, data = d))[2])
      }
    },
    .groups = "drop"
  )

master <- master %>%
  left_join(advanced_day_slopes, by = "participant")


#===============================================================================
# 06A Selection, coverage and measurement-density diagnostics
#===============================================================================
# Diary- und Outro-Ausfälle können die integrierte Stichprobe selektiv machen.
# Deshalb werden Übergänge zwischen den Messstufen mit standardisierten
# Mittelwertdifferenzen beschrieben. Zusätzlich wird geprüft, ob scheinbar
# inhaltliche Kennwerte mechanisch mit der Zahl beobachteter Posts zusammenhängen.

screening_selection <- screening %>%
  mutate(In_Daily = as.integer(participant %in% daily_person$participant))

daily_selection <- master %>%
  mutate(In_Outro = as.integer(outro_available))

screening_attrition_specs <- tribble(
  ~Variable, ~Label,
  "intro_age_num", "Alter",
  "intro_intensity", "Nutzungsintensität",
  "n_platforms_weekly", "Wöchentliches Plattformrepertoire",
  "intro_ib_undirected", "Need: ungerichtet",
  "intro_ib_thematic", "Need: thematisch",
  "intro_ib_social", "Need: sozial",
  "intro_ib_problem", "Need: problembezogen",
  "incidentality_index", "Screening-Incidentality"
)

daily_attrition_specs <- tribble(
  ~Variable, ~Label,
  "intro_age_num", "Alter",
  "intro_intensity", "Nutzungsintensität",
  "n_screenshots", "Uploads",
  "n_active_days", "Aktive Diary-Tage",
  "n_public_content_posts", "Öffentlich relevante Posts",
  "share_incidental_strict", "Incidentality strikt",
  "share_read_thoroughly", "Gründliche Rezeption",
  "topic_shannon", "Themen-Diversität",
  "source_shannon", "Quellen-Diversität"
)

attrition_screening_daily <- pmap_dfr(
  screening_attrition_specs,
  function(Variable, Label) {
    if (!Variable %in% names(screening_selection)) return(tibble())
    x <- clean_numeric(screening_selection[[Variable]])
    g <- screening_selection$In_Daily
    tibble(
      Transition = "Screening → Diary",
      Measure = Label,
      N_Not_Retained = sum(!is.na(x) & g == 0L),
      N_Retained = sum(!is.na(x) & g == 1L),
      M_Not_Retained = safe_mean(x[g == 0L]),
      M_Retained = safe_mean(x[g == 1L]),
      SMD = standardized_mean_difference(x, g)
    )
  }
)

attrition_daily_outro <- pmap_dfr(
  daily_attrition_specs,
  function(Variable, Label) {
    if (!Variable %in% names(daily_selection)) return(tibble())
    x <- clean_numeric(daily_selection[[Variable]])
    g <- daily_selection$In_Outro
    tibble(
      Transition = "Diary → Outro",
      Measure = Label,
      N_Not_Retained = sum(!is.na(x) & g == 0L),
      N_Retained = sum(!is.na(x) & g == 1L),
      M_Not_Retained = safe_mean(x[g == 0L]),
      M_Retained = safe_mean(x[g == 1L]),
      SMD = standardized_mean_difference(x, g)
    )
  }
)

attrition_table <- bind_rows(attrition_screening_daily, attrition_daily_outro) %>%
  mutate(
    across(c(M_Not_Retained, M_Retained, SMD), ~ round(.x, 3)),
    Flag = case_when(
      is.na(SMD) ~ NA_character_,
      abs(SMD) >= .50 ~ "großer Unterschied",
      abs(SMD) >= .20 ~ "beachtenswert",
      TRUE ~ "klein"
    ),
    Note = "Deskriptive Selektionsdiagnostik; SMD statt Signifikanztest"
  )

coverage_summary <- master %>%
  summarise(
    Participants = n(),
    Median_Uploads = safe_median(n_screenshots),
    IQR_Uploads = IQR(n_screenshots, na.rm = TRUE),
    Median_Active_Days = safe_median(n_active_days),
    Mean_Active_Day_Coverage = safe_mean(n_active_days / 7),
    Percent_With_7_Active_Days = 100 * safe_mean(n_active_days >= 7),
    Median_Public_Posts = safe_median(n_public_content_posts),
    Min_Public_Posts = safe_min(n_public_content_posts),
    Max_Public_Posts = safe_max(n_public_content_posts)
  )

coverage_outcomes <- c(
  topic_richness = "Themen-Richness, roh",
  topic_richness_rarefied = paste0("Themen-Richness, rarefied n=", rarefaction_post_count),
  topic_shannon = "Themen-Shannon",
  source_richness = "Quellen-Richness, roh",
  source_richness_rarefied = paste0("Quellen-Richness, rarefied n=", rarefaction_post_count),
  source_shannon = "Quellen-Shannon",
  n_unique_account_names = "Unique Accounts, roh",
  account_richness_rarefied = paste0("Account-Richness, rarefied n=", rarefaction_post_count),
  share_topic_novelty = "Themenneuheit",
  share_productive_serendipity_strict = "Produktive Serendipität"
)

coverage_dependencies <- map_dfr(
  names(coverage_outcomes),
  ~ bootstrap_spearman(
    master,
    "n_public_content_posts", .x,
    "Zahl öffentlicher Posts", coverage_outcomes[[.x]],
    family = "Measurement density",
    reps = bootstrap_reps,
    seed = exploration_seed + 50
  )
) %>%
  mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH"))



#===============================================================================
# 07 Integrated sample table
#===============================================================================
# Diese Tabelle ist als kompakte Zusatz-/Appendix-Tabelle gedacht: zentrale
# Konstrukte aus allen drei Messzeitpunkten mit N, M und SD auf Personenebene.

sample_variables <- tribble(
  ~Stage, ~Variable, ~Label,
  "Screening", "intro_age_num", "Alter",
  "Screening", "intro_intensity", "Social-Media-Nutzungsintensität",
  "Screening", "n_platforms_weekly", "Wöchentlich genutzte Plattformen",
  "Screening", "intro_ib_undirected", "Ungerichtetes Informationsbedürfnis",
  "Screening", "intro_ib_thematic", "Thematisches Informationsbedürfnis",
  "Screening", "intro_ib_social", "Soziales Informationsbedürfnis",
  "Screening", "intro_ib_problem", "Problembezogenes Informationsbedürfnis",
  "Screening", "incidentality_index", "Screening-Incidentality",
  "Diary", "n_public_content_posts", "Öffentlich relevante Posts pro Person",
  "Diary QC", "share_publicly_relevant", "Anteil beurteilbarer Uploads, die Public-Relevance-Gate erfüllen",
  "Diary QC", "share_not_assessable_uploads", "Anteil nicht beurteilbarer Uploads",
  "Diary", "share_incidental_strict", "Incidentality, strikt",
  "Diary", "share_incidental_broad", "Incidentality, breit",
  "Diary", "share_read_thoroughly", "Gründlich gelesen/angeschaut",
  "Diary", "share_researched", "Weiter recherchiert",
  "Diary", "share_engaged", "Sichtbar interagiert",
  "Diary", "topic_shannon", "Themen-Diversität (Shannon)",
  "Diary", "topic_evenness_adv", "Themen-Evenness",
  "Diary", "topic_richness_rarefied", paste0("Themen-Richness rarefied (n=", rarefaction_post_count, ")"),
  "Diary", "source_shannon", "Quellen-Diversität (Shannon)",
  "Diary", "source_evenness_adv", "Quellen-Evenness",
  "Diary", "source_richness_rarefied", paste0("Quellen-Richness rarefied (n=", rarefaction_post_count, ")"),
  "Diary", "share_topic_novelty", "Diary-interne Themenneuheit",
  "Diary", "share_productive_serendipity_strict", "Produktive Serendipität, strikt",
  "Diary", "mean_post_need_fit_z", "Mittlerer Post–Need-Fit (z)",
  "Diary", "need_profile_alignment", "Need–Content-Profilalignment",
  "Outro", "reactivity_index", "Reaktivität",
  "Outro", "ease_index", "Ease of Use"
)

integrated_sample_table <- pmap_dfr(
  sample_variables,
  function(Stage, Variable, Label) {
    if (!Variable %in% names(master)) return(tibble())
    s <- mean_ci(master[[Variable]])
    tibble(
      Stage = Stage,
      Measure = Label,
      N = s$N,
      M = s$Mean,
      SD = s$SD,
      CI95_Low = s$CI_Low,
      CI95_High = s$CI_High
    )
  }
) %>%
  mutate(across(c(M, SD, CI95_Low, CI95_High), ~ round(.x, 3)))


#===============================================================================
# 08 Correlation atlas across all three levels
#===============================================================================
# Der Atlas ist bewusst breit und dient der Musterentdeckung. Er kommt primär als
# Grafik und in die Konsole; die Excel-Datei enthält stattdessen eine kuratierte
# theoriegeleitete Auswahl, damit sie publication-orientiert bleibt. Variablen,
# die bereits aus Screening+Diary gemeinsam konstruiert wurden (Need Fit, Alignment),
# werden aus diesem Vollkreuz ausgeschlossen, um mathematische Kopplung zu vermeiden.

screening_predictors <- c(
  intro_age_num = "Alter",
  intro_intensity = "Nutzungsintensität",
  n_platforms_weekly = "Plattformrepertoire (wöchentlich)",
  intro_ib_undirected = "Need: ungerichtet",
  intro_ib_thematic = "Need: thematisch",
  intro_ib_social = "Need: sozial",
  intro_ib_problem = "Need: problembezogen",
  incidentality_index = "Screening-Incidentality"
)

integrated_outcomes <- c(
  share_incidental_strict = "Incidentality strikt",
  share_incidental_broad = "Incidentality breit",
  share_targeted = "Gezielte Exposition",
  share_read_thoroughly = "Gründliche Rezeption",
  share_researched = "Weiterrecherche",
  share_engaged = "Engagement",
  processing_intensity = "Verarbeitungsintensität",
  topic_shannon = "Themen-Diversität",
  source_shannon = "Quellen-Diversität",
  platform_shannon = "Plattform-Diversität",
  share_current_affairs = "Aktuelles/Public Affairs",
  share_practical_service = "Praktische Info/Service",
  share_knowledge_interests = "Wissen/Interessen/Kultur",
  share_journalistic_sources = "Journalistische Quellen",
  share_peer_sources = "Peer-Quellen",
  share_topic_novelty = "Themenneuheit",
  share_productive_serendipity_strict = "Produktive Serendipität",
  # Cross-level Alignment-/Need-Fit-Marker werden hier bewusst ausgelassen:
  # sie enthalten selbst Screening-Information und würden im Vollkreuz teilweise
  # mathematische Kopplung/Selbstkorrelation erzeugen. Dafür gibt es dedizierte
  # theoriegeleitete Abschnitte weiter unten.
  reactivity_index = "Reaktivität",
  ease_index = "Ease of Use"
)

correlation_atlas <- crossing(
  Predictor = names(screening_predictors),
  Outcome = names(integrated_outcomes)
) %>%
  pmap_dfr(
    function(Predictor, Outcome) {
      safe_spearman(
        master,
        Predictor,
        Outcome,
        screening_predictors[[Predictor]],
        integrated_outcomes[[Outcome]],
        family = "Screening → Diary/Outro"
      ) %>%
        mutate(
          Predictor_Variable = Predictor,
          Outcome_Variable = Outcome,
          .before = 1
        )
    }
  ) %>%
  mutate(
    P_Adjusted_BH = p.adjust(P_Value, method = "BH")
  )

# Publication-Tabelle: nur vorab theoretisch sinnvolle Brücken statt des ganzen
# explorativen Atlas. Die Familienstruktur hält die BH-Korrektur nachvollziehbar.
theory_specs <- tribble(
  ~Family, ~X, ~Y, ~X_Label, ~Y_Label,
  "Incidentality calibration", "incidentality_index", "share_incidental_strict", "Screening-Incidentality", "Diary-Incidentality, strikt",
  "Incidentality calibration", "incidentality_index", "share_incidental_broad", "Screening-Incidentality", "Diary-Incidentality, breit",
  "Platform repertoire", "n_platforms_weekly", "platform_richness", "Wöchentl. Screening-Repertoire", "Diary-Plattform-Richness",
  "Platform repertoire", "n_platforms_weekly", "platform_shannon", "Wöchentl. Screening-Repertoire", "Diary-Plattform-Diversität",
  "Need → processing", "need_profile_alignment", "processing_intensity", "Need–Content-Alignment", "Verarbeitungsintensität",
  "Need → serendipity", "need_profile_alignment", "share_productive_serendipity_strict", "Need–Content-Alignment", "Produktive Serendipität",
  "Use → processing", "intro_intensity", "share_read_thoroughly", "Nutzungsintensität", "Gründliche Rezeption",
  "Use → processing", "intro_intensity", "share_researched", "Nutzungsintensität", "Weiterrecherche",
  "Reactivity", "reactivity_index", "upload_day_presence_slope", "Reaktivität", "Tagestrend: beobachteter Upload-Tag",
  "Reactivity", "reactivity_index", "upload_intensity_active_day_slope", "Reaktivität", "Tagestrend: Uploadintensität an aktiven Tagen",
  "Reactivity", "reactivity_index", "targeted_post_day_slope", "Reaktivität", "Tagestrend gezielter Exposition",
  "Reactivity", "reactivity_index", "thorough_reading_day_slope", "Reaktivität", "Tagestrend gründlicher Rezeption",
  "Reactivity", "reactivity_index", "absolute_incidentality_gap_broad", "Reaktivität", "Absoluter Incidentality-Gap",
  "Ease", "ease_index", "n_screenshots", "Ease of Use", "Anzahl Uploads",
  "Ease", "ease_index", "n_active_days", "Ease of Use", "Aktive Diary-Tage"
)

theory_correlations <- pmap_dfr(
  theory_specs,
  function(Family, X, Y, X_Label, Y_Label) {
    estimate <- bootstrap_spearman(
      master, X, Y, X_Label, Y_Label, Family,
      reps = bootstrap_reps, seed = exploration_seed + 100
    )
    influence <- loo_spearman_summary(master, X, Y)
    bind_cols(estimate, influence)
  }
) %>%
  group_by(Family) %>%
  mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH")) %>%
  ungroup() %>%
  mutate(
    across(c(Spearman_Rho, CI95_Low, CI95_High, P_Value, P_Adjusted_BH,
             LOO_Min, LOO_Max, LOO_Median), ~ round(.x, 4)),
    Note = "Explorativ; Spearman; Personen-Bootstrap-CI; LOO-Einflusscheck; BH innerhalb der Familie"
  )


#===============================================================================
# 09 Need–content correspondence: raw shares + compositional primary analysis
#===============================================================================
# Das theoretische Mapping bleibt bewusst sparsam:
#   ungerichtet     ↔ Aktuelles / öffentliche Angelegenheiten
#   thematisch      ↔ Wissen / Interessen / Kultur
#   sozial          ↔ Peer-Quellen
#   problembezogen  ↔ praktische Information / Service
#
# Da Topic- und Source-Anteile jeweils zu einer Komposition gehören, sind einzelne
# Share-Korrelationen durch die Summenkonstante gekoppelt. Primär verwenden wir
# deshalb CLR-Koordinaten innerhalb der jeweiligen Topic-/Source-Komposition.
# Raw-share-Korrelationen bleiben als transparente Sensitivitätsanalyse erhalten.

need_content_specs <- tribble(
  ~Need, ~Need_Variable, ~Diary_Variable, ~Diary_Domain, ~Composition,
  "Ungerichtet", "intro_ib_undirected", "share_current_affairs", "Aktuelles & öffentliche Angelegenheiten", "Topic",
  "Thematisch", "intro_ib_thematic", "share_knowledge_interests", "Wissen, Interessen & Kultur", "Topic",
  "Sozial", "intro_ib_social", "share_peer_sources", "Private Person / Peer", "Source",
  "Problembezogen", "intro_ib_problem", "share_practical_service", "Praktische Information & Service", "Topic"
)

# Raw shares: nur Sensitivität/Anschaulichkeit ---------------------------------
need_content_matched_raw <- pmap_dfr(
  need_content_specs,
  function(Need, Need_Variable, Diary_Variable, Diary_Domain, Composition) {
    estimate <- bootstrap_spearman(
      master,
      Need_Variable,
      Diary_Variable,
      Need,
      Diary_Domain,
      family = "Matched Need–Content: raw share",
      reps = bootstrap_reps,
      seed = exploration_seed + 200
    )
    influence <- loo_spearman_summary(master, Need_Variable, Diary_Variable)
    
    bind_cols(estimate, influence) %>%
      mutate(
        Need = Need,
        Screening_Variable = Need_Variable,
        Diary_Variable = Diary_Variable,
        Composition = Composition,
        Estimand = "Raw share correlation",
        .before = 1
      )
  }
) %>%
  mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH"))

# Kompositionale Primäranalyse -------------------------------------------------
topic_composition <- make_clr_composition(
  daily_posts,
  "topic_macro",
  min_total = minimum_composition_posts,
  pseudocount = .5
)

source_composition <- make_clr_composition(
  daily_posts,
  "source_macro",
  min_total = minimum_composition_posts,
  pseudocount = .5
)

master_coda <- master %>%
  left_join(topic_composition$data, by = "participant") %>%
  left_join(source_composition$data, by = "participant", suffix = c("", "_source"))

# Falls identische bereinigte Kategorienamen zwischen Topic und Source auftreten,
# wird anhand der Mapping-Tabelle die tatsächlich erzeugte Variable aufgelöst.
resolve_clr <- function(composition, label, composition_name) {
  v <- get_clr_variable(composition, label)
  if (is.na(v)) return(NA_character_)
  if (composition_name == "Source" && v %in% names(topic_composition$data)) {
    paste0(v, "_source")
  } else {
    v
  }
}

# Für die optionale Personen-Typologie werden kompositionale Inhaltsanteile nicht
# als mehrere rohe Shares eingespeist. Stattdessen werden wenige explizite CLR-
# Koordinaten verwendet; so dominieren Summenkonstante und Beobachtungsdichte die
# Clusterlösung weniger stark.
profile_coda_specs <- tribble(
  ~New_Variable, ~Composition, ~Category,
  "topic_clr_current_affairs", "Topic", "Aktuelles & öffentliche Angelegenheiten",
  "topic_clr_practical_service", "Topic", "Praktische Information & Service",
  "topic_clr_knowledge_interests", "Topic", "Wissen, Interessen & Kultur",
  "source_clr_journalistic", "Source", "Journalistische Medien",
  "source_clr_peer", "Source", "Private Person / Peer"
)

profile_coda_coordinates <- tibble(participant = master_coda$participant)
for (i in seq_len(nrow(profile_coda_specs))) {
  spec <- profile_coda_specs[i, ]
  obj <- if (spec$Composition[[1]] == "Topic") topic_composition else source_composition
  source_var <- resolve_clr(obj, spec$Category[[1]], spec$Composition[[1]])
  profile_coda_coordinates[[spec$New_Variable[[1]]]] <- if (
    !is.na(source_var) && source_var %in% names(master_coda)
  ) {
    clean_numeric(master_coda[[source_var]])
  } else {
    NA_real_
  }
}

master <- master %>%
  left_join(profile_coda_coordinates, by = "participant")

need_content_coda_specs <- need_content_specs %>%
  mutate(
    CLR_Variable = pmap_chr(
      list(Diary_Domain, Composition),
      function(Diary_Domain, Composition) {
        if (Composition == "Topic") {
          resolve_clr(topic_composition, Diary_Domain, "Topic")
        } else {
          resolve_clr(source_composition, Diary_Domain, "Source")
        }
      }
    )
  )

need_content_matched_coda <- pmap_dfr(
  need_content_coda_specs,
  function(Need, Need_Variable, Diary_Variable, Diary_Domain, Composition, CLR_Variable) {
    if (is.na(CLR_Variable) || !CLR_Variable %in% names(master_coda)) {
      return(tibble(
        Need = Need,
        Screening_Variable = Need_Variable,
        Diary_Variable = Diary_Variable,
        Composition = Composition,
        Estimand = "CLR log-ratio correlation",
        Family = "Matched Need–Content: CLR",
        Predictor = Need,
        Outcome = Diary_Domain,
        N = 0L,
        Spearman_Rho = NA_real_, CI95_Low = NA_real_, CI95_High = NA_real_,
        P_Value = NA_real_, LOO_Min = NA_real_, LOO_Max = NA_real_,
        LOO_Median = NA_real_, LOO_Sign_Stable = NA
      ))
    }
    
    estimate <- bootstrap_spearman(
      master_coda,
      Need_Variable,
      CLR_Variable,
      Need,
      paste0(Diary_Domain, " (CLR)"),
      family = "Matched Need–Content: CLR",
      reps = bootstrap_reps,
      seed = exploration_seed + 300
    )
    influence <- loo_spearman_summary(master_coda, Need_Variable, CLR_Variable)
    
    bind_cols(estimate, influence) %>%
      mutate(
        Need = Need,
        Screening_Variable = Need_Variable,
        Diary_Variable = Diary_Variable,
        Composition = Composition,
        Estimand = "CLR log-ratio correlation",
        .before = 1
      )
  }
) %>%
  mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH"))

# Diese vier CLR-Beziehungen sind die bevorzugte theoriegeleitete Exploration.
# Raw shares werden daneben gezeigt, damit die Robustheit gegenüber der
# Kompositionsbehandlung unmittelbar sichtbar ist.
need_content_matched <- need_content_matched_coda

# Breite 4×4-Matrix bleibt reine Discovery-Grafik auf Raw Shares. Sie darf nicht
# als Sammlung unabhängiger Effekte interpretiert werden.
need_content_matrix <- crossing(
  Need_Variable = need_content_specs$Need_Variable,
  Diary_Variable = need_content_specs$Diary_Variable
) %>%
  pmap_dfr(
    function(Need_Variable, Diary_Variable) {
      safe_spearman(
        master,
        Need_Variable,
        Diary_Variable,
        need_content_specs$Need[match(Need_Variable, need_content_specs$Need_Variable)],
        need_content_specs$Diary_Domain[match(Diary_Variable, need_content_specs$Diary_Variable)],
        family = "Need–Content raw-share matrix"
      ) %>%
        mutate(
          Need_Variable = Need_Variable,
          Diary_Variable = Diary_Variable,
          Matched = match(Need_Variable, need_content_specs$Need_Variable) ==
            match(Diary_Variable, need_content_specs$Diary_Variable),
          .before = 1
        )
    }
  ) %>%
  mutate(
    P_Adjusted_BH = p.adjust(P_Value, method = "BH"),
    Note = "Discovery only; raw shares sind kompositional gekoppelt"
  )

# Das Profilalignment über vier theoretisch zugeordnete Dimensionen ist ein
# anschaulicher Personenmarker, aber kein validierter Skalenwert. Wegen nur vier
# Profilpunkten wird es nicht als primäre theoriegeleitete Beziehung behandelt.
need_alignment_summary <- bind_rows(
  mean_ci(master$need_profile_alignment) %>%
    transmute(
      Metric = "Need–Content-Profilalignment (4-Domänen-Proxy)",
      N, M = Mean, SD, CI95_Low = CI_Low, CI95_High = CI_High
    ),
  tibble(
    Metric = "Match dominantes Screening-Bedürfnis ↔ beobachtete Domäne",
    N = sum(!is.na(master$dominant_need_match)),
    M = safe_mean(master$dominant_need_match),
    SD = safe_sd(master$dominant_need_match),
    CI95_Low = NA_real_,
    CI95_High = NA_real_
  )
)

need_content_publication <- bind_rows(
  need_content_matched_coda %>%
    transmute(
      Section = "Primary compositional",
      Measure = Need,
      Outcome,
      N,
      Estimate = Spearman_Rho,
      CI95_Low,
      CI95_High,
      P_Value,
      P_Adjusted_BH,
      LOO_Min, LOO_Max, LOO_Sign_Stable,
      Note = "Spearman mit CLR-Koordinate; Personen-Bootstrap-CI; LOO-Einflusscheck"
    ),
  need_content_matched_raw %>%
    transmute(
      Section = "Sensitivity raw share",
      Measure = Need,
      Outcome,
      N,
      Estimate = Spearman_Rho,
      CI95_Low,
      CI95_High,
      P_Value,
      P_Adjusted_BH,
      LOO_Min, LOO_Max, LOO_Sign_Stable,
      Note = "Raw share; kompositionale Kopplung beachten; LOO-Einflusscheck"
    ),
  need_alignment_summary %>%
    transmute(
      Section = "Profile proxy",
      Measure = Metric,
      Outcome = NA_character_,
      N,
      Estimate = M,
      CI95_Low,
      CI95_High,
      P_Value = NA_real_,
      P_Adjusted_BH = NA_real_,
      LOO_Min = NA_real_, LOO_Max = NA_real_, LOO_Sign_Stable = NA,
      Note = if_else(
        str_detect(Metric, "Match"),
        "Estimate = Anteil; rein deskriptiv",
        "Estimate = mittleres 4-Domänen-Profilalignment; explorativer Proxy"
      )
    )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))


#===============================================================================
# 10 Incidentality, novelty and productive serendipity
#===============================================================================
# Produktive Serendipität wird als kumulativer Prozess operationalisiert:
# ungeplante Exposition → plus diary-interne Neuheit → plus sinnvolle Verarbeitung.
# Die Funnel-Raten beziehen sich deshalb immer auf denselben Nenner (alle
# öffentlich relevanten Posts). Zusätzlich werden bedingte Übergangsraten separat
# ausgewiesen. So entsteht kein scheinbarer Funnel aus wechselnden Nennern.

make_serendipity_path <- function(data, incidental_var, productive_var, definition) {
  inc <- clean_numeric(data[[incidental_var]])
  novelty <- clean_numeric(data$any_repertoire_novelty)
  processing <- clean_numeric(data$processed_meaningfully)
  productive <- clean_numeric(data[[productive_var]])
  
  cumulative <- tibble(
    Definition = definition,
    Metric_Type = "Cumulative share of all public posts",
    Step = factor(
      c("Incidentality", "+ Neuheit", "+ sinnvolle Verarbeitung"),
      levels = c("Incidentality", "+ Neuheit", "+ sinnvolle Verarbeitung")
    ),
    Rate = c(
      safe_mean(inc),
      safe_mean(as.integer(inc == 1L & novelty == 1L)),
      safe_mean(productive)
    )
  )
  
  conditional <- tibble(
    Definition = definition,
    Transition = c("Neuheit | Incidentality", "Verarbeitung | Incidentality + Neuheit"),
    Rate = c(
      safe_mean(if_else(inc == 1L, novelty, NA_real_)),
      safe_mean(if_else(inc == 1L & novelty == 1L, processing, NA_real_))
    )
  )
  
  list(cumulative = cumulative, conditional = conditional)
}

ser_strict <- make_serendipity_path(
  daily_posts,
  "incidental_strict",
  "productive_serendipity_strict",
  "Strikt"
)

ser_broad <- make_serendipity_path(
  daily_posts,
  "incidental_broad",
  "productive_serendipity_broad",
  "Breit"
)

serendipity_funnel <- bind_rows(ser_strict$cumulative, ser_broad$cumulative) %>%
  mutate(Percent = 100 * Rate)

serendipity_transitions <- bind_rows(ser_strict$conditional, ser_broad$conditional) %>%
  mutate(Percent = 100 * Rate)

# Für Gruppenvergleiche werden zunächst personenspezifische Raten innerhalb der
# jeweiligen Kategorie gebildet und erst danach über Personen gemittelt. Dadurch
# dominiert eine Person mit sehr vielen Uploads nicht den Gruppenmittelwert.
participant_weighted_serendipity <- function(data, group_var, dimension_label) {
  d <- data %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(participant, Category = as.character(.data[[group_var]])) %>%
    summarise(
      N_Posts_Person = n(),
      Incidental_Strict = safe_mean(incidental_strict),
      Novelty = safe_mean(any_repertoire_novelty),
      Meaningful_Processing = safe_mean(processed_meaningfully),
      Productive_Serendipity = safe_mean(productive_serendipity_strict),
      .groups = "drop"
    )
  
  d %>%
    group_by(Category) %>%
    summarise(
      Dimension = dimension_label,
      N_Participants = n_distinct(participant),
      N_Posts = sum(N_Posts_Person),
      Incidental_Strict = safe_mean(Incidental_Strict),
      Novelty = safe_mean(Novelty),
      Meaningful_Processing = safe_mean(Meaningful_Processing),
      Productive_Serendipity = safe_mean(Productive_Serendipity),
      Productive_SD = safe_sd(Productive_Serendipity),
      Productive_CI_Low = if_else(
        N_Participants > 1,
        Productive_Serendipity - qt(.975, N_Participants - 1) * Productive_SD / sqrt(N_Participants),
        NA_real_
      ),
      Productive_CI_High = if_else(
        N_Participants > 1,
        Productive_Serendipity + qt(.975, N_Participants - 1) * Productive_SD / sqrt(N_Participants),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    select(Dimension, Category, everything())
}

serendipity_by_platform <- participant_weighted_serendipity(
  daily_posts, "platform", "Plattform"
)

serendipity_by_topic <- participant_weighted_serendipity(
  daily_posts, "topic_macro", "Themenfamilie"
)

serendipity_by_source <- participant_weighted_serendipity(
  daily_posts, "source_macro", "Quellenfamilie"
)

serendipity_publication <- bind_rows(
  serendipity_by_platform,
  serendipity_by_topic,
  serendipity_by_source
) %>%
  mutate(
    across(
      c(Incidental_Strict, Novelty, Meaningful_Processing, Productive_Serendipity,
        Productive_SD, Productive_CI_Low, Productive_CI_High),
      ~ round(100 * .x, 1)
    ),
    Note = "Teilnehmergewichtet: erst Rate pro Person/Kategorie, dann Mittel über Personen"
  )

# Postgewichtete Werte bleiben nur als Vergleich, falls die Verteilung der
# tatsächlich hochgeladenen Posts selbst die Forschungsfrage ist.
serendipity_postweighted <- daily_posts %>%
  group_by(platform) %>%
  summarise(
    N_Posts = n(),
    Incidental_Strict = safe_mean(incidental_strict),
    Novelty = safe_mean(any_repertoire_novelty),
    Meaningful_Processing = safe_mean(processed_meaningfully),
    Productive_Serendipity = safe_mean(productive_serendipity_strict),
    .groups = "drop"
  )


#===============================================================================
# 11 Calibration and method reactivity
#===============================================================================
# Hier werden verschiedene Arten der Übereinstimmung bewusst getrennt betrachtet:
# Incidentality, Plattformprofil und Nutzungskontext. Reaktivität/Ease werden als
# methodische Meta-Ebene mit diesen Mustern verbunden.

calibration_measures <- tribble(
  ~Measure, ~Variable,
  "|Gap| Incidentality breit", "absolute_incidentality_gap_broad",
  "|Gap| Incidentality strikt", "absolute_incidentality_gap_strict",
  "Plattformprofil-Alignment", "platform_profile_alignment",
  "Räumlicher Kontext-Alignment", "local_context_alignment",
  "Sozialer Kontext-Alignment", "social_context_alignment",
  "Need–Content-Alignment", "need_profile_alignment"
)

calibration_summary <- pmap_dfr(
  calibration_measures,
  function(Measure, Variable) {
    s <- mean_ci(master[[Variable]])
    tibble(
      Measure = Measure,
      N = s$N,
      M = s$Mean,
      SD = s$SD,
      CI95_Low = s$CI_Low,
      CI95_High = s$CI_High
    )
  }
)

reactivity_specs <- tribble(
  ~Family, ~X, ~Y, ~X_Label, ~Y_Label,
  "Reactivity × calibration", "reactivity_index", "absolute_incidentality_gap_broad", "Reaktivität", "|Incidentality-Gap| breit",
  "Reactivity × calibration", "reactivity_index", "platform_profile_alignment", "Reaktivität", "Plattformprofil-Alignment",
  "Reactivity × calibration", "reactivity_index", "context_alignment_mean", "Reaktivität", "Kontext-Alignment",
  "Reactivity × day trend", "reactivity_index", "upload_day_presence_slope", "Reaktivität", "Trend: beobachteter Upload-Tag",
  "Reactivity × day trend", "reactivity_index", "upload_intensity_active_day_slope", "Reaktivität", "Trend: Uploadintensität an aktiven Tagen",
  "Reactivity × method/QC trend", "reactivity_index", "public_relevance_day_slope", "Reaktivität", "Trend: Public-Relevance-Gate erfüllt (QC)",
  "Reactivity × day trend", "reactivity_index", "targeted_post_day_slope", "Reaktivität", "Targeted-Trend",
  "Reactivity × day trend", "reactivity_index", "thorough_reading_day_slope", "Reaktivität", "Reading-Trend",
  "Ease × participation", "ease_index", "n_screenshots", "Ease of Use", "Uploads",
  "Ease × participation", "ease_index", "n_active_days", "Ease of Use", "Aktive Tage",
  "Ease × calibration", "ease_index", "platform_profile_alignment", "Ease of Use", "Plattformprofil-Alignment",
  "Scale relation", "reactivity_index", "ease_index", "Reaktivität", "Ease of Use"
)

reactivity_associations <- pmap_dfr(
  reactivity_specs,
  function(Family, X, Y, X_Label, Y_Label) {
    estimate <- bootstrap_spearman(
      master, X, Y, X_Label, Y_Label, Family,
      reps = bootstrap_reps, seed = exploration_seed + 400
    )
    bind_cols(estimate, loo_spearman_summary(master, X, Y))
  }
) %>%
  group_by(Family) %>%
  mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH")) %>%
  ungroup()

# Item-spezifische Reaktivitätsbrücken werden nur dort gerechnet, wo der
# Iteminhalt einen plausiblen Diary-Gegenpart besitzt. Die beiden Feed-bezogenen
# Items erhalten bewusst keinen pseudo-objektiven "Validationstest", weil die
# Studie keine vollständige Feed-Exposition beobachtet.
reactivity_item_specs <- tribble(
  ~Family, ~X, ~Y, ~X_Label, ~Y_Label,
  "Specific reactivity item", "reactivity_upload_search_item", "share_targeted",
  "Gezielt nach Uploads gesucht (Item 1)", "Anteil gezielter Diary-Posts",
  "Specific reactivity item", "reactivity_upload_search_item", "targeted_post_day_slope",
  "Gezielt nach Uploads gesucht (Item 1)", "Tagestrend gezielter Exposition",
  "Specific reactivity item", "reactivity_usage_change_item", "upload_day_presence_slope",
  "Eigene Nutzung verändert (Item 3, invertiert)", "Trend: beobachteter Upload-Tag",
  "Specific reactivity item", "reactivity_usage_change_item", "upload_intensity_active_day_slope",
  "Eigene Nutzung verändert (Item 3, invertiert)", "Trend: Uploadintensität an aktiven Tagen",
  "Specific reactivity item", "reactivity_low_representativeness_item", "absolute_incidentality_gap_broad",
  "Uploads weniger repräsentativ (Item 5, invertiert)", "|Incidentality-Gap| breit",
  "Specific reactivity item", "reactivity_low_representativeness_item", "platform_profile_alignment",
  "Uploads weniger repräsentativ (Item 5, invertiert)", "Plattformprofil-Alignment"
)

reactivity_item_bridges <- pmap_dfr(
  reactivity_item_specs,
  function(Family, X, Y, X_Label, Y_Label) {
    estimate <- bootstrap_spearman(
      master, X, Y, X_Label, Y_Label, Family,
      reps = bootstrap_reps, seed = exploration_seed + 450
    )
    bind_cols(estimate, loo_spearman_summary(master, X, Y))
  }
) %>%
  mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH"))

calibration_publication <- bind_rows(
  calibration_summary %>%
    transmute(
      Section = "Deskriptiv", Measure, Outcome = NA_character_, N,
      Estimate = M, SD, CI95_Low, CI95_High,
      P_Value = NA_real_, P_Adjusted_BH = NA_real_,
      LOO_Min = NA_real_, LOO_Max = NA_real_, LOO_Sign_Stable = NA
    ),
  reactivity_associations %>%
    transmute(
      Section = Family, Measure = Predictor, Outcome, N,
      Estimate = Spearman_Rho, SD = NA_real_, CI95_Low, CI95_High,
      P_Value, P_Adjusted_BH, LOO_Min, LOO_Max, LOO_Sign_Stable
    ),
  reactivity_item_bridges %>%
    transmute(
      Section = Family, Measure = Predictor, Outcome, N,
      Estimate = Spearman_Rho, SD = NA_real_, CI95_Low, CI95_High,
      P_Value, P_Adjusted_BH, LOO_Min, LOO_Max, LOO_Sign_Stable
    )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))


#===============================================================================
# 12 Content ecology: platform, topic, source and format fingerprints
#===============================================================================
# Standardisierte Chi²-Residuals zeigen nicht nur absolute Häufigkeiten, sondern
# welche Kombinationen häufiger/seltener auftreten als bei Unabhängigkeit erwartet.
# Das eignet sich gut zur Exploration von Plattform-Affordances und Quellenlogiken.

platform_topic_residuals <- chisq_residual_table(
  daily_posts, "platform", "topic_macro", "Platform", "Topic"
)

platform_source_residuals <- chisq_residual_table(
  daily_posts, "platform", "source_macro", "Platform", "Source"
)

topic_source_residuals <- chisq_residual_table(
  daily_posts, "topic_macro", "source_macro", "Topic", "Source"
)

platform_format <- daily_posts %>%
  filter(!is.na(platform), !is.na(media_format)) %>%
  count(platform, media_format, name = "N") %>%
  group_by(platform) %>%
  mutate(Percent = 100 * N / sum(N)) %>%
  ungroup()

processing_by_incidentality_person <- daily_posts %>%
  filter(!is.na(incidentality)) %>%
  group_by(participant, incidentality) %>%
  summarise(
    N_Posts_Person = n(),
    Thorough_Read = safe_mean(interaction_read),
    Research = safe_mean(interaction_research),
    Engagement = safe_mean(interaction_engagement),
    Any_Processing = safe_mean(processed_meaningfully),
    Need_Fit = safe_mean(post_need_fit_z),
    Novelty = safe_mean(any_repertoire_novelty),
    .groups = "drop"
  )

processing_by_incidentality <- processing_by_incidentality_person %>%
  group_by(incidentality) %>%
  summarise(
    N_Participants = n_distinct(participant),
    N_Posts = sum(N_Posts_Person),
    Thorough_Read = safe_mean(Thorough_Read),
    Research = safe_mean(Research),
    Engagement = safe_mean(Engagement),
    Any_Processing = safe_mean(Any_Processing),
    Need_Fit = safe_mean(Need_Fit),
    Novelty = safe_mean(Novelty),
    .groups = "drop"
  )


#===============================================================================
# 12A Within-person platform differentiation
#===============================================================================
# Globale Plattformunterschiede können schlicht widerspiegeln, dass andere
# Personen Facebook als TikTok/X/Instagram nutzen. Deshalb wird zusätzlich nur
# innerhalb derselben Multi-Plattform-Person verglichen. Für Inhaltsprofile wird
# Jensen–Shannon-Divergenz (0 = identisch, 1 = maximal verschieden) genutzt;
# Verhaltensmarker werden als gepaarte Plattformdifferenzen berechnet.
#
# Voraussetzung: pro Person und verglichener Plattform mindestens
# `minimum_posts_per_platform_within` öffentlich relevante Posts. Die Ergebnisse
# gelten deshalb ausdrücklich nur für ausreichend beobachtete Multi-Plattform-
# Nutzende und sind keine populationsweiten Plattformkausaleffekte.

platform_order <- c("Facebook", "Instagram", "TikTok", "X")
platform_pairs <- combn(platform_order, 2, simplify = FALSE)

platform_profile_dimensions <- c(
  topic_macro = "Themenfamilie",
  source_macro = "Quellenfamilie",
  media_format = "Format"
)

platform_complementarity_person <- map_dfr(
  names(platform_profile_dimensions),
  function(dimension_var) {
    category_values <- as.character(daily_posts[[dimension_var]])
    categories <- sort(unique(category_values[!is.na(category_values)]))
    
    if (length(categories) < 2) return(tibble())
    
    map_dfr(platform_pairs, function(pair) {
      pair_data <- daily_posts %>%
        filter(
          platform %in% pair,
          !is.na(.data[[dimension_var]])
        )
      
      eligible_ids <- pair_data %>%
        count(participant, platform, name = "N_Posts_Platform") %>%
        filter(N_Posts_Platform >= minimum_posts_per_platform_within) %>%
        count(participant, name = "N_Eligible_Platforms") %>%
        filter(N_Eligible_Platforms == 2) %>%
        pull(participant)
      
      map_dfr(eligible_ids, function(id) {
        d <- pair_data %>% filter(participant == id)
        p <- table(factor(
          as.character(d[[dimension_var]][d$platform == pair[[1]]]),
          levels = categories
        ))
        q <- table(factor(
          as.character(d[[dimension_var]][d$platform == pair[[2]]]),
          levels = categories
        ))
        
        tibble(
          participant = id,
          Dimension = platform_profile_dimensions[[dimension_var]],
          Platform_1 = pair[[1]],
          Platform_2 = pair[[2]],
          Pair = paste(pair, collapse = " ↔ "),
          JSD = jensen_shannon_divergence(as.numeric(p), as.numeric(q))
        )
      })
    })
  }
)

platform_complementarity_summary <- if (nrow(platform_complementarity_person) > 0) {
  platform_complementarity_person %>%
    group_by(Dimension, Platform_1, Platform_2, Pair) %>%
    group_modify(~ {
      ci <- bootstrap_mean_ci(
        .x$JSD,
        reps = bootstrap_reps,
        seed = exploration_seed + 1200 + sum(utf8ToInt(.y$Pair[[1]]))
      )
      tibble(
        N_Participants = ci$N,
        Mean_JSD = ci$Mean,
        CI95_Low = ci$CI95_Low,
        CI95_High = ci$CI95_High
      )
    }) %>%
    ungroup()
} else {
  tibble(
    Dimension = character(), Platform_1 = character(), Platform_2 = character(),
    Pair = character(), N_Participants = integer(), Mean_JSD = double(),
    CI95_Low = double(), CI95_High = double()
  )
}

# Gepaarte Verhaltensprofile: erst Rate pro Person × Plattform, dann Differenz
# innerhalb derselben Person. Positive Werte bedeuten Plattform 2 > Plattform 1.
platform_behavior_person <- daily_posts %>%
  filter(!is.na(platform)) %>%
  group_by(participant, platform) %>%
  summarise(
    N_Posts_Platform = n(),
    Incidentality_Strict = safe_mean(incidental_strict),
    Thorough_Read = safe_mean(interaction_read),
    Further_Research = safe_mean(interaction_research),
    Productive_Serendipity = safe_mean(productive_serendipity_strict),
    .groups = "drop"
  )

platform_behavior_metrics <- c(
  Incidentality_Strict = "Incidentality strikt",
  Thorough_Read = "Gründliche Rezeption",
  Further_Research = "Weiterrecherche",
  Productive_Serendipity = "Produktive Serendipität"
)

platform_behavior_differences_person <- map_dfr(platform_pairs, function(pair) {
  d <- platform_behavior_person %>%
    filter(
      platform %in% pair,
      N_Posts_Platform >= minimum_posts_per_platform_within
    ) %>%
    select(participant, platform, all_of(names(platform_behavior_metrics))) %>%
    pivot_wider(
      names_from = platform,
      values_from = all_of(names(platform_behavior_metrics)),
      names_sep = "__"
    )
  
  if (nrow(d) == 0) return(tibble())
  
  map_dfr(names(platform_behavior_metrics), function(metric) {
    v1 <- paste0(metric, "__", pair[[1]])
    v2 <- paste0(metric, "__", pair[[2]])
    if (!all(c(v1, v2) %in% names(d))) return(tibble())
    
    tibble(
      participant = d$participant,
      Outcome = platform_behavior_metrics[[metric]],
      Platform_1 = pair[[1]],
      Platform_2 = pair[[2]],
      Pair = paste(pair, collapse = " ↔ "),
      Difference = clean_numeric(d[[v2]]) - clean_numeric(d[[v1]])
    ) %>%
      filter(!is.na(Difference))
  })
})

platform_behavior_differences_summary <- if (nrow(platform_behavior_differences_person) > 0) {
  platform_behavior_differences_person %>%
    group_by(Outcome, Platform_1, Platform_2, Pair) %>%
    group_modify(~ {
      ci <- bootstrap_mean_ci(
        .x$Difference,
        reps = bootstrap_reps,
        seed = exploration_seed + 1300 + sum(utf8ToInt(paste0(.y$Pair[[1]], .y$Outcome[[1]])))
      )
      tibble(
        N_Participants = ci$N,
        Mean_Difference = ci$Mean,
        CI95_Low = ci$CI95_Low,
        CI95_High = ci$CI95_High
      )
    }) %>%
    ungroup() %>%
    mutate(Direction = paste0(Platform_2, " minus ", Platform_1))
} else {
  tibble(
    Outcome = character(), Platform_1 = character(), Platform_2 = character(),
    Pair = character(), N_Participants = integer(), Mean_Difference = double(),
    CI95_Low = double(), CI95_High = double(), Direction = character()
  )
}

platform_within_publication <- bind_rows(
  platform_complementarity_summary %>%
    transmute(
      Section = "Within-person content complementarity",
      Measure = Dimension,
      Comparison = Pair,
      Direction = NA_character_,
      N_Participants,
      Estimate = Mean_JSD,
      CI95_Low,
      CI95_High,
      Note = paste0(
        "Jensen–Shannon-Divergenz (0–1); ≥",
        minimum_posts_per_platform_within,
        " öffentliche Posts pro Plattform/Person"
      )
    ),
  platform_behavior_differences_summary %>%
    transmute(
      Section = "Within-person behavior difference",
      Measure = Outcome,
      Comparison = Pair,
      Direction,
      N_Participants,
      Estimate = Mean_Difference,
      CI95_Low,
      CI95_High,
      Note = paste0(
        "Gepaarte Rate; Estimate = Plattform 2 minus Plattform 1; ≥",
        minimum_posts_per_platform_within,
        " öffentliche Posts pro Plattform/Person"
      )
    )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))


#===============================================================================
# 13 Temporal trajectories and possible diary reactivity
#===============================================================================
# Tagesverläufe werden zunächst über Personen gemittelt. Da das Screenshot-RDS
# unbeobachtete Tage nicht von echten Null-Upload-Tagen unterscheiden kann, bleibt
# `n_uploads` dort NA. `Upload-Tag beobachtet` zeigt separat, welcher Anteil der
# Personen an einem Studientag mindestens einen Screenshot beigetragen hat.
# Inhaltliche Tagesmittel konditionieren damit auf beobachtete Upload-Tage.
# Das ist ein Verlaufssignal, kein Nachweis eines kausalen Reaktivitätseffekts.

day_metrics_long <- participant_day %>%
  select(
    participant, study_day, day_has_upload, n_uploads, share_public, share_incidental_strict,
    share_targeted, share_read, share_research, share_engaged,
    share_novel, share_serendipity, mean_need_fit
  ) %>%
  pivot_longer(
    cols = -c(participant, study_day),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = recode(
      Metric,
      day_has_upload = "Upload-Tag beobachtet",
      n_uploads = "Uploads je beobachtetem Upload-Tag",
      share_public = "Public-Relevance-Gate erfüllt (QC)",
      share_incidental_strict = "Incidentality strikt",
      share_targeted = "Gezielte Exposition",
      share_read = "Gründliche Rezeption",
      share_research = "Weiterrecherche",
      share_engaged = "Engagement",
      share_novel = "Repertoire-Neuheit",
      share_serendipity = "Produktive Serendipität",
      mean_need_fit = "Post–Need-Fit"
    )
  )

day_summary_advanced <- cluster_bootstrap_day_summary(
  day_metrics_long,
  reps = bootstrap_reps,
  seed = exploration_seed + 500
)

# Novelty-Permutationsnull ------------------------------------------------------
# First-occurrence novelty fällt zwangsläufig über die Tage, selbst wenn die
# Reihenfolge der Themen vollständig zufällig wäre. Das Nullmodell permutiert die
# Themenreihenfolge innerhalb jeder Person, lässt Tageszahl und Uploadverteilung
# aber unverändert. Interessant ist die Abweichung vom mechanisch erwartbaren
# Verlauf, nicht der rohe negative Tageseffekt.

novelty_permutation <- tibble()

if (run_novelty_permutation && nrow(daily_posts) > 0 && "topic_novelty" %in% names(daily_posts)) {
  novelty_base <- daily_posts %>%
    arrange(participant, study_day, screenshot_id) %>%
    select(participant, study_day, topic_coded, topic_novelty)
  
  observed_novelty <- novelty_base %>%
    group_by(participant, study_day) %>%
    summarise(Person_Rate = safe_mean(topic_novelty), .groups = "drop") %>%
    group_by(study_day) %>%
    summarise(
      Observed = safe_mean(Person_Rate),
      N_Participants = n_distinct(participant),
      .groups = "drop"
    )
  
  split_person <- split(novelty_base, novelty_base$participant)
  set.seed(exploration_seed + 600)
  
  novelty_null <- map_dfr(seq_len(novelty_permutation_reps), function(b) {
    permuted <- map_dfr(split_person, function(dp) {
      perm_topic <- sample(dp$topic_coded, nrow(dp), replace = FALSE)
      tibble(
        participant = dp$participant,
        study_day = dp$study_day,
        Novel = as.integer(!duplicated(perm_topic))
      )
    })
    
    permuted %>%
      group_by(participant, study_day) %>%
      summarise(Person_Rate = safe_mean(Novel), .groups = "drop") %>%
      group_by(study_day) %>%
      summarise(Null_Rate = safe_mean(Person_Rate), .groups = "drop") %>%
      mutate(Iteration = b)
  })
  
  novelty_permutation <- novelty_null %>%
    group_by(study_day) %>%
    summarise(
      Null_Mean = safe_mean(Null_Rate),
      Null_Low = quantile(Null_Rate, .025, na.rm = TRUE, type = 6),
      Null_High = quantile(Null_Rate, .975, na.rm = TRUE, type = 6),
      .groups = "drop"
    ) %>%
    left_join(observed_novelty, by = "study_day") %>%
    mutate(Observed_Minus_Null = Observed - Null_Mean)
}

reactivity_day_summary <- participant_day %>%
  filter(!is.na(reactivity_group)) %>%
  select(
    participant, study_day, reactivity_group,
    day_has_upload, n_uploads, share_targeted, share_read, share_serendipity
  ) %>%
  pivot_longer(
    cols = c(day_has_upload, n_uploads, share_targeted, share_read, share_serendipity),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = recode(
      Metric,
      day_has_upload = "Upload-Tag beobachtet",
      n_uploads = "Uploads je beobachtetem Upload-Tag",
      share_targeted = "Gezielte Exposition",
      share_read = "Gründliche Rezeption",
      share_serendipity = "Produktive Serendipität"
    )
  ) %>%
  group_by(study_day, reactivity_group, Metric) %>%
  summarise(
    N = sum(!is.na(Value)),
    Mean = safe_mean(Value),
    .groups = "drop"
  )


#===============================================================================
# 14 Exploratory participant typology: PAM/Gower + stability check
#===============================================================================
# Profile sind leicht überinterpretierbar, besonders bei kleinen Stichproben.
# Deshalb:
#   - keine k-means-Zwangsannahme sphärischer Cluster,
#   - Gower-Distanz kann Missingness direkt verarbeiten,
#   - PAM ist robuster gegenüber Ausreißern,
#   - K wird nur zwischen 2 und 5 per Silhouette erkundet,
#   - die gewählte Lösung wird über wiederholte 80%-Subsamples mit Adjusted Rand
#     Index (ARI) auf Stabilität geprüft.
# Eine instabile Lösung bleibt sichtbar, wird aber explizit NICHT als
# substantielle Typologie interpretiert. Kompositionale Inhaltsdimensionen gehen
# als CLR-Koordinaten, Diversität primär als Evenness ein.

cluster_vars <- c(
  "share_incidental_strict",
  "share_targeted",
  "topic_clr_current_affairs",
  "topic_clr_practical_service",
  "topic_clr_knowledge_interests",
  "source_clr_journalistic",
  "source_clr_peer",
  "topic_evenness_adv",
  "source_evenness_adv",
  "share_read_thoroughly",
  "share_researched",
  "share_topic_novelty",
  "share_productive_serendipity_strict",
  "mean_post_need_fit_z"
)

cluster_labels <- c(
  share_incidental_strict = "Incidentality strikt",
  share_targeted = "Gezielte Exposition",
  topic_clr_current_affairs = "Public Affairs (CLR)",
  topic_clr_practical_service = "Praktische Info (CLR)",
  topic_clr_knowledge_interests = "Wissen/Interessen (CLR)",
  source_clr_journalistic = "Journalistische Quellen (CLR)",
  source_clr_peer = "Peer-Quellen (CLR)",
  topic_evenness_adv = "Themen-Evenness",
  source_evenness_adv = "Quellen-Evenness",
  share_read_thoroughly = "Gründliche Rezeption",
  share_researched = "Weiterrecherche",
  share_topic_novelty = "Themenneuheit",
  share_productive_serendipity_strict = "Produktive Serendipität",
  mean_post_need_fit_z = "Post–Need-Fit"
)

cluster_assignment <- tibble()
cluster_profiles <- tibble()
pca_scores <- tibble()
cluster_choice <- tibble()
cluster_stability <- tibble()
profile_solution_interpretable <- FALSE

if (run_profile_typology) {
  cluster_input <- master %>%
    select(participant, any_of(cluster_vars))
  
  candidate_cluster_vars <- intersect(cluster_vars, names(cluster_input))
  usable_vars <- candidate_cluster_vars[
    map_lgl(cluster_input[candidate_cluster_vars], ~ {
      x <- clean_numeric(.x)
      sum(!is.na(x)) >= max(15, floor(.40 * nrow(cluster_input))) &&
        n_distinct(x[!is.na(x)]) >= 2
    })
  ]
  
  cluster_input <- cluster_input %>% select(participant, all_of(usable_vars))
  
  if (nrow(cluster_input) >= minimum_typology_n && length(usable_vars) >= 5) {
    # Gower kann fehlende Werte paarweise berücksichtigen. Zeilen mit zu viel
    # Missingness werden vorab ausgeschlossen, weil ihre Distanzen kaum stabil
    # interpretierbar sind.
    cluster_input <- cluster_input %>%
      mutate(
        prop_observed = rowMeans(!is.na(across(all_of(usable_vars))))
      ) %>%
      filter(prop_observed >= .70) %>%
      select(-prop_observed)
    
    if (nrow(cluster_input) >= minimum_typology_n) {
      gower_dist <- cluster::daisy(
        cluster_input %>% select(all_of(usable_vars)),
        metric = "gower"
      )
      
      max_k <- min(5, nrow(cluster_input) - 1)
      silhouette_results <- map_dfr(2:max_k, function(k) {
        fit <- cluster::pam(gower_dist, k = k, diss = TRUE)
        tibble(
          K = k,
          Mean_Silhouette = fit$silinfo$avg.width
        )
      })
      
      best_k <- silhouette_results$K[which.max(silhouette_results$Mean_Silhouette)]
      pam_final <- cluster::pam(gower_dist, k = best_k, diss = TRUE)
      
      cluster_assignment <- tibble(
        participant = cluster_input$participant,
        exploration_cluster = factor(pam_final$clustering)
      )
      
      # Subsample-Stabilität: dieselben Personen werden in einer 80%-Teilstichprobe
      # erneut geclustert. ARI ist invariant gegenüber vertauschten Clusterlabels.
      set.seed(exploration_seed + 700)
      cluster_stability <- map_dfr(seq_len(profile_stability_reps), function(b) {
        idx <- sort(sample(seq_len(nrow(cluster_input)),
                           size = max(best_k * 4, floor(.80 * nrow(cluster_input))),
                           replace = FALSE))
        sub_data <- cluster_input %>% slice(idx)
        sub_dist <- cluster::daisy(sub_data %>% select(all_of(usable_vars)), metric = "gower")
        sub_fit <- tryCatch(
          cluster::pam(sub_dist, k = best_k, diss = TRUE),
          error = function(e) NULL
        )
        
        if (is.null(sub_fit)) return(tibble(Iteration = b, ARI = NA_real_))
        
        original_labels <- pam_final$clustering[idx]
        tibble(
          Iteration = b,
          ARI = adjusted_rand_index(original_labels, sub_fit$clustering)
        )
      })
      
      mean_ari <- safe_mean(cluster_stability$ARI)
      selected_silhouette <- silhouette_results$Mean_Silhouette[
        silhouette_results$K == best_k
      ][[1]]
      
      profile_solution_interpretable <-
        is.finite(selected_silhouette) &&
        selected_silhouette >= minimum_cluster_silhouette &&
        is.finite(mean_ari) &&
        mean_ari >= minimum_profile_stability_ari
      
      cluster_choice <- silhouette_results %>%
        mutate(
          Selected = K == best_k,
          Mean_SubSample_ARI = if_else(Selected, mean_ari, NA_real_),
          Interpretability_Flag = case_when(
            !Selected ~ NA_character_,
            profile_solution_interpretable ~ "hinreichend stabil für Hypothesengenerierung",
            TRUE ~ "instabil/schwach – nicht als Typologie interpretieren"
          )
        )
      
      # Clusterprofile werden für Lesbarkeit auf z-standardisierten Originalmaßen
      # dargestellt; Median-Imputation findet NUR für diese Darstellung/PCA statt,
      # nicht für die eigentliche PAM/Gower-Clusterzuordnung.
      profile_matrix <- cluster_input %>%
        select(participant, all_of(usable_vars)) %>%
        mutate(
          across(
            all_of(usable_vars),
            ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x)
          )
        )
      
      scaled_matrix <- profile_matrix %>%
        column_to_rownames("participant") %>%
        as.matrix() %>%
        scale()
      
      cluster_profile_data <- as_tibble(scaled_matrix, rownames = "participant") %>%
        left_join(cluster_assignment, by = "participant")
      
      cluster_profiles <- cluster_profile_data %>%
        pivot_longer(
          cols = -c(participant, exploration_cluster),
          names_to = "Variable",
          values_to = "Z"
        ) %>%
        group_by(exploration_cluster, Variable) %>%
        summarise(
          N = n(),
          Mean_Z = mean(Z),
          .groups = "drop"
        ) %>%
        mutate(Label = unname(cluster_labels[Variable]))
      
      if (ncol(scaled_matrix) >= 2) {
        pca <- prcomp(scaled_matrix, center = FALSE, scale. = FALSE)
        explained <- summary(pca)$importance[2, 1:2]
        
        pca_scores <- as_tibble(pca$x[, 1:2, drop = FALSE], rownames = "participant") %>%
          left_join(cluster_assignment, by = "participant") %>%
          mutate(
            PC1_Label = paste0("PC1 (", round(100 * explained[[1]], 1), "%)"),
            PC2_Label = paste0("PC2 (", round(100 * explained[[2]], 1), "%)")
          )
      }
      
      master <- master %>% left_join(cluster_assignment, by = "participant")
    }
  }
}


#===============================================================================
# 15 Optional multilevel models and ICCs
#===============================================================================
# Modelle ergänzen die deskriptiven Muster um verschachtelungsadäquate
# Explorationen. Sie sind NICHT präregistriert. Zeitvariable Prädiktoren werden,
# wo theoretisch relevant, in Within- und Between-Person-Komponenten zerlegt.
# Plattformkoeffizienten bleiben assoziativ: Plattformwahl ist nicht randomisiert.
# Das Public-Relevance-Gate wird bewusst NICHT als inhaltliches Outcome modelliert,
# weil die Uploadinstruktion bereits auf öffentlich relevante Inhalte zielte.

icc_results <- bind_rows(
  logistic_icc(daily_integrated, "public_relevance", "Public-Relevance-Gate erfüllt (QC)"),
  logistic_icc(daily_posts, "incidental_strict", "Incidentality strikt"),
  logistic_icc(daily_posts, "processed_meaningfully", "Sinnvolle Verarbeitung"),
  logistic_icc(daily_posts, "interaction_research", "Weiterrecherche")
)

model_status <- tibble()
model_terms <- tibble()

if (run_mixed_models) {
  model_data_posts <- daily_posts %>%
    mutate(
      platform = factor(platform),
      study_day_z = safe_z(study_day)
    )
  
  # Public relevance wird NICHT als inhaltliches Outcome modelliert: Die
  # Teilnehmenden sollten gezielt öffentlich relevante Beiträge hochladen. Das
  # Gate ist daher eine methodische Adhärenz-/QC-Variable, keine Feed-Prävalenz.
  
  # 1) Incidentality selbst: deskriptive Plattform-/Personenmarker.
  m2 <- safe_glmer(
    model_data_posts,
    incidental_strict ~ platform + study_day_z + age_z_adv + intensity_z_adv +
      (1 | participant),
    binomial(),
    "Strict incidental exposure",
    "OR"
  )
  
  # 2) Verarbeitung: situative Incidentality und situativer Need Fit werden von
  # ihren stabilen Personenmitteln getrennt. Damit beantwortet der Within-Term:
  # "Wird ein Post für dieselbe Person eher verarbeitet, wenn er incidental /
  # need-passender ist als ihre eigenen üblichen Posts?"
  m3 <- safe_glmer(
    model_data_posts,
    processed_meaningfully ~
      incidental_strict_within * post_need_fit_within +
      incidental_strict_between + post_need_fit_between +
      platform + study_day_z + (1 | participant),
    binomial(),
    "Meaningful processing: within/between",
    "OR"
  )
  
  # Random-slope-Sensitivität für den zentralen Within-Incidentality-Effekt.
  # Sie wird nur versucht, wenn genügend Personen sowohl incidental als auch
  # nicht-incidental Posts beitragen; Singularität/Konvergenz wird transparent
  # im Status berichtet.
  n_within_incidental_variation <- model_data_posts %>%
    group_by(participant) %>%
    summarise(N_States = n_distinct(incidental_strict[!is.na(incidental_strict)]), .groups = "drop") %>%
    summarise(N = sum(N_States >= 2)) %>%
    pull(N)
  
  if (n_within_incidental_variation >= minimum_model_participants) {
    m3_random_slope <- safe_glmer(
      model_data_posts,
      processed_meaningfully ~
        incidental_strict_within * post_need_fit_within +
        incidental_strict_between + post_need_fit_between +
        platform + study_day_z +
        (1 + incidental_strict_within | participant),
      binomial(),
      "Meaningful processing: random-slope sensitivity",
      "OR"
    )
  } else {
    m3_random_slope <- list(
      status = tibble(
        Model = "Meaningful processing: random-slope sensitivity",
        N = nrow(model_data_posts),
        Participants = n_distinct(model_data_posts$participant),
        Events = sum(model_data_posts$processed_meaningfully == 1L, na.rm = TRUE),
        NonEvents = sum(model_data_posts$processed_meaningfully == 0L, na.rm = TRUE),
        Singular = NA,
        Convergence_Message = NA_character_,
        Status = "Übersprungen: zu wenige Personen mit Within-Incidentality-Variation"
      ),
      tidy = tibble()
    )
  }
  
  m4 <- safe_glmer(
    model_data_posts,
    interaction_research ~
      incidental_strict_within * post_need_fit_within +
      incidental_strict_between + post_need_fit_between +
      platform + study_day_z + (1 | participant),
    binomial(),
    "Further research: within/between",
    "OR"
  )
  
  # 4) Unter tatsächlich incidental Posts: Welche Posts werden produktiv
  # serendipitär? Auch hier wird Need Fit within/between zerlegt.
  m5 <- safe_glmer(
    model_data_posts %>% filter(incidental_strict == 1L),
    productive_serendipity_strict ~
      post_need_fit_within + post_need_fit_between + platform + study_day_z +
      (1 | participant),
    binomial(),
    "Serendipity among strict incidental posts",
    "OR"
  )
  
  # 5) Reaktivität wurde erst im Outro berichtet. Die Interaktion ist daher eine
  # retrospektive Methodensensitivität: Personen mit höherer berichteter
  # Reaktivität zeigen möglicherweise andere zeitliche Targeted-Trends.
  m6 <- safe_glmer(
    model_data_posts %>% filter(!is.na(reactivity_z_adv)),
    targeted_exposure ~ study_day_z * reactivity_z_adv + platform +
      (1 | participant),
    binomial(),
    "Targeted exposure × reactivity over time",
    "OR"
  )
  
  model_status <- bind_rows(
    m2$status, m3$status, m3_random_slope$status, m4$status, m5$status, m6$status
  )
  
  model_terms <- bind_rows(
    m2$tidy, m3$tidy, m3_random_slope$tidy, m4$tidy, m5$tidy, m6$tidy
  )
  
  if (nrow(model_terms) > 0 && "P_Value" %in% names(model_terms)) {
    model_terms <- model_terms %>%
      mutate(
        # Eine gemeinsame FDR-Korrektur über die gesamte explorative Modellfamilie
        # verhindert, dass aus mehreren Modellen nachträglich der günstigste
        # Einzel-p-Wert hervorgehoben wird.
        P_Adjusted_BH = p.adjust(P_Value, method = "BH")
      )
  }
}


#===============================================================================
# 16 Additional focused exploratory summaries for figures
#===============================================================================

age_outcomes <- c(
  intro_intensity = "Nutzungsintensität",
  n_platforms_weekly = "Screening-Plattformrepertoire",
  share_incidental_strict = "Incidentality strikt",
  share_targeted = "Gezielte Exposition",
  share_read_thoroughly = "Gründliche Rezeption",
  share_researched = "Weiterrecherche",
  topic_shannon = "Themen-Diversität",
  source_shannon = "Quellen-Diversität",
  platform_shannon = "Plattform-Diversität",
  share_current_affairs = "Public Affairs",
  share_peer_sources = "Peer-Quellen",
  share_topic_novelty = "Themenneuheit",
  share_productive_serendipity_strict = "Produktive Serendipität",
  need_profile_alignment = "Need–Content-Alignment",
  reactivity_index = "Reaktivität",
  ease_index = "Ease of Use"
)

age_correlations <- map_dfr(
  names(age_outcomes),
  ~ bootstrap_spearman(
    master,
    "intro_age_num",
    .x,
    "Alter",
    age_outcomes[[.x]],
    family = "Alter",
    reps = bootstrap_reps,
    seed = exploration_seed + 750
  )
) %>%
  mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH"))

reactivity_slopes_long <- master %>%
  select(
    participant, reactivity_index,
    upload_day_presence_slope, upload_intensity_active_day_slope,
    public_relevance_day_slope, targeted_post_day_slope, thorough_reading_day_slope
  ) %>%
  pivot_longer(
    cols = ends_with("day_slope"),
    names_to = "Slope",
    values_to = "Value"
  ) %>%
  mutate(
    Slope = recode(
      Slope,
      upload_day_presence_slope = "Beobachteter Upload-Tag",
      upload_intensity_active_day_slope = "Uploadintensität an aktiven Tagen",
      public_relevance_day_slope = "Public-Relevance-Gate erfüllt (QC)",
      targeted_post_day_slope = "Gezielte Exposition",
      thorough_reading_day_slope = "Gründliche Rezeption"
    )
  )

ease_participation_long <- master %>%
  select(participant, ease_index, n_screenshots, n_active_days) %>%
  pivot_longer(
    cols = c(n_screenshots, n_active_days),
    names_to = "Outcome",
    values_to = "Value"
  ) %>%
  mutate(
    Outcome = recode(
      Outcome,
      n_screenshots = "Uploads",
      n_active_days = "Aktive Diary-Tage"
    )
  )


#===============================================================================
# 16A Robustness multiverse and compositional repertoire maps
#===============================================================================
# Statt eine einzelne plausible Operationalisierung nachträglich zu bevorzugen,
# werden zentrale Beziehungen über mehrere sachlich vertretbare Spezifikationen
# nebeneinander gerechnet. Entscheidend ist, ob Richtung und Größenordnung stabil
# bleiben – nicht welche Variante den kleinsten p-Wert liefert.

robustness_specs <- tibble()
robustness_summary <- tibble()

if (run_sensitivity_multiverse) {
  
  # A) Screening- vs. Diary-Incidentality: strict/broad × Mindestzahl Posts ------
  robustness_incidentality <- crossing(
    Min_Public_Posts = sensitivity_public_post_thresholds,
    Definition = c("Strikt", "Breit")
  ) %>%
    pmap_dfr(function(Min_Public_Posts, Definition) {
      d <- master %>% filter(n_public_content_posts >= Min_Public_Posts)
      outcome <- if (Definition == "Strikt") "share_incidental_strict" else "share_incidental_broad"
      
      bootstrap_spearman(
        d,
        "incidentality_index", outcome,
        "Screening-Incidentality", paste0("Diary-Incidentality, ", Definition),
        family = "Incidentality calibration multiverse",
        reps = bootstrap_reps,
        seed = exploration_seed + 800 + Min_Public_Posts
      ) %>%
        mutate(
          Relation = "Screening → Diary Incidentality",
          Specification = paste0(Definition, "; ≥", Min_Public_Posts, " public posts")
        )
    })
  
  # B) Need–Content: Raw Shares über Beobachtungsdichte --------------------------
  robustness_need_raw <- crossing(
    Min_Public_Posts = sensitivity_public_post_thresholds,
    Spec_Row = seq_len(nrow(need_content_specs))
  ) %>%
    pmap_dfr(function(Min_Public_Posts, Spec_Row) {
      spec <- need_content_specs[Spec_Row, ]
      d <- master %>% filter(n_public_content_posts >= Min_Public_Posts)
      
      bootstrap_spearman(
        d,
        spec$Need_Variable,
        spec$Diary_Variable,
        spec$Need,
        spec$Diary_Domain,
        family = "Need–Content multiverse: raw",
        reps = bootstrap_reps,
        seed = exploration_seed + 900 + Min_Public_Posts + Spec_Row
      ) %>%
        mutate(
          Relation = paste0("Need: ", spec$Need),
          Specification = paste0("Raw share; ≥", Min_Public_Posts, " public posts")
        )
    })
  
  # C) Need–Content: CLR über Mindestzahl Posts UND Pseudocount -----------------
  robustness_need_coda <- crossing(
    Min_Public_Posts = sensitivity_public_post_thresholds,
    Pseudocount = c(.25, .5, 1),
    Spec_Row = seq_len(nrow(need_content_specs))
  ) %>%
    pmap_dfr(function(Min_Public_Posts, Pseudocount, Spec_Row) {
      spec <- need_content_specs[Spec_Row, ]
      
      topic_obj <- make_clr_composition(
        daily_posts, "topic_macro",
        min_total = Min_Public_Posts,
        pseudocount = Pseudocount
      )
      source_obj <- make_clr_composition(
        daily_posts, "source_macro",
        min_total = Min_Public_Posts,
        pseudocount = Pseudocount
      )
      
      temp <- master %>%
        left_join(topic_obj$data, by = "participant") %>%
        left_join(source_obj$data, by = "participant", suffix = c("", "_source")) %>%
        filter(n_public_content_posts >= Min_Public_Posts)
      
      obj <- if (spec$Composition == "Topic") topic_obj else source_obj
      clr_v <- get_clr_variable(obj, spec$Diary_Domain)
      if (!is.na(clr_v) && spec$Composition == "Source" && clr_v %in% names(topic_obj$data)) {
        clr_v <- paste0(clr_v, "_source")
      }
      
      if (is.na(clr_v) || !clr_v %in% names(temp)) {
        return(tibble(
          Family = "Need–Content multiverse: CLR",
          Predictor = spec$Need,
          Outcome = spec$Diary_Domain,
          N = 0L,
          Spearman_Rho = NA_real_, CI95_Low = NA_real_, CI95_High = NA_real_, P_Value = NA_real_,
          Relation = paste0("Need: ", spec$Need),
          Specification = paste0("CLR pc=", Pseudocount, "; ≥", Min_Public_Posts, " public posts")
        ))
      }
      
      bootstrap_spearman(
        temp,
        spec$Need_Variable,
        clr_v,
        spec$Need,
        paste0(spec$Diary_Domain, " (CLR)"),
        family = "Need–Content multiverse: CLR",
        reps = bootstrap_reps,
        seed = exploration_seed + 1000 + 100 * Spec_Row + 10 * Min_Public_Posts + round(10 * Pseudocount)
      ) %>%
        mutate(
          Relation = paste0("Need: ", spec$Need),
          Specification = paste0("CLR pc=", Pseudocount, "; ≥", Min_Public_Posts, " public posts")
        )
    })
  
  robustness_specs <- bind_rows(
    robustness_incidentality,
    robustness_need_raw,
    robustness_need_coda
  ) %>%
    mutate(
      Estimate = Spearman_Rho,
      Robust_Direction = case_when(
        Estimate > 0 ~ "positiv",
        Estimate < 0 ~ "negativ",
        TRUE ~ NA_character_
      )
    )
  
  robustness_summary <- robustness_specs %>%
    filter(!is.na(Estimate)) %>%
    group_by(Relation) %>%
    summarise(
      N_Specifications = n(),
      Median_Estimate = median(Estimate),
      Min_Estimate = min(Estimate),
      Max_Estimate = max(Estimate),
      Proportion_Positive = mean(Estimate > 0),
      Proportion_Negative = mean(Estimate < 0),
      .groups = "drop"
    ) %>%
    mutate(
      Direction_Stable = pmax(Proportion_Positive, Proportion_Negative) >= .80,
      Note = "Robustheit über plausible Spezifikationen; kein Signifikanzranking"
    )
}

# CLR-PCA visualisiert die RELATIVE thematische Zusammensetzung, ohne einzelne
# Shares isoliert zu behandeln. Sie ist deskriptiv und benötigt mindestens zwei
# nichtkonstante CLR-Koordinaten.
topic_coda_pca_scores <- tibble()
topic_coda_pca_loadings <- tibble()

if (run_compositional_analysis && nrow(topic_composition$data) >= 10) {
  clr_vars <- setdiff(names(topic_composition$data), "participant")
  clr_mat <- as.matrix(topic_composition$data[, clr_vars, drop = FALSE])
  keep_var <- apply(clr_mat, 2, sd, na.rm = TRUE) > 0
  clr_mat <- clr_mat[, keep_var, drop = FALSE]
  
  if (ncol(clr_mat) >= 2) {
    topic_pca <- prcomp(clr_mat, center = TRUE, scale. = FALSE)
    explained <- summary(topic_pca)$importance[2, 1:2]
    
    topic_coda_pca_scores <- as_tibble(topic_pca$x[, 1:2, drop = FALSE]) %>%
      mutate(participant = topic_composition$data$participant, .before = 1) %>%
      left_join(
        master %>% select(participant, dominant_information_need, reactivity_index),
        by = "participant"
      ) %>%
      mutate(
        PC1_Label = paste0("PC1 (", round(100 * explained[[1]], 1), "%)"),
        PC2_Label = paste0("PC2 (", round(100 * explained[[2]], 1), "%)")
      )
    
    load <- as.data.frame(topic_pca$rotation[, 1:2, drop = FALSE]) %>%
      rownames_to_column("CLR_Variable") %>%
      as_tibble() %>%
      left_join(topic_composition$map, by = "CLR_Variable")
    
    topic_coda_pca_loadings <- load
  }
}

# Coverage-Heatmap: Visualisiert tatsächliche Beobachtungsdichte pro Person/Tag.
coverage_heatmap <- participant_day %>%
  group_by(participant) %>%
  mutate(Total_Uploads = sum(n_uploads, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    participant_order = forcats::fct_reorder(participant, Total_Uploads)
  )



#===============================================================================
# 17 Figures: integrated atlas and need–content patterns
#===============================================================================

if (create_figures) {
  atlas_plot_data <- correlation_atlas %>%
    filter(!is.na(Spearman_Rho)) %>%
    mutate(
      Predictor = factor(Predictor, levels = rev(unname(screening_predictors))),
      Outcome = factor(Outcome, levels = unname(integrated_outcomes)),
      Label = sprintf("%.2f", Spearman_Rho)
    )
  
  p_atlas <- ggplot(atlas_plot_data, aes(x = Outcome, y = Predictor, fill = Spearman_Rho)) +
    geom_tile(color = unname(project_colors["white"]), linewidth = .7) +
    geom_text(aes(label = Label), size = 2.7, color = unname(project_colors["dark"])) +
    scale_fill_gradient2(
      low = unname(project_colors["red"]),
      mid = unname(project_colors["white"]),
      high = unname(project_colors["primary"]),
      midpoint = 0,
      limits = c(-1, 1),
      name = "Spearman ρ"
    ) +
    labs(
      title = "Explorativer Zusammenhangsatlas über alle drei Erhebungsebenen",
      subtitle = "Screening-Merkmale → Diary-Muster und Outro; Effektgrößen statt Signifikanzsternchen",
      x = NULL,
      y = NULL,
      caption = paste0(analysis_note, ". Breiter Discovery-Atlas; Zahlen = Spearman ρ. FDR wird berechnet, aber nicht visuell als Sternchen privilegiert; Share-Outcomes sind teils kompositional gekoppelt.")
    ) +
    theme_project(base_size = 10, legend_position = "right") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid = element_blank()
    )
  
  save_adv_plot(p_atlas, "01_Integrated_Correlation_Atlas.png", 16, 7.8)
  
  
  need_matrix_plot <- need_content_matrix %>%
    filter(!is.na(Spearman_Rho)) %>%
    mutate(
      Need = factor(Predictor, levels = rev(need_content_specs$Need)),
      Domain = factor(Outcome, levels = need_content_specs$Diary_Domain),
      Label = paste0(sprintf("%.2f", Spearman_Rho), if_else(Matched, " ●", ""))
    )
  
  p_need_matrix <- ggplot(need_matrix_plot, aes(x = Domain, y = Need, fill = Spearman_Rho)) +
    geom_tile(aes(linewidth = Matched), color = unname(project_colors["white"])) +
    geom_text(aes(label = Label), size = 3.6, color = unname(project_colors["dark"])) +
    scale_linewidth_manual(values = c(`FALSE` = .5, `TRUE` = 2), guide = "none") +
    scale_fill_gradient2(
      low = unname(project_colors["red"]),
      mid = unname(project_colors["white"]),
      high = unname(project_colors["primary"]),
      midpoint = 0,
      limits = c(-1, 1),
      name = "Spearman ρ"
    ) +
    labs(
      title = "Discovery view: Informationsbedürfnisse × beobachtete Raw Shares",
      subtitle = "● markiert die theoretisch zugeordnete Need–Content-Kombination",
      x = "Beobachtete Diary-Domäne",
      y = "Screening-Bedürfnis",
      caption = "Raw Shares sind kompositional gekoppelt; primäre Need–Content-Exploration siehe CLR/CoDA-Grafik 32."
    ) +
    theme_project(legend_position = "right") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1), panel.grid = element_blank())
  
  save_adv_plot(p_need_matrix, "02_Need_Content_Correlation_Matrix.png", 10.5, 6)
  
  
  dominant_need_table <- master %>%
    filter(
      !is.na(dominant_information_need),
      !is.na(observed_dominant_need),
      !str_detect(dominant_information_need, "Kein eindeutiges"),
      !str_detect(observed_dominant_need, "Kein eindeutiges")
    ) %>%
    count(dominant_information_need, observed_dominant_need, name = "N") %>%
    group_by(dominant_information_need) %>%
    mutate(Percent = 100 * N / sum(N)) %>%
    ungroup()
  
  if (nrow(dominant_need_table) > 0) {
    p_dominant_need <- ggplot(
      dominant_need_table,
      aes(x = observed_dominant_need, y = dominant_information_need, fill = Percent)
    ) +
      geom_tile(color = unname(project_colors["white"]), linewidth = .7) +
      geom_text(aes(label = paste0(N, "\n", round(Percent), "%")), size = 3.4) +
      scale_fill_gradient(
        low = unname(project_colors["lighter"]),
        high = unname(project_colors["primary"]),
        name = "% innerhalb\nScreening-Need"
      ) +
      labs(
        title = "Dominantes Bedürfnis im Screening vs. dominante beobachtete Diary-Domäne",
        subtitle = "Zeilenprozente; nur eindeutige Dominanzen",
        x = "Dominante beobachtete Diary-Domäne",
        y = "Dominantes Screening-Bedürfnis",
        caption = analysis_note
      ) +
      theme_project(legend_position = "right") +
      theme(axis.text.x = element_text(angle = 20, hjust = 1), panel.grid = element_blank())
    
    save_adv_plot(p_dominant_need, "03_Dominant_Need_vs_Observed_Domain.png", 10, 6.5)
  }
  
  
  p_need_alignment <- master %>%
    filter(!is.na(need_profile_alignment)) %>%
    ggplot(aes(x = need_profile_alignment)) +
    geom_histogram(
      binwidth = .2,
      boundary = -1,
      fill = unname(project_colors["primary"]),
      color = unname(project_colors["white"]),
      linewidth = .5
    ) +
    geom_vline(
      xintercept = safe_mean(master$need_profile_alignment),
      linetype = "22",
      linewidth = .9,
      color = unname(project_colors["accent"])
    ) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .5)) +
    labs(
      title = "Personenspezifisches Need–Content-Profilalignment",
      subtitle = "Spearman-Korrelation über vier theoretisch zugeordnete Need-/Content-Domänen",
      x = "Profilalignment (−1 bis +1)",
      y = "Teilnehmende",
      caption = "Vier-Punkt-Profilmaß: stark explorativ und bei kleinen Profilen instabil."
    ) +
    theme_project(legend_position = "none")
  
  save_adv_plot(p_need_alignment, "04_Need_Profile_Alignment.png", 8.5, 5.5)
}


#===============================================================================
# 18 Figures: calibration, serendipity and processing
#===============================================================================

if (create_figures) {
  calibration_long <- master %>%
    select(participant, incidentality_index, share_incidental_strict, share_incidental_broad) %>%
    pivot_longer(
      cols = c(share_incidental_strict, share_incidental_broad),
      names_to = "Diary_Definition",
      values_to = "Diary_Incidentality"
    ) %>%
    mutate(
      Diary_Definition = recode(
        Diary_Definition,
        share_incidental_strict = "Strikt",
        share_incidental_broad = "Breit"
      )
    )
  
  p_calibration <- ggplot(
    calibration_long,
    aes(x = incidentality_index, y = Diary_Incidentality)
  ) +
    geom_point(alpha = .65, size = 2.2, color = unname(project_colors["primary"])) +
    geom_smooth(method = "lm", se = TRUE, linewidth = .8, color = unname(project_colors["accent"])) +
    facet_wrap(~ Diary_Definition) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Screening-Incidentality und beobachtete Diary-Incidentality",
      subtitle = "Zwei Operationalisierungen der Diary-Incidentality",
      x = "Screening-Incidentality (1–5)",
      y = "Anteil incidentaler Diary-Posts",
      caption = paste0(
        "Unterschiedliche Messrahmen; Abweichungen sind Kalibrierung, nicht automatisch Bias. ",
        "Die lineare Glättung dient nur der visuellen Orientierung; tabellarisch werden ",
        "Spearman-Zusammenhänge mit Personen-Bootstrap berichtet."
      )
    ) +
    theme_project()
  
  save_adv_plot(p_calibration, "05_Incidentality_Calibration.png", 10, 5.8)
  
  
  calibration_map_data <- master %>%
    filter(
      !is.na(absolute_incidentality_gap_broad),
      !is.na(platform_profile_alignment)
    )
  
  if (nrow(calibration_map_data) >= 5) {
    p_calibration_map <- ggplot(
      calibration_map_data,
      aes(
        x = absolute_incidentality_gap_broad,
        y = platform_profile_alignment,
        color = reactivity_index,
        size = n_screenshots
      )
    ) +
      geom_point(alpha = .75) +
      scale_color_gradient(
        low = unname(project_colors["secondary"]),
        high = unname(project_colors["accent"]),
        na.value = unname(project_colors["medium"]),
        name = "Reaktivität"
      ) +
      scale_size_continuous(range = c(2, 7), name = "Uploads") +
      labs(
        title = "Kalibrierungslandkarte der Teilnehmenden",
        subtitle = "Incidentality-Gap × Plattformprofil-Alignment; Farbe = Outro-Reaktivität",
        x = "Absoluter Screening–Diary-Incidentality-Gap",
        y = "Plattformprofil-Alignment",
        caption = analysis_note
      ) +
      theme_project()
    
    save_adv_plot(p_calibration_map, "06_Calibration_Reactivity_Map.png", 9, 6.3)
  }
  
  
  p_funnel <- ggplot(
    serendipity_funnel,
    aes(x = Step, y = Percent, group = Definition, color = Definition)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.4) +
    geom_text(
      aes(label = paste0(round(Percent, 1), "%")),
      vjust = -1,
      size = 3.2,
      show.legend = FALSE
    ) +
    scale_color_project(values = unname(project_colors[c("primary", "accent")])) +
    scale_y_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(.02, .13))) +
    labs(
      title = "Vom zufälligen Encounter zur produktiven Serendipität",
      subtitle = "Kumulative Stufen mit identischem Nenner: alle öffentlich relevanten Posts",
      x = NULL,
      y = "Anteil",
      caption = "Neuheit ist diary-intern; Verarbeitung umfasst Lesen, Recherche oder sichtbare Interaktion."
    ) +
    theme_project()
  
  save_adv_plot(p_funnel, "07_Serendipity_Pathway.png", 10, 5.8)
  
  
  p_ser_platform <- serendipity_by_platform %>%
    mutate(Category = forcats::fct_reorder(Category, Productive_Serendipity)) %>%
    ggplot(aes(x = Productive_Serendipity, y = Category)) +
    geom_col(width = .62, fill = unname(project_colors["primary"]), alpha = .88) +
    geom_errorbarh(
      aes(xmin = pmax(0, Productive_CI_Low), xmax = pmin(1, Productive_CI_High)),
      height = .16, color = unname(project_colors["dark"]), linewidth = .6
    ) +
    geom_text(
      aes(label = paste0(round(100 * Productive_Serendipity, 1), "%")),
      hjust = -.08,
      size = 3.4
    ) +
    scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .18))) +
    labs(
      title = "Produktive Serendipität nach Plattform",
      subtitle = "Strikte Definition; teilnehmergewichtete Raten",
      x = "Anteil produktiver Serendipität",
      y = NULL,
      caption = analysis_note
    ) +
    theme_project(legend_position = "none")
  
  save_adv_plot(p_ser_platform, "08_Serendipity_By_Platform.png", 8.5, 5.2)
  
  
  serendipity_groups <- bind_rows(
    serendipity_by_topic %>%
      transmute(Grouping = "Themenfamilie", Category, Productive_Serendipity, Productive_CI_Low, Productive_CI_High),
    serendipity_by_source %>%
      transmute(Grouping = "Quellenfamilie", Category, Productive_Serendipity, Productive_CI_Low, Productive_CI_High)
  ) %>%
    filter(!is.na(Category), !is.na(Productive_Serendipity)) %>%
    group_by(Grouping) %>%
    mutate(Category = forcats::fct_reorder(Category, Productive_Serendipity)) %>%
    ungroup()
  
  p_ser_groups <- ggplot(
    serendipity_groups,
    aes(x = Productive_Serendipity, y = Category, fill = Grouping)
  ) +
    geom_col(width = .62, show.legend = FALSE, alpha = .88) +
    geom_errorbarh(
      aes(xmin = pmax(0, Productive_CI_Low), xmax = pmin(1, Productive_CI_High)),
      height = .14, color = unname(project_colors["dark"]), linewidth = .5
    ) +
    geom_text(
      aes(label = paste0(round(100 * Productive_Serendipity, 1), "%")),
      hjust = -.05,
      size = 3,
      show.legend = FALSE
    ) +
    facet_wrap(~ Grouping, scales = "free_y") +
    scale_fill_project(values = unname(project_colors[c("secondary", "accent")])) +
    scale_x_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .18))) +
    labs(
      title = "Wo entsteht produktive Serendipität?",
      subtitle = "Explorative Unterschiede zwischen Themen- und Quellenfamilien",
      x = "Produktive Serendipität, strikt",
      y = NULL,
      caption = analysis_note
    ) +
    theme_project()
  
  save_adv_plot(p_ser_groups, "09_Serendipity_By_Content_Ecology.png", 12, 7)
  
  
  p_needfit <- daily_posts %>%
    filter(!is.na(incidentality), !is.na(post_need_fit_within)) %>%
    ggplot(aes(x = incidentality, y = post_need_fit_within, fill = incidentality)) +
    geom_violin(alpha = .55, trim = FALSE, width = .9) +
    geom_boxplot(width = .16, outlier.shape = NA, fill = unname(project_colors["white"]), linewidth = .45) +
    facet_wrap(~ processed_meaningfully, labeller = as_labeller(c(`0` = "Keine bedeutungsvolle Verarbeitung", `1` = "Bedeutungsvoll verarbeitet"))) +
    scale_fill_project() +
    labs(
      title = "Need Fit, Incidentality und Verarbeitung",
      subtitle = "Within-Person-Abweichung vom eigenen mittleren Need Fit",
      x = NULL,
      y = "Post–Need-Fit: Within-Person-Abweichung",
      caption = "Deskriptive Post-Level-Grafik; Multilevel-Modell trennt Within- und Between-Person-Komponenten explizit."
    ) +
    theme_project(legend_position = "none") +
    theme(axis.text.x = element_text(angle = 18, hjust = 1))
  
  save_adv_plot(p_needfit, "10_NeedFit_Incidentality_Processing.png", 11.5, 6.2)
  
  
  processing_long <- processing_by_incidentality %>%
    select(incidentality, Thorough_Read, Research, Engagement, Any_Processing) %>%
    pivot_longer(-incidentality, names_to = "Processing", values_to = "Rate") %>%
    mutate(
      Processing = recode(
        Processing,
        Thorough_Read = "Gründlich gelesen/angeschaut",
        Research = "Weiter recherchiert",
        Engagement = "Sichtbar interagiert",
        Any_Processing = "Mindestens eine Verarbeitung"
      )
    )
  
  p_processing <- ggplot(
    processing_long,
    aes(x = incidentality, y = Rate, group = Processing, color = Processing)
  ) +
    geom_line(linewidth = .85) +
    geom_point(size = 2.8) +
    scale_color_project() +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Wie wird Information je nach Encounter-Modus verarbeitet?",
      subtitle = "Teilnehmergewichtete Raten innerhalb der drei Incidentality-Kategorien",
      x = NULL,
      y = "Anteil",
      caption = analysis_note
    ) +
    theme_project() +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
  
  save_adv_plot(p_processing, "11_Processing_By_Incidentality.png", 10.5, 6)
}


#===============================================================================
# 19 Figures: content ecology fingerprints
#===============================================================================

if (create_figures) {
  residual_plot <- function(data, x, y, title, subtitle, filename, width, height) {
    if (nrow(data) == 0) return(invisible(NULL))
    
    p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = Std_Residual)) +
      geom_tile(
        aes(alpha = !Sparse_Cell),
        color = unname(project_colors["white"]), linewidth = .7
      ) +
      geom_text(
        aes(label = case_when(
          Sparse_Cell ~ "·",
          abs(Std_Residual) >= 1 ~ sprintf("%.1f", Std_Residual),
          TRUE ~ ""
        )),
        size = 3,
        color = unname(project_colors["dark"])
      ) +
      scale_alpha_manual(values = c(`FALSE` = .35, `TRUE` = 1), guide = "none") +
      scale_fill_gradient2(
        low = unname(project_colors["red"]),
        mid = unname(project_colors["white"]),
        high = unname(project_colors["primary"]),
        midpoint = 0,
        name = "Std. Residual"
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = NULL,
        y = NULL,
        caption = "Deskriptiver Fingerprint, kein inferenzieller Chi²-Test. Schwach dargestellte Zellen (·) haben erwartete Häufigkeit < 5."
      ) +
      theme_project(base_size = 10, legend_position = "right") +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank()
      )
    
    save_adv_plot(p, filename, width, height)
  }
  
  residual_plot(
    platform_topic_residuals,
    "Topic", "Platform",
    "Plattform-Fingerprints nach Themenfamilie",
    "Standardisierte Residuen aus Plattform × Themenfamilie",
    "12_Platform_Topic_Fingerprint.png",
    11, 5.8
  )
  
  residual_plot(
    platform_source_residuals,
    "Source", "Platform",
    "Plattform-Fingerprints nach Quellenfamilie",
    "Standardisierte Residuen aus Plattform × Quellenfamilie",
    "13_Platform_Source_Fingerprint.png",
    13, 5.8
  )
  
  residual_plot(
    topic_source_residuals,
    "Source", "Topic",
    "Welche Quellenlogiken treten mit welchen Themen auf?",
    "Standardisierte Residuen aus Themenfamilie × Quellenfamilie",
    "14_Topic_Source_Ecology.png",
    13, 6.5
  )
  
  
  p_format <- ggplot(platform_format, aes(x = platform, y = Percent, fill = media_format)) +
    geom_col(width = .7) +
    scale_fill_project() +
    scale_y_continuous(labels = label_number(suffix = "%"), limits = c(0, 100)) +
    labs(
      title = "Formale Medienformate unterscheiden sich deutlich nach Plattform",
      subtitle = "Zusammensetzung der öffentlich relevanten Diary-Posts",
      x = NULL,
      y = "Anteil innerhalb der Plattform",
      fill = "Format",
      caption = analysis_note
    ) +
    theme_project()
  
  save_adv_plot(p_format, "15_Platform_Format_Composition.png", 9.5, 6)
}


#===============================================================================
# 20 Figures: temporal patterns and method reactivity
#===============================================================================

if (create_figures) {
  temporal_selected <- day_summary_advanced %>%
    filter(Metric %in% c(
      "Incidentality strikt", "Gezielte Exposition", "Gründliche Rezeption",
      "Weiterrecherche", "Repertoire-Neuheit", "Produktive Serendipität"
    ))
  
  p_temporal <- ggplot(
    temporal_selected,
    aes(x = study_day, y = Mean, color = Metric, group = Metric)
  ) +
    geom_ribbon(
      aes(ymin = CI_Low, ymax = CI_High, fill = Metric),
      alpha = .08,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = .9) +
    geom_point(size = 2.4) +
    scale_color_project() +
    scale_fill_project() +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Wie verändert sich das beobachtete Informationshandeln über sieben Tage?",
      subtitle = "Personengewichtete Tagesmittel; 95%-CI via Cluster-Bootstrap über Personen",
      x = "Studientag",
      y = "Anteil",
      caption = "Deskriptiver Verlauf; Tageseffekte können normale zeitliche Variation oder Diary-Reaktivität abbilden."
    ) +
    theme_project()
  
  save_adv_plot(p_temporal, "16_Temporal_Behaviour_Trajectories.png", 11, 6.4)
  
  
  upload_summary <- day_summary_advanced %>%
    filter(Metric == "Uploads je beobachtetem Upload-Tag")
  
  p_uploads <- ggplot(upload_summary, aes(x = study_day, y = Mean)) +
    geom_ribbon(
      aes(ymin = CI_Low, ymax = CI_High),
      fill = unname(project_colors["light"]),
      alpha = .8
    ) +
    geom_line(linewidth = 1.05, color = unname(project_colors["primary"])) +
    geom_point(size = 2.8, color = unname(project_colors["accent"])) +
    scale_x_continuous(breaks = 1:7) +
    labs(
      title = "Uploadintensität an beobachteten Upload-Tagen",
      subtitle = "Tage ohne Screenshot werden nicht als Null-Uploads interpretiert",
      x = "Studientag",
      y = "Mittlere Uploadzahl je beobachtetem Upload-Tag",
      caption = "Personengewichtet; 95%-CI via Personen-Cluster-Bootstrap."
    ) +
    theme_project(legend_position = "none")
  
  save_adv_plot(p_uploads, "17_Uploads_Over_Time.png", 8.5, 5.3)
  
  upload_day_coverage <- day_summary_advanced %>%
    filter(Metric == "Upload-Tag beobachtet")
  
  p_upload_coverage <- ggplot(
    upload_day_coverage,
    aes(x = study_day, y = Mean)
  ) +
    geom_ribbon(
      aes(ymin = CI_Low, ymax = CI_High),
      fill = unname(project_colors["light"]),
      alpha = .8
    ) +
    geom_line(linewidth = 1.05, color = unname(project_colors["primary"])) +
    geom_point(size = 2.8, color = unname(project_colors["accent"])) +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Beobachtungsdichte über die sieben Diary-Tage",
      subtitle = "Anteil der eingeschlossenen Personen mit mindestens einem Screenshot",
      x = "Studientag",
      y = "Personen mit beobachtetem Upload-Tag",
      caption = "Coverage-Marker, nicht gleichbedeutend mit Fragebogen-Compliance oder tatsächlicher Exposition."
    ) +
    theme_project(legend_position = "none")
  
  save_adv_plot(p_upload_coverage, "17b_Upload_Day_Coverage.png", 8.5, 5.3)
  
  
  if (nrow(reactivity_day_summary) > 0) {
    p_reactivity_days <- ggplot(
      reactivity_day_summary,
      aes(x = study_day, y = Mean, color = reactivity_group, group = reactivity_group)
    ) +
      geom_line(linewidth = .95) +
      geom_point(size = 2.3) +
      facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
      scale_color_project(values = unname(project_colors[c("secondary", "primary", "accent")])) +
      scale_x_continuous(breaks = 1:7) +
      labs(
        title = "Tagesverläufe nach nachträglich berichteter Reaktivität",
        subtitle = "Tertile dienen nur der Visualisierung; kontinuierlicher Index ist analytisch vorzuziehen",
        x = "Studientag",
        y = "Mittelwert",
        color = "Reaktivität",
        caption = "Outro-Reaktivität wurde nach dem Diary erhoben; keine kausale Interpretation."
      ) +
      theme_project()
    
    save_adv_plot(p_reactivity_days, "18_Temporal_Patterns_By_Reactivity.png", 11, 8)
  }
}


#===============================================================================
# 21 Figures: repertoires, age and Outro bridges
#===============================================================================

if (create_figures) {
  repertoire_long <- master %>%
    select(participant, n_platforms_weekly, platform_richness, platform_shannon) %>%
    pivot_longer(
      cols = c(platform_richness, platform_shannon),
      names_to = "Diary_Metric",
      values_to = "Value"
    ) %>%
    mutate(
      Diary_Metric = recode(
        Diary_Metric,
        platform_richness = "Diary-Plattform-Richness",
        platform_shannon = "Diary-Plattform-Diversität"
      )
    )
  
  p_repertoire <- ggplot(
    repertoire_long,
    aes(x = n_platforms_weekly, y = Value)
  ) +
    geom_jitter(width = .08, height = 0, alpha = .55, size = 2.1, color = unname(project_colors["primary"])) +
    geom_smooth(method = "lm", se = TRUE, color = unname(project_colors["accent"]), linewidth = .8) +
    facet_wrap(~ Diary_Metric, scales = "free_y") +
    scale_x_continuous(breaks = 0:4) +
    labs(
      title = "Screening-Repertoire vs. tatsächlich im Diary auftauchende Plattformökologie",
      x = "Wöchentlich genutzte Plattformen im Screening",
      y = NULL,
      caption = paste0(
        "Diary-Richness basiert nur auf hochgeladenen Beiträgen, nicht auf sämtlicher Nutzung. ",
        "Die lineare Glättung ist ausschließlich eine visuelle Orientierung."
      )
    ) +
    theme_project()
  
  save_adv_plot(p_repertoire, "19_Screening_Diary_Platform_Repertoire.png", 10, 5.6)
  
  
  age_plot_data <- age_correlations %>%
    filter(!is.na(Spearman_Rho)) %>%
    mutate(
      Outcome = forcats::fct_reorder(Outcome, Spearman_Rho)
    )
  
  p_age <- ggplot(age_plot_data, aes(x = Spearman_Rho, y = Outcome)) +
    geom_vline(xintercept = 0, color = unname(project_colors["grid"]), linewidth = .8) +
    geom_errorbarh(
      aes(xmin = CI95_Low, xmax = CI95_High),
      height = .16, linewidth = .65,
      color = unname(project_colors["secondary"])
    ) +
    geom_point(size = 3, color = unname(project_colors["primary"])) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25)) +
    labs(
      title = "Altersheterogenität innerhalb der 60+-Stichprobe",
      subtitle = "Spearman-Zusammenhänge mit Personen-Bootstrap-CIs",
      x = "Spearman ρ mit Alter",
      y = NULL,
      caption = paste0(
        "Explorativ; kontinuierliches Alter, keine Defizitinterpretation. ",
        "BH-adjustierte p-Werte stehen in der Ergebnistabelle, werden hier aber ",
        "nicht als binäre Signifikanzmarkierung hervorgehoben."
      )
    ) +
    theme_project(legend_position = "none")
  
  save_adv_plot(p_age, "20_Age_Correlation_Forest.png", 9.5, 7.2)
  
  
  if (sum(!is.na(reactivity_slopes_long$reactivity_index) & !is.na(reactivity_slopes_long$Value)) >= 8) {
    p_reactivity_slopes <- ggplot(
      reactivity_slopes_long,
      aes(x = reactivity_index, y = Value)
    ) +
      geom_point(alpha = .6, color = unname(project_colors["primary"])) +
      geom_smooth(method = "lm", se = TRUE, color = unname(project_colors["accent"]), linewidth = .8) +
      facet_wrap(~ Slope, scales = "free_y", ncol = 2) +
      labs(
        title = "Berichtete Reaktivität und personenspezifische Diary-Tagestrends",
        subtitle = "Slope > 0 bedeutet Zunahme über die sieben Tage",
        x = "Outro-Reaktivitätsindex",
        y = "Personenspezifischer Tages-Slope",
        caption = paste0(
          "Explorative methodische Plausibilisierung; Outro wurde nach dem Diary erhoben. ",
          "Die lineare Glättung ist nur eine visuelle Orientierung."
        )
      ) +
      theme_project()
    
    save_adv_plot(p_reactivity_slopes, "21_Reactivity_vs_Diary_Trends.png", 10.5, 7.4)
  }
  
  
  if (sum(!is.na(ease_participation_long$ease_index) & !is.na(ease_participation_long$Value)) >= 8) {
    p_ease <- ggplot(ease_participation_long, aes(x = ease_index, y = Value)) +
      geom_point(alpha = .65, color = unname(project_colors["primary"])) +
      geom_smooth(method = "lm", se = TRUE, color = unname(project_colors["accent"]), linewidth = .8) +
      facet_wrap(~ Outcome, scales = "free_y") +
      labs(
        title = "Usability und tatsächliche Diary-Teilnahme",
        subtitle = "Outro Ease of Use × Uploadzahl / aktive Tage",
        x = "Ease-of-Use-Index",
        y = NULL,
        caption = paste0(
          "Explorativ; mögliche Selektion und umgekehrte Kausalität beachten. ",
          "Die lineare Glättung ist nur eine visuelle Orientierung."
        )
      ) +
      theme_project()
    
    save_adv_plot(p_ease, "22_Ease_vs_Participation.png", 9.5, 5.5)
  }
}


#===============================================================================
# 22 Figures: participant profiles and integrated three-level maps
#===============================================================================

if (create_figures && nrow(cluster_profiles) > 0) {
  p_clusters <- cluster_profiles %>%
    mutate(
      exploration_cluster = paste0("Profil ", exploration_cluster),
      Label = forcats::fct_reorder(Label, Mean_Z)
    ) %>%
    ggplot(aes(x = exploration_cluster, y = Label, fill = Mean_Z)) +
    geom_tile(color = unname(project_colors["white"]), linewidth = .8) +
    geom_text(aes(label = sprintf("%.2f", Mean_Z)), size = 3.1) +
    scale_fill_gradient2(
      low = unname(project_colors["red"]),
      mid = unname(project_colors["white"]),
      high = unname(project_colors["primary"]),
      midpoint = 0,
      name = "Mittelwert z"
    ) +
    labs(
      title = "Explorative Informationsnutzungs-Profile",
      subtitle = "PAM/Gower; K per Silhouette gewählt und über Subsamples stabilitätsgeprüft",
      x = NULL,
      y = NULL,
      caption = "Typologie nur hypothesengenerierend; Missingness fließt via Gower ein, Imputation nur für die PCA-Darstellung."
    ) +
    theme_project(legend_position = "right") +
    theme(panel.grid = element_blank())
  
  save_adv_plot(p_clusters, "23_Exploratory_Profile_Heatmap.png", 9.5, 7.5)
  
  if (nrow(pca_scores) > 0) {
    x_lab <- unique(pca_scores$PC1_Label)[1]
    y_lab <- unique(pca_scores$PC2_Label)[1]
    
    p_pca <- ggplot(
      pca_scores,
      aes(x = PC1, y = PC2, color = exploration_cluster)
    ) +
      geom_hline(yintercept = 0, color = unname(project_colors["grid"])) +
      geom_vline(xintercept = 0, color = unname(project_colors["grid"])) +
      geom_point(size = 3, alpha = .78) +
      scale_color_project() +
      labs(
        title = "Teilnehmende im explorativen Profilraum",
        subtitle = "PCA dient nur der zweidimensionalen Visualisierung der Clusterlösung",
        x = x_lab,
        y = y_lab,
        color = "Profil",
        caption = analysis_note
      ) +
      theme_project()
    
    save_adv_plot(p_pca, "24_Profile_PCA_Map.png", 8.5, 6.5)
  }
}


if (create_figures) {
  public_connection_map <- master %>%
    filter(!is.na(topic_evenness_adv), !is.na(share_current_affairs))
  
  if (nrow(public_connection_map) >= 8) {
    p_public_connection <- ggplot(
      public_connection_map,
      aes(
        x = topic_evenness_adv,
        y = share_current_affairs,
        color = share_incidental_strict,
        size = share_productive_serendipity_strict
      )
    ) +
      geom_point(alpha = .78) +
      scale_color_gradient(
        low = unname(project_colors["secondary"]),
        high = unname(project_colors["accent"]),
        labels = percent_format(accuracy = 1),
        name = "Incidentality\nstrikt"
      ) +
      scale_size_continuous(
        range = c(2, 7),
        labels = percent_format(accuracy = 1),
        name = "Produktive\nSerendipität"
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = "Explorative Public-Connection-Landschaft",
        subtitle = "Theoretisch informierte Visualisierung – keine validierte Operationalisierung eines Gesamt-Konstrukts",
        x = "Themen-Evenness (0–1)",
        y = "Anteil Aktuelles & öffentliche Angelegenheiten",
        caption = "Jeder Punkt ist eine Person. Evenness reduziert, eliminiert aber nicht die Abhängigkeit von Beobachtungsdichte; die Achsen/Ästhetiken bleiben getrennte Indikatoren."
      ) +
      theme_project()
    
    save_adv_plot(p_public_connection, "25_Public_Connection_Map.png", 9, 6.5)
  }
  
  
  three_level_map <- master %>%
    filter(
      !is.na(need_profile_alignment),
      !is.na(share_productive_serendipity_strict)
    )
  
  if (nrow(three_level_map) >= 8) {
    p_three_level <- ggplot(
      three_level_map,
      aes(
        x = need_profile_alignment,
        y = share_productive_serendipity_strict,
        color = reactivity_index,
        size = ease_index
      )
    ) +
      geom_hline(
        yintercept = safe_mean(three_level_map$share_productive_serendipity_strict),
        linetype = "22", color = unname(project_colors["grid"])
      ) +
      geom_vline(
        xintercept = 0,
        linetype = "22", color = unname(project_colors["grid"])
      ) +
      geom_point(alpha = .78) +
      geom_smooth(
        data = three_level_map,
        mapping = aes(x = need_profile_alignment, y = share_productive_serendipity_strict),
        inherit.aes = FALSE,
        method = "lm", se = TRUE,
        color = unname(project_colors["dark"]), linewidth = .7
      ) +
      scale_color_gradient(
        low = unname(project_colors["secondary"]),
        high = unname(project_colors["accent"]),
        na.value = unname(project_colors["medium"]),
        name = "Outro-\nReaktivität"
      ) +
      scale_size_continuous(range = c(2, 7), name = "Ease of Use") +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = "Drei Ebenen in einer Grafik: Need-Fit, Serendipität und Methodenerleben",
        subtitle = "Screening-Profilalignment × Diary-Serendipität; Farbe/Größe aus dem Outro",
        x = "Need–Content-Profilalignment",
        y = "Produktive Serendipität, strikt",
        caption = paste0(
          analysis_note,
          " Der lineare Smoother ist nur eine visuelle Orientierung; keine kausale Modellierung."
        )
      ) +
      theme_project()
    
    save_adv_plot(p_three_level, "26_Three_Level_Need_Serendipity_Reactivity.png", 9.5, 6.7)
  }
}


#===============================================================================
# 23 Optional alluvial: Screening → Diary → Outro
#===============================================================================
# Diese Grafik zeigt als reine Deskription, wie dominante Screening-Bedürfnisse,
# dominante beobachtete Diary-Domänen und Outro-Reaktivitätsgruppen zusammenlaufen.

alluvial_data <- master %>%
  filter(
    !is.na(dominant_information_need),
    !is.na(observed_dominant_need),
    !is.na(reactivity_group),
    !str_detect(dominant_information_need, "Kein eindeutiges"),
    !str_detect(observed_dominant_need, "Kein eindeutiges")
  ) %>%
  count(
    Screening_Need = dominant_information_need,
    Diary_Domain = observed_dominant_need,
    Reactivity = reactivity_group,
    name = "N"
  )

if (
  create_figures &&
  run_alluvial &&
  nrow(alluvial_data) > 0 &&
  requireNamespace("ggalluvial", quietly = TRUE)
) {
  p_alluvial <- ggplot(
    alluvial_data,
    aes(
      axis1 = Screening_Need,
      axis2 = Diary_Domain,
      axis3 = Reactivity,
      y = N
    )
  ) +
    ggalluvial::geom_alluvium(
      aes(fill = Screening_Need),
      width = 1 / 12,
      alpha = .65
    ) +
    ggalluvial::geom_stratum(
      width = 1 / 8,
      fill = unname(project_colors["lighter"]),
      color = unname(project_colors["medium"])
    ) +
    ggalluvial::geom_text(
      stat = "stratum",
      aes(label = after_stat(stratum)),
      size = 3
    ) +
    scale_x_discrete(
      limits = c("Screening-Need", "Diary-Domäne", "Outro-Reaktivität"),
      expand = c(.08, .08)
    ) +
    scale_fill_project() +
    labs(
      title = "Informationspfade über alle drei Erhebungsebenen",
      subtitle = "Dominantes Screening-Bedürfnis → dominante Diary-Domäne → Reaktivitäts-Tertil",
      x = NULL,
      y = "Teilnehmende",
      fill = "Screening-Need",
      caption = "Reaktivitäts-Tertile nur zur Visualisierung; kleine Ströme nicht überinterpretieren."
    ) +
    theme_project() +
    theme(panel.grid = element_blank())
  
  save_adv_plot(p_alluvial, "27_Alluvial_Screening_Diary_Outro.png", 13, 7)
} else if (run_alluvial && !requireNamespace("ggalluvial", quietly = TRUE)) {
  message("ggalluvial nicht installiert: Alluvial-Grafik wird übersprungen.")
}


#===============================================================================
# 24 Figure: multilevel model forest
#===============================================================================

if (create_figures && nrow(model_terms) > 0) {
  model_plot_data <- model_terms %>%
    filter(
      is.finite(Estimate_Exp),
      is.finite(CI95_Low),
      is.finite(CI95_High),
      Estimate_Exp > 0,
      CI95_Low > 0,
      CI95_High > 0
    ) %>%
    mutate(
      Term_Label = str_replace_all(Term, ":", " × "),
      Term_Label = str_replace_all(Term_Label, c(
        "platform" = "Plattform: ",
        "age_z_adv" = "Alter (z)",
        "intensity_z_adv" = "Nutzungsintensität (z)",
        "incidental_strict_within" = "Incidentality strikt – within",
        "incidental_strict_between" = "Incidentality strikt – between",
        "incidental_strict" = "Incidentality strikt",
        "post_need_fit_within" = "Post–Need-Fit – within",
        "post_need_fit_between" = "Post–Need-Fit – between",
        "study_day_z" = "Studientag (z)",
        "reactivity_z_adv" = "Reaktivität (z)"
      ))
    )
  
  p_models <- ggplot(
    model_plot_data,
    aes(x = Estimate_Exp, y = forcats::fct_reorder(Term_Label, Estimate_Exp))
  ) +
    geom_vline(xintercept = 1, linetype = "22", color = unname(project_colors["grid"]), linewidth = .8) +
    geom_segment(
      aes(
        x = CI95_Low, xend = CI95_High,
        yend = forcats::fct_reorder(Term_Label, Estimate_Exp)
      ),
      linewidth = .7,
      color = unname(project_colors["secondary"])
    ) +
    geom_point(size = 2.6, color = unname(project_colors["primary"])) +
    facet_wrap(~ Model, scales = "free_y", ncol = 2) +
    scale_x_log10() +
    labs(
      title = "Explorative multilevel Modelle",
      subtitle = "Exponentierte Koeffizienten und 95%-CI; Random Intercept für Person",
      x = "Odds Ratio (log-Skala)",
      y = NULL,
      caption = "Nicht präregistriert; Modelle werden nur bei Mindestfallzahlen geschätzt."
    ) +
    theme_project()
  
  save_adv_plot(p_models, "28_Multilevel_Model_Forest.png", 12, 10)
}


#===============================================================================
# 24A Figures: within-person platform comparisons
#===============================================================================

if (create_figures && nrow(platform_complementarity_summary) > 0) {
  p_platform_jsd <- platform_complementarity_summary %>%
    filter(!is.na(Mean_JSD)) %>%
    ggplot(aes(x = Mean_JSD, y = Pair)) +
    geom_errorbarh(
      aes(xmin = CI95_Low, xmax = CI95_High),
      height = .14,
      color = unname(project_colors["secondary"]),
      linewidth = .55
    ) +
    geom_point(size = 2.8, color = unname(project_colors["primary"])) +
    geom_text(
      aes(label = paste0("n=", N_Participants)),
      hjust = -.15,
      size = 3,
      color = unname(project_colors["medium"])
    ) +
    facet_wrap(~ Dimension, ncol = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .25)) +
    labs(
      title = "Wie unterschiedlich funktionieren Plattformen für dieselbe Person?",
      subtitle = "Within-Person Jensen–Shannon-Divergenz der Inhaltsprofile",
      x = "Jensen–Shannon-Divergenz (0 = gleich, 1 = maximal verschieden)",
      y = NULL,
      caption = paste0(
        "Nur Multi-Plattform-Personen mit ≥", minimum_posts_per_platform_within,
        " öffentlichen Posts auf beiden Plattformen; Personen-Bootstrap-CIs."
      )
    ) +
    theme_project()
  
  save_adv_plot(p_platform_jsd, "38_Within_Person_Platform_Complementarity.png", 10, 9)
}

if (create_figures && nrow(platform_behavior_differences_summary) > 0) {
  p_platform_diff <- platform_behavior_differences_summary %>%
    filter(!is.na(Mean_Difference)) %>%
    ggplot(aes(x = Mean_Difference, y = Pair)) +
    geom_vline(xintercept = 0, color = unname(project_colors["grid"]), linewidth = .7) +
    geom_errorbarh(
      aes(xmin = CI95_Low, xmax = CI95_High),
      height = .14,
      color = unname(project_colors["secondary"]),
      linewidth = .55
    ) +
    geom_point(size = 2.8, color = unname(project_colors["accent"])) +
    geom_text(
      aes(label = paste0("n=", N_Participants)),
      hjust = if_else(Mean_Difference >= 0, -.15, 1.15),
      size = 2.8,
      color = unname(project_colors["medium"])
    ) +
    facet_wrap(~ Outcome, ncol = 2, scales = "free_y") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Within-Person-Plattformunterschiede im Informationsverhalten",
      subtitle = "Gepaarte Unterschiede kontrollieren stabile Personenunterschiede deskriptiv",
      x = "Plattform 2 minus Plattform 1 (Prozentpunkte)",
      y = NULL,
      caption = paste0(
        "Keine randomisierte Plattformwirkung. Nur Personen mit ≥",
        minimum_posts_per_platform_within,
        " öffentlichen Posts auf beiden Plattformen; Personen-Bootstrap-CIs."
      )
    ) +
    theme_project(base_size = 9)
  
  save_adv_plot(p_platform_diff, "39_Within_Person_Platform_Behavior_Differences.png", 13, 9)
}


if (create_figures && nrow(reactivity_item_bridges) > 0) {
  p_reactivity_items_bridge <- reactivity_item_bridges %>%
    filter(!is.na(Spearman_Rho)) %>%
    mutate(
      Bridge = paste0(Predictor, " → ", Outcome),
      Bridge = forcats::fct_reorder(Bridge, Spearman_Rho)
    ) %>%
    ggplot(aes(x = Spearman_Rho, y = Bridge)) +
    geom_vline(xintercept = 0, color = unname(project_colors["grid"]), linewidth = .7) +
    geom_errorbarh(
      aes(xmin = CI95_Low, xmax = CI95_High),
      height = .15,
      color = unname(project_colors["secondary"]),
      linewidth = .55
    ) +
    geom_point(size = 2.8, color = unname(project_colors["accent"])) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25)) +
    labs(
      title = "Reaktivität item-spezifisch mit Diary-Mustern verknüpft",
      subtitle = "Nur inhaltlich passende Brücken; Personen-Bootstrap-CIs",
      x = "Spearman ρ",
      y = NULL,
      caption = "Feed-bezogene Reaktivitätsitems werden nicht pseudo-validiert, weil keine vollständige Feed-Exposition beobachtet wurde."
    ) +
    theme_project(base_size = 9)
  
  save_adv_plot(p_reactivity_items_bridge, "40_Reactivity_Item_Diary_Bridges.png", 12, 6.8)
}


#===============================================================================
# 24B Figures: methodological robustness, coverage and compositional diagnostics
#===============================================================================
# Diese Grafiken sind bewusst "wissenschaftliche Kontrollinstrumente": Sie zeigen
# Selektivität, Messdichte, Robustheit und die Auswirkungen plausibler analytischer
# Entscheidungen. Gerade bei einer kleinen, verschachtelten Diary-Stichprobe sind
# solche Abbildungen oft informativer als weitere Einzel-p-Werte.

if (create_figures && nrow(attrition_table) > 0) {
  p_attrition <- attrition_table %>%
    filter(!is.na(SMD)) %>%
    mutate(
      Measure = forcats::fct_reorder(Measure, abs(SMD)),
      Direction = if_else(SMD >= 0, "Retained höher", "Retained niedriger")
    ) %>%
    ggplot(aes(x = SMD, y = Measure, color = Transition)) +
    geom_vline(xintercept = 0, color = unname(project_colors["grid"]), linewidth = .7) +
    geom_vline(xintercept = c(-.2, .2), color = unname(project_colors["medium"]), linetype = "22", linewidth = .5) +
    geom_point(size = 3) +
    scale_color_project() +
    labs(
      title = "Selektionsdiagnostik zwischen den Erhebungsebenen",
      subtitle = "Standardisierte Mittelwertdifferenzen; positive Werte = retained sample höher",
      x = "Standardisierte Mittelwertdifferenz (SMD)",
      y = NULL,
      caption = "Deskriptive Diagnose, kein Signifikanztest. |SMD| ≈ .20 dient nur als visuelle Orientierung."
    ) +
    theme_project()
  
  save_adv_plot(p_attrition, "29_Attrition_Selection_SMD.png", 10.5, 7)
}


if (create_figures && nrow(coverage_heatmap) > 0) {
  p_coverage <- ggplot(
    coverage_heatmap,
    aes(x = factor(study_day), y = participant_order, fill = n_uploads)
  ) +
    geom_tile(color = unname(project_colors["white"]), linewidth = .25) +
    scale_fill_gradient(
      low = unname(project_colors["lighter"]),
      high = unname(project_colors["primary"]),
      na.value = unname(project_colors["grid"]),
      name = "Uploads"
    ) +
    labs(
      title = "Screenshot-Beobachtungsdichte über die sieben Diary-Tage",
      subtitle = "Eine Zeile = eine Person; nach Gesamtzahl der beobachteten Uploads sortiert",
      x = "Studientag",
      y = "Teilnehmende",
      caption = paste0(
        "Graue Zellen = an diesem Tag kein Screenshot im Screenshot-Level-Datensatz. ",
        "Das kann fehlende Teilnahme oder tatsächlich keinen Upload bedeuten und wird nicht als verifizierte Null interpretiert."
      )
    ) +
    theme_project(legend_position = "right") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank()
    )
  
  save_adv_plot(p_coverage, "30_Diary_Coverage_Heatmap.png", 9.5, 9)
}


if (create_figures && nrow(coverage_dependencies) > 0) {
  p_density <- coverage_dependencies %>%
    filter(!is.na(Spearman_Rho)) %>%
    mutate(Outcome = forcats::fct_reorder(Outcome, Spearman_Rho)) %>%
    ggplot(aes(x = Spearman_Rho, y = Outcome)) +
    geom_vline(xintercept = 0, color = unname(project_colors["grid"]), linewidth = .7) +
    geom_errorbarh(
      aes(xmin = CI95_Low, xmax = CI95_High),
      height = .18,
      color = unname(project_colors["medium"]),
      linewidth = .65
    ) +
    geom_point(size = 3.1, color = unname(project_colors["accent"])) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25)) +
    labs(
      title = "Wie stark hängen Diary-Marker von der Beobachtungsdichte ab?",
      subtitle = "Zahl öffentlicher Posts × Diversität, Neuheit und Serendipität",
      x = "Spearman ρ mit Zahl öffentlicher Posts",
      y = NULL,
      caption = "Personen-Bootstrap-CI. Starke Zusammenhänge können auf mechanische Messdichteeffekte hinweisen."
    ) +
    theme_project(legend_position = "none")
  
  save_adv_plot(p_density, "31_Measurement_Density_Dependencies.png", 10, 6.5)
}


# Rarefaction macht den Messdichteeffekt für Richness direkt sichtbar: Rohwerte
# steigen typischerweise mit der Zahl beobachteter Posts; rarefied Richness fragt,
# wie viele Kategorien bei derselben festen Postzahl erwartet würden.
if (create_figures) {
  rarefaction_plot_data <- bind_rows(
    master %>%
      transmute(
        participant, n_public_content_posts,
        Dimension = "Themen",
        Raw = topic_richness,
        Rarefied = topic_richness_rarefied
      ),
    master %>%
      transmute(
        participant, n_public_content_posts,
        Dimension = "Quellen",
        Raw = source_richness,
        Rarefied = source_richness_rarefied
      ),
    master %>%
      transmute(
        participant, n_public_content_posts,
        Dimension = "Accounts",
        Raw = n_unique_account_names,
        Rarefied = account_richness_rarefied
      )
  ) %>%
    pivot_longer(
      cols = c(Raw, Rarefied),
      names_to = "Estimand",
      values_to = "Richness"
    ) %>%
    filter(!is.na(n_public_content_posts), !is.na(Richness)) %>%
    mutate(
      Estimand = recode(
        Estimand,
        Raw = "Roh-Richness",
        Rarefied = paste0("Rarefied (n=", rarefaction_post_count, ")")
      )
    )
  
  if (nrow(rarefaction_plot_data) > 0) {
    p_rarefaction <- ggplot(
      rarefaction_plot_data,
      aes(x = n_public_content_posts, y = Richness, color = Estimand)
    ) +
      geom_point(alpha = .45, size = 2) +
      geom_smooth(method = "lm", se = FALSE, linewidth = .75) +
      facet_wrap(~ Dimension, scales = "free_y") +
      scale_color_project(values = unname(project_colors[c("accent", "primary")])) +
      labs(
        title = "Richness und Messdichte: Rohwerte vs. Rarefaction",
        subtitle = paste0(
          "Erwartete Richness bei gleicher Stichprobentiefe von ",
          rarefaction_post_count,
          " beobachteten Posts"
        ),
        x = "Öffentlich relevante Posts pro Person",
        y = "Kategorielle Richness",
        color = NULL,
        caption = paste0(
          "Rarefaction standardisiert nur die beobachtete Uploadstichprobe; ",
          "sie rekonstruiert keine nicht beobachteten Feed-Inhalte. ",
          "Linien dienen nur der visuellen Orientierung."
        )
      ) +
      theme_project()
    
    save_adv_plot(p_rarefaction, "31b_Rarefied_Richness.png", 10.5, 6.2)
  }
}


if (create_figures && nrow(need_content_matched_coda) > 0) {
  need_compare <- bind_rows(
    need_content_matched_coda %>%
      transmute(Need, Estimand = "CLR / compositional", Spearman_Rho, CI95_Low, CI95_High),
    need_content_matched_raw %>%
      transmute(Need, Estimand = "Raw share", Spearman_Rho, CI95_Low, CI95_High)
  ) %>%
    filter(!is.na(Spearman_Rho))
  
  p_need_coda <- ggplot(
    need_compare,
    aes(x = Spearman_Rho, y = Need, color = Estimand)
  ) +
    geom_vline(xintercept = 0, color = unname(project_colors["grid"]), linewidth = .7) +
    geom_errorbarh(
      aes(xmin = CI95_Low, xmax = CI95_High),
      height = .16,
      position = position_dodge(width = .45),
      linewidth = .6
    ) +
    geom_point(position = position_dodge(width = .45), size = 3) +
    scale_color_manual(values = c(
      "CLR / compositional" = unname(project_colors["primary"]),
      "Raw share" = unname(project_colors["accent"])
    )) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25)) +
    labs(
      title = "Need–Content-Zusammenhänge: kompositional vs. Raw Shares",
      subtitle = "Theoretisch gematchte Beziehungen; Personen-Bootstrap-CIs",
      x = "Spearman ρ",
      y = NULL,
      caption = "CLR ist die bevorzugte Exploration; Raw Shares dienen als transparente Sensitivitätsanalyse."
    ) +
    theme_project()
  
  save_adv_plot(p_need_coda, "32_Need_Content_CoDA_vs_Raw.png", 10, 6)
}


if (create_figures && nrow(robustness_specs) > 0) {
  p_multiverse <- robustness_specs %>%
    filter(!is.na(Estimate)) %>%
    mutate(
      Relation = factor(Relation),
      Specification = forcats::fct_reorder(Specification, Estimate)
    ) %>%
    ggplot(aes(x = Estimate, y = Specification, color = Relation)) +
    geom_vline(xintercept = 0, color = unname(project_colors["grid"]), linewidth = .7) +
    geom_errorbarh(
      aes(xmin = CI95_Low, xmax = CI95_High),
      height = .14,
      linewidth = .45,
      alpha = .55
    ) +
    geom_point(size = 2.3, alpha = .85) +
    facet_wrap(~ Relation, scales = "free_y", ncol = 2) +
    scale_color_project() +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .5)) +
    labs(
      title = "Specification multiverse: Bleiben zentrale Muster stabil?",
      subtitle = "Plausible Operationalisierungen, Mindest-Postzahlen und Pseudocounts nebeneinander",
      x = "Spearman ρ",
      y = NULL,
      caption = "Kein Picking der günstigsten Spezifikation: Robustheit wird über Richtung und Größenordnung bewertet."
    ) +
    theme_project(base_size = 9, legend_position = "none")
  
  save_adv_plot(p_multiverse, "33_Specification_Multiverse.png", 15, 12)
}


if (create_figures && nrow(novelty_permutation) > 0) {
  p_novelty_null <- ggplot(novelty_permutation, aes(x = study_day)) +
    geom_ribbon(
      aes(ymin = Null_Low, ymax = Null_High),
      fill = unname(project_colors["light"]),
      alpha = .9
    ) +
    geom_line(aes(y = Null_Mean), color = unname(project_colors["medium"]), linetype = "22", linewidth = .8) +
    geom_line(aes(y = Observed), color = unname(project_colors["primary"]), linewidth = 1.1) +
    geom_point(aes(y = Observed), color = unname(project_colors["primary"]), size = 2.8) +
    scale_x_continuous(breaks = 1:7) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Themenneuheit über die Zeit: beobachtet vs. mechanisches Nullmodell",
      subtitle = "Graues Band = 95%-Permutationsbereich bei zufälliger Themenreihenfolge innerhalb der Person",
      x = "Studientag",
      y = "Anteil erstmals beobachteter Themen",
      caption = "Teilnehmergewichtete Tagesraten; die erwartete mechanische Abnahme von First-occurrence novelty wird explizit berücksichtigt."
    ) +
    theme_project(legend_position = "none")
  
  save_adv_plot(p_novelty_null, "34_Novelty_Permutation_Null.png", 9.5, 5.8)
}


if (create_figures && nrow(topic_coda_pca_scores) > 0) {
  x_lab <- unique(topic_coda_pca_scores$PC1_Label)[1]
  y_lab <- unique(topic_coda_pca_scores$PC2_Label)[1]
  
  p_topic_coda <- ggplot(
    topic_coda_pca_scores,
    aes(x = PC1, y = PC2, color = dominant_information_need)
  ) +
    geom_hline(yintercept = 0, color = unname(project_colors["grid"])) +
    geom_vline(xintercept = 0, color = unname(project_colors["grid"])) +
    geom_point(size = 3, alpha = .78) +
    scale_color_project() +
    labs(
      title = "Relative Themenrepertoires im kompositionalen Raum",
      subtitle = "PCA auf CLR-transformierter Themenkomposition; Farbe = dominantes Screening-Bedürfnis",
      x = x_lab,
      y = y_lab,
      color = "Dominantes Need",
      caption = "CLR berücksichtigt die relative Natur von Themenanteilen; rein explorative Repertoirekarte."
    ) +
    theme_project()
  
  save_adv_plot(p_topic_coda, "35_Topic_Compositional_PCA.png", 9.5, 6.5)
}


if (create_figures && nrow(icc_results) > 0) {
  p_icc <- icc_results %>%
    filter(!is.na(ICC)) %>%
    mutate(Outcome = forcats::fct_reorder(Outcome, ICC)) %>%
    ggplot(aes(x = ICC, y = Outcome)) +
    geom_col(width = .58, fill = unname(project_colors["secondary"])) +
    geom_text(aes(label = sprintf("%.2f", ICC)), hjust = -.08, size = 3.3) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, .12))) +
    labs(
      title = "Wie stark clustern zentrale Diary-Outcomes innerhalb von Personen?",
      subtitle = "Logistische Random-Intercept-ICCs auf Latent-Response-Skala",
      x = "ICC",
      y = NULL,
      caption = "Dient der Modellbegründung; kein inhaltlicher Haupteffekt."
    ) +
    theme_project(legend_position = "none")
  
  save_adv_plot(p_icc, "36_Diary_Outcome_ICCs.png", 9, 5.5)
}


if (create_figures && nrow(serendipity_transitions) > 0) {
  p_ser_transition <- ggplot(
    serendipity_transitions,
    aes(x = Transition, y = Rate, fill = Definition)
  ) +
    geom_col(position = position_dodge(width = .72), width = .62) +
    geom_text(
      aes(label = paste0(round(100 * Rate, 1), "%")),
      position = position_dodge(width = .72),
      vjust = -.4,
      size = 3.2
    ) +
    scale_fill_project(values = unname(project_colors[c("primary", "accent")])) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .12))) +
    labs(
      title = "Bedingte Übergänge im Serendipity-Prozess",
      subtitle = "Was passiert, nachdem ein Post incidental bzw. incidental + neu ist?",
      x = NULL,
      y = "Bedingte Wahrscheinlichkeit",
      caption = "Ergänzt den kumulativen Funnel um Übergangsraten mit explizitem Nenner."
    ) +
    theme_project()
  
  save_adv_plot(p_ser_transition, "37_Serendipity_Conditional_Transitions.png", 10, 5.8)
}



# Zentrale Missingness-/QC-Diagnostik auf Personenebene. Sie bleibt kompakt und
# dient der Interpretation der integrierten Analysen, nicht als eigener Ergebnisblock.
central_method_vars <- c(
  intro_age_num = "Alter",
  incidentality_index = "Screening-Incidentality",
  n_public_content_posts = "Öffentliche Posts",
  share_incidental_strict = "Diary-Incidentality strikt",
  mean_post_need_fit_z = "Mittlerer Post–Need-Fit",
  reactivity_index = "Outro-Reaktivität",
  ease_index = "Outro Ease of Use"
)

central_missingness <- imap_dfr(central_method_vars, function(label, variable) {
  if (!variable %in% names(master)) return(tibble())
  tibble(
    Measure = label,
    N_Total = nrow(master),
    N_Valid = sum(!is.na(master[[variable]])),
    N_Missing = sum(is.na(master[[variable]])),
    Percent_Missing = safe_percent(N_Missing, N_Total)
  )
})

qc_summary <- tibble(
  Measure = c(
    "Nicht beurteilbare Uploads",
    "Upload erfüllt Public-Relevance-Gate (unter beurteilbaren Uploads)"
  ),
  N = c(
    sum(clean_numeric(daily_all$public_rel_coded) == -1L, na.rm = TRUE),
    sum(daily_all$public_relevance == 1L, na.rm = TRUE)
  ),
  Denominator = c(
    nrow(daily_all),
    sum(!is.na(daily_all$public_relevance))
  )
) %>%
  mutate(Percent = safe_percent(N, Denominator))

# Content-Coding-Reliabilität kann aus dem finalen Ein-Code-pro-Screenshot-Datensatz
# nicht geschätzt werden. Wir dokumentieren lediglich, wie viele Codierende im
# finalen Sheet vorkommen. Bei mehreren Codierenden erinnert eine Warnung daran,
# eine separate doppelt codierte Reliabilitätsstichprobe zu berichten.
coder_summary <- if ("coder" %in% names(daily_all)) {
  daily_all %>%
    mutate(Coder = clean_text(coder)) %>%
    filter(!is.na(Coder)) %>%
    count(Coder, name = "N_Posts") %>%
    mutate(Percent_Posts = safe_percent(N_Posts, sum(N_Posts)))
} else {
  tibble(Coder = character(), N_Posts = integer(), Percent_Posts = double())
}

n_coders_final <- n_distinct(coder_summary$Coder)

if (n_coders_final > 1) {
  warning(
    "Mehrere Codierende sind im finalen Coding Sheet vertreten (n = ",
    n_coders_final,
    "). Intercoder-Reliabilität ist aus einem finalen Code pro Screenshot nicht ",
    "schätzbar; hierfür eine separat doppelt codierte Reliabilitätsstichprobe verwenden."
  )
}

coder_method_diagnostic <- tibble(
  Section = "Content-coding reliability",
  Measure = "Codierende im finalen Coding Sheet",
  N = n_coders_final,
  Participants = NA_integer_,
  Estimate = NA_real_,
  CI95_Low = NA_real_,
  CI95_High = NA_real_,
  P_Adjusted_BH = NA_real_,
  Note = case_when(
    n_coders_final == 0 ~
      "Coder-Variable fehlt/ist leer; Intercoder-Reliabilität in separater Reliabilitätsdatei dokumentieren.",
    n_coders_final == 1 ~
      "Ein Coder im finalen Sheet; Intercoder-Reliabilität ist daraus nicht schätzbar.",
    TRUE ~
      "Mehrere Coder, aber nur ein finaler Code je Screenshot; separate Double-Coding-Reliabilität erforderlich."
  )
)

# Beobachtungsdichte pro Studientag: Anteil der eingeschlossenen Personen mit
# mindestens einem aufgezeichneten Screenshot an diesem Tag. Das ist KEIN sicherer
# Questionnaire-Compliance-Indikator, weil Tage ohne Screenshot mehrdeutig sind.
temporal_coverage_diagnostic <- participant_day %>%
  group_by(study_day) %>%
  summarise(
    N_Participants = n_distinct(participant),
    N_With_Upload = sum(day_has_upload == 1L, na.rm = TRUE),
    Upload_Day_Rate = safe_mean(day_has_upload),
    .groups = "drop"
  )


#===============================================================================
# 25 Publication-oriented profile and model tables
#===============================================================================

profile_publication <- if (nrow(cluster_profiles) > 0) {
  cluster_profiles %>%
    transmute(
      Cluster = paste0("Profil ", exploration_cluster),
      Measure = Label,
      N,
      Mean_Z = round(Mean_Z, 3),
      Note = if_else(profile_solution_interpretable,
                     "Explorative PAM/Gower-Typologie; Stabilitätskriterien erfüllt",
                     "Explorative PAM/Gower-Typologie; Lösung instabil/schwach – nicht substantiv interpretieren")
    )
} else {
  tibble(
    Cluster = character(), Measure = character(), N = integer(),
    Mean_Z = double(), Note = character()
  )
}

model_publication <- if (nrow(model_terms) > 0) {
  model_terms %>%
    mutate(
      Term_Pretty = str_replace_all(Term, ":", " × "),
      Term_Pretty = str_replace_all(Term_Pretty, c(
        "platform" = "Plattform: ",
        "age_z_adv" = "Alter (z)",
        "intensity_z_adv" = "Nutzungsintensität (z)",
        "incidental_strict_within" = "Incidentality strikt – within",
        "incidental_strict_between" = "Incidentality strikt – between",
        "incidental_strict" = "Incidentality strikt",
        "post_need_fit_within" = "Post–Need-Fit – within",
        "post_need_fit_between" = "Post–Need-Fit – between",
        "study_day_z" = "Studientag (z)",
        "reactivity_z_adv" = "Reaktivität (z)"
      ))
    ) %>%
    transmute(
      Model,
      Effect_Scale,
      Term = Term_Pretty,
      Estimate = round(Estimate_Exp, 3),
      CI95_Low = round(CI95_Low, 3),
      CI95_High = round(CI95_High, 3),
      P_Value = round(P_Value, 4),
      P_Adjusted_BH = round(P_Adjusted_BH, 4),
      Note = "Exploratives GLMM; Random Intercept Person; zeitvariable Prädiktoren wo relevant within/between getrennt"
    )
} else {
  tibble(
    Model = character(), Effect_Scale = character(), Term = character(),
    Estimate = double(), CI95_Low = double(), CI95_High = double(),
    P_Value = double(), P_Adjusted_BH = double(), Note = character()
  )
}


selection_publication <- attrition_table %>%
  select(
    Transition, Measure, N_Not_Retained, N_Retained,
    M_Not_Retained, M_Retained, SMD, Flag, Note
  )

robustness_publication <- if (nrow(robustness_summary) > 0) {
  robustness_summary %>%
    mutate(
      across(c(Median_Estimate, Min_Estimate, Max_Estimate, Proportion_Positive, Proportion_Negative), ~ round(.x, 3))
    )
} else {
  tibble(
    Relation = character(), N_Specifications = integer(),
    Median_Estimate = double(), Min_Estimate = double(), Max_Estimate = double(),
    Proportion_Positive = double(), Proportion_Negative = double(),
    Direction_Stable = logical(), Note = character()
  )
}

method_diagnostics_publication <- bind_rows(
  icc_results %>%
    transmute(
      Section = "Diary clustering / ICC",
      Measure = Outcome,
      N,
      Participants,
      Estimate = ICC,
      CI95_Low = NA_real_,
      CI95_High = NA_real_,
      P_Adjusted_BH = NA_real_,
      Note = "Logistic random-intercept ICC auf Latent-Response-Skala"
    ),
  coverage_dependencies %>%
    transmute(
      Section = "Measurement density",
      Measure = Outcome,
      N,
      Participants = NA_integer_,
      Estimate = Spearman_Rho,
      CI95_Low,
      CI95_High,
      P_Adjusted_BH,
      Note = "Zusammenhang mit Zahl öffentlicher Posts; Personen-Bootstrap-CI"
    ),
  central_missingness %>%
    transmute(
      Section = "Integrated missingness",
      Measure,
      N = N_Valid,
      Participants = N_Total,
      Estimate = Percent_Missing,
      CI95_Low = NA_real_,
      CI95_High = NA_real_,
      P_Adjusted_BH = NA_real_,
      Note = "Estimate = Prozent fehlend in integrierter Personenstichprobe"
    ),
  qc_summary %>%
    transmute(
      Section = "Upload/coding QC",
      Measure,
      N,
      Participants = NA_integer_,
      Estimate = Percent,
      CI95_Low = NA_real_,
      CI95_High = NA_real_,
      P_Adjusted_BH = NA_real_,
      Note = "Methodischer QC-Marker; keine Prävalenz der Feed-Inhalte"
    ),
  temporal_coverage_diagnostic %>%
    transmute(
      Section = "Temporal coverage",
      Measure = paste0("Studientag ", study_day, ": mindestens ein Upload"),
      N = N_With_Upload,
      Participants = N_Participants,
      Estimate = 100 * Upload_Day_Rate,
      CI95_Low = NA_real_,
      CI95_High = NA_real_,
      P_Adjusted_BH = NA_real_,
      Note = "Estimate = Prozent mit beobachtetem Upload-Tag; Tage ohne Screenshot sind mehrdeutig."
    ),
  coder_method_diagnostic
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))



#===============================================================================
# 26 Single Excel workbook with publication-ready tables
#===============================================================================
# Excel enthält nur Tabellen, die sich unmittelbar für Manuskript/Appendix oder
# Ergebnisdiskussion eignen. Breite Discovery-Ausgaben bleiben Grafik/Konsole.

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = unname(project_colors["primary"]),
  halign = "center",
  valign = "center",
  border = "Bottom"
)

add_excel_sheet(workbook, "Table_1_Integrated", integrated_sample_table, header_style)
add_excel_sheet(workbook, "Table_2_Theory_Cor", theory_correlations, header_style)
add_excel_sheet(workbook, "Table_3_Need_Content", need_content_publication, header_style)
add_excel_sheet(workbook, "Table_4_Serendipity", serendipity_publication, header_style)
add_excel_sheet(workbook, "Table_5_Calibration", calibration_publication, header_style)
add_excel_sheet(workbook, "Table_6_Selection", selection_publication, header_style)
add_excel_sheet(workbook, "Table_7_Robustness", robustness_publication, header_style)
add_excel_sheet(workbook, "Table_8_Platform_Within", platform_within_publication, header_style)
add_excel_sheet(workbook, "Table_9_Method_Diagnostics", method_diagnostics_publication, header_style)
add_excel_sheet(workbook, "Table_10_Mixed_Models", model_publication, header_style)
add_excel_sheet(workbook, "Table_11_Profiles", profile_publication, header_style)

openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = overwrite_outputs
)


#===============================================================================
# 27 Console report: discovery-oriented, compact and method-aware
#===============================================================================

cat(
  "\n============================================================\n",
  "ADVANCED EXPLORATION COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat("Mode: ", analysis_note, "\n", sep = "")
cat("Screening participants: ", nrow(screening), "\n", sep = "")
cat("Diary participants: ", nrow(daily_person), "\n", sep = "")
cat("Diary screenshots: ", nrow(daily_all), "\n", sep = "")
cat("Public content posts: ", nrow(daily_posts), "\n", sep = "")
cat("Outro participants available: ", sum(master$outro_available, na.rm = TRUE), "\n", sep = "")
cat("Integrated participant sample: ", nrow(master), "\n", sep = "")

cat("\n--- Coverage / measurement density ---\n")
print(coverage_summary)

cat("\nUpload/coding QC (nicht als Feed-Prävalenz interpretieren):\n")
print(qc_summary %>% mutate(Percent = round(Percent, 1)))

cat("\nObserved upload-day coverage by study day:\n")
print(
  temporal_coverage_diagnostic %>%
    mutate(Upload_Day_Percent = round(100 * Upload_Day_Rate, 1)) %>%
    select(study_day, N_Participants, N_With_Upload, Upload_Day_Percent),
  n = Inf
)

cat("\nContent coding / coder coverage:\n")
if (nrow(coder_summary) > 0) {
  print(coder_summary %>% mutate(Percent_Posts = round(Percent_Posts, 1)), n = Inf)
} else {
  cat("No non-missing coder labels in final coding sheet.\n")
}
cat(coder_method_diagnostic$Note[[1]], "\n")

cat("\nIntegrated missingness (central measures):\n")
print(central_missingness)

if (nrow(attrition_table) > 0) {
  cat("\nLargest selection differences by |SMD|:\n")
  attrition_table %>%
    filter(!is.na(SMD)) %>%
    arrange(desc(abs(SMD))) %>%
    select(Transition, Measure, SMD, Flag) %>%
    slice_head(n = 8) %>%
    print(n = 8)
}

if (nrow(coverage_dependencies) > 0) {
  cat("\nDiary markers most related to number of public posts:\n")
  coverage_dependencies %>%
    filter(!is.na(Spearman_Rho)) %>%
    arrange(desc(abs(Spearman_Rho))) %>%
    select(Outcome, N, Spearman_Rho, CI95_Low, CI95_High, P_Adjusted_BH) %>%
    slice_head(n = 8) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    print(n = 8)
}

cat("\n--- Strongest cross-level correlations (broad discovery atlas) ---\n")
correlation_atlas %>%
  filter(!is.na(Spearman_Rho)) %>%
  arrange(desc(abs(Spearman_Rho))) %>%
  select(Predictor, Outcome, N, Spearman_Rho, P_Adjusted_BH) %>%
  slice_head(n = 15) %>%
  mutate(
    Spearman_Rho = round(Spearman_Rho, 3),
    P_Adjusted_BH = round(P_Adjusted_BH, 4)
  ) %>%
  print(n = 15)

cat("\n--- Matched Need–Content relations: preferred CLR analysis ---\n")
need_content_matched_coda %>%
  select(Need, Outcome, N, Spearman_Rho, CI95_Low, CI95_High,
         LOO_Min, LOO_Max, LOO_Sign_Stable, P_Value, P_Adjusted_BH) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf)

cat("\nRaw-share sensitivity for the same Need–Content relations:\n")
need_content_matched_raw %>%
  select(Need, Outcome, N, Spearman_Rho, CI95_Low, CI95_High,
         LOO_Min, LOO_Max, LOO_Sign_Stable) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf)

cat(
  "\nDominant need/domain proxy match: ",
  round(100 * safe_mean(master$dominant_need_match), 1),
  "% (only uniquely classifiable participants; descriptive proxy)\n",
  sep = ""
)

cat(
  "Need-profile alignment proxy: M = ",
  round(safe_mean(master$need_profile_alignment), 3),
  ", SD = ",
  round(safe_sd(master$need_profile_alignment), 3),
  "\n",
  sep = ""
)

if (nrow(reactivity_item_bridges) > 0) {
  cat("\n--- Item-specific reactivity ↔ Diary bridges ---\n")
  reactivity_item_bridges %>%
    select(Predictor, Outcome, N, Spearman_Rho, CI95_Low, CI95_High,
           LOO_Min, LOO_Max, LOO_Sign_Stable, P_Adjusted_BH) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    print(n = Inf)
}

cat("\n--- Productive serendipity ---\n")
cat(
  "Strict, post-weighted: ",
  round(100 * safe_mean(daily_posts$productive_serendipity_strict), 1),
  "% of public posts\n",
  sep = ""
)
cat(
  "Broad, post-weighted: ",
  round(100 * safe_mean(daily_posts$productive_serendipity_broad), 1),
  "% of public posts\n",
  sep = ""
)
cat("Conditional transitions:\n")
print(serendipity_transitions %>% mutate(Percent = round(Percent, 1)))

if (nrow(novelty_permutation) > 0) {
  cat("\n--- Novelty vs mechanical permutation null (participant-weighted) ---\n")
  novelty_permutation %>%
    select(study_day, Observed, Null_Mean, Null_Low, Null_High, Observed_Minus_Null) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    print(n = Inf)
}

if (nrow(robustness_summary) > 0) {
  cat("\n--- Specification robustness ---\n")
  robustness_summary %>%
    arrange(desc(Direction_Stable), Relation) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    print(n = Inf)
}

if (nrow(platform_complementarity_summary) > 0) {
  cat("\n--- Within-person platform complementarity ---\n")
  platform_complementarity_summary %>%
    arrange(Dimension, desc(Mean_JSD)) %>%
    mutate(across(c(Mean_JSD, CI95_Low, CI95_High), ~ round(.x, 3))) %>%
    print(n = Inf)
}

if (nrow(platform_behavior_differences_summary) > 0) {
  cat("\n--- Within-person platform behavior differences ---\n")
  platform_behavior_differences_summary %>%
    arrange(Outcome, Pair) %>%
    mutate(across(c(Mean_Difference, CI95_Low, CI95_High), ~ round(.x, 3))) %>%
    print(n = Inf)
}

if (nrow(icc_results) > 0) {
  cat("\n--- Diary clustering / ICC ---\n")
  print(icc_results %>% mutate(ICC = round(ICC, 3)), n = Inf)
}

if (nrow(cluster_choice) > 0) {
  cat("\n--- Exploratory profile solution ---\n")
  print(cluster_choice)
  cat("Selected K: ", cluster_choice$K[cluster_choice$Selected][1], "\n", sep = "")
  cat("Interpretable/stable enough for hypothesis generation: ", profile_solution_interpretable, "\n", sep = "")
}

if (nrow(model_status) > 0) {
  cat("\n--- Multilevel model status ---\n")
  print(model_status, n = Inf)
}

cat("\nExcel: ", output_excel, "\n", sep = "")
cat("Figures: ", figure_folder, "\n", sep = "")
cat(
  "============================================================\n",
  "ALL RESULTS IN THIS SCRIPT ARE EXPLORATORY.\n",
  "Effect sizes, uncertainty, clustering and robustness take precedence over\n",
  "isolated significance decisions.\n",
  "============================================================\n",
  sep = ""
)
