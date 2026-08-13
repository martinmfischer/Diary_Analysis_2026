################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    03_Simulate_Daily_Coding.R
# Purpose: Füllt die manuellen Coding-Spalten des in 04b geladenen Coding Sheets
#          mit reproduzierbaren simulierten Werten für Pipeline-Tests.
#
# Einbindung in 04b, NACH dem Einlesen von `screening` und `coding`:
#
#   if (simulate_coding) {
#     source(file.path("02_Scripts", "03_Simulate_Daily_Coding.R"))
#   }
#
# Erwartet bereits im Environment:
#   coding, screening, topic_levels, source_levels, platform_levels
#   sowie die Helper clean_text() und clean_numeric().
#
# Wichtig:
#   - Standardmäßig werden NUR fehlende manuelle Codes ergänzt.
#   - Teilnehmendenangaben (Incidentality, Interaktionen, Kontexte) bleiben real.
#   - public_rel_coded = 0/-1 erhält keine Topic/Source/Format-Codierung.
#   - Die Simulation dient ausschließlich dem Test der Analyse-Pipeline.
################################################################################


#===============================================================================
# 01 Settings  [NUR FUER PIPELINE-TESTS: fuer reale Analyse simulate_coding <- FALSE]
#===============================================================================

# Einstellungen können in 04b vor dem source() gesetzt werden. Fehlen sie,
# verwendet das Modul diese zurückhaltenden Defaults.
if (!exists("simulation_seed")) simulation_seed <- 20260810
if (!exists("simulation_overwrite_existing")) simulation_overwrite_existing <- FALSE
if (!exists("simulation_use_screening_patterns")) simulation_use_screening_patterns <- TRUE
if (!exists("simulation_platform_mismatch_rate")) simulation_platform_mismatch_rate <- 0.03

# Da die Teilnehmenden ausdrücklich öffentlich relevante Beiträge hochladen
# sollten, ist Code 1 in der Simulation klar dominant.
if (!exists("simulation_public_relevance_prob")) {
  simulation_public_relevance_prob <- c(`1` = 0.94, `0` = 0.05, `-1` = 0.01)
}


#===============================================================================
# 02 Preconditions
#===============================================================================

# Das Modul greift bewusst auf die bereits in 04b definierten Kategorien zurück,
# damit keine zweite, potenziell abweichende Version des Codebuchs entsteht.
required_objects <- c(
  "coding", "screening", "topic_levels", "source_levels", "platform_levels"
)
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]

if (length(missing_objects) > 0) {
  stop(
    "Simulationsmodul: Folgende Objekte fehlen: ",
    paste(missing_objects, collapse = ", "),
    ". Das Modul erst nach dem Einlesen von Screening und Coding sourcen."
  )
}

required_functions <- c("clean_text", "clean_numeric")
missing_functions <- required_functions[!vapply(required_functions, exists, logical(1))]
if (length(missing_functions) > 0) {
  stop(
    "Simulationsmodul: Helper fehlen: ",
    paste(missing_functions, collapse = ", "),
    ". Zuerst 00_Helpers.R sourcen."
  )
}

required_columns <- c(
  "participant", "public_rel_coded", "topic_coded", "source_coded",
  "source_name_coded", "platform_coded", "media_format",
  "coding_completed", "platform_reported"
)
missing_columns <- setdiff(required_columns, names(coding))
if (length(missing_columns) > 0) {
  stop(
    "Simulationsmodul: Im Coding Sheet fehlen: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (abs(sum(simulation_public_relevance_prob) - 1) > 1e-8) {
  stop("simulation_public_relevance_prob muss sich zu 1 summieren.")
}

set.seed(simulation_seed)


#===============================================================================
# 03 Screening information for plausible patterns
#===============================================================================

# Die Simulation nutzt optional die vier Informationsbedürfnisse, damit spätere
# Screening–Diary-Analysen nicht nur vollständig zufällige Testmuster zeigen.
screening_sim <- screening

if (!"participant" %in% names(screening_sim)) {
  participant_candidate <- intersect(
    c("personalParticipantCode", "personal_participant_code"),
    names(screening_sim)
  )
  if (length(participant_candidate) == 0) {
    stop("Simulation module: no participant identifier found in the screening data.")
  }
  screening_sim$participant <- screening_sim[[participant_candidate[[1]]]]
}

need_variables <- c(
  "intro_ib_undirected", "intro_ib_thematic",
  "intro_ib_social", "intro_ib_problem"
)
for (v in need_variables) {
  if (!v %in% names(screening_sim)) screening_sim[[v]] <- NA_real_
}

screening_sim <- screening_sim |>
  dplyr::transmute(
    participant = clean_text(participant),
    intro_ib_undirected = clean_numeric(intro_ib_undirected),
    intro_ib_thematic = clean_numeric(intro_ib_thematic),
    intro_ib_social = clean_numeric(intro_ib_social),
    intro_ib_problem = clean_numeric(intro_ib_problem)
  ) |>
  dplyr::distinct(participant, .keep_all = TRUE)

simulation_data <- coding |>
  dplyr::mutate(.simulation_row = dplyr::row_number()) |>
  dplyr::left_join(screening_sim, by = "participant")


#===============================================================================
# 04 Simulate public relevance and platform validation
#===============================================================================

# Public relevance wird zuerst simuliert, weil sie als Gate bestimmt, ob Topic,
# Source und Format überhaupt benötigt werden.
n <- nrow(simulation_data)
sim_public <- sample(
  as.integer(names(simulation_public_relevance_prob)),
  size = n,
  replace = TRUE,
  prob = simulation_public_relevance_prob
)

current_public <- suppressWarnings(as.integer(clean_numeric(simulation_data$public_rel_coded)))
if (simulation_overwrite_existing) {
  simulation_data$public_rel_coded <- sim_public
} else {
  simulation_data$public_rel_coded <- dplyr::coalesce(current_public, sim_public)
}

# Die berichtete Plattform wird fast immer bestätigt. Eine kleine Zahl zufälliger
# Korrekturen hält den Platform-Mismatch-Check der Analyse testbar.
reported_platform <- clean_text(simulation_data$platform_reported)
current_platform <- clean_text(simulation_data$platform_coded)
sim_platform <- reported_platform

for (i in seq_len(n)) {
  if (is.na(sim_platform[[i]]) || !sim_platform[[i]] %in% platform_levels) {
    sim_platform[[i]] <- sample(platform_levels, 1)
  } else if (runif(1) < simulation_platform_mismatch_rate) {
    sim_platform[[i]] <- sample(setdiff(platform_levels, sim_platform[[i]]), 1)
  }
}

if (simulation_overwrite_existing) {
  simulation_data$platform_coded <- sim_platform
} else {
  simulation_data$platform_coded <- dplyr::coalesce(current_platform, sim_platform)
}


#===============================================================================
# 05 Simulate Topic, Source and Format for public posts
#===============================================================================

# Topic-Verteilungen variieren moderat nach Plattform und – optional – nach den
# Screening-Informationsbedürfnissen. Die Effekte sind absichtlich nicht stark.
base_topic_weights <- c(
  0.12, 0.08, 0.08, 0.09, 0.07,
  0.08, 0.06, 0.06, 0.06, 0.06,
  0.09, 0.05, 0.06, 0.08, 0.06
)
base_source_weights <- c(
  0.28, 0.07, 0.08, 0.09, 0.08,
  0.07, 0.10, 0.11, 0.05, 0.04, 0.03
)

sim_topic <- rep(NA_character_, n)
sim_source <- rep(NA_character_, n)
sim_format <- rep(NA_integer_, n)

for (i in seq_len(n)) {
  if (simulation_data$public_rel_coded[[i]] != 1L) next
  
  platform_i <- simulation_data$platform_coded[[i]]
  topic_w <- base_topic_weights
  
  if (simulation_use_screening_patterns) {
    undirected <- simulation_data$intro_ib_undirected[[i]]
    thematic   <- simulation_data$intro_ib_thematic[[i]]
    problem    <- simulation_data$intro_ib_problem[[i]]
    
    undirected_effect <- ifelse(is.na(undirected), 0, undirected - 3)
    thematic_effect   <- ifelse(is.na(thematic), 0, thematic - 3)
    problem_effect    <- ifelse(is.na(problem), 0, problem - 3)
    
    topic_w[c(1, 2, 3, 4, 7, 8)] <-
      topic_w[c(1, 2, 3, 4, 7, 8)] * exp(0.18 * undirected_effect)
    topic_w[c(5, 11, 12, 13)] <-
      topic_w[c(5, 11, 12, 13)] * exp(0.20 * thematic_effect)
    topic_w[c(6, 9, 10, 14)] <-
      topic_w[c(6, 9, 10, 14)] * exp(0.20 * problem_effect)
  }
  
  if (platform_i == "Facebook") topic_w[c(1, 4, 11, 14)] <- topic_w[c(1, 4, 11, 14)] * 1.25
  if (platform_i == "Instagram") topic_w[c(6, 11, 13, 14)] <- topic_w[c(6, 11, 13, 14)] * 1.30
  if (platform_i == "TikTok") topic_w[c(5, 11, 13)] <- topic_w[c(5, 11, 13)] * 1.45
  if (platform_i == "X") topic_w[c(1, 2, 5, 7)] <- topic_w[c(1, 2, 5, 7)] * 1.35
  
  topic_i <- sample(topic_levels, 1, prob = topic_w)
  sim_topic[[i]] <- topic_i
  
  source_w <- base_source_weights
  topic_index <- match(topic_i, topic_levels)
  
  if (topic_index %in% c(1, 2)) source_w[c(1, 3, 4)] <- source_w[c(1, 3, 4)] * 1.60
  if (topic_index %in% c(5, 6)) source_w[c(4, 6)] <- source_w[c(4, 6)] * 1.80
  if (topic_index %in% c(7, 14)) source_w[c(4, 5)] <- source_w[c(4, 5)] * 1.50
  if (topic_index %in% c(11, 13)) source_w[c(1, 7, 8)] <- source_w[c(1, 7, 8)] * 1.45
  
  if (simulation_use_screening_patterns) {
    social <- simulation_data$intro_ib_social[[i]]
    social_effect <- ifelse(is.na(social), 0, social - 3)
    source_w[9] <- source_w[9] * exp(0.22 * social_effect)
  }
  
  sim_source[[i]] <- sample(source_levels, 1, prob = source_w)
  
  # Format folgt primär den Plattform-Affordances. Code -1 bleibt selten, damit
  # auch der neue "nicht bestimmbar"-Pfad gelegentlich getestet wird.
  format_prob <- switch(
    platform_i,
    Facebook  = c(`1` = .35, `2` = .40, `3` = .18, `4` = .06, `-1` = .01),
    Instagram = c(`1` = .05, `2` = .52, `3` = .37, `4` = .05, `-1` = .01),
    TikTok    = c(`1` = .02, `2` = .05, `3` = .88, `4` = .04, `-1` = .01),
    X         = c(`1` = .55, `2` = .29, `3` = .10, `4` = .05, `-1` = .01),
    c(`1` = .30, `2` = .35, `3` = .28, `4` = .06, `-1` = .01)
  )
  sim_format[[i]] <- as.integer(sample(names(format_prob), 1, prob = format_prob))
}

fill_or_replace <- function(current, simulated, overwrite) {
  if (overwrite) return(simulated)
  current_clean <- clean_text(current)
  dplyr::coalesce(current_clean, simulated)
}

simulation_data$topic_coded <- fill_or_replace(
  simulation_data$topic_coded, sim_topic, simulation_overwrite_existing
)
simulation_data$source_coded <- fill_or_replace(
  simulation_data$source_coded, sim_source, simulation_overwrite_existing
)

current_format <- suppressWarnings(as.integer(clean_numeric(simulation_data$media_format)))
if (simulation_overwrite_existing) {
  simulation_data$media_format <- sim_format
} else {
  simulation_data$media_format <- dplyr::coalesce(current_format, sim_format)
}

# Für nicht öffentliche oder nicht beurteilbare simulierte Posts bleiben die
# inhaltlichen Coding-Felder leer, sofern keine reale Codierung erhalten wird.
sim_nonpublic <- simulation_data$public_rel_coded %in% c(0L, -1L)
if (simulation_overwrite_existing) {
  simulation_data$topic_coded[sim_nonpublic] <- NA_character_
  simulation_data$source_coded[sim_nonpublic] <- NA_character_
  simulation_data$source_name_coded[sim_nonpublic] <- NA_character_
  simulation_data$media_format[sim_nonpublic] <- NA_integer_
}


#===============================================================================
# 06 Simulate concrete source names
#===============================================================================

# Source-Namen sind nur deskriptiv. Die Beispiele sind generisch und sollen keine
# realen Häufigkeiten oder tatsächlichen Accounts im Sample vortäuschen.
source_name_examples <- list(
  "Journalistisches Medium" = c("Tagesschau", "ZDFheute", "MDR Aktuell", "Lokale Tageszeitung"),
  "Alternatives oder parteiisches Medienangebot" = c("Alternatives Nachrichtenportal", "Politik-Blog"),
  "Partei oder Politiker:in" = c("Bundespolitiker:in", "Kommunalpolitiker:in", "Parteiverband"),
  "Staatliche oder öffentliche Institution" = c("Stadtverwaltung", "Polizei", "Bundesbehörde"),
  "NGO, Verband, Verein, Initiative oder Bewegung" = c("Verbraucherzentrale", "Umweltverband", "Lokale Initiative"),
  "Wissenschaft, Expert:in oder Faktencheck" = c("Universität", "Forschungsinstitut", "Faktencheck"),
  "Unternehmen oder Marke" = c("Verkehrsunternehmen", "Energieversorger", "Einzelhandel"),
  "Journalist:in, Creator, Influencer:in oder öffentliche Person" = c("Journalist:in", "Wissenscreator", "Öffentliche Person"),
  "Private Person / Peer" = c("Privater Account", "Bekannte Person"),
  "Kollektiv, Meme-, Satire- oder Aggregator-Seite" = c("Satireseite", "Meme-Aggregator", "Lokaler Sammelaccount"),
  "Sonstige / Quelle nicht erkennbar" = c("Quelle nicht erkennbar")
)

names(source_name_examples) <- source_levels  # align keys to (English) source_levels by position
sim_source_name <- rep(NA_character_, n)
for (i in seq_len(n)) {
  source_i <- clean_text(simulation_data$source_coded[[i]])
  if (
    simulation_data$public_rel_coded[[i]] == 1L &&
    !is.na(source_i) &&
    source_i %in% names(source_name_examples)
  ) {
    sim_source_name[[i]] <- sample(source_name_examples[[source_i]], 1)
  }
}

simulation_data$source_name_coded <- fill_or_replace(
  simulation_data$source_name_coded,
  sim_source_name,
  simulation_overwrite_existing
)


#===============================================================================
# 07 Mark simulated rows as completed
#===============================================================================

# Nach dem Füllen gelten die simulierten Zeilen als bearbeitet. Dadurch kann die
# normale strict_coding_check-Logik in 04b unverändert weiterlaufen.
simulation_data$coding_completed <- TRUE

if ("coder" %in% names(simulation_data)) {
  if (simulation_overwrite_existing) {
    simulation_data$coder <- "SIMULATED"
  } else {
    simulation_data$coder <- dplyr::coalesce(clean_text(simulation_data$coder), "SIMULATED")
  }
}

if ("coding_date" %in% names(simulation_data)) {
  current_date <- suppressWarnings(as.Date(simulation_data$coding_date))
  if (simulation_overwrite_existing) {
    simulation_data$coding_date <- Sys.Date()
  } else {
    simulation_data$coding_date <- dplyr::coalesce(current_date, Sys.Date())
  }
}

# Kennzeichnung bleibt im in-memory Datensatz erhalten und kann bei Bedarf in
# Analyseoutputs kontrolliert werden; die Rohdaten werden nicht verändert.
simulation_data$coding_was_simulated <- TRUE

coding <- simulation_data |>
  dplyr::arrange(.simulation_row) |>
  dplyr::select(
    -dplyr::all_of(c(".simulation_row", need_variables))
  )

cat(
  "\nSIMULATED CODING ADDED\n",
  "  Seed: ", simulation_seed, "\n",
  "  Rows: ", nrow(coding), "\n",
  "  Public relevant: ", sum(clean_numeric(coding$public_rel_coded) == 1, na.rm = TRUE), "\n",
  "  Not public: ", sum(clean_numeric(coding$public_rel_coded) == 0, na.rm = TRUE), "\n",
  "  Not assessable: ", sum(clean_numeric(coding$public_rel_coded) == -1, na.rm = TRUE), "\n",
  sep = ""
)
