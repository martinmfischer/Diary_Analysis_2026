################################################################################
# Project: Tagebuchstudie – öffentlich relevante Informationsnutzung
# File:    04c_Outro_Analysis.R
# Version: 2026-08-10
#
# Purpose:
#   Kompakte Auswertung der Abschlussbefragung. Im Mittelpunkt stehen die
#   methodisch relevanten Fragen: Hat die Teilnahme das Verhalten verändert
#   (Reaktivität)? Wie gut war die App nutzbar? Lassen sich diese Angaben in
#   ausgewählten Diary-Mustern wiederfinden?
#
# Outputs:
#   03_Output/Outro_Results.xlsx
#     - Scales              publication-ready Skalenübersicht
#     - Items               publication-ready Itemdeskriptiven
#     - Method_Associations wenige theoriegeleitete explorative Zusammenhänge
#     - Open_Text           Freitext für qualitative Sichtung
#   03_Output/outro_prepared.rds
#   04_Figures/Outro_*.png
#
# Notes:
#   - Höhere Reactivity-Werte = stärkere studienbedingte Reaktivität.
#   - Items 2, 3 und 5 werden dafür invertiert.
#   - Höhere Ease-of-Use-Werte = höhere wahrgenommene Benutzerfreundlichkeit.
#   - Reliabilität wird mit Cronbachs Alpha und Omega total berichtet.
#   - Ein möglicher präregistrierter Einzelausschluss orientiert sich an Omega
#     total; Omega hierarchical wird für die Ein-Faktor-Lösung nicht verwendet.
################################################################################

rm(list = ls())


#===============================================================================
# 01 Settings and paths
#===============================================================================
# Nur wenige zentrale Schalter. Diagnostische Detailoutputs werden bewusst nicht
# geschrieben; Probleme erscheinen als stop()/warning() oder im Konsolenreport.

omega_cutoff <- 0.70
apply_single_item_exclusion <- TRUE
create_figures <- TRUE

helper_script <- file.path("02_Scripts", "00_Helpers.R")
data_folder <- "01_Data"
output_folder <- "03_Output"
figure_folder <- "04_Figures"

daily_participant_file <- file.path(
  output_folder,
  "daily_participant_level.rds"
)

output_excel <- file.path(
  output_folder,
  "Outro_Results.xlsx"
)

output_rds <- file.path(
  output_folder,
  "outro_prepared.rds"
)


#===============================================================================
# 02 Packages and shared helpers
#===============================================================================
# Das gemeinsame Helper-Script liefert Cleaning, Reliabilität, Excel-Helfer und
# insbesondere das gemeinsame Grafiktheme von Screening, Daily und Outro.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  psych,
  janitor,
  openxlsx,
  fs
)

if (!file.exists(helper_script)) {
  stop("Helper-Script nicht gefunden: ", helper_script)
}

source(helper_script)

fs::dir_create(output_folder)
fs::dir_create(figure_folder)


#===============================================================================
# 03 Locate and load Outro data
#===============================================================================
# Der Dateiname darf flexibel bleiben; bei mehreren Treffern wird nicht geraten,
# sondern abgebrochen, damit nicht versehentlich die falsche Welle analysiert wird.

outro_files <- fs::dir_ls(
  path = data_folder,
  type = "file",
  regexp = "(?i)(outro|abschluss|closing).*\\.rds$"
)

if (length(outro_files) == 0) {
  stop("Keine Outro-/Abschluss-RDS-Datei in 01_Data gefunden.")
}

if (length(outro_files) > 1) {
  stop(
    "Mehrere mögliche Outro-Dateien gefunden:\n- ",
    paste(outro_files, collapse = "\n- ")
  )
}

outro_file <- outro_files[[1]]
outro_raw <- readRDS(outro_file) %>% janitor::clean_names()

participant_variable <- first_existing(
  outro_raw,
  c(
    "personal_participant_code",
    "personalparticipantcode",
    "participant",
    "participant_code"
  ),
  description = "Participant Code"
)

reactivity_raw_items <- paste0("outro_reactivity_", 1:5)
ease_items <- paste0("outro_ease_", 1:8)
closed_items <- c(reactivity_raw_items, ease_items)

required_variables <- c(participant_variable, closed_items)
missing_required <- setdiff(required_variables, names(outro_raw))

if (length(missing_required) > 0) {
  stop(
    "Benötigte Outro-Variablen fehlen:\n- ",
    paste(missing_required, collapse = "\n- ")
  )
}

# Freitext ist methodisch nützlich, aber kein Grund, die quantitative Analyse zu
# stoppen. Falls die Variablen fehlen, werden leere Spalten ergänzt.
if (!"outro_problems_free" %in% names(outro_raw)) {
  outro_raw$outro_problems_free <- NA_character_
}
if (!"outro_suggestions_free" %in% names(outro_raw)) {
  outro_raw$outro_suggestions_free <- NA_character_
}


#===============================================================================
# 04 Clean participants and item values
#===============================================================================
# Doppelte Abschlüsse werden nur dann eindeutig aufgelöst, wenn ein committed-
# Zeitpunkt vorliegt. Skalenwerte außerhalb 1–5 gelten als Datenfehler.

outro <- outro_raw %>%
  mutate(
    participant = clean_text(.data[[participant_variable]]),
    across(all_of(closed_items), ~ na_if(clean_numeric(.x), -1)),
    problems_free = clean_text(outro_problems_free),
    suggestions_free = clean_text(outro_suggestions_free)
  ) %>%
  filter(!is.na(participant))

duplicate_participants <- outro %>%
  count(participant, name = "N_Rows") %>%
  filter(N_Rows > 1)

if (nrow(duplicate_participants) > 0) {
  if (!"committed" %in% names(outro)) {
    stop(
      "Doppelte Participant Codes im Outro, aber keine Variable `committed` ",
      "zur eindeutigen Auswahl des letzten Abschlusses."
    )
  }
  
  warning(
    nrow(duplicate_participants),
    " doppelte Participant Codes: letzter committed-Abschluss wird verwendet."
  )
  
  outro <- outro %>%
    arrange(participant, desc(committed)) %>%
    distinct(participant, .keep_all = TRUE)
}

range_issues <- purrr::map_dfr(
  closed_items,
  ~ outro %>%
    filter(!is.na(.data[[.x]]), .data[[.x]] < 1 | .data[[.x]] > 5) %>%
    transmute(participant, Item = .x, Value = .data[[.x]])
)

if (nrow(range_issues) > 0) {
  stop(
    "Outro enthält ", nrow(range_issues),
    " Werte außerhalb der vorgesehenen Skala 1–5."
  )
}


#===============================================================================
# 05 Construct directionally aligned items
#===============================================================================
# Die Reaktivitätsitems werden so ausgerichtet, dass ein hoher Wert immer mehr
# studienbedingte Veränderung bedeutet. Die Originalitems bleiben erhalten.

outro <- outro %>%
  mutate(
    outro_reactivity_2_reversed = 6 - outro_reactivity_2,
    outro_reactivity_3_reversed = 6 - outro_reactivity_3,
    outro_reactivity_5_reversed = 6 - outro_reactivity_5,
    outro_complete = if_all(all_of(closed_items), ~ !is.na(.x))
  )

reactivity_items <- c(
  "outro_reactivity_1",
  "outro_reactivity_2_reversed",
  "outro_reactivity_3_reversed",
  "outro_reactivity_4",
  "outro_reactivity_5_reversed"
)

reactivity_labels <- c(
  outro_reactivity_1 =
    "Gezielt nach hochladbaren Beiträgen gesucht",
  outro_reactivity_2_reversed =
    "Angezeigte Inhalte wirkten weniger wie zuvor",
  outro_reactivity_3_reversed =
    "Eigene Social-Media-Nutzung wich stärker vom Üblichen ab",
  outro_reactivity_4 =
    "Mehr öffentlich relevante Inhalte angezeigt",
  outro_reactivity_5_reversed =
    "Uploads bildeten die normale Nutzung weniger gut ab"
)

ease_labels <- c(
  outro_ease_1 = "App ist benutzerfreundlich",
  outro_ease_2 = "Teilnahme erfordert wenige Schritte",
  outro_ease_3 = "Nutzung der App ist mühelos",
  outro_ease_4 = "Fehler lassen sich schnell beheben",
  outro_ease_5 = "App kann zuverlässig genutzt werden",
  outro_ease_6 = "Download und Installation waren einfach",
  outro_ease_7 = "Eingabe des Login-Codes war einfach",
  outro_ease_8 = "Orientierung in der App war einfach"
)


#===============================================================================
# 06 Restrict to the Diary analysis sample
#===============================================================================
# Das kompakte Daily-Script speichert im Participant-RDS bereits nur Personen,
# die das Diary-Inklusionskriterium erfüllen. Deshalb reicht hier ein ID-Match;
# ein zusätzlicher Abschlussindikator ist nicht mehr nötig.

daily_filter_applied <- FALSE
daily_participant <- NULL

if (file.exists(daily_participant_file)) {
  daily_participant <- readRDS(daily_participant_file) %>%
    janitor::clean_names()
  
  daily_participant_variable <- first_existing(
    daily_participant,
    c("participant", "personal_participant_code", "personalparticipantcode"),
    description = "Participant Code in Daily participant data"
  )
  
  daily_participant <- daily_participant %>%
    mutate(
      participant = clean_text(.data[[daily_participant_variable]])
    ) %>%
    filter(!is.na(participant)) %>%
    distinct(participant, .keep_all = TRUE)
  
  eligible_daily_ids <- daily_participant$participant
  daily_filter_applied <- TRUE
  
  outro_analysis <- outro %>%
    filter(outro_complete, participant %in% eligible_daily_ids)
  
} else {
  warning(
    "daily_participant_level.rds nicht gefunden; Outro wird nur nach ",
    "vollständigem Abschluss gefiltert."
  )
  
  outro_analysis <- outro %>%
    filter(outro_complete)
}

if (nrow(outro_analysis) == 0) {
  stop("Nach Anwendung der Outro-/Daily-Kriterien verbleiben keine Fälle.")
}


#===============================================================================
# 07 Reliability and preregistered item-deletion check
#===============================================================================
# Reliabilität ist hier ein Skalencheck, nicht das Hauptergebnis. Berichtet werden
# Alpha und Omega total. Ein einzelnes Item wird nur ausgeschlossen, wenn Omega
# total zunächst < .70 liegt und nach genau einem Ausschluss mindestens .70 ist.

reactivity_reliability <- calculate_scale_reliability(
  outro_analysis,
  reactivity_items,
  "Reactivity"
)

ease_reliability <- calculate_scale_reliability(
  outro_analysis,
  ease_items,
  "Ease of Use"
)

select_by_omega_total <- function(reliability_result, items, cutoff, allow_exclusion) {
  full_omega <- reliability_result$summary$Omega_Total[[1]]
  selected_items <- items
  excluded_item <- NA_character_
  
  candidates <- reliability_result$leave_one_out %>%
    filter(!is.na(Omega_Total)) %>%
    arrange(desc(Omega_Total))
  
  if (
    allow_exclusion &&
    !is.na(full_omega) &&
    full_omega < cutoff &&
    nrow(candidates) > 0 &&
    candidates$Omega_Total[[1]] >= cutoff
  ) {
    excluded_item <- candidates$Item_Removed[[1]]
    selected_items <- setdiff(items, excluded_item)
  }
  
  list(
    selected_items = selected_items,
    excluded_item = excluded_item,
    full_omega = full_omega,
    best_omega_deleted = if (nrow(candidates) == 0) NA_real_ else candidates$Omega_Total[[1]]
  )
}

reactivity_selection <- select_by_omega_total(
  reactivity_reliability,
  reactivity_items,
  omega_cutoff,
  apply_single_item_exclusion
)

ease_selection <- select_by_omega_total(
  ease_reliability,
  ease_items,
  omega_cutoff,
  apply_single_item_exclusion
)

reactivity_final_items <- reactivity_selection$selected_items
ease_final_items <- ease_selection$selected_items

outro_analysis$reactivity_index <- complete_mean(
  outro_analysis,
  reactivity_final_items
)

outro_analysis$ease_index <- complete_mean(
  outro_analysis,
  ease_final_items
)

reactivity_reliability_final <- calculate_scale_reliability(
  outro_analysis,
  reactivity_final_items,
  "Reactivity"
)

ease_reliability_final <- calculate_scale_reliability(
  outro_analysis,
  ease_final_items,
  "Ease of Use"
)


#===============================================================================
# 08 Publication table: scales
#===============================================================================
# Eine kompakte Tabelle enthält alles, was für Methoden-/Ergebnistext typischerweise
# gebraucht wird: N, Itemzahl, Lage/Streuung, CI und Reliabilität.

reactivity_desc <- descriptive_summary(
  outro_analysis$reactivity_index,
  "Reaktivität"
)

ease_desc <- descriptive_summary(
  outro_analysis$ease_index,
  "Ease of Use"
)

scale_table <- bind_rows(
  reactivity_desc %>%
    transmute(
      Scale = "Reaktivität",
      N = N_Valid,
      Items = length(reactivity_final_items),
      M = Mean,
      SD,
      CI95_Lower,
      CI95_Upper,
      Alpha = reactivity_reliability_final$summary$Cronbach_Alpha[[1]],
      Omega_Total = reactivity_reliability_final$summary$Omega_Total[[1]],
      Excluded_Item = reactivity_selection$excluded_item,
      Interpretation = "Höher = stärkere studienbedingte Reaktivität"
    ),
  ease_desc %>%
    transmute(
      Scale = "Ease of Use",
      N = N_Valid,
      Items = length(ease_final_items),
      M = Mean,
      SD,
      CI95_Lower,
      CI95_Upper,
      Alpha = ease_reliability_final$summary$Cronbach_Alpha[[1]],
      Omega_Total = ease_reliability_final$summary$Omega_Total[[1]],
      Excluded_Item = ease_selection$excluded_item,
      Interpretation = "Höher = bessere wahrgenommene Benutzerfreundlichkeit"
    )
) %>%
  mutate(
    across(c(M, SD, CI95_Lower, CI95_Upper), ~ round(.x, 2)),
    across(c(Alpha, Omega_Total), ~ round(.x, 3))
  )


#===============================================================================
# 09 Publication table: items
#===============================================================================
# Itemwerte sind bei Reaktivität besonders wichtig, weil die fünf Fragen mehrere
# Formen möglicher Reaktivität abbilden. Deshalb bleiben sie neben dem Index sichtbar.

reactivity_item_table <- item_descriptives(
  outro_analysis,
  reactivity_items,
  reactivity_labels
) %>%
  mutate(
    Scale = "Reaktivität",
    Item_Number = match(Item, reactivity_items),
    Coding = "Richtungsgereinigt: höher = mehr Reaktivität"
  )

ease_item_table <- item_descriptives(
  outro_analysis,
  ease_items,
  ease_labels
) %>%
  mutate(
    Scale = "Ease of Use",
    Item_Number = match(Item, ease_items),
    Coding = if_else(
      Item_Number <= 5,
      "Core usability",
      "App-spezifisch"
    )
  )

item_table <- bind_rows(
  reactivity_item_table,
  ease_item_table
) %>%
  arrange(factor(Scale, levels = c("Reaktivität", "Ease of Use")), Item_Number) %>%
  transmute(
    Scale,
    Item = Item_Number,
    Statement = Variable,
    Coding,
    N = N_Valid,
    M = round(Mean, 2),
    SD = round(SD, 2),
    CI95_Lower = round(CI95_Lower, 2),
    CI95_Upper = round(CI95_Upper, 2)
  )


#===============================================================================
# 10 Join selected Diary indicators
#===============================================================================
# Für die methodische Validierung reichen wenige Diary-Marker. Die im Daily-Script
# vorberechneten Tagesslopes sind besonders relevant: Sie prüfen, ob subjektiv
# berichtete Reaktivität mit Veränderungen über die sieben Tage zusammenhängt.

if (!is.null(daily_participant)) {
  daily_keep <- c(
    "participant",
    "n_screenshots",
    "n_active_days",
    "share_publicly_relevant",
    "share_incidental_broad",
    "absolute_incidentality_gap_broad",
    "upload_count_day_slope",
    "targeted_post_day_slope",
    "thorough_reading_day_slope"
  )
  
  outro_analysis <- outro_analysis %>%
    left_join(
      daily_participant %>% select(any_of(daily_keep)),
      by = "participant"
    )
}


#===============================================================================
# 11 Exploratory method associations
#===============================================================================
# Nur theoriegeleitete Zusammenhänge werden geprüft. Sie dienen als methodischer
# Plausibilitätscheck und werden nicht als kausale Effekte der Studie interpretiert.

association_specs <- tribble(
  ~Family, ~X, ~Y, ~X_Label, ~Y_Label,
  "Skalenbezug", "reactivity_index", "ease_index",
  "Reaktivität", "Ease of Use",
  "Reaktivität × Diary", "reactivity_index", "upload_count_day_slope",
  "Reaktivität", "Trend der Uploadzahl über die Tage",
  "Reaktivität × Diary", "reactivity_index", "targeted_post_day_slope",
  "Reaktivität", "Trend gezielter Exposition über die Tage",
  "Reaktivität × Diary", "reactivity_index", "thorough_reading_day_slope",
  "Reaktivität", "Trend gründlicher Rezeption über die Tage",
  "Reaktivität × Diary", "reactivity_index", "absolute_incidentality_gap_broad",
  "Reaktivität", "Absoluter Screening–Diary-Incidentality-Gap",
  "Ease × Teilnahme", "ease_index", "n_screenshots",
  "Ease of Use", "Anzahl hochgeladener Screenshots",
  "Ease × Teilnahme", "ease_index", "n_active_days",
  "Ease of Use", "Anzahl aktiver Diary-Tage"
)

available_associations <- association_specs %>%
  filter(X %in% names(outro_analysis), Y %in% names(outro_analysis))

method_associations <- if (nrow(available_associations) > 0) {
  pmap_dfr(
    available_associations,
    function(Family, X, Y, X_Label, Y_Label) {
      spearman_test(
        outro_analysis,
        X,
        Y,
        X_Label,
        Y_Label
      ) %>%
        mutate(Family = Family, .before = 1)
    }
  ) %>%
    group_by(Family) %>%
    mutate(P_Adjusted_BH = p.adjust(P_Value, method = "BH")) %>%
    ungroup() %>%
    transmute(
      Family,
      Predictor = Variable_1,
      Outcome = Variable_2,
      N,
      Spearman_Rho = round(Spearman_Rho, 3),
      P_Value = round(P_Value, 4),
      P_Adjusted_BH = round(P_Adjusted_BH, 4),
      Note = "Explorativ; Teilnehmer-Ebene"
    )
} else {
  tibble(
    Family = character(), Predictor = character(), Outcome = character(),
    N = integer(), Spearman_Rho = double(), P_Value = double(),
    P_Adjusted_BH = double(), Note = character()
  )
}


#===============================================================================
# 12 Open text for qualitative inspection
#===============================================================================
# Freitexte werden nicht quantitativ überinterpretiert. Sie bleiben in einer
# einzigen übersichtlichen Tabelle für die manuelle methodische Sichtung erhalten.

open_text <- outro_analysis %>%
  select(participant, problems_free, suggestions_free) %>%
  pivot_longer(
    cols = c(problems_free, suggestions_free),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  mutate(
    Question = recode(
      Question,
      problems_free = "Probleme / Unsicherheiten",
      suggestions_free = "Verbesserungsvorschläge"
    )
  ) %>%
  arrange(Question, participant)


#===============================================================================
# 13 Save prepared participant-level data
#===============================================================================
# Das RDS enthält die vollständige finale Outro-Stichprobe inklusive der wenigen
# verbundenen Daily-Marker und ist der maschinenlesbare Input für weitere Analysen.

saveRDS(
  outro_analysis,
  output_rds
)


#===============================================================================
# 14 Publication-oriented Excel workbook
#===============================================================================
# Excel enthält bewusst nur direkt interpretierbare Tabellen. QC, Missingness und
# Item-Deletion-Details werden nicht in zahlreiche separate Arbeitsblätter zerlegt.

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = unname(project_colors["primary"]),
  halign = "center",
  valign = "center",
  border = "Bottom"
)

add_excel_sheet(workbook, "Scales", scale_table, header_style)
add_excel_sheet(workbook, "Items", item_table, header_style)
add_excel_sheet(workbook, "Method_Associations", method_associations, header_style)
add_excel_sheet(workbook, "Open_Text", open_text, header_style)

# Lange Itemtexte und Freitexte bekommen feste Breiten und Zeilenumbruch.
openxlsx::setColWidths(
  workbook,
  "Items",
  cols = which(names(item_table) %in% c("Statement", "Coding")),
  widths = c(48, 30)
)

openxlsx::setColWidths(
  workbook,
  "Open_Text",
  cols = which(names(open_text) == "Response"),
  widths = 70
)

if (nrow(open_text) > 0) {
  openxlsx::addStyle(
    workbook,
    "Open_Text",
    style = openxlsx::createStyle(wrapText = TRUE, valign = "top"),
    rows = 2:(nrow(open_text) + 1),
    cols = seq_len(ncol(open_text)),
    gridExpand = TRUE
  )
}

openxlsx::saveWorkbook(
  workbook,
  file = output_excel,
  overwrite = TRUE
)


#===============================================================================
# 15 Figure: Reactivity items
#===============================================================================
# Punkt + 95%-CI ist für Likert-Mittelwerte kompakter und lesbarer als Balken.
# Die neutrale Skalenmitte erleichtert die inhaltliche Einordnung.

if (create_figures) {
  reactivity_plot_data <- item_table %>%
    filter(Scale == "Reaktivität") %>%
    mutate(
      Statement = forcats::fct_rev(factor(Statement, levels = unique(Statement)))
    )
  
  p_reactivity <- ggplot(
    reactivity_plot_data,
    aes(x = M, y = Statement)
  ) +
    geom_vline(
      xintercept = 3,
      linetype = "22",
      linewidth = 0.55,
      color = unname(project_colors["grid"])
    ) +
    geom_errorbar(
      aes(xmin = CI95_Lower, xmax = CI95_Upper),
      width = 0.16,
      linewidth = 0.7,
      color = unname(project_colors["accent"])
    ) +
    geom_point(
      size = 3,
      color = unname(project_colors["dark"])
    ) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Wahrgenommene Reaktivität",
      subtitle = "Richtungsgereinigte Itemmittelwerte mit 95%-Konfidenzintervallen",
      x = "Zustimmung / Reaktivität (1–5)",
      y = NULL,
      caption = "Höhere Werte bedeuten stärkere studienbedingte Reaktivität; gestrichelt = Skalenmitte."
    ) +
    theme_project(legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 10)
    )
  
  save_project_plot(
    p_reactivity,
    file.path(figure_folder, "Outro_Reactivity_Items.png"),
    width = 9.5,
    height = 5.4
  )
  
  
  #===============================================================================
  # 16 Figure: Ease-of-use items
  #===============================================================================
  # Die acht Usability-Items werden im selben Layout dargestellt, sodass Screening,
  # Daily und Outro visuell aus einem Guss bleiben.
  
  ease_plot_data <- item_table %>%
    filter(Scale == "Ease of Use") %>%
    mutate(
      Statement = forcats::fct_rev(factor(Statement, levels = unique(Statement)))
    )
  
  p_ease <- ggplot(
    ease_plot_data,
    aes(x = M, y = Statement)
  ) +
    geom_vline(
      xintercept = 3,
      linetype = "22",
      linewidth = 0.55,
      color = unname(project_colors["grid"])
    ) +
    geom_errorbar(
      aes(xmin = CI95_Lower, xmax = CI95_Upper),
      width = 0.16,
      linewidth = 0.7,
      color = unname(project_colors["primary"])
    ) +
    geom_point(
      size = 3,
      color = unname(project_colors["dark"])
    ) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Wahrgenommene Benutzerfreundlichkeit",
      subtitle = "Itemmittelwerte mit 95%-Konfidenzintervallen",
      x = "Zustimmung / Ease of Use (1–5)",
      y = NULL,
      caption = "Höhere Werte bedeuten bessere wahrgenommene Benutzerfreundlichkeit; gestrichelt = Skalenmitte."
    ) +
    theme_project(legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 10)
    )
  
  save_project_plot(
    p_ease,
    file.path(figure_folder, "Outro_Ease_Items.png"),
    width = 9.5,
    height = 6.5
  )
  
  
  #===============================================================================
  # 17 Figure: method associations
  #===============================================================================
  # Die explorativen Korrelationen werden nur visualisiert, wenn tatsächlich
  # auswertbare Zusammenhänge vorliegen. Der Plot zeigt Effektgrößen, nicht Signifikanz.
  
  association_plot_data <- method_associations %>%
    filter(
      Family != "Skalenbezug",
      !is.na(Spearman_Rho)
    ) %>%
    mutate(
      Label = paste(Predictor, "↔", Outcome),
      Label = forcats::fct_reorder(Label, Spearman_Rho)
    )
  
  if (nrow(association_plot_data) > 0) {
    p_associations <- ggplot(
      association_plot_data,
      aes(x = Spearman_Rho, y = Label, color = Family)
    ) +
      geom_vline(
        xintercept = 0,
        linewidth = 0.6,
        color = unname(project_colors["medium"])
      ) +
      geom_segment(
        aes(x = 0, xend = Spearman_Rho, yend = Label),
        linewidth = 0.9,
        alpha = 0.7
      ) +
      geom_point(size = 3.2) +
      scale_x_continuous(
        limits = c(-1, 1),
        breaks = seq(-1, 1, 0.25)
      ) +
      scale_color_project() +
      labs(
        title = "Methodische Plausibilitätschecks",
        subtitle = "Explorative Spearman-Korrelationen auf Teilnehmendenebene",
        x = "Spearman ρ",
        y = NULL,
        caption = "Explorativ; Effektgrößen dienen der methodischen Einordnung und nicht dem Nachweis kausaler Reaktivität."
      ) +
      theme_project() +
      theme(panel.grid.major.y = element_blank())
    
    save_project_plot(
      p_associations,
      file.path(figure_folder, "Outro_Method_Associations.png"),
      width = 10,
      height = 5.8
    )
  }
}


#===============================================================================
# 18 Console report
#===============================================================================
# Alles, was primär QC oder Workflow-Dokumentation ist, landet in der Konsole
# statt in zusätzlichen Excel-Blättern.

cat(
  "\n============================================================\n",
  "OUTRO ANALYSIS COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat("Outro file: ", outro_file, "\n", sep = "")
cat("Raw participants: ", n_distinct(outro_raw[[participant_variable]], na.rm = TRUE), "\n", sep = "")
cat("Duplicate participant codes: ", nrow(duplicate_participants), "\n", sep = "")
cat("Complete Outro questionnaires: ", sum(outro$outro_complete, na.rm = TRUE), "\n", sep = "")
cat("Daily sample filter applied: ", daily_filter_applied, "\n", sep = "")
cat("Final analysis sample: ", nrow(outro_analysis), "\n\n", sep = "")

cat(
  "Reactivity: M = ", round(reactivity_desc$Mean, 2),
  ", SD = ", round(reactivity_desc$SD, 2),
  ", alpha = ", round(reactivity_reliability_final$summary$Cronbach_Alpha[[1]], 3),
  ", omega total = ", round(reactivity_reliability_final$summary$Omega_Total[[1]], 3),
  "\n",
  sep = ""
)

cat(
  "  Item excluded: ",
  ifelse(is.na(reactivity_selection$excluded_item), "none", reactivity_selection$excluded_item),
  "\n",
  sep = ""
)

cat(
  "Ease of Use: M = ", round(ease_desc$Mean, 2),
  ", SD = ", round(ease_desc$SD, 2),
  ", alpha = ", round(ease_reliability_final$summary$Cronbach_Alpha[[1]], 3),
  ", omega total = ", round(ease_reliability_final$summary$Omega_Total[[1]], 3),
  "\n",
  sep = ""
)

cat(
  "  Item excluded: ",
  ifelse(is.na(ease_selection$excluded_item), "none", ease_selection$excluded_item),
  "\n\n",
  sep = ""
)

cat("Open-text responses – problems: ", sum(!is.na(outro_analysis$problems_free)), "\n", sep = "")
cat("Open-text responses – suggestions: ", sum(!is.na(outro_analysis$suggestions_free)), "\n", sep = "")
cat("Exploratory method associations: ", nrow(method_associations), "\n\n", sep = "")

cat("Excel: ", output_excel, "\n", sep = "")
cat("Prepared RDS: ", output_rds, "\n", sep = "")
if (create_figures) cat("Figures: ", figure_folder, "/Outro_*.png\n", sep = "")
cat("============================================================\n")
