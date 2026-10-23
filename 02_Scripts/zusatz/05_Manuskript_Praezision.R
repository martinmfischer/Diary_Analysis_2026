# ==============================================================================
# Ergänzung für die Manuskriptrevision: Unsicherheit des Survey-Diary-Abgleichs
# ==============================================================================
# Separat ausführen. Die Hauptanalysen und ihre Stichprobe bleiben unverändert.
# Explorativ ergänzt am 20.10.2026; keine präregistrierten Intervallschätzungen.

# 01 Vorhandene Paper-Ergebnisse berechnen --------------------------------------

source("02_Scripts/02_Paper_Analysen.R", encoding = "UTF-8", local = TRUE)
pacman::p_load(boot)

# 02 Personen gemeinsam mit ihren beiden Messwerten ziehen ----------------------
# Perzentilintervalle mit 9.999 Ziehungen; keine Korrektur für Mehrfachvergleiche.

set.seed(20261020)
manuscript_correlations <- bind_rows(lapply(c("Zufall", "Ungeplant"), function(definition) {
  pairs <- person_data %>%
    transmute(Index = incidentality_index, Anteil = .data[[definition]]) %>%
    filter(is.finite(Index), is.finite(Anteil))

  bootstrap <- boot::boot(pairs, statistic = function(data, indices) {
    cor(data$Index[indices], data$Anteil[indices], method = "spearman")
  }, R = 9999)
  stopifnot(all(is.finite(bootstrap$t)))
  interval <- boot::boot.ci(bootstrap, type = "perc")$percent[4:5]

  tibble(
    Definition = definition, N = nrow(pairs),
    Spearman_rho = bootstrap$t0,
    CI_lower = interval[1], CI_upper = interval[2],
    Bootstrap_samples = bootstrap$R, Seed = 20261020
  )
}))

# 03 Nur die ergänzende Ergebnistabelle speichern -------------------------------

readr::write_csv(manuscript_correlations,
  "03_Output/Manuskript_Korrelationen_KI.csv")
print(manuscript_correlations)
