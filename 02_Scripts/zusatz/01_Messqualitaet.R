# Ergänzende Omega-Diagnostik und Anzahl offener Outro-Antworten.
# Voraussetzung: vollständiger Lauf von 02_Paper_Analysen.R.
# Die zusätzliche Faktorenmodellierung ist nicht präregistriert.

pacman::p_load(GPArotation)

# Hierarchisches Omega: Modellwahl nach Kenntnis der Daten, nicht präregistriert.
# Drei Gruppenfaktoren; bei fünf Items zwei mit gleichen Generalfaktorladungen.
# Pearson-Korrelationen: Omega bezieht sich auf standardisierte Items.
estimate_omega <- function(items, scale_name, n_factors) {
  fit <- psych::omega(
    items,
    nfactors = n_factors, fm = "minres", rotate = "oblimin",
    option = "equal", flip = FALSE, poly = FALSE, covar = FALSE, plot = FALSE
  )
  loadings <- as.data.frame(fit$schmid$sl)
  invalid <- any(!is.finite(as.matrix(loadings[c("g", "h2", "u2")])) |
    loadings$u2 < 0 | loadings$h2 > 1) ||
    !is.finite(fit$omega_h) || !is.finite(fit$omega.tot) ||
    fit$omega_h < 0 || fit$omega.tot > 1 || fit$omega_h > fit$omega.tot

  status <- if (invalid) {
    "Unzulässige Lösung; Omega nicht interpretieren"
  } else if (any(loadings$u2 < .01)) {
    "Grenznahe Lösung (Restvarianz < 0,01)"
  } else {
    "Keine unzulässigen Varianzen"
  }

  list(
    model = fit,
    summary = tibble(
      Skala = scale_name,
      Gruppenfaktoren = n_factors,
      Omega_Hierarchisch = if (invalid) NA_real_ else fit$omega_h,
      Omega_Total = if (invalid) NA_real_ else fit$omega.tot,
      Modellstatus = status
    ),
    diagnostics = tibble(
      Skala = scale_name, Item = rownames(loadings),
      Generalfaktorladung = loadings$g,
      Kommunalitaet = loadings$h2, Restvarianz = loadings$u2
    )
  )
}

incidentality_omega <- estimate_omega(incidentality_complete, "Incidentality", 3)
reactivity_omega <- estimate_omega(reactivity_complete, "Reaktivität", 2)
ease_omega <- estimate_omega(ease_complete, "Benutzerfreundlichkeit", 3)

incidentality_reliability_table <- incidentality_reliability_table %>%
  left_join(incidentality_omega$summary, by = "Skala")
outro_reliability <- outro_reliability %>%
  left_join(bind_rows(reactivity_omega$summary, ease_omega$summary), by = "Skala")

omega_diagnostics <- bind_rows(
  incidentality_omega$diagnostics, reactivity_omega$diagnostics, ease_omega$diagnostics
)

open_response_counts <- tibble(Feld = character(), Antworten = integer())

for (variable in intersect(c("outro_problems_free", "outro_suggestions_free"), names(outro))) {
  open_response_counts <- bind_rows(
    open_response_counts,
    tibble(Feld = variable, Antworten = sum(!is.na(clean_text(outro[[variable]]))))
  )
}
