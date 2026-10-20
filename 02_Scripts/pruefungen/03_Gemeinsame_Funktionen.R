# Gemeinsame Auswertungsregeln mit kleinen, bekannten Beispielen prüfen.
# Aus dem Projekt-Hauptverzeichnis ausführen; verändert keine Studiendaten.

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, knitr, stringr)
source("02_Scripts/funktionen/Daten_und_Statistik.R", encoding = "UTF-8")

responses <- as.data.frame(setNames(
  rep(list(c(1, 5, -1)), 6), paste0("intro_incidentality_", 1:6)
))
scored <- incidentality_scores(responses)
stopifnot(
  identical(scored$intro_incidentality_5, c(5, 1, NA_real_)),
  isTRUE(all.equal(rowMeans(scored)[1:2], c(10 / 6, 26 / 6))),
  is.na(rowMeans(scored)[3]),
  identical(responses$intro_incidentality_5, c(1, 5, -1)),
  safe_spearman(c(1, 2, 3, NA), c(3, 2, 1, 8)) == -1,
  is.na(safe_spearman(c(1, 1, 1), c(1, 2, 3))),
  is.na(safe_spearman(c(1, 2, Inf), c(1, 2, 3))),
  is.na(safe_spearman(numeric(), numeric())),
  identical(markdown_table(data.frame(Wert = c(.1234, NA_real_))),
    as.character(knitr::kable(data.frame(Wert = c(.1234, NA_real_)),
      format = "pipe", digits = 2, row.names = FALSE))),
  isTRUE(is_marked_screenshot_duplicate("Screenshot Duplikat")),
  isFALSE(is_marked_screenshot_duplicate("TN hat offensichtlich doppelt teilgenommen"))
)
message("Indexpolung, fehlende Werte, Korrelationen, Markdown und Duplikatregel geprüft")
