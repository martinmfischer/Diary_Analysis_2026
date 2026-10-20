# Berichtsgestaltung: report_theme() und style_report()
# Eingaben: ggplot-Grafiken bzw. Markdown-Zeilen und Berichtsart
# Rückgabe: Theme bzw. formatierte Markdown-Zeilen; keine eigene Dateiausgabe

# Gestaltung ohne Änderung der Auswertung
report_theme <- function() {
  ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "#263747"),
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        colour = "#173D50",
        margin = ggplot2::margin(b = 12)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 11,
        colour = "#566774",
        margin = ggplot2::margin(b = 12)
      ),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(
        size = 9,
        colour = "#566774",
        hjust = 0,
        margin = ggplot2::margin(t = 12)
      ),
      plot.margin = ggplot2::margin(16, 20, 14, 16),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E5EBEE", linewidth = .35),
      axis.text = ggplot2::element_text(colour = "#354957", size = 10),
      axis.title = ggplot2::element_text(size = 11),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = "#173D50",
        size = 11,
        margin = ggplot2::margin(
          8,
          8,
          8,
          8
        )
      ),
      strip.background = ggplot2::element_rect(fill = "#EDF3F5", colour = NA),
      panel.spacing = grid::unit(1.2, "lines"),
      legend.position = "bottom", legend.title = ggplot2::element_text(face = "bold")
    )
}

style_report <- function(lines, kind) {
  aliases <- c(
    Mittel_A = "Mittel A (%)", Mittel_B = "Mittel B (%)",
    Differenz_pp = "Differenz (pp)", Median_Differenz = "Median-Differenz (pp)",
    Spearman_rho = "Spearman ρ", SD_Prozentpunkte = "SD (pp)",
    Mittlerer_Anteil_Prozent = "Mittlerer Anteil (%)",
    Differenz_IG_minus_FB = "Instagram − Facebook (pp)",
    Screenshot_Prozent = "Beiträge (%)", Personen_Prozent = "Personenmittel (%)"
  )
  result <- character()
  for (i in seq_along(lines)) {
    line <- lines[i]
    if (startsWith(line, "|") && i < length(lines) && grepl("^\\|[-: |]+$", lines[i + 1])) {
      cells <- trimws(strsplit(line, "|", fixed = TRUE)[[1]][-1])
      cells <- vapply(
        cells,
        function(x) {
          if (x %in% names(aliases)) {
            aliases[[x]]
          } else {
            gsub("_",
              " ",
              x,
              fixed = TRUE
            )
          }
        },
        character(1)
      )
      line <- paste0("| ", paste(cells, collapse = " | "), " |")
    }
    if (grepl("^\\*\\*(Zweck|Status|Leseschlüssel)", line)) line <- paste0("> ", line)
    if (nchar(line) > 600 && !grepl("^[|#!>*-]", line)) {
      sentences <- strsplit(line, "(?<=[.!?]) (?=[A-ZÄÖÜ])", perl = TRUE)[[1]]
      chunks <- character()
      chunk <- ""
      for (sentence in sentences) {
        if (nchar(chunk) > 280) {
          chunks <- c(chunks, chunk, "")
          chunk <- ""
        }
        chunk <- paste0(chunk, if (nzchar(chunk)) " " else "", sentence)
      }
      line <- c(chunks, chunk)
    }
    result <- c(result, line)
  }
  headings <- sub("^## ", "", lines[grepl("^## ", lines)])
  headings <- headings[!headings %in% c("Agenda", "Erste Befunde")]
  anchor <- function(x) gsub(" +", "-", gsub("[^[:alnum:] _-]", "", tolower(x)))
  nav <- paste0("- [", headings, "](#", vapply(headings, anchor, character(1)), ")")
  date_line <- which(grepl("^Erstellt:", result))[1]
  if (!is.na(date_line)) {
    result[date_line] <- sub("^Erstellt:", "**Auswertungsstand:**", result[date_line])
    subtitle <- if (kind == "essentials") "**Tagebuchstudie · Stichprobe, Inhalte und Messqualität**" else "**Tagebuchstudie · Zusammenhänge, Vergleiche und Robustheit**"
    result <- append(result,
      c(
        "",
        subtitle,
        "",
        "## Inhalt",
        "",
        nav,
        "",
        "---",
        ""
      ),
      after = date_line
    )
  }
  result
}
