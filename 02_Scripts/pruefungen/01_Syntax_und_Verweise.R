# ==============================================================================
# Aktive R-Skripte auf Syntax und gültige source()-Pfade prüfen
# ==============================================================================
# Im Projekt-Hauptverzeichnis ausführen; benötigt ausschließlich Basis-R
# Führt keine Auswertung aus, verändert keine Dateien und liest keine Studiendaten

script_files <- list.files(
  "02_Scripts", pattern = "[.]R$", recursive = TRUE, full.names = TRUE
)
if (length(script_files) == 0L) {
  stop("Keine Skripte gefunden. Bitte im Projekt-Hauptverzeichnis ausführen")
}

check_source_paths <- function(expression, script_file) {
  # Leere Indizes wie daten[, 1] erscheinen im Syntaxbaum als fehlendes Argument
  if (missing(expression)) return(invisible(NULL))
  if (!is.call(expression) && !is.expression(expression)) return(invisible(NULL))

  if (is.call(expression) && identical(expression[[1]], as.name("source"))) {
    source_path <- expression[[2]]
    if (is.character(source_path) && !file.exists(source_path)) {
      stop("Ungültiger source()-Pfad in ", script_file, ": ", source_path)
    }
  }
  for (part in as.list(expression)) {
    check_source_paths(part, script_file)
  }
  invisible(NULL)
}

for (script_file in script_files) {
  parsed_script <- parse(script_file, encoding = "UTF-8")
  check_source_paths(parsed_script, script_file)
}

pdf_templates <- c(
  "02_Scripts/berichte/vorlagen/PDF_Kopf.tex",
  "02_Scripts/berichte/vorlagen/PDF_Tabellen.lua"
)
stopifnot(all(file.exists(pdf_templates)))
message(length(script_files), " R-Skripte: Syntax, source()-Pfade und PDF-Vorlagen geprüft")
