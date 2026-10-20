# PDF-Ausgabe: render_report_pdf(markdown_file)
# Eingabe: bestehender Markdown-Bericht mit relativen Grafikpfaden
# Ausgabe: gleichnamiges PDF neben dem Bericht; Zwischendateien unter tmp/pdfs
# Layoutdateien: berichte/vorlagen/PDF_Kopf.tex und PDF_Tabellen.lua

# PDF aus dem fertigen Markdown-Bericht, einschließlich relativer Bildpfade.
# Voraussetzungen: Pandoc (z. B. über Quarto/RStudio) und XeLaTeX.
render_report_pdf <- function(markdown_file) {
  pacman::p_load(rmarkdown)

  if (!rmarkdown::pandoc_available()) {
    quarto <- Sys.which("quarto")
    if (nzchar(quarto)) {
      rmarkdown::find_pandoc(dir = file.path(dirname(quarto), "tools"), cache = FALSE)
    }
  }
  if (!rmarkdown::pandoc_available()) stop("PDF-Export benötigt Pandoc (Quarto oder RStudio).")
  if (!nzchar(Sys.which("xelatex"))) stop("PDF-Export benötigt XeLaTeX (TeX Live oder TinyTeX).")

  input <- normalizePath(markdown_file, winslash = "/", mustWork = TRUE)
  header <- normalizePath("02_Scripts/berichte/vorlagen/PDF_Kopf.tex",
    winslash = "/",
    mustWork = TRUE
  )
  output <- sub("\\.md$", ".pdf", input)
  dir.create("tmp/pdfs", recursive = TRUE, showWarnings = FALSE)
  build_dir <- file.path(
    normalizePath("tmp/pdfs", winslash = "/"),
    tools::file_path_sans_ext(basename(markdown_file))
  )
  dir.create(build_dir, showWarnings = FALSE)
  build_dir <- normalizePath(build_dir, winslash = "/", mustWork = TRUE)
  temporary_tex <- file.path(build_dir, "report.tex")
  temporary_pdf <- file.path(build_dir, "report.pdf")

  pandoc <- file.path(
    rmarkdown::find_pandoc()$dir,
    if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  )
  table_filter <- normalizePath("02_Scripts/berichte/vorlagen/PDF_Tabellen.lua",
    winslash = "/",
    mustWork = TRUE
  )
  status <- system2(pandoc, args = c(
    shQuote(input), "--from=markdown+pipe_tables+gfm_auto_identifiers", "--to=latex",
    "--output", shQuote(temporary_tex),
    "--resource-path", shQuote(dirname(input)),
    "--lua-filter", shQuote(table_filter),
    "--standalone",
    "--variable=lang:de-DE", "--variable=papersize:a4",
    "--variable=geometry:margin=18mm", "--variable=fontsize:10pt",
    "--variable=colorlinks:true",
    "--include-in-header", shQuote(header)
  ))
  if (status != 0) stop("PDF-Export fehlgeschlagen (Pandoc-Code ", status, ").")
  # Direkt kompilieren: vermeidet Probleme mit Pandocs temporären Windows-Pfaden.
  old_wd <- setwd(dirname(input))
  on.exit(setwd(old_wd), add = TRUE)
  for (pass in 1:2) {
    status <- system2(Sys.which("xelatex"),
      args = c(
        "-interaction=nonstopmode", "-halt-on-error",
        shQuote(paste0(
          "-output-directory=",
          build_dir
        )),
        shQuote(temporary_tex)
      ),
      stdout = file.path(
        build_dir,
        "render.log"
      ),
      stderr = file.path(
        build_dir,
        "errors.log"
      )
    )
    if (status != 0) stop("XeLaTeX-Fehler. Details: ", file.path(build_dir, "render.log"))
  }
  if (!file.exists(temporary_pdf) || file.size(temporary_pdf) == 0) stop("PDF wurde nicht erzeugt.")
  if (!file.copy(temporary_pdf, output, overwrite = TRUE)) {
    stop("PDF konnte nicht ersetzt werden. Ist die Datei in einem PDF-Programm gesperrt?")
  }
  message("PDF gespeichert: ", output)
  invisible(output)
}
