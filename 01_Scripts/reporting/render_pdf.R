# Shared report layout and PDF rendering ---------------------------------------

# PDF dependencies belong to reporting, not the analysis scripts.
render_report_pdf <- function(markdown_file) {
  if (!rmarkdown::pandoc_available()) {
    stop("Install Pandoc or run from RStudio/Quarto.")
  }

  if (!nzchar(Sys.which("xelatex"))) {
    stop("PDF rendering requires XeLaTeX (e.g. TinyTeX).")
  }

  header <- normalizePath(
    "01_Scripts/reporting/templates/header.tex",
    winslash = "/",
    mustWork = TRUE
  )

  table_filter <- normalizePath(
    "01_Scripts/reporting/templates/tables.lua",
    winslash = "/",
    mustWork = TRUE
  )

  dir.create("tmp", showWarnings = FALSE)
  build_directory <- tempfile("report_", tmpdir = "tmp")
  dir.create(build_directory)

  # rmarkdown handles compilation; templates only control presentation.
  rmarkdown::render(
    markdown_file,
    output_format = rmarkdown::pdf_document(
      latex_engine = "xelatex",
      fig_crop = FALSE,
      includes = rmarkdown::includes(in_header = header),
      pandoc_args = c(
        paste0("--lua-filter=", table_filter),
        "--variable=lang:en-US",
        "--variable=papersize:a4",
        "--variable=geometry:margin=18mm",
        "--variable=fontsize:10pt"
      )
    ),
    intermediates_dir = normalizePath(build_directory),
    quiet = TRUE
  )
}

# Every section states its purpose, shows a table and can include a figure.
report_section <- function(title, purpose, table, figure = NULL) {
  c(
    paste0("## ", title),
    "",
    purpose,
    "",
    knitr::kable(
      table,
      format = "pipe",
      digits = 3,
      col.names = gsub("_", " ", names(table))
    ),
    "",
    if (!is.null(figure)) {
      c(sprintf("![%s](%s)", title, figure), "")
    }
  )
}
