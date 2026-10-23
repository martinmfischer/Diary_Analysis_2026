# Native rmarkdown/Pandoc rendering replaces the former manual compilation loop.
# Requires Pandoc and XeLaTeX; only report scripts load these dependencies.
render_report_pdf <- function(markdown_file) {
  if (!rmarkdown::pandoc_available()) stop("Install Pandoc or run from RStudio/Quarto.")
  if (!nzchar(Sys.which("xelatex"))) stop("PDF rendering requires XeLaTeX (e.g. TinyTeX).")
  header <- normalizePath("01_Scripts/reporting/templates/header.tex", winslash = "/", mustWork = TRUE)
  table_filter <- normalizePath("01_Scripts/reporting/templates/tables.lua", winslash = "/", mustWork = TRUE)
  dir.create("tmp", showWarnings = FALSE)
  build <- tempfile("report_", tmpdir = "tmp")
  dir.create(build)
  rmarkdown::render(markdown_file,
    output_format = rmarkdown::pdf_document(latex_engine = "xelatex", fig_crop = FALSE,
      includes = rmarkdown::includes(in_header = header),
      pandoc_args = c(paste0("--lua-filter=", table_filter), "--variable=lang:en-US",
        "--variable=papersize:a4", "--variable=geometry:margin=18mm", "--variable=fontsize:10pt")),
    intermediates_dir = normalizePath(build), quiet = TRUE)
}

# A small layout helper: add a matching table/figure section to either report.
report_section <- function(title, purpose, table, figure = NULL) {
  c(paste0("## ", title), "", purpose, "",
    knitr::kable(table, format = "pipe", digits = 3,
      col.names = gsub("_", " ", names(table))), "",
    if (!is.null(figure)) c(sprintf("![%s](%s)", title, figure), ""))
}
