# Shared report layout and PDF rendering ---------------------------------------

# 1. Find or install XeLaTeX ---------------------------------------------------

ensure_xelatex <- function() {
  if (!nzchar(Sys.which("xelatex"))) {
    tex_directory <- tinytex::tinytex_root(error = FALSE)

    if (!nzchar(tex_directory)) {
      message("XeLaTeX is missing. Installing TinyTeX once; this requires internet access.")
      tex_directory <- tinytex::install_tinytex()
    }

    # Make a new installation available in this R session, without a restart.
    binary_directories <- list.dirs(
      file.path(tex_directory, "bin"), recursive = FALSE, full.names = TRUE
    )
    Sys.setenv(PATH = paste(
      c(binary_directories, Sys.getenv("PATH")), collapse = .Platform$path.sep
    ))

    if (!nzchar(Sys.which("xelatex")) && nzchar(tex_directory)) {
      tinytex::tlmgr_install("xetex")
    }
  }

  xelatex <- Sys.which("xelatex")
  if (!nzchar(xelatex)) {
    stop("TinyTeX setup did not make XeLaTeX available. Check the installation messages above.")
  }
  if (system2(xelatex, "--version", stdout = FALSE, stderr = FALSE) != 0) {
    stop("XeLaTeX was found but could not run: ", xelatex)
  }

  # These packages supply the language, font and layout used by our template.
  if (tinytex::is_tinytex()) {
    report_packages <- c(
      "babel-english", "tex-gyre", "titlesec", "fvextra",
      "fancyhdr", "caption", "colortbl", "float"
    )
    missing <- setdiff(report_packages, tinytex::tl_pkgs())
    if (length(missing)) {
      message("Installing missing report packages: ", paste(missing, collapse = ", "))
      tinytex::tlmgr_update(all = FALSE)
      tinytex::tlmgr_install(missing)
    }
  }

  invisible(xelatex)
}

# 2. Render the PDF ------------------------------------------------------------
# PDF dependencies belong to reporting, not the analysis scripts.

render_report_pdf <- function(markdown_file) {
  if (!rmarkdown::pandoc_available()) {
    quarto <- Sys.which("quarto")
    if (nzchar(quarto)) {
      rmarkdown::find_pandoc(dir = file.path(dirname(quarto), "tools"), cache = FALSE)
    }
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Install Pandoc or run from RStudio/Quarto.")
  }

  ensure_xelatex()

  header <- normalizePath(
    "02_Scripts/reporting/templates/header.tex",
    winslash = "/",
    mustWork = TRUE
  )

  table_filter <- normalizePath(
    "02_Scripts/reporting/templates/tables.lua",
    winslash = "/",
    mustWork = TRUE
  )

  input <- normalizePath(markdown_file, winslash = "/", mustWork = TRUE)
  output <- paste0(tools::file_path_sans_ext(input), ".pdf")
  build_directory <- tempfile("diary_report_")
  dir.create(build_directory)
  build_directory <- normalizePath(build_directory, mustWork = TRUE)
  original_directory <- getwd()
  on.exit({
    setwd(original_directory)
    unlink(build_directory, recursive = TRUE)
  }, add = TRUE)

  # Keep relative image paths intact while compiling outside the outputs folder.
  figures <- file.path(dirname(input), "figures")
  if (dir.exists(figures)) {
    if (!file.copy(figures, build_directory, recursive = TRUE)) {
      stop("Could not copy report figures to the temporary build directory.")
    }
  }

  rmarkdown::pandoc_convert(
    input, to = "latex", output = file.path(build_directory, "report.tex"),
    options = c(
      "--standalone", "--pdf-engine=xelatex",
      paste0("--include-in-header=", header),
      paste0("--lua-filter=", table_filter),
      "--variable=lang:en-US", "--variable=papersize:a4",
      "--variable=geometry:margin=18mm", "--variable=fontsize:10pt"
    )
  )

  # Unlike direct Pandoc PDF output, latexmk can install missing TeX packages.
  setwd(build_directory)
  tinytex::latexmk("report.tex", engine = "xelatex", pdf_file = output)
  message("PDF saved: ", output)
  invisible(output)
}

# 3. Assemble report sections --------------------------------------------------

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
