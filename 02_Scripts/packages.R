# Install missing packages from CRAN, then attach them in the requested order.
load_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing)) {
    if (file.access(.libPaths()[1], 2) != 0) {
      user_library <- strsplit(
        Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE
      )[[1]][1]
      dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
      .libPaths(c(user_library, .libPaths()))
    }
    install.packages(missing, repos = "https://cloud.r-project.org")
  }

  invisible(suppressPackageStartupMessages(
    lapply(packages, library, character.only = TRUE)
  ))
}
