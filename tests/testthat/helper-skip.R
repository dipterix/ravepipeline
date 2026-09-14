# Skips a test that compiles a pipeline from its `main.Rmd` when a suggested
# package the compilation needs is missing: `globals` finds the dependencies of
# the pipeline code chunks, and `rmarkdown`, when installed, needs `pandoc`
skip_if_cannot_compile_pipeline <- function() {
  testthat::skip_if_not_installed("globals")
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    testthat::skip_if_not(
      rmarkdown::pandoc_available(),
      message = "`rmarkdown` is installed but `pandoc` is not available"
    )
  }
}
