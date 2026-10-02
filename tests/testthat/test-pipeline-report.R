# Renders a real report with `rmarkdown` and `pandoc` in a background R
# process, more than a CRAN check machine should be asked to do.
testthat::test_that("the pipeline stylesheet is embedded in a report, not linked", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("rmarkdown")
  skip_if_cannot_compile_pipeline()

  root_path <- tempfile(pattern = "test-report-")
  on.exit({ unlink(root_path, recursive = TRUE) }, add = TRUE)

  pipeline_path <- pipeline_create_template(
    root_path = file.path(root_path, "modules"), pipeline_name = "report_demo",
    overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")
  yaml::write_yaml(
    x = list(n = 100, pch = 16, col = "steelblue"),
    file = file.path(pipeline_path, "settings.yaml")
  )
  pipeline_build(pipeline_path)

  # A report whose pipeline ships its own stylesheet, with a marker rule
  writeLines(".rave-report-test-marker { color: #123456; }",
             file.path(pipeline_path, "report_styles.css"))
  writeLines(c("---", "title: Test report", "---", "", "A test report."),
             file.path(pipeline_path, "report-test.Rmd"))
  yaml::write_yaml(
    x = list(list(name = "test", entry = "report-test.Rmd")),
    file = file.path(pipeline_path, "report-list.yaml")
  )

  job_id <- pipeline_report_generate(
    name = "test", output_format = "html_document",
    output_dir = file.path(root_path, "reports"), pipe_dir = pipeline_path)
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE, after = FALSE)

  report_path <- resolve_job(job_id, timeout = 120, auto_remove = FALSE,
                             unresolved = "error")
  html <- readLines(as.character(report_path), warn = FALSE)

  # The stylesheet is inside the report...
  testthat::expect_true(any(grepl(".rave-report-test-marker", html, fixed = TRUE)))
  # ...and not linked as well: there is no `report_styles.css` next to it
  testthat::expect_false(any(grepl('href="report_styles.css"', html, fixed = TRUE)))

  # `pandoc` warns about each file it cannot embed. A callr job keeps its
  # output; an RStudio job does not
  outputs <- file.path(get_job_path(job_id, check = FALSE), "process_outputs.txt")
  if (file.exists(outputs)) {
    testthat::expect_false(any(grepl(
      "Could not fetch resource", readLines(outputs, warn = FALSE), fixed = TRUE)))
  }
})
