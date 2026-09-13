# Each test compiles a temporary module whose `R/shared-analysis.R` defines an
# analysis. Compiling knits `main.Rmd`, which also runs the analysis once
new_analysis_module <- function(shared_lines, inputs_key = TRUE) {
  project <- tempfile()
  dir.create(file.path(project, "modules"), recursive = TRUE)
  path <- pipeline_create_template(
    root_path = file.path(project, "modules"), pipeline_name = "demo_mod",
    overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")
  if (inputs_key) {
    cat("analysis_inputs_demo: []\n", file = file.path(path, "settings.yaml"), append = TRUE)
  }
  writeLines(shared_lines, file.path(path, "R", "shared-analysis.R"))
  list(project = project, path = path)
}

compile_module <- function(module) {
  suppressMessages(pipeline_render("demo_mod", project_path = module$project))
}

# The shared script also reads `pipeline` at top level, as some modules do
demo_analysis <- c(
  'top_level_n <- pipeline$get_settings("n")',
  'demo_analyzer <- ravepipeline::RAVEPipelineAnalysis$new("demo", namespace = "demo_mod")',
  'demo_analyzer$set_preprocess(function(value, pipeline_targets) {',
  '  list(k = if (length(value$k)) value$k else 2, data = pipeline_targets$input_data)',
  '}, pipeline_targets = "input_data")',
  'demo_analyzer$set_analyze(function(value, options) nrow(value$data) * value$k)'
)

testthat::test_that("an analysis is compiled into a pipeline target", {
  testthat::skip_on_cran()
  module <- new_analysis_module(demo_analysis)
  on.exit({ unlink(module$project, recursive = TRUE) }, add = TRUE)
  compile_module(module)

  pipe <- pipeline_from_path(module$path)
  target_table <- pipe$target_table
  expect_equal(
    target_table$Description[target_table$Names == "analysis_results_demo"],
    "Build analysis result-demo"
  )

  utils::capture.output(suppressMessages(pipe$run(
    names = "analysis_results_demo", type = "vanilla",
    scheduler = "none", return_values = FALSE)))
  # `input_data` has `n` (100) rows, and no saved input sets `k` (2)
  expect_equal(pipe$read("analysis_results_demo"), 200)
})

testthat::test_that("compiling an analysis requires its inputs in the settings", {
  testthat::skip_on_cran()
  module <- new_analysis_module(demo_analysis, inputs_key = FALSE)
  on.exit({ unlink(module$project, recursive = TRUE) }, add = TRUE)
  expect_error(compile_module(module), "analysis_inputs_demo")
})

testthat::test_that("each analysis target is created only once", {
  testthat::skip_on_cran()
  alias <- new_analysis_module(c(demo_analysis, "alias_analyzer <- demo_analyzer"))
  same_name <- new_analysis_module(c(
    demo_analysis,
    'other_analyzer <- ravepipeline::RAVEPipelineAnalysis$new("demo", namespace = "demo_mod")'
  ))
  on.exit({ unlink(c(alias$project, same_name$project), recursive = TRUE) }, add = TRUE)
  expect_error(compile_module(alias), "analysis_results_demo")
  expect_error(compile_module(same_name), "analysis_results_demo")
})

testthat::test_that("the setup can run again in the same environment", {
  testthat::skip_on_cran()
  module <- new_analysis_module(demo_analysis)
  on.exit({ unlink(module$project, recursive = TRUE) }, add = TRUE)

  # the first run leaves the lazy `analysis_results_demo` in `env`; the second
  # run must not fail on it
  env <- new.env()
  for (i in 1:2) {
    expect_error(suppressMessages(
      pipeline_setup_rmd("demo_mod", env = env, project_path = module$project)
    ), NA)
  }
  expect_true(exists("analysis_results_demo", envir = env, inherits = FALSE))
})
