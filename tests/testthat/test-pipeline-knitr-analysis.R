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
  skip_if_cannot_compile_pipeline()
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
  expect_equal(pipe$read("analysis_results_demo")$results, 200)
})

testthat::test_that("compiling an analysis requires its inputs in the settings", {
  testthat::skip_on_cran()
  skip_if_cannot_compile_pipeline()
  module <- new_analysis_module(demo_analysis, inputs_key = FALSE)
  on.exit({ unlink(module$project, recursive = TRUE) }, add = TRUE)
  expect_error(compile_module(module), "analysis_inputs_demo")
})

testthat::test_that("each analysis target is created only once", {
  testthat::skip_on_cran()
  skip_if_cannot_compile_pipeline()
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
  # outside of knitting, the setup changes the knitr chunk options for good
  chunk_options <- knitr::opts_chunk$get()
  on.exit({ knitr::opts_chunk$restore(chunk_options) }, add = TRUE)

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

# Compiles a module and builds `input_data`, which the analyses below read
compiled_pipeline <- function(module) {
  compile_module(module)
  pipe <- pipeline_from_path(module$path)
  utils::capture.output(suppressMessages(pipe$run(
    names = "input_data", type = "vanilla",
    scheduler = "none", return_values = FALSE)))
  pipe
}

testthat::test_that("run() computes each step in debug mode", {
  testthat::skip_on_cran()
  skip_if_cannot_compile_pipeline()
  module <- new_analysis_module(demo_analysis)
  on.exit({ unlink(module$project, recursive = TRUE) }, add = TRUE)
  pipe <- compiled_pipeline(module)

  # an analysis that is not compiled into the pipeline runs in debug mode only
  draft <- RAVEPipelineAnalysis$new("draft", namespace = "demo_mod")
  draft$set_preprocess(function(value, pipeline_targets) {
    list(k = 3, n = nrow(pipeline_targets$input_data))
  }, pipeline_targets = "input_data")
  expect_error(draft$run(pipe), "eval_method")

  expect_equal(draft$run(pipe, step = "inputs", eval_method = "debug"), list())
  expect_equal(
    draft$run(pipe, step = "preprocess", eval_method = "debug"),
    list(k = 3, n = 100)
  )
  # without an analyze step, the processed value is the result
  expect_equal(
    draft$run(pipe, step = "analyze", eval_method = "debug"),
    list(k = 3, n = 100)
  )

  draft$set_analyze(function(value, options) value$k * value$n)
  draft$set_visualize(function(value, options) cat("result:", value, "\n"))
  expect_equal(draft$run(pipe, step = "analyze", eval_method = "debug"), 300)
  expect_output(draft$run(pipe, eval_method = "debug"), "result: 300")
})

testthat::test_that("run() saves the inputs, then builds the analysis target", {
  testthat::skip_on_cran()
  skip_if_cannot_compile_pipeline()
  module <- new_analysis_module(demo_analysis)
  on.exit({ unlink(module$project, recursive = TRUE) }, add = TRUE)
  pipe <- compiled_pipeline(module)

  # the analysis as the module defines it
  env <- new.env()
  env$pipeline <- pipe
  sys.source(file.path(module$path, "R", "shared-analysis.R"), envir = env)
  analysis <- env$demo_analyzer

  # the store hook changes `k`, so the target only sees the new value if the
  # inputs are saved before it is built
  analysis$set_store_inputs_to_pipeline(function(inputs, pipeline) {
    inputs$k <- inputs$k + 1
    inputs
  })
  pipe$set_settings(analysis_inputs_demo = list(k = 5))
  utils::capture.output(result <- suppressMessages(analysis$run(
    pipe, step = "analyze", type = "vanilla", scheduler = "none")))
  expect_equal(result, 600)
  expect_equal(pipe$read("analysis_results_demo")$results, 600)
  expect_equal(
    pipeline_from_path(module$path)$get_settings("analysis_inputs_demo"),
    list(k = 6)
  )
})

testthat::test_that("run() renders the visualization as HTML", {
  testthat::skip_on_cran()
  # rendering HTML needs `rmarkdown` (with `pandoc`) and `htmltools`
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("htmltools")
  skip_if_cannot_compile_pipeline()
  module <- new_analysis_module(demo_analysis)
  on.exit({ unlink(module$project, recursive = TRUE) }, add = TRUE)
  pipe <- compiled_pipeline(module)

  draft <- RAVEPipelineAnalysis$new("draft", namespace = "demo_mod")
  draft$set_preprocess(function(value, pipeline_targets) {
    nrow(pipeline_targets$input_data)
  }, pipeline_targets = "input_data")
  draft$set_visualize(function(value, options) {
    cat(sprintf("rows: %d\nsecond line\n", value))
  })

  # renders quietly, even for an analysis that is not compiled into the
  # pipeline, and printed lines stay on separate lines
  expect_silent(
    html <- draft$run(pipe, eval_method = "debug", visualization_method = "html")
  )
  expect_match(as.character(html), "rows: 100\n[^\n]*second line")
})
