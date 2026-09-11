# A bare pipeline shared by the tests below; removed at the end of this file
demo_root <- tempfile()
demo_pipeline <- local({
  pipeline_path <- pipeline_create_template(
    root_path = demo_root, pipeline_name = "analysis_demo",
    overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")
  pipeline_from_path(pipeline_path)
})

testthat::test_that("identifiers and namespace", {
  analysis <- RAVEPipelineAnalysis$new("demo", demo_pipeline)
  expect_equal(analysis$name, "demo")
  expect_equal(analysis$get_id("x"), "demo__x")
  expect_equal(analysis$get_id(c("x", "y")), c("demo__x", "demo__y"))
  expect_equal(analysis$get_id("x", with_namespace = TRUE), "analysis_demo-demo__x")
  expect_equal(analysis$ns(NULL), "analysis_demo")

  custom <- RAVEPipelineAnalysis$new("demo", demo_pipeline, namespace = "module")
  expect_equal(custom$get_id("x", with_namespace = TRUE), "module-demo__x")

  bare <- RAVEPipelineAnalysis$new("demo", demo_pipeline, namespace = NULL)
  expect_equal(bare$get_id("x", with_namespace = TRUE), "demo__x")
  expect_length(bare$ns(NULL), 0)
})

testthat::test_that("name is validated and read-only", {
  expect_error(RAVEPipelineAnalysis$new("my-analysis", demo_pipeline), "`name` must be")
  expect_error(RAVEPipelineAnalysis$new(c("a", "b"), demo_pipeline), "`name` must be")
  expect_error(RAVEPipelineAnalysis$new("1st", demo_pipeline), "`name` must be")
  expect_error(RAVEPipelineAnalysis$new(NA_character_, demo_pipeline), "`name` must be")
  expect_error(RAVEPipelineAnalysis$new("demo", list()), "PipelineTools")

  analysis <- RAVEPipelineAnalysis$new("demo", demo_pipeline)
  expect_error(analysis$name <- "other", "read-only")
  expect_equal(analysis$name, "demo")
})

testthat::test_that("step functions are checked", {
  analysis <- RAVEPipelineAnalysis$new("demo", demo_pipeline)
  expect_error(analysis$set_input_ui("x", function(inputId) NULL), "`pipeline`")
  expect_error(analysis$set_input_ui("x", "text"), "either `NULL` or a function")
  expect_error(analysis$set_input_ui(c("x", "y"), function(...) NULL), "single non-empty string")
  expect_error(analysis$set_shiny_server(function(input, output) NULL), "`session`")
  expect_error(analysis$set_preprocess(function(value) NULL), "`pipeline`")
  expect_error(analysis$set_analyze(function(value, pipeline) NULL), "`options`")
  expect_error(analysis$set_visualize(`[`), "missing")

  # a `...` formal accepts any argument
  expect_silent(analysis$set_analyze(function(value, ...) NULL))
  expect_silent(analysis$set_preprocess(function(...) NULL))
})

testthat::test_that("inputs are registered, rendered, and collected", {
  analysis <- RAVEPipelineAnalysis$new("demo", demo_pipeline)
  expect_identical(analysis$input_names, character(0))
  expect_length(analysis$collect_inputs(list()), 0)

  analysis$set_input_ui("n", function(inputId, pipeline) {
    list(id = inputId, pipeline_name = pipeline$pipeline_name)
  })
  analysis$set_input_ui("col", function(inputId, pipeline) inputId)
  expect_equal(analysis$input_names, c("n", "col"))
  expect_equal(
    analysis$render_input("n"),
    list(id = "analysis_demo-demo__n", pipeline_name = "analysis_demo")
  )
  expect_null(analysis$render_input("missing"))

  expect_equal(
    analysis$collect_inputs(list(demo__n = 5, demo__col = "red", other = 1)),
    list(n = 5, col = "red")
  )
  expect_equal(
    analysis$collect_inputs(list(`analysis_demo-demo__n` = 5), with_namespace = TRUE),
    list(n = 5, col = NULL)
  )

  # `NULL` removes the input
  analysis$set_input_ui("col", NULL)
  expect_equal(analysis$input_names, "n")
})

testthat::test_that("pre-process receives the pipeline; analyze does not pre-process", {
  analysis <- RAVEPipelineAnalysis$new("demo", demo_pipeline)

  # without step functions, values pass through
  expect_equal(analysis$preprocess_data(list(a = 1)), list(a = 1))
  expect_equal(analysis$analyze_data(list(a = 1)), list(a = 1))

  analysis$set_preprocess(function(value, pipeline) {
    value$pipeline_name <- pipeline$pipeline_name
    value
  })
  expect_equal(
    analysis$preprocess_data(list(a = 1)),
    list(a = 1, pipeline_name = "analysis_demo")
  )
  expect_equal(analysis$analyze_data(list(a = 1)), list(a = 1))

  analysis$set_analyze(function(value, pipeline, options) value$a + 1)
  expect_equal(analysis$analyze_data(list(a = 1)), 2)
})

testthat::test_that("options persist through analyze and are temporary for visualize", {
  analysis <- RAVEPipelineAnalysis$new("demo", demo_pipeline)
  expect_identical(analysis$options, list())
  analysis$options <- list(a = 1, b = 2)

  analysis$set_analyze(function(value, pipeline, options) options)
  expect_equal(analysis$analyze_data(NULL, b = 3, c = 4), list(a = 1, b = 3, c = 4))
  expect_equal(analysis$options, list(a = 1, b = 3, c = 4))

  # `.list` takes precedence over `...`
  analysis$analyze_data(NULL, a = 5, .list = list(a = 6))
  expect_equal(analysis$options$a, 6)

  # options must be named
  expect_error(analysis$analyze_data(NULL, 1), "must be named")
  expect_error(analysis$options <- list(1), "named list")
  expect_error(analysis$options <- c(a = 1), "named list")
  analysis$options <- NULL
  expect_identical(analysis$options, list())

  analysis$options <- list(main = "default")
  analysis$set_visualize(function(value, pipeline, options) options$main)
  expect_equal(analysis$visualize_data(NULL, main = "temporary"), "temporary")
  expect_equal(analysis$options$main, "default")

  # options are restored even if the visualize step fails
  analysis$set_visualize(function(value, pipeline, options) stop("visualize failed"))
  expect_error(analysis$visualize_data(NULL, main = "temporary"), "visualize failed")
  expect_equal(analysis$options$main, "default")
})

testthat::test_that("visualize_data keeps the visibility of the result", {
  analysis <- RAVEPipelineAnalysis$new("demo", demo_pipeline)
  expect_equal(withVisible(analysis$visualize_data(1)), list(value = NULL, visible = FALSE))

  analysis$set_visualize(function(value, pipeline, options) value + 1)
  expect_equal(withVisible(analysis$visualize_data(1)), list(value = 2, visible = TRUE))

  analysis$set_visualize(function(value, pipeline, options) invisible(value + 1))
  expect_equal(
    withVisible(analysis$visualize_data(1, main = "x")),
    list(value = 2, visible = FALSE)
  )
})

testthat::test_that("shiny_server needs a server function and a session", {
  analysis <- RAVEPipelineAnalysis$new("demo", demo_pipeline)
  expect_null(analysis$shiny_server())

  # outside of a shiny session (or without shiny installed)
  analysis$set_shiny_server(function(input, output, session) NULL)
  expect_error(analysis$shiny_server(), "shiny")
})

unlink(demo_root, recursive = TRUE)
