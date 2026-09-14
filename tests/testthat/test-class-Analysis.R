# A bare pipeline shared by the tests below; removed at the end of this file.
# Its settings are `n` (100), `pch` (16), and `col` ("steelblue")
demo_root <- tempfile()
demo_pipeline <- local({
  pipeline_path <- pipeline_create_template(
    root_path = demo_root, pipeline_name = "analysis_demo",
    overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")
  pipeline_from_path(pipeline_path)
})

testthat::test_that("constructor requires a valid name and namespace", {
  expect_error(RAVEPipelineAnalysis$new("my-analysis", "module"), "`name` must be")
  expect_error(RAVEPipelineAnalysis$new(c("a", "b"), "module"), "`name` must be")
  expect_error(RAVEPipelineAnalysis$new("1st", "module"), "`name` must be")
  expect_error(RAVEPipelineAnalysis$new(NA_character_, "module"), "`name` must be")

  expect_error(RAVEPipelineAnalysis$new("demo"), "namespace")
  expect_error(RAVEPipelineAnalysis$new("demo", NULL), "`namespace` must be")
  expect_error(RAVEPipelineAnalysis$new("demo", ""), "`namespace` must be")
  expect_error(RAVEPipelineAnalysis$new("demo", NA_character_), "`namespace` must be")
  expect_error(RAVEPipelineAnalysis$new("demo", c("a", "b")), "`namespace` must be")

  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  expect_error(analysis$name <- "other", "read-only")
  expect_equal(analysis$name, "demo")

  # the description defaults to the name with underscores as spaces
  expect_equal(RAVEPipelineAnalysis$new("my__demo_x", "module")$description, "my demo x")
  expect_equal(
    RAVEPipelineAnalysis$new("demo", "module", description = "A demo")$description,
    "A demo"
  )
})

testthat::test_that("identifiers derive from the analysis name and namespace", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  expect_equal(analysis$get_id("x"), "demo__x")
  expect_equal(analysis$get_id(c("x", "y")), c("demo__x", "demo__y"))
  expect_equal(analysis$get_id("x", with_namespace = TRUE), "module-demo__x")
  expect_equal(analysis$`@ns`(NULL), "module")

  expect_equal(analysis$inputs_settings_name, "analysis_inputs_demo")
  expect_equal(analysis$results_target_name, "analysis_results_demo")
  expect_error(analysis$inputs_settings_name <- "x")
  expect_error(analysis$results_target_name <- "x")
  expect_equal(analysis$inputs_settings_name, "analysis_inputs_demo")
})

testthat::test_that("step functions are checked", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  expect_error(analysis$set_input_ui("x", function(inputId, pipeline) NULL), "missing: `restored_inputs`")
  expect_error(analysis$set_input_ui("x", "text"), "either `NULL` or a function")
  expect_error(analysis$set_input_ui(c("x", "y"), function(...) NULL), "single non-empty string")
  expect_error(analysis$set_collect_inputs_from_shiny(function(input) NULL), "missing: `session`")
  expect_error(analysis$set_collect_inputs_from_pipeline(function(pipeline) NULL), "missing: `pipeline_settings`")
  expect_error(analysis$set_store_inputs_to_pipeline(function(inputs) NULL), "missing: `pipeline`")
  expect_error(analysis$set_store_inputs_to_pipeline(function(pipeline) NULL), "missing: `inputs`")
  expect_error(analysis$set_shiny_server(function(input, output) NULL), "missing: `session`")
  expect_error(analysis$set_preprocess(function(value, pipeline) NULL), "missing: `pipeline_targets`")
  expect_error(analysis$set_analyze(function(value) NULL), "missing: `options`")
  expect_error(analysis$set_visualize(function(value) NULL), "missing: `options`")
  expect_error(analysis$set_visualize(`[`), "missing")

  # a `...` formal accepts any argument
  expect_silent(analysis$set_analyze(function(value, ...) NULL))
  expect_silent(analysis$set_preprocess(function(...) NULL))
})

testthat::test_that("pipeline targets are unique non-empty names, cleared with the step", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  preprocess <- function(value, pipeline_targets) value
  expect_error(analysis$set_preprocess(preprocess, pipeline_targets = 1), "`pipeline_targets` must be")
  expect_error(analysis$set_preprocess(preprocess, pipeline_targets = NA_character_), "`pipeline_targets` must be")
  expect_error(analysis$set_preprocess(preprocess, pipeline_targets = ""), "`pipeline_targets` must be")
  expect_error(analysis$set_preprocess(preprocess, pipeline_targets = c("a", "a")), "`pipeline_targets` must be")
  expect_identical(analysis$pipeline_targets, character(0))

  analysis$set_preprocess(preprocess, pipeline_targets = c("a", "b"))
  expect_equal(analysis$pipeline_targets, c("a", "b"))
  expect_error(analysis$pipeline_targets <- "c")
  expect_equal(analysis$pipeline_targets, c("a", "b"))

  analysis$set_preprocess(NULL)
  expect_identical(analysis$pipeline_targets, character(0))
})

testthat::test_that("inputs are stored to and restored from the pipeline settings", {
  analysis <- RAVEPipelineAnalysis$new("roundtrip", "module")
  analysis$set_input_ui("n", function(inputId, restored_inputs) {
    list(id = inputId, restored_inputs = restored_inputs)
  })

  # nothing stored yet
  expect_identical(analysis$`@collect_inputs_from_pipeline`(demo_pipeline), list())
  expect_equal(
    analysis$`@render_input`("n", demo_pipeline),
    list(id = "module-roundtrip__n", restored_inputs = list())
  )
  expect_null(analysis$`@render_input`("missing", demo_pipeline))

  saved <- analysis$`@store_inputs_to_pipeline`(list(n = 5L, col = "red"), demo_pipeline)
  expect_equal(saved, list(n = 5L, col = "red"))
  expect_equal(
    demo_pipeline$get_settings("analysis_inputs_roundtrip"),
    list(n = 5L, col = "red")
  )

  # the settings file is written, so a pipeline loaded from disk restores them
  reloaded <- pipeline_from_path(demo_pipeline$pipeline_path)
  expect_equal(
    analysis$`@collect_inputs_from_pipeline`(reloaded),
    list(n = 5L, col = "red")
  )
  expect_equal(
    analysis$`@render_input`("n", reloaded),
    list(id = "module-roundtrip__n", restored_inputs = list(n = 5L, col = "red"))
  )
})

testthat::test_that("a pipeline is required to render and store inputs", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  analysis$set_input_ui("n", function(inputId, restored_inputs) inputId)
  expect_error(analysis$`@render_input`("n"), "pipeline")
  expect_error(analysis$`@render_input`("n", list()), "PipelineTools")
  expect_error(analysis$`@store_inputs_to_pipeline`(list(n = 1), list()), "PipelineTools")
})

testthat::test_that("inputs are collected from a pipeline or from its settings", {
  analysis <- RAVEPipelineAnalysis$new("from_settings", "module")
  settings <- list(n = 3, analysis_inputs_from_settings = list(k = 1))
  expect_equal(analysis$`@collect_inputs_from_pipeline`(settings), list(k = 1))
  expect_identical(analysis$`@collect_inputs_from_pipeline`(list(n = 3)), list())

  # a custom collector receives the settings list, even when given a pipeline
  analysis$set_collect_inputs_from_pipeline(function(pipeline_settings) {
    list(is_list = is.list(pipeline_settings), n = pipeline_settings$n)
  })
  expect_equal(analysis$`@collect_inputs_from_pipeline`(settings), list(is_list = TRUE, n = 3))
  expect_equal(
    analysis$`@collect_inputs_from_pipeline`(demo_pipeline),
    list(is_list = TRUE, n = demo_pipeline$get_settings("n"))
  )
})

testthat::test_that("custom hooks save extra settings and collect them back", {
  analysis <- RAVEPipelineAnalysis$new("hooks", "module")

  # the store hook saves `objects` to its own settings and returns the rest
  analysis$set_store_inputs_to_pipeline(function(inputs, pipeline) {
    pipeline$set_settings(hooks_objects = inputs$objects)
    inputs$objects <- NULL
    inputs$n <- as.integer(inputs$n)
    inputs
  })
  analysis$set_collect_inputs_from_pipeline(function(pipeline_settings) {
    inputs <- as.list(pipeline_settings$analysis_inputs_hooks)
    inputs$objects <- pipeline_settings$hooks_objects
    inputs
  })

  saved <- analysis$`@store_inputs_to_pipeline`(
    list(n = "7", objects = list("a", "b")), demo_pipeline)
  expect_equal(saved, list(n = 7L))
  expect_equal(demo_pipeline$get_settings("analysis_inputs_hooks"), list(n = 7L))
  expect_equal(demo_pipeline$get_settings("hooks_objects"), list("a", "b"))
  expect_equal(
    analysis$`@collect_inputs_from_pipeline`(demo_pipeline),
    list(n = 7L, objects = list("a", "b"))
  )

  # whatever the hook returns is saved, even an empty list
  analysis$set_store_inputs_to_pipeline(function(inputs, pipeline) list())
  analysis$`@store_inputs_to_pipeline`(list(n = "9"), demo_pipeline)
  expect_length(demo_pipeline$get_settings("analysis_inputs_hooks"), 0)

  # `NULL` restores the defaults
  analysis$set_store_inputs_to_pipeline(NULL)
  analysis$set_collect_inputs_from_pipeline(NULL)
  analysis$`@store_inputs_to_pipeline`(list(n = "8"), demo_pipeline)
  expect_equal(analysis$`@collect_inputs_from_pipeline`(demo_pipeline), list(n = "8"))
})

testthat::test_that("shiny inputs are collected from the module scope", {
  testthat::skip_if_not_installed("shiny")
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  analysis$set_input_ui("n", function(inputId, restored_inputs) NULL)
  analysis$set_input_ui("col", function(inputId, restored_inputs) NULL)

  session <- shiny::MockShinySession$new()
  session$setInputs(`module-demo__n` = 5, demo__col = "unscoped", other = 1)

  # the default reads the registered inputs under the module namespace, and
  # works outside of a reactive context
  expect_equal(
    analysis$`@collect_inputs_from_shiny`(session),
    list(n = 5, col = NULL)
  )
  # any scope of the session works
  expect_equal(
    analysis$`@collect_inputs_from_shiny`(session$makeScope("other")),
    list(n = 5, col = NULL)
  )

  # a custom collector gets the module-scoped session
  analysis$set_collect_inputs_from_shiny(function(session) {
    list(
      n = session$input[[analysis$get_id("n")]],
      other = session$rootScope()$input$other
    )
  })
  expect_equal(
    shiny::isolate(analysis$`@collect_inputs_from_shiny`(session)),
    list(n = 5, other = 1)
  )

  # `NULL` restores the default
  analysis$set_collect_inputs_from_shiny(NULL)
  expect_equal(
    analysis$`@collect_inputs_from_shiny`(session),
    list(n = 5, col = NULL)
  )
})

testthat::test_that("shiny server runs under the analysis namespace", {
  testthat::skip_if_not_installed("shiny")
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  session <- shiny::MockShinySession$new()
  expect_null(analysis$`@shiny_server`(session))

  analysis$set_shiny_server(function(input, output, session) session$ns("x"))
  expect_equal(analysis$`@shiny_server`(session), "module-x")
  expect_equal(analysis$`@shiny_server`(session$makeScope("other")), "module-x")
})

testthat::test_that("shiny methods require a session", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  expect_error(analysis$`@collect_inputs_from_shiny`(), "session")
  expect_error(analysis$`@collect_inputs_from_shiny`(NULL), "`session` must be")
  expect_error(analysis$`@collect_inputs_from_shiny`(list()), "`session` must be")
  expect_error(analysis$`@shiny_server`(), "session")
  expect_error(analysis$`@shiny_server`(NULL), "`session` must be")
})

testthat::test_that("declared pipeline targets reach the preprocess step only", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")

  # without step functions, values pass through
  expect_equal(analysis$`@preprocess_data`(list(a = 1)), list(a = 1))
  expect_equal(analysis$`@analyze_data`(list(a = 1))$results, list(a = 1))

  echo <- function(value, pipeline_targets) {
    list(value = value, pipeline_targets = pipeline_targets)
  }

  # no declared targets: extra values are dropped
  analysis$set_preprocess(echo)
  expect_length(
    analysis$`@preprocess_data`(1, pipeline_targets = list(x = 1))$pipeline_targets, 0)

  analysis$set_preprocess(echo, pipeline_targets = c("x", "y"))
  expect_equal(
    analysis$`@preprocess_data`(list(a = 1), pipeline_targets = list(y = 2, z = 3, x = 1)),
    list(value = list(a = 1), pipeline_targets = list(x = 1, y = 2))
  )

  # a target whose value is `NULL` is still given
  expect_equal(
    analysis$`@preprocess_data`(1, pipeline_targets = list(x = NULL, y = 2))$pipeline_targets,
    list(x = NULL, y = 2)
  )

  # every declared target must be given
  expect_error(analysis$`@preprocess_data`(1, pipeline_targets = list(x = 1)), "missing: `y`")
  expect_error(analysis$`@preprocess_data`(1), "missing: `x`, `y`")

  # the analyze step receives the processed value and the options only
  analysis$set_analyze(function(value, ...) list(value = value, ...))
  analysis$set_option(a = 1)
  expect_equal(
    analysis$`@analyze_data`(list(b = 2))$results,
    list(value = list(b = 2), options = list(a = 1))
  )
})

testthat::test_that("options are named lists, set whole or one key at a time", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  expect_length(analysis$options, 0)

  analysis$options <- list(a = 1, n = list(p = 1))
  for (bad in list(NULL, list(1), c(a = 1))) {
    expect_error(analysis$options <- bad, "named list")
  }
  expect_equal(analysis$options, list(a = 1, n = list(p = 1)))

  # top-level keys are replaced whole; `.list` wins over `...`
  analysis$set_option(n = list(q = 2), b = "x", .list = list(b = "y"))
  expect_equal(analysis$options, list(a = 1, n = list(q = 2), b = "y"))

  # unnamed values are rejected without changing the options
  expect_error(analysis$set_option(1), "must be named")
  expect_error(analysis$set_option(c = 1, 2), "must be named")
  expect_error(analysis$set_option(.list = list(3), .clear_first = TRUE), "must be named")
  expect_equal(analysis$options, list(a = 1, n = list(q = 2), b = "y"))

  analysis$set_option(z = 0, .clear_first = TRUE)
  expect_equal(analysis$options, list(z = 0))
  analysis$options <- list()
  expect_length(analysis$options, 0)

  # the analyze and visualize steps receive the current options
  analysis$set_option(main = "title")
  analysis$set_analyze(function(value, options) options)
  analysis$set_visualize(function(value, options) options)
  expect_equal(analysis$`@analyze_data`(NULL)$results, list(main = "title"))
  expect_equal(analysis$`@visualize_data`(NULL), list(main = "title"))
})

testthat::test_that("visualize keeps the visibility of the result", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  expect_equal(
    withVisible(analysis$`@visualize_data`(1)),
    list(value = NULL, visible = FALSE)
  )

  analysis$set_visualize(function(value, options) value + 1)
  expect_equal(
    withVisible(analysis$`@visualize_data`(1)),
    list(value = 2, visible = TRUE)
  )

  analysis$set_visualize(function(value, options) invisible(value + 1))
  expect_equal(
    withVisible(analysis$`@visualize_data`(1)),
    list(value = 2, visible = FALSE)
  )
})

testthat::test_that("build_targets reads the saved inputs by default", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  analysis$set_preprocess(function(value, pipeline_targets) {
    list(k = value$k, n = pipeline_targets$n)
  }, pipeline_targets = "n")
  analysis$set_analyze(function(value, options) value$k * value$n)

  specs <- analysis$`@build_targets`(varname = "my_analysis")
  expect_length(specs, 1)
  expect_equal(specs[[1]]$export, "analysis_results_demo")
  expect_equal(specs[[1]]$label, "__Build_analysis_result-demo")
  expect_setequal(specs[[1]]$deps, c("analysis_inputs_demo", "n"))
  expect_true(specs[[1]]$is_delayed)

  # the generated code runs where the analysis and the target values live
  env <- new.env()
  env$my_analysis <- analysis
  env$analysis_inputs_demo <- list(k = 3)
  env$n <- 10
  expect_equal(eval(str2lang(specs[[1]]$code), envir = env)$results, 30)
})

testthat::test_that("build_targets adds a cleaned-inputs target for a custom collector", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  analysis$set_collect_inputs_from_pipeline(function(pipeline_settings) {
    list(k = pipeline_settings$k_setting)
  })
  analysis$set_preprocess(function(value, pipeline_targets) {
    list(k = value$k, n = pipeline_targets$n)
  }, pipeline_targets = "n")
  analysis$set_analyze(function(value, options) value$k * value$n)

  specs <- analysis$`@build_targets`(varname = "my_analysis")
  expect_equal(
    vapply(specs, `[[`, "", "export"),
    c("analysis_cleaned_inputs_demo", "analysis_results_demo")
  )
  expect_equal(specs[[1]]$label, "__Collect_analysis_inputs-demo")
  expect_setequal(specs[[1]]$deps, c("analysis_inputs_demo", "settings"))
  expect_false(isTRUE(specs[[1]]$is_delayed))
  expect_setequal(specs[[2]]$deps, c("analysis_cleaned_inputs_demo", "n"))

  env <- new.env()
  env$my_analysis <- analysis
  env$settings <- list(k_setting = 4)
  env$n <- 10
  for (spec in specs) {
    eval(str2lang(spec$code), envir = env)
  }
  expect_equal(env$analysis_cleaned_inputs_demo, list(k = 4))
  expect_equal(env$analysis_results_demo$results, 40)
})

testthat::test_that("the analysis result records which analysis produced it", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")

  # without an analyze step, the processed value is the result
  result <- analysis$`@analyze_data`(list(a = 1))
  expect_s3_class(result, "RAVEPipelineAnalysis_results")
  expect_equal(result$analysis_name, "demo")
  expect_equal(result$results, list(a = 1))

  analysis$set_analyze(function(value, options) value$a + 1)
  result <- analysis$`@analyze_data`(list(a = 1))
  expect_s3_class(result, "RAVEPipelineAnalysis_results")
  expect_equal(result$results, 2)
})

testthat::test_that("visualize unwraps the results of the same analysis only", {
  analysis <- RAVEPipelineAnalysis$new("demo", "module")
  analysis$set_analyze(function(value, options) value * 2)
  analysis$set_visualize(function(value, options) value + 1)
  expect_equal(analysis$`@visualize_data`(analysis$`@analyze_data`(1)), 3)

  # a plain value is visualized as is
  expect_equal(analysis$`@visualize_data`(1), 2)

  other <- RAVEPipelineAnalysis$new("other", "module")
  expect_error(analysis$`@visualize_data`(other$`@analyze_data`(1)), "`other`")
})

testthat::test_that("render_inputs renders every registered input", {
  testthat::skip_if_not_installed("htmltools")
  analysis <- RAVEPipelineAnalysis$new("render_all", "module")
  analysis$set_input_ui("n", function(inputId, restored_inputs) {
    htmltools::tags$input(id = inputId)
  })
  analysis$set_input_ui("col", function(inputId, restored_inputs) {
    htmltools::tags$input(id = inputId)
  })
  ui <- analysis$render_inputs(demo_pipeline)
  expect_s3_class(ui, "shiny.tag.list")
  expect_match(as.character(ui), "module-render_all__n", fixed = TRUE)
  expect_match(as.character(ui), "module-render_all__col", fixed = TRUE)
})

testthat::test_that("the roundtrip check reads the inputs back from a settings file", {
  analysis <- RAVEPipelineAnalysis$new("round_trip", "module")
  root <- pipeline_root()

  demo_pipeline$set_settings(analysis_inputs_round_trip = list(n = 5, d = 5L, s = "x"))
  expect_true(analysis$`@test_roundtrip_pipeline_inputs`(demo_pipeline))

  # the file drops the names of an atomic vector
  demo_pipeline$set_settings(analysis_inputs_round_trip = list(v = c(a = 1)))
  expect_false(analysis$`@test_roundtrip_pipeline_inputs`(demo_pipeline))

  # the check forks the pipeline without changing the pipeline root
  expect_identical(pipeline_root(), root)
})

unlink(demo_root, recursive = TRUE)
