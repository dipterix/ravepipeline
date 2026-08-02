# The preference store lives in the user config directory, so point it at a
# temporary folder: without this the tests would write to real user preferences.
# `R_USER_CONFIG_DIR` must be set before the pipeline is created, since
# `PipelineTools$initialize` resolves the store up front.
with_preference_pipeline <- function(fun) {
  config_root <- tempfile(pattern = "rave-prefs-")
  module_root <- tempfile(pattern = "rave-prefs-modules-")
  pipeline_root_folder <- file.path(module_root, "modules")

  old_config <- Sys.getenv("R_USER_CONFIG_DIR", unset = NA)
  Sys.setenv(R_USER_CONFIG_DIR = config_root)

  on.exit({
    if (is.na(old_config)) {
      Sys.unsetenv("R_USER_CONFIG_DIR")
    } else {
      Sys.setenv(R_USER_CONFIG_DIR = old_config)
    }
    unlink(config_root, recursive = TRUE)
    unlink(module_root, recursive = TRUE)
  }, add = TRUE)

  utils::capture.output(type = "message", {
    pipeline_create_template(
      root_path = pipeline_root_folder,
      pipeline_name = "preference_demo",
      overwrite = TRUE, activate = FALSE, template_type = "rmd-bare"
    )
    pipe <- pipeline(
      pipeline_name = "preference_demo",
      paths = pipeline_root_folder,
      temporary = TRUE
    )
  })

  fun(pipe)
}

testthat::test_that("define_preference stores metadata but no value", {
  with_preference_pipeline(function(p) {

    p$define_preference("lazy", default = 42)

    testthat::expect_false(p$has_preferences("preference_demo.default.lazy"))
    testthat::expect_true(
      p$has_preferences("preference_demo.preference_metadata.default.lazy"))
    testthat::expect_equal(p$use_preference("lazy"), 42)
  })
})

testthat::test_that("setting then resetting a preference", {
  with_preference_pipeline(function(p) {
    p$define_preference("cex", default = 1.2, type = "numeric", domain = "graphics")

    testthat::expect_equal(p$use_preference("cex", value = 2), 2)
    testthat::expect_true(p$has_preferences("preference_demo.graphics.cex"))
    testthat::expect_equal(p$use_preference("cex"), 2)

    testthat::expect_true(p$reset_preference("cex", verbose = FALSE))
    testthat::expect_false(p$has_preferences("preference_demo.graphics.cex"))
    testthat::expect_equal(p$use_preference("cex"), 1.2)

    # `NULL` is the other way to reset
    p$use_preference("cex", value = 2)
    testthat::expect_equal(p$use_preference("cex", value = NULL), 1.2)
  })
})

testthat::test_that("revised defaults only affect preferences never overridden", {
  with_preference_pipeline(function(p) {

    p$define_preference("revised", default = 1, type = "numeric")
    testthat::expect_equal(p$use_preference("revised"), 1)

    p$define_preference("revised", default = 2, type = "numeric")
    testthat::expect_equal(p$use_preference("revised"), 2)

    p$use_preference("revised", value = 7)
    p$define_preference("revised", default = 3, type = "numeric")
    testthat::expect_equal(p$use_preference("revised"), 7)
  })
})

testthat::test_that("storage types are checked with typeof", {
  with_preference_pipeline(function(p) {

    # `mode(1L)` is "numeric", so an integer preference has to be matched on
    # `typeof` instead
    p$define_preference("count", default = 10L, type = "integer")
    testthat::expect_identical(p$use_preference("count"), 10L)
    testthat::expect_error(p$use_preference("count", value = "a"), "integer")

    p$define_preference("flag", default = TRUE, type = "logical")
    testthat::expect_identical(p$use_preference("flag", value = FALSE), FALSE)
    testthat::expect_error(p$use_preference("flag", value = 1), "logical")

    # `numeric` accepts both double and integer
    p$define_preference("num", default = 1.5, type = "numeric")
    testthat::expect_identical(p$use_preference("num", value = 2L), 2L)

    # a bad default is rejected when the preference is declared
    testthat::expect_error(
      p$define_preference("bad", default = "x", type = "numeric"),
      "does not pass the validator")
  })
})

testthat::test_that("named_list requires every element to be named", {
  with_preference_pipeline(function(p) {
    p$define_preference("params", default = list(a = 1), type = "named_list")

    testthat::expect_equal(p$use_preference("params"), list(a = 1))
    testthat::expect_equal(p$use_preference("params", value = list(b = 2)), list(b = 2))
    testthat::expect_error(p$use_preference("params", value = list(1)), "named")
    testthat::expect_error(p$use_preference("params", value = c(a = 1)), "named_list")
  })
})

testthat::test_that("global preferences resolve and reset", {
  with_preference_pipeline(function(p) {
    p$define_preference("shared", default = 3, type = "numeric", global = TRUE)

    testthat::expect_equal(p$use_preference("shared", value = 99), 99)
    testthat::expect_true(p$has_preferences("global.default.shared"))

    testthat::expect_true(p$reset_preference("shared", verbose = FALSE))
    testthat::expect_false(p$has_preferences("global.default.shared"))
    testthat::expect_equal(p$use_preference("shared"), 3)
  })
})

testthat::test_that("the same name in different domains stays independent", {
  with_preference_pipeline(function(p) {
    p$define_preference("dup", default = 1, type = "numeric", domain = "graphics")
    p$define_preference("dup", default = "a", type = "character", domain = "export")

    testthat::expect_equal(p$use_preference("preference_demo.graphics.dup"), 1)
    testthat::expect_equal(p$use_preference("preference_demo.export.dup"), "a")

    p$use_preference("preference_demo.graphics.dup", value = 5)
    testthat::expect_equal(p$use_preference("preference_demo.graphics.dup"), 5)
    testthat::expect_equal(p$use_preference("preference_demo.export.dup"), "a")
  })
})

testthat::test_that("full keys must name the declared domain", {
  with_preference_pipeline(function(p) {
    p$define_preference("only_graphics", default = 1, type = "numeric",
                        domain = "graphics")

    testthat::expect_equal(p$use_preference("preference_demo.graphics.only_graphics"), 1)
    testthat::expect_error(
      p$use_preference("preference_demo.export.only_graphics"), "unset")
    testthat::expect_error(
      p$use_preference("preference_demo.not_a_domain.only_graphics"), "domain")
  })
})

testthat::test_that("validators reject values and report why", {
  with_preference_pipeline(function(p) {

    # returning FALSE is a rejection, not a malformed validator
    p$define_preference("positive", default = 1, type = "numeric",
                        validator = function(value) { value > 0 })
    testthat::expect_error(p$use_preference("positive", value = -1),
                           "did not pass its validator")

    # a character return is used as the error message
    p$define_preference("small", default = 1, type = "numeric",
                        validator = function(value) {
                          if (value > 10) { return("must be at most 10") }
                          TRUE
                        })
    testthat::expect_error(p$use_preference("small", value = 100), "at most 10")
    testthat::expect_equal(p$use_preference("small", value = 5), 5)

    # validators are re-evaluated with this namespace as parent, so package
    # internals resolve
    p$define_preference("choice", default = "a", type = "character",
                        validator = function(value) {
                          identical(value %OF% c("a", "b"), value)
                        })
    testthat::expect_equal(p$use_preference("choice", value = "b"), "b")
    testthat::expect_error(p$use_preference("choice", value = "z"),
                           "did not pass its validator")
  })
})

testthat::test_that("stale stored values read as the default without writing", {
  with_preference_pipeline(function(p) {
    p$define_preference("typed", default = 1.5, type = "numeric")

    # written through the low-level API, bypassing the declared type
    p$set_preferences("preference_demo.default.typed" = "corrupt")

    testthat::expect_equal(p$use_preference("typed", verbose = FALSE), 1.5)
    # reads never write: the stale entry is left for `reset_preference` to clear
    testthat::expect_true(p$has_preferences("preference_demo.default.typed"))

    p$reset_preference("typed", verbose = FALSE)
    testthat::expect_false(p$has_preferences("preference_demo.default.typed"))
  })
})

testthat::test_that("undeclared preferences are an error", {
  with_preference_pipeline(function(p) {

    testthat::expect_error(p$use_preference("never_declared"),
                           "define_preference")
    testthat::expect_false(p$reset_preference("never_declared", verbose = FALSE))

    # `NULL` defaults are legal: they declare a preference with no value
    p$define_preference("empty")
    testthat::expect_null(p$use_preference("empty"))
  })
})
