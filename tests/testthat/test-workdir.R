testthat::test_that("Working directory", {
  ..old_wd <- getwd()

  expect_same_wd <- function(expr) {
    expr <- substitute(expr)
    eval(expr, parent.frame())

    ..new_wd <- getwd()
    testthat::expect_identical(..old_wd, ..new_wd, "Working directory was changed!")
  }

  expect_same_wd({
    # For demonstrating this example only
    project_root <- tempfile()
    dir.create(project_root, showWarnings = FALSE, recursive = TRUE)


    # Add a module
    module_id <- "mylab_my_first_module"
    module_add(
      module_id = module_id,
      module_label = "My Pipeline",
      path = project_root
    )
  })

  expect_same_wd({
    # show the structure
    fs <- list.files(
      project_root,
      recursive = TRUE,
      full.names = FALSE,
      include.dirs = TRUE
    )

    testthat::expect_true(length(project_root) > 0)

    unlink(project_root, recursive = TRUE)
  })


})
