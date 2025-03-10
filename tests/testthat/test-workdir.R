testthat::test_that("Working directory", {
  ..old_wd <- getwd()
  ..old_opts <- options()

  expect_same_wd <- function(expr) {
    expr <- substitute(expr)
    eval(expr, parent.frame())

    ..new_wd <- getwd()
    testthat::expect_identical(..new_wd, ..old_wd, "Working directory was changed!")
    nms <- names(..old_opts)
    testthat::expect_identical(options()[nms], ..old_opts[nms], "Options has changed!")
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
      path = project_root,
      type = "bare"
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

    pipeline_build(pipe_dir = file.path(project_root, "modules", module_id))

    pipeline_render(module_id = module_id, project_path = project_root)

    unlink(project_root, recursive = TRUE)
  })


})
