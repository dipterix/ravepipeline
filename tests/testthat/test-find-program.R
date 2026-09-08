# `find_program()` promises a path or `""`, never an error and never a false
# negative. Both halves broke on Linux without anyone noticing, because macOS
# and Windows take different branches of its `switch`.

ravepipeline <- asNamespace("ravepipeline")


testthat::test_that("find_program() reports a missing program as an empty string", {
  # The editor launcher is routinely absent -- a CI runner, a container, a
  # machine without Homebrew -- and `vscode_cli()` relies on hearing that
  # quietly rather than by exception.
  testthat::expect_identical(
    ravepipeline$find_program("rave-no-such-program-9f3a"), "")
})


testthat::test_that("find_program() finds a program that is installed", {
  # `Rscript` ships with R, so it is present wherever these tests run -- but a
  # stripped `PATH` is still possible, and that is not what this is testing.
  testthat::skip_if(!nzchar(Sys.which("Rscript")), "Rscript is not on PATH")

  found <- ravepipeline$find_program("Rscript")

  testthat::expect_true(nzchar(found))
  testthat::expect_true(file.exists(found))
  # A name carried over from `Sys.which` would break callers comparing with
  # `identical` against a plain path.
  testthat::expect_null(names(found))
})
