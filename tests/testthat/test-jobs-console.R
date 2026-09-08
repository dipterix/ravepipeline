# Each test spawns a real `Rscript` subprocess, which is more than a CRAN check
# machine should be asked to do.
testthat::skip_on_cran()

# The script `prepare_job()` generates is standalone, so both console modes can
# be run through a real `Rscript` and inspected -- no editor, no backend needed.
run_job_script <- function(tee_console) {
  job_id <- prepare_job(
    fun = function() {
      cat("STDOUT-MARK\n")
      message("MESSAGE-MARK")
      TRUE
    },
    workdir = tempdir(),
    tee_console = tee_console
  )
  job_root <- get_job_path(job_id, check = FALSE)

  # `callr` rather than `system2`, whose `env=` is the shell's `VAR=value cmd`
  # prefix and so means nothing to `cmd.exe`. `callr` also passes `.libPaths()`
  # through, which is how the child finds the library under test.
  child <- callr::rscript(
    file.path(job_root, "script.R"),
    show = FALSE,
    fail_on_status = FALSE,
    # Nothing a profile prints may be mistaken for the job's own output
    user_profile = FALSE,
    system_profile = FALSE
  )

  log <- tryCatch(
    readLines(file.path(job_root, "console_outputs.txt"), warn = FALSE),
    error = function(e) { character(0L) },
    warning = function(w) { character(0L) }
  )
  status <- get_job_status(job_id)
  remove_job(job_id)

  list(
    status = status$status,
    stdout = paste(child$stdout, collapse = "\n"),
    stderr = paste(child$stderr, collapse = "\n"),
    console = paste(c(child$stdout, child$stderr), collapse = "\n"),
    log = paste(log, collapse = "\n")
  )
}


testthat::test_that("a console-backed job streams output and still logs it", {
  # `rs_job` and `vscode_task` have a console the user is looking at, so the
  # job's output has to reach it live -- while the log file stays complete,
  # since `resolve_job()` and `log_maxline` read it.
  res <- run_job_script(tee_console = TRUE)

  testthat::expect_equal(res$status, 3)

  # Both land on stdout specifically: `cat` through the split, and `message`
  # through the handler that reroutes it there.
  testthat::expect_match(res$stdout, "STDOUT-MARK")
  testthat::expect_match(res$stdout, "MESSAGE-MARK")

  testthat::expect_match(res$log, "STDOUT-MARK")
  testthat::expect_match(res$log, "MESSAGE-MARK")
})


testthat::test_that("a job without a console stays quiet", {
  # `callr` hands the child a pipe nothing drains, so anything written to the
  # real stdout could block the job once the buffer fills. Nothing may leak.
  res <- run_job_script(tee_console = FALSE)

  testthat::expect_equal(res$status, 3)

  testthat::expect_false(grepl("STDOUT-MARK", res$console, fixed = TRUE))
  testthat::expect_false(grepl("MESSAGE-MARK", res$console, fixed = TRUE))

  testthat::expect_match(res$log, "STDOUT-MARK")
  testthat::expect_match(res$log, "MESSAGE-MARK")
})
