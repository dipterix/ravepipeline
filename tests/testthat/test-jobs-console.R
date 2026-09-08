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

  console <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--no-save", "--no-restore", "--no-echo",
      shQuote(file.path(job_root, "script.R"))),
    stdout = TRUE,
    stderr = TRUE,
    # The child must find the same library this session is testing against
    env = sprintf("R_LIBS=%s",
                  paste(.libPaths(), collapse = .Platform$path.sep))
  ))

  log <- tryCatch(
    readLines(file.path(job_root, "console_outputs.txt"), warn = FALSE),
    error = function(e) { character(0L) }
  )
  status <- get_job_status(job_id)
  remove_job(job_id)

  list(
    status = status$status,
    console = paste(console, collapse = "\n"),
    log = paste(log, collapse = "\n")
  )
}


testthat::test_that("a console-backed job streams output and still logs it", {
  # `rs_job` and `vscode_task` have a console the user is looking at, so the
  # job's output has to reach it live -- while the log file stays complete,
  # since `resolve_job()` and `log_maxline` read it.
  res <- run_job_script(tee_console = TRUE)

  testthat::expect_equal(res$status, 3)

  testthat::expect_match(res$console, "STDOUT-MARK")
  testthat::expect_match(res$console, "MESSAGE-MARK")

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
