# These tests prepare or start real background jobs, more than a CRAN check
# machine should be asked to do.

testthat::test_that("resolve_job sleeps between checks while it waits on an old job", {
  testthat::skip_on_cran()

  # A job reported running for 100 s, longer than the `timeout` below. It has
  # no process, so `resolve_job` keeps checking until it gives up
  job_id <- prepare_job(fun = function() { "done" })
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)
  status_path <- file.path(get_job_path(job_id, check = FALSE), "status.rds")
  status <- readRDS(status_path)
  status$status <- 2
  status$start_time <- Sys.time() - 100
  saveRDS(status, status_path)

  started <- proc.time()
  resolve_job(job_id, timeout = 1, auto_remove = FALSE, unresolved = "silent")
  used <- proc.time() - started

  # It measured the time left from the job's start, so once the job was older
  # than `timeout` it never slept and kept a processor core busy
  testthat::expect_lt(used[["user.self"]] + used[["sys.self"]], 0.5)
})

testthat::test_that("a job whose function body is a single value runs", {
  testthat::skip_on_cran()

  # `utils::removeSource()` rejected the body when it was a constant
  job_id <- start_job(fun = function() "done", method = "callr")
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)

  testthat::expect_identical(
    resolve_job(job_id, timeout = 60, auto_remove = FALSE, unresolved = "error"),
    "done"
  )
})

testthat::test_that("lapply_jobs runs a function whose body is a single value", {
  testthat::skip_on_cran()

  results <- with_rave_parallel(
    lapply_jobs(1:2, function(x) 42, .workers = 2, .always = TRUE),
    .workers = 2
  )
  testthat::expect_identical(unlist(results), c(42, 42))
})
