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

testthat::test_that("a status file that cannot be read is not taken for a failed job", {
  testthat::skip_on_cran()

  # Once a job's process has exited, its status file holds whatever the job
  # wrote last, possibly its final status. A file that cannot be read at that
  # moment (held by a virus scanner, say) must not be overwritten with a
  # failure: `resolve_job()` would then raise and delete the job's result
  job_id <- prepare_job(fun = function() { "done" })
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)
  status_path <- file.path(get_job_path(job_id, check = FALSE), "status.rds")
  running <- readRDS(status_path)
  running$status <- 2
  writeLines("not an RDS file", status_path)

  status <- job_status_ended(job_id, running, status_path,
                             "its process exited with status 0")
  testthat::expect_equal(status$status, 2)
  testthat::expect_identical(readLines(status_path), "not an RDS file")
})

testthat::test_that("a primitive function is refused before any job starts", {
  testthat::skip_on_cran()

  # A primitive has no body to send, so the job used to run `function() NULL`:
  # `start_job(sum)` returned NULL, and workers failed with "unused argument"
  testthat::expect_error(start_job(sum, method = "callr"), "primitive")
  testthat::expect_error(
    with_rave_parallel(
      lapply_jobs(1:2, sqrt, .workers = 2, .always = TRUE),
      .workers = 2
    ),
    "primitive"
  )
})

testthat::test_that("a job given a function's name runs that function", {
  testthat::skip_on_cran()

  # `formals()` and `body()` accept a function's name, so this has worked all
  # along; `utils::removeSource()` does not accept one
  job_id <- start_job(fun = "mean", fun_args = list(c(1, 2, 3)), method = "callr")
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)

  testthat::expect_equal(
    resolve_job(job_id, timeout = 60, auto_remove = FALSE, unresolved = "error"),
    2
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
