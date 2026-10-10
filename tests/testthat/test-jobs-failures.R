# Each test here starts real background R processes, more than a CRAN check
# machine should be asked to do. Each also guards a wait that used to last
# forever, so each limits its own run time: should the bug come back, the test
# fails instead of hanging the whole suite.

testthat::test_that("a job whose packages cannot be loaded fails with that error", {
  testthat::skip_on_cran()
  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit({ setTimeLimit() }, add = TRUE)

  # The job's error handler used to need `ravepipeline` loaded, so it failed
  # as well when a package could not be loaded (`ravepipeline` itself, e.g.
  # while it was being reinstalled): the job stayed "initialized" and
  # `start_job()` waited for it forever
  job_id <- start_job(
    fun = function() { "done" },
    packages = "ravepipelineNoSuchPackage",
    method = "callr"
  )
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)

  testthat::expect_equal(check_job(job_id)$status, -1)
  testthat::expect_error(
    resolve_job(job_id, timeout = 30, auto_remove = FALSE, unresolved = "error"),
    "ravepipelineNoSuchPackage"
  )
})

testthat::test_that("a job whose process ends without a result is reported as failed", {
  testthat::skip_on_cran()
  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit({ setTimeLimit() }, add = TRUE)

  # The process ends, as when it is killed or crashes, without the job's
  # error handler running: only the session holding the process can tell.
  # `parent` keeps this from ever quitting the test session itself
  job_id <- start_job(
    fun = function(parent) {
      if (Sys.getpid() != parent) {
        quit(save = "no", status = 3)
      }
      "not in a separate process"
    },
    fun_args = list(parent = Sys.getpid()),
    method = "callr"
  )
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)

  # Used to wait out the whole timeout and then call the job still running
  started <- Sys.time()
  testthat::expect_error(
    resolve_job(job_id, timeout = 30, auto_remove = FALSE, unresolved = "error"),
    "status 3"
  )
  testthat::expect_lt(as.numeric(Sys.time() - started, units = "secs"), 20)
  testthat::expect_equal(check_job(job_id)$status, -1)
})

testthat::test_that("a job whose process exits before the job starts is reported as failed", {
  testthat::skip_on_cran()

  # What `start_job()` waits on when the process dies before the job's script
  # writes anything (R fails to start, a start-up profile errors): a prepared
  # job whose process has already exited
  job_id <- prepare_job(fun = function() { "done" })
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)
  process <- callr::r_bg(function() { quit(save = "no", status = 3) })
  process$wait(timeout = 30000)
  attr(job_id, "callr_process") <- process

  status <- check_job(job_id)
  testthat::expect_equal(status$status, -1)
  testthat::expect_match(conditionMessage(status$error), "status 3")

  # Saved with the job, so a check by the bare ID finds it too
  testthat::expect_equal(check_job(as.character(job_id))$status, -1)
})

testthat::test_that("a job that finished is not reported as failed once its process exits", {
  testthat::skip_on_cran()
  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit({ setTimeLimit() }, add = TRUE)

  job_id <- start_job(fun = function() { "done" }, method = "callr")
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)

  # The process has exited by now; the job's own final status must win
  attr(job_id, "callr_process")$wait(timeout = 30000)
  testthat::expect_equal(check_job(job_id)$status, 3)
  testthat::expect_identical(
    resolve_job(job_id, timeout = 30, auto_remove = FALSE, unresolved = "error"),
    "done"
  )
})

testthat::test_that("an interrupted job is reported as interrupted", {
  testthat::skip_on_cran()
  # An interrupt reaches a background process on Windows only through its
  # console, which a `callr` process may not have
  testthat::skip_on_os("windows")
  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit({ setTimeLimit() }, add = TRUE)

  # Ctrl+C in an editor terminal interrupts the job's R code. With no handler
  # for it the job ended without a final status, and backends without a
  # process handle (`rs_job`, `vscode_task`) waited for it forever
  job_id <- start_job(fun = function() { Sys.sleep(30); "late" }, method = "callr")
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)
  while (check_job(job_id)$status < 2) {
    Sys.sleep(0.05)
  }
  attr(job_id, "callr_process")$interrupt()

  testthat::expect_error(
    resolve_job(job_id, timeout = 30, auto_remove = FALSE, unresolved = "error"),
    "interrupted"
  )
})

testthat::test_that("lapply_jobs fails when a worker process ends without a result", {
  testthat::skip_on_cran()
  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit({ setTimeLimit() }, add = TRUE)

  # Used to wait for the dead worker forever
  parent <- Sys.getpid()
  testthat::expect_error(
    with_rave_parallel(
      lapply_jobs(1:3, function(x, parent) {
        if (x == 2 && Sys.getpid() != parent) {
          quit(save = "no", status = 3)
        }
        x
      }, parent = parent, .workers = 2, .always = TRUE),
      .workers = 2
    ),
    "status 3"
  )
})

testthat::test_that("lapply_jobs stops its other workers when it fails", {
  testthat::skip_on_cran()
  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit({ setTimeLimit() }, add = TRUE)

  # The first worker fails while the second keeps writing a heartbeat. The
  # third element waits for a free worker, which checks every running job, so
  # the failure is seen while the second worker is still running
  heartbeat <- tempfile()
  on.exit({ unlink(heartbeat) }, add = TRUE)
  parent <- Sys.getpid()
  testthat::expect_error(
    with_rave_parallel(
      lapply_jobs(1:3, function(x, parent, heartbeat) {
        if (Sys.getpid() == parent) {
          return(x)
        }
        if (x == 1) {
          Sys.sleep(1)
          stop("the first worker failed")
        }
        if (x == 2) {
          for (i in seq_len(300)) {
            writeLines(as.character(i), heartbeat)
            Sys.sleep(0.1)
          }
        }
        x
      }, parent = parent, heartbeat = heartbeat, .workers = 2, .always = TRUE),
      .workers = 2
    ),
    "the first worker failed"
  )

  # Its clean-up used to drop the process handles, so the second worker kept
  # running in the background
  read_beat <- function() {
    if (file.exists(heartbeat)) readLines(heartbeat) else character(0L)
  }
  beat <- read_beat()
  Sys.sleep(1)
  testthat::expect_identical(read_beat(), beat)
})

testthat::test_that("a mirai job whose packages cannot be loaded fails with that error", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit({ setTimeLimit() }, add = TRUE)

  job_id <- start_job(
    fun = function() { "done" },
    packages = "ravepipelineNoSuchPackage",
    method = "mirai"
  )
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)

  testthat::expect_error(
    resolve_job(job_id, timeout = 30, auto_remove = FALSE, unresolved = "error"),
    "ravepipelineNoSuchPackage"
  )
})

testthat::test_that("a mirai job whose process ends without a result is reported as failed", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("mirai")
  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit({ setTimeLimit() }, add = TRUE)

  job_id <- start_job(
    fun = function(parent) {
      if (Sys.getpid() != parent) {
        quit(save = "no", status = 3)
      }
      "not in a separate process"
    },
    fun_args = list(parent = Sys.getpid()),
    method = "mirai"
  )
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)

  started <- Sys.time()
  testthat::expect_error(
    resolve_job(job_id, timeout = 30, auto_remove = FALSE, unresolved = "error")
  )
  testthat::expect_lt(as.numeric(Sys.time() - started, units = "secs"), 20)
  testthat::expect_equal(check_job(job_id)$status, -1)
})

testthat::test_that("a promise for a job that was removed is rejected", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("later")

  # A removed job reads as -2, which the promise took for "still running":
  # it never settled, and a `shiny::ExtendedTask` waiting on it never ran
  # another task
  job_id <- start_job(fun = function() { Sys.sleep(30); "late" }, method = "callr")
  remove_job(job_id)

  settled <- NULL
  promises::then(
    promises::as.promise(job_id),
    onFulfilled = function(value) { settled <<- "fulfilled" },
    onRejected = function(error) { settled <<- "rejected" }
  )
  deadline <- Sys.time() + 5
  while (is.null(settled) && Sys.time() < deadline) {
    later::run_now(0.1)
  }
  testthat::expect_identical(settled, "rejected")
})

testthat::test_that("a job stopped by garbage collection is reported as failed", {
  testthat::skip_on_cran()

  job_id <- start_job(fun = function() { Sys.sleep(30); "late" }, method = "callr")
  plain_id <- as.character(job_id)
  on.exit({ try(remove_job(plain_id), silent = TRUE) }, add = TRUE)

  # Once nothing holds the object `start_job()` returned, its process is
  # stopped. The job used to read "running" forever after that, to anyone
  # checking it by the bare ID
  rm(job_id)
  invisible(gc())

  status <- check_job(plain_id)
  testthat::expect_equal(status$status, -1)
  testthat::expect_match(conditionMessage(status$error), "garbage")
})
