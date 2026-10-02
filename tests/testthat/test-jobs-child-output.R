# Spawns a real background R process, more than a CRAN check machine should
# be asked to do.
testthat::test_that("a callr job whose child program prints a lot finishes", {
  testthat::skip_on_cran()

  # A program the job starts writes to the job process's stdout, not to the
  # job's log: with an undrained pipe there, this job would never finish
  job_id <- start_job(
    fun = function() {
      system2(file.path(R.home("bin"), "Rscript"),
              c("-e", shQuote("for (i in 1:20000) cat(strrep('x', 60), i, '\\n')")))
      "done"
    },
    method = "callr"
  )
  on.exit({ try(remove_job(job_id), silent = TRUE) }, add = TRUE)

  result <- resolve_job(job_id, timeout = 120, auto_remove = FALSE,
                        unresolved = "error")
  testthat::expect_identical(result, "done")

  outputs <- file.path(get_job_path(job_id, check = FALSE), "process_outputs.txt")
  testthat::expect_true(file.exists(outputs))
  testthat::expect_gt(file.size(outputs), 64 * 1024)
})
