test_that("with_rave_parallel", {
  max_worker <- raveio_getopt("max_worker")

  opt <- options("rave.parallel.workers" = NULL)
  on.exit({ options(opt) })

  testthat::expect_true(on_rave_daemon())

  testthat::expect_equal(calculate_workers(always = TRUE), 0)
  testthat::expect_equal(calculate_workers(workers = 1, always = TRUE), 0)

  with_rave_parallel({

    testthat::expect_false(on_rave_daemon())
    testthat::expect_equal(calculate_workers(workers = 1, always = FALSE), 0)
    testthat::expect_equal(calculate_workers(workers = 1, always = TRUE), 1)
    testthat::expect_equal(calculate_workers(always = TRUE), max_worker)

  }, .workers = 0)


  with_rave_parallel({

    testthat::expect_equal(calculate_workers(always = TRUE), min(max_worker, 2))

  }, .workers = 2)

  with_rave_parallel({

    testthat::expect_equal(calculate_workers(always = TRUE), 1)
    testthat::expect_equal(calculate_workers(always = FALSE), 0)

  }, .workers = 1)

  testthat::expect_null(getOption("rave.parallel.workers"))

})
