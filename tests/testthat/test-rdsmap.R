test_that('RDS map', {
  path <- tempfile()
  generator <- ravepipeline:::FileMap

  self <- generator$new(path = path)
  private <- self$.__enclos_env__$private
  self$reset()
  self$validate()
  expect_equal(self$size(), 0)
  expect_identical(self$as_list(), list())

  expect_identical(self$set('a', 124), self$digest(124))
  self$validate()
  expect_equal(self$get('a'), 124)
  self$set('b', 11231)
  expect_equal(self$get('b'), 11231)
  self$validate()
  self$set('a', 123)
  self$validate()
  expect_true(all(c('a', 'b') %in% self$keys()))

  expect_equal(dim(self$keys(include_signatures = TRUE)), c(2,2))
  expect_true(self$has(keys = 'a'))
  expect_true(self$has(keys = 'a', signature = 123))
  expect_true(self$has(keys = 'a', signature = self$digest(123), sig_encoded = TRUE))

  expect_false(self$has(keys = 'a', signature = self$digest(123), sig_encoded = FALSE))

  self$validate()
  expect_equal(self$mget(c('a', 'b', 'a')), list(a=123, b = 11231, a = 123))

  expect_equal(self$as_list(sort = TRUE), list(a=123, b = 11231))

  expect_null( self$get('c') )
  expect_equal( self$get('c', missing_default = 'aaa'), 'aaa' )
  self$missing_default <- 'hahaha'
  expect_equal(self$get('c'), 'hahaha')
  expect_equal( self$get('c', missing_default = 'aaa'), 'aaa' )

  self$remove('a')

  self <- generator$new(path = path)

  expect_equal(unname(self$has(c('a', 'b'))), c(FALSE, TRUE))

  expect_equal(self$size(), 1)

  self$reset()
  expect_equal(self$size(), 0)
  expect_equal(unname(self$has(c('a', 'b'))), c(FALSE, FALSE))

  expect_true(self$is_valid)
  self$destroy()

  expect_false(self$is_valid)

  expect_error({
    self$validate()
  }, label = "rdsmap should fail after disposal")
})

