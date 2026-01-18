test_that("@return extraction works with \\describe{} blocks", {
  roxygen_block <- c(
    "#' Test Function",
    "#' @description A test function",
    "#' @return A list containing:",
    "#' \\describe{",
    "#'   \\item{success}{Logical, whether operation succeeded}",
    "#'   \\item{message}{Character, status message}",
    "#'   \\item{count}{Integer, number of items}",
    "#'   \\item{items}{Character vector, list of item names}",
    "#' }",
    "#' @keywords mcp-tool mcp-category-test"
  )
  
  tool <- ravepipeline:::extract_tool_from_roxygen_block(
    roxygen_block, "test_func", "testpkg", "test.R"
  )
  
  expect_true(!is.null(tool$returns))
  expect_equal(tool$returns$type, "object")
  expect_true(!is.null(tool$returns$properties))
  expect_equal(tool$returns$properties$success$type, "boolean")
  expect_equal(tool$returns$properties$message$type, "string")
  expect_equal(tool$returns$properties$count$type, "integer")
  expect_equal(tool$returns$properties$items$type, "array")
  expect_equal(tool$returns$properties$items$items$type, "string")
})

test_that("@returns (plural) works same as @return", {
  roxygen_block <- c(
    "#' Test Function",
    "#' @returns A character string"
  )
  
  tool <- ravepipeline:::extract_tool_from_roxygen_block(
    roxygen_block, "test_func", "testpkg", "test.R"
  )
  
  expect_true(!is.null(tool$returns))
  expect_equal(tool$returns$type, "string")
})

test_that("simple @return without \\describe{} is parsed", {
  roxygen_block <- c(
    "#' Test Function",
    "#' @return Character vector of names"
  )
  
  tool <- ravepipeline:::extract_tool_from_roxygen_block(
    roxygen_block, "test_func", "testpkg", "test.R"
  )
  
  expect_true(!is.null(tool$returns))
  expect_equal(tool$returns$type, "array")
  expect_equal(tool$returns$items$type, "string")
})

test_that("missing @return is handled gracefully", {
  roxygen_block <- c(
    "#' Test Function",
    "#' @description A test function",
    "#' @keywords mcp-tool"
  )
  
  tool <- ravepipeline:::extract_tool_from_roxygen_block(
    roxygen_block, "test_func", "testpkg", "test.R"
  )
  
  expect_true(is.null(tool$returns))
})

test_that("nested braces in item descriptions are handled", {
  roxygen_block <- c(
    "#' Test Function",
    "#' @return A list:",
    "#' \\describe{",
    "#'   \\item{data}{List with \\code{x} and \\code{y} coordinates}",
    "#'   \\item{meta}{Object with \\code{name}, \\code{id}, and \\code{type}}",
    "#' }"
  )
  
  tool <- ravepipeline:::extract_tool_from_roxygen_block(
    roxygen_block, "test_func", "testpkg", "test.R"
  )
  
  expect_true(!is.null(tool$returns))
  expect_equal(tool$returns$type, "object")
  expect_true(!is.null(tool$returns$properties$data))
  expect_true(!is.null(tool$returns$properties$meta))
})

test_that("multi-line @return descriptions are concatenated", {
  roxygen_block <- c(
    "#' Test Function",
    "#' @return A list containing:",
    "#' \\describe{",
    "#'   \\item{name}{Character. The name of",
    "#'     the object}",
    "#'   \\item{value}{Numeric. The computed",
    "#'     value}",
    "#' }"
  )
  
  tool <- ravepipeline:::extract_tool_from_roxygen_block(
    roxygen_block, "test_func", "testpkg", "test.R"
  )
  
  expect_true(!is.null(tool$returns))
  expect_equal(tool$returns$type, "object")
  expect_equal(tool$returns$properties$name$type, "string")
  expect_equal(tool$returns$properties$value$type, "number")
})

test_that("array return types are detected", {
  roxygen_block <- c(
    "#' Test Function",
    "#' @return A list:",
    "#' \\describe{",
    "#'   \\item{files}{Character[], array of file paths}",
    "#'   \\item{sizes}{Numeric vector of file sizes}",
    "#' }"
  )
  
  tool <- ravepipeline:::extract_tool_from_roxygen_block(
    roxygen_block, "test_func", "testpkg", "test.R"
  )
  
  expect_true(!is.null(tool$returns))
  expect_equal(tool$returns$properties$files$type, "array")
  expect_equal(tool$returns$properties$files$items$type, "string")
  expect_equal(tool$returns$properties$sizes$type, "array")
  expect_equal(tool$returns$properties$sizes$items$type, "number")
})
