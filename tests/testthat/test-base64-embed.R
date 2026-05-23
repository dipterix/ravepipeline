require(testthat)

# ---- helpers -----------------------------------------------------------

make_html <- function() {
  f <- tempfile(fileext = ".html")
  writeLines(c("<html>", "<head></head>", "<body></body>", "</html>"), f)
  f
}

# ---- html_embed_write --------------------------------------------------

test_that("html_embed_write returns html_path invisibly", {
  f <- make_html()
  on.exit(unlink(f))

  result <- html_embed_write(f, json_string = list(x = "{}"))
  expect_identical(result, f)
})

test_that("html_embed_write places tags after </body>", {
  f <- make_html()
  on.exit(unlink(f))

  html_embed_write(f, json_string = list(payload = '{"a":1}'))

  content      <- readLines(f)
  body_line    <- grep("</body>", content, ignore.case = TRUE)[[1]]
  script_lines <- grep("data-for='payload'", content, fixed = TRUE)
  expect_true(length(script_lines) >= 1L)
  expect_true(all(script_lines > body_line))
})

test_that("html_embed_write json_string uses application/json MIME type", {
  f <- make_html()
  on.exit(unlink(f))

  html_embed_write(f, json_string = list(myjson = "{}"))
  content <- readLines(f)
  tag     <- grep("data-for='myjson'", content, fixed = TRUE, value = TRUE)[[1]]
  expect_true(grepl("data-type='application/json'", tag, fixed = TRUE))
})

test_that("html_embed_write text_string uses text/plain MIME type", {
  f <- make_html()
  on.exit(unlink(f))

  html_embed_write(f, text_string = list(mytxt = "hello"))
  content <- readLines(f)
  tag     <- grep("data-for='mytxt'", content, fixed = TRUE, value = TRUE)[[1]]
  expect_true(grepl("data-type='text/plain'", tag, fixed = TRUE))
})

test_that("html_embed_write binary_paths uses application/octet-stream MIME type", {
  f   <- make_html()
  tmp <- tempfile(fileext = ".bin")
  on.exit({
    unlink(f)
    unlink(tmp)
  })
  writeBin(as.raw(1:4), tmp)

  html_embed_write(f, binary_paths = list(bindata = tmp))
  content <- readLines(f)
  tag     <- grep("data-for='bindata'", content, fixed = TRUE, value = TRUE)[[1]]
  expect_true(grepl("data-type='application/octet-stream'", tag, fixed = TRUE))
})

test_that("html_embed_write empty json_string writes data-size='0' tag", {
  f <- make_html()
  on.exit(unlink(f))

  html_embed_write(f, json_string = list(empty = ""))
  content <- readLines(f)
  tag     <- grep("data-for='empty'", content, fixed = TRUE, value = TRUE)[[1]]
  expect_true(grepl("data-size='0'", tag, fixed = TRUE))
})

test_that("html_embed_write empty binary file writes data-size='0' tag", {
  f   <- make_html()
  tmp <- tempfile(fileext = ".bin")
  on.exit({
    unlink(f)
    unlink(tmp)
  })
  writeBin(raw(0), tmp)

  html_embed_write(f, binary_paths = list(emptybin = tmp))
  content <- readLines(f)
  tag     <- grep("data-for='emptybin'", content, fixed = TRUE, value = TRUE)[[1]]
  expect_true(grepl("data-size='0'", tag, fixed = TRUE))
})

test_that("html_embed_write missing_action='error' stops when file absent", {
  expect_error(
    html_embed_write("/no/such/file.html",
                     json_string    = list(x = "{}"),
                     missing_action = "error"),
    regexp = "file not found"
  )
})

test_that("html_embed_write missing_action='warning' warns and creates file", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))

  expect_warning(
    html_embed_write(f, json_string = list(x = "{}"), missing_action = "warning"),
    regexp = "creating new file"
  )
  expect_true(file.exists(f))
})

test_that("html_embed_write missing_action='ignore' (default) creates file silently", {
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))

  expect_no_warning(html_embed_write(f, json_string = list(x = "{}"), missing_action = "ignore"))
  expect_true(file.exists(f))
})

test_that("html_embed_write errors when binary_paths file not found", {
  f <- make_html()
  on.exit(unlink(f))

  expect_error(
    html_embed_write(f, binary_paths = list(x = "/no/such/file.bin")),
    regexp = "file not found"
  )
})

# ---- html_embed_read ---------------------------------------------------

test_that("html_embed_read with name=NULL returns manifest object", {
  f <- make_html()
  on.exit(unlink(f))

  html_embed_write(f, json_string = list(alpha = '{"a":1}'))
  manifest <- html_embed_read(f)
  expect_s3_class(manifest, "ravepipeline_html_embed_manifest")
  expect_true("alpha" %in% names(manifest$header))
})

test_that("html_embed_read round-trips a JSON string (parse_json=FALSE)", {
  f <- make_html()
  on.exit(unlink(f))

  original <- '{"hello":"world","nums":[1,2,3]}'
  html_embed_write(f, json_string = list(myjson = original))
  manifest <- html_embed_read(f, name = "myjson", parse_json = FALSE)
  expect_identical(manifest$content[["myjson"]], original)
})

test_that("html_embed_read parses JSON into R object when parse_json=TRUE", {
  f <- make_html()
  on.exit(unlink(f))

  html_embed_write(f, json_string = list(obj = '{"x":1}'))
  manifest <- html_embed_read(f, name = "obj")
  decoded  <- manifest$content[["obj"]]
  expect_type(decoded, "list")
  expect_equal(decoded$x, 1)
})

test_that("html_embed_read round-trips a text string", {
  f <- make_html()
  on.exit(unlink(f))

  original <- "plain text content"
  html_embed_write(f, text_string = list(mytxt = original))
  manifest <- html_embed_read(f, name = "mytxt")
  expect_identical(manifest$content[["mytxt"]], original)
})

test_that("html_embed_read round-trips a binary file", {
  f   <- make_html()
  tmp <- tempfile(fileext = ".bin")
  on.exit({
    unlink(f)
    unlink(tmp)
  })

  original <- as.raw(0:255)
  writeBin(original, tmp)
  html_embed_write(f, binary_paths = list(mybin = tmp))
  manifest <- html_embed_read(f, name = "mybin")
  expect_identical(manifest$content[["mybin"]], original)
})

test_that("html_embed_read round-trips large binary (multi-chunk)", {
  f   <- make_html()
  tmp <- tempfile(fileext = ".bin")
  on.exit({
    unlink(f)
    unlink(tmp)
  })

  chunk_size <- ravepipeline:::HTML_EMBED_CHUNK_SIZE
  n_bytes    <- as.integer(chunk_size) * 2L + 100L
  original   <- as.raw(sample.int(256L, n_bytes, replace = TRUE) - 1L)
  writeBin(original, tmp)

  html_embed_write(f, binary_paths = list(bigbin = tmp))
  content      <- readLines(f)
  script_count <- length(grep("data-for='bigbin'", content, fixed = TRUE))
  expect_true(script_count >= 2L)

  manifest <- html_embed_read(f, name = "bigbin")
  expect_identical(manifest$content[["bigbin"]], original)
})

test_that("html_embed_read round-trips mixed json + text + binary", {
  f   <- make_html()
  tmp <- tempfile(fileext = ".bin")
  on.exit({
    unlink(f)
    unlink(tmp)
  })

  json_str     <- '{"key":"value"}'
  txt_str      <- "some text"
  original_raw <- as.raw(1:10)
  writeBin(original_raw, tmp)

  html_embed_write(f,
    json_string  = list(j = json_str),
    text_string  = list(t = txt_str),
    binary_paths = list(b = tmp)
  )

  manifest <- html_embed_read(f, name = c("j", "t", "b"), parse_json = FALSE)
  expect_identical(manifest$content[["j"]], json_str)
  expect_identical(manifest$content[["t"]], txt_str)
  expect_identical(manifest$content[["b"]], original_raw)
})

test_that("html_embed_read accepts manifest object as path argument", {
  f <- make_html()
  on.exit(unlink(f))

  html_embed_write(f, json_string = list(x = '{"v":1}'))
  manifest1 <- html_embed_read(f)
  manifest2 <- html_embed_read(manifest1, name = "x")
  expect_identical(manifest1, manifest2)
  expect_false(is.null(manifest2$content[["x"]]))
})

test_that("html_embed_read errors when file not found", {
  expect_error(
    html_embed_read("/no/such/file.html"),
    regexp = "file not found"
  )
})

test_that("print.ravepipeline_html_embed_manifest outputs expected header", {
  f <- make_html()
  on.exit(unlink(f))

  html_embed_write(f, json_string = list(item1 = "{}", item2 = "{}"))
  manifest <- html_embed_read(f)
  expect_output(print(manifest), "HTML Embed Manifest")
})
