# This file is licensed under MIT by Zhengjia Wang

save_json <- function (x, con = stdout(), ..., digits = ceiling(-log10(.Machine$double.eps)),
                       pretty = TRUE, serialize = TRUE) {
  if (serialize) {
    s <- jsonlite::serializeJSON(x, digits = digits, pretty = pretty)
  }
  else {
    s <- jsonlite::toJSON(x, digits = digits, pretty = pretty,
                          ...)
  }
  writeLines(s, con)
  invisible()
}

load_json <- function (con, ...) {
  s <- readLines(con)
  args <- list(...)
  s <- trimws(paste(s, collapse = ""))
  re <- NULL
  if (nzchar(s)) {
    ok <- FALSE
    tryCatch({
      withRestarts({
        re <- jsonlite::unserializeJSON(s)
        ok <- TRUE
      }, abort = function() {
      })
    }, error = function(e) {
    })
    if (!ok) {
      tryCatch({
        withRestarts({
          args$txt <- s
          re <- do.call(jsonlite::fromJSON, args)
          ok <- TRUE
        }, abort = function() {
        })
      }, error = function(e) {
      })
    }
  }
  re
}
