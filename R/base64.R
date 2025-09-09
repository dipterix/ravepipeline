#' @name base64-utils
#' @title Convert from and to 'base64' string
#' @description
#' Encode or decode 'base64' raw or url-safe string
#' @param x for encoders, this is an R raw or character vectors; for decoders
#' this is 'base64' encoded strings
#' @param expr expression for plot, will saved to a \code{\link[grDevices]{png}}
#' and converted to 'base64' string
#' @param width,height image size in pixels
#' @param ... passed to \code{\link[grDevices]{png}}
#' @param quoted,envir non-standard evaluation settings
#' @returns \code{base64_encode}, \code{base64_plot} returns 'base64' string in
#' raw format; \code{base64_urlencode} returns 'base64' string url-safe format;
#' \code{base64_urldecode} returns the original string; \code{base64_decode}
#' returns original raw vectors.
#' @examples
#'
#' # ---- For direct base64URI ------------------------------------
#'
#'
#' file_raw <- as.raw(1:255)
#'
#' # raw base64
#' base64_raw <- base64_encode(file_raw)
#' base64_raw
#'
#' as.integer(base64_decode(base64_raw))
#'
#' # ---- For URL-save base64 ------------------------------------
#' # Can be used in URL
#' base64_url <- base64_urlencode(
#'   paste(c(letters, LETTERS, 0:9),
#'   collapse = ""))
#' base64_url
#'
#' base64_urldecode(base64_url)
#'
#' # ---- Convert R plots to base64 --------------------------------
#' img <- base64_plot({
#'   plot(1:10)
#' }, width = 320, height = 320)
#'
#' # summary
#' print(img)
#'
#' # get base64 content
#' img_base64 <- format(img, type = "content")
#'
#' # save to png
#' tmppng <- tempfile(fileext = ".png")
#' writeBin(base64_decode(img_base64), con = tmppng)
#'
#' # cleanup
#' unlink(tmppng)
#'
#' # Format as svg
#' format(img, type = "html_svg")
#'
#' @export
base64_urlencode <- function (x) {
  if (!length(x)) {
    return(character())
  }
  re <- vapply(enc2utf8(as.character(x)), function(s) {
    base64enc::base64encode(charToRaw(s))
  }, "", USE.NAMES = FALSE)
  re <- gsub("[=]{0,}$", "", re)
  re <- gsub("[+]", "-", re)
  gsub("/", "_", re)
  # unname(base64url::base64_urlencode(x = as.character(x)))
}

#' @rdname base64-utils
#' @export
base64_encode <- function(x) {
  stopifnot2(is.raw(x), msg = "`base64_encode`: the input `x` must be a raw vector")
  #
  # str <- base64url::base64_urlencode(rawToChar(x))
  # str <- gsub("-", "+", str, fixed = TRUE)
  # str <- gsub("_", "/", str, fixed = TRUE)
  # n <- nchar(str)
  # npad <- ceiling(n / 4) * 4 - n
  # if(npad > 0) {
  #   str <- paste(c(str, strrep("=", npad)), collapse = "")
  # }
  # str
  base64enc::base64encode(x)
}

#' @rdname base64-utils
#' @export
base64_urldecode <- function (x) {
  x <- gsub("-", "+", x)
  x <- gsub("_", "/", x)
  vapply(x, function(s) {
    rawToChar(base64enc::base64decode(what = s), multiple = FALSE)
  }, "", USE.NAMES = FALSE)
  # unname(base64url::base64_urldecode(x = x))
}


#' @rdname base64-utils
#' @export
base64_decode <- function(x) {
  # x <- gsub("+", "-", x, fixed = TRUE)
  # x <- gsub("/", "_", x, fixed = TRUE)
  # x <- gsub("[=]+$", "", x)
  # x <- base64url::base64_urldecode(x = x)
  # charToRaw(x)
  base64enc::base64decode(x)
}

#' @rdname base64-utils
#' @export
base64_plot <- function(expr, width = 480, height = 480, ...,
                        quoted = FALSE, envir = parent.frame()) {
  if(!quoted) {
    expr <- substitute(expr)
  }

  tmpfile <- tempfile(fileext = ".png")
  grDevices::png(
    filename = tmpfile, width = width, height = height,
    units = "px", ...)
  dev_on <- TRUE
  on.exit({
    if( dev_on ) {
      grDevices::dev.off()
    }
    unlink(tmpfile)
  })
  eval(expr, envir = envir)
  grDevices::dev.off()
  dev_on <- FALSE
  x <- readBin(tmpfile, "raw", n = file.size(tmpfile))
  x <- base64_encode(x)
  # data:image/png;base64,
  structure(
    paste0("data:image/png;base64,", x),
    class = "base64_png",
    width = width,
    height = height,
    units = "px"
  )
}

#' @export
c.base64_png <- function(x, ...) {
  args <- list(...)
  lapply(args, function(s) {
    if(inherits(s, "base64_png")) {
      stop("Cannot concaternate base64 image strings")
    }
    NULL
  })
  x
}

#' @export
format.base64_png <- function(x, ..., type = c("asis", "content", "summary", "html_svg", "html_img"),
                              width = NA, height = NA, opacity = 1, svg_x = 0, svg_y = 0) {
  type <- match.arg(type)
  switch (
    type,
    "asis" = {
      x <- unclass(x)
      attributes(x) <- NULL
    },
    "summary" = {
      width <- attr(x, "width") %||% width
      height <- attr(x, "height") %||% height
      units <- attr(x, "units") %||% "px"
      prefix <- gsub(";.*$", "", unclass(x))
      x <- sprintf(
        "<Base64DataURI: type=%s, width=%.4g%s, height=%.4g%s>",
        prefix, width, units, height, units
      )
    },
    "content" = {
      x <- gsub("^data[^;]{0,};base64,", "", unclass(x))
      attributes(x) <- NULL
      x
    },
    "html_svg" = {
      stopifnot(package_installed("htmltools"))
      units <- attr(x, "units") %||% "px"
      if(is.na(width)) {
        width <- attr(x, "width")
      }
      if(is.na(height)) {
        height <- attr(x, "height")
      }
      tgs <- call_pkg_fun("htmltools", "tags", .call_pkg_function = FALSE)
      x <- tgs$image(
        x = sprintf("%.0f", svg_x),
        y = sprintf("%.0f", svg_y),
        width = width,
        height = height,
        `xlink:href` = unclass(x),
        opacity = as.character(opacity),
        # <animate attributeName="opacity" values="1;0;1" dur="4s" repeatCount="indefinite"/>
        ...
      )
    },
    "html_img" = {
      stopifnot(package_installed("htmltools"))
      x <- call_pkg_fun("htmltools", "img", src = unclass(x), ...)
    }
  )
  x
}

#' @export
print.base64_png <- function(x, ...) {
  # x <- base64_plot({plot(1:10)})
  cat(format(x, type = "summary"), "\n")
}

