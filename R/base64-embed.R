#' @title Embed binary data or JSON strings into HTML files
#' @name html-embed
#' @description
#' \code{html_embed_write} encodes JSON strings, plain-text strings, and
#' binary files as base64 \code{<script>} tags and injects them into an HTML
#' file.
#'
#' \code{html_embed_read} reads \code{<script>} tags back out of a saved
#' HTML file and reconstructs the original data.
#'
#' @details
#' \code{html_embed_write} streams data after \code{</body>} (or before
#' \code{</html>}, or appends when neither tag is found).  Large inputs are
#' split into \eqn{\approx 48\,\mathrm{KB}} chunks; each chunk gets its own
#' \code{<script>} tag with a sequential \code{data-partition} index.
#'
#' \code{html_embed_read}: when \code{name} is \code{NULL} it returns a
#' manifest object that lists all embedded entries; subsequent calls with a
#' specific \code{name} use seek positions stored in the manifest to retrieve
#' only the requested partitions.  Files written by compatible tools
#' (e.g. \pkg{threeBrain}) are handled transparently.
#'
#' The per-entry \code{<script>} tag format:
#' \preformatted{
#' <script type='text/plain;charset=UTF-8'
#'         data-for='<name>'
#'         data-partition='<N>'
#'         data-type='application/json|text/plain|application/octet-stream'
#'         data-size='<total bytes>'
#'         data-start='<byte offset>'
#'         data-partition-size='<this chunk bytes>'>
#' BASE64 (72-character wrapped lines)
#' </script>
#' }
#'
#' @param html_path character; path to the HTML file to write.  If the file
#'   does not exist, behavior is controlled by \code{missing_action}.
#' @param json_string named list of character strings; each element is a
#'   UTF-8 JSON string.  The list name becomes the \code{data-for} attribute.
#' @param text_string named list of character strings; each element is an
#'   arbitrary UTF-8 plain-text string.  The list name becomes the
#'   \code{data-for} attribute.
#' @param binary_paths named list of character strings; each element is an
#'   absolute path to a binary file to embed.  The list name becomes the
#'   \code{data-for} attribute.
#' @param missing_action character; what to do when \code{html_path} does
#'   not exist.  \code{"error"} (default) stops with an error;
#'   \code{"warning"} emits a warning and creates the file;
#'   \code{"ignore"} creates the file silently.
#' @param path character path to an HTML file, or a manifest object returned
#'   by a previous call to \code{html_embed_read(path, name = NULL)}.
#' @param name character; the \code{data-for} name of the entry to decode.
#'   When \code{NULL} (default) the function returns a manifest object that
#'   lists all embedded entries without decoding them.
#' @param parse_json logical; when \code{TRUE} (default) JSON entries are
#'   parsed with \code{jsonlite::fromJSON} before being returned.
#' @param update logical; when \code{FALSE} (default) already-decoded entries
#'   cached in the manifest are returned as-is without re-reading the file.
#' @returns
#' \code{html_embed_write}: \code{html_path}, invisibly.
#'
#' \code{html_embed_read}: when \code{name} is \code{NULL}, a manifest object
#'   of class \code{ravepipeline_html_embed_manifest} listing all embedded
#'   entries.  When \code{name} is specified the manifest is returned with the
#'   requested entry decoded and cached; access it via
#'   \code{manifest$content[[name]]}: a character string for JSON/text data,
#'   or a raw vector for binary data.
#' @examples
#'
#' html_file <- tempfile(fileext = ".html")
#' writeLines(
#'   c("<html>", "<head></head>", "<body></body>", "</html>"),
#'   html_file
#' )
#'
#' # ---- Write: embed JSON and binary data into an HTML file --------
#' tmp <- tempfile(fileext = ".bin")
#' writeBin(as.raw(0:255), tmp)
#'
#' html_embed_write(
#'   html_file,
#'   json_string  = list(meta = '{"version":1}'),
#'   text_string  = list(note = "hello world"),
#'   binary_paths = list(data = tmp)
#' )
#'
#' # ---- Read: list all embedded entries ----------------------------
#' manifest <- html_embed_read(html_file)
#' print(manifest)
#'
#' # ---- Read: decode a specific entry ------------------------------
#' manifest <- html_embed_read(html_file, name = "meta")
#' manifest$content[["meta"]]   # character (JSON string or parsed object)
#'
#' manifest <- html_embed_read(manifest, name = "data")
#' manifest$content[["data"]]   # raw vector
#'
#' unlink(c(tmp, html_file))
#'
NULL

# Chunk size yields a base64 string just under the 65536-char JS string limit.
HTML_EMBED_CHUNK_SIZE <- floor(65529 / 73 * 54)  # 48 492

# Extract a named HTML attribute value (single or double quotes).
html_embed_attr_val <- function(tag, attr) {
  pattern <- paste0("\\b", attr, "=['\"]([^'\"]*)['\"]")
  m <- regmatches(tag, regexpr(pattern, tag, perl = TRUE))
  if (!length(m)) return(NA_character_)
  sub(pattern, "\\1", m, perl = TRUE)
}

as_integer2 <- function(x) {
  suppressWarnings({ as.integer(x) })
}

html_embed_parse_manifest <- function(con) {

  stopifnot("`con` must be a connection" = inherits(con, "connection"))

  # Manifest must exist within <head></head>
  # con <- file("~/junk.html", "rb")
  # con <- file("~/Downloads/junk2.html", "rb")

  manifest <- fastmap::fastmap()

  # Somehow this is fast
  while (length(line <- readLines(con, n = 1)) > 0) {

    if (grepl(
      pattern = "<script[^>]*\\bdata-for=['\"]['\"]?",
      x = line,
      perl = TRUE,
      ignore.case = TRUE
    )) {

      data_name <- html_embed_attr_val(line, "data-for")

      if (is.na(data_name) || !nzchar(data_name)) {
        next
      }

      # actual data
      partitions <- as_integer2(html_embed_attr_val(line, "data-partition")) + 1
      type <- html_embed_attr_val(line, "data-type")
      size <- as_integer2(html_embed_attr_val(line, "data-size"))

      if (is.na(partitions) || is.na(type) || is.na(size)) {
        next
      }

      # ceiling(size / HTML_EMBED_CHUNK_SIZE) is good enough, but just in case
      expected_npartitions <- max(ceiling(size / HTML_EMBED_CHUNK_SIZE) + 1, partitions)

      entry <- manifest$get(
        data_name,
        missing = list(
          partitions = partitions,
          type = type,
          size = size,
          seek_info = rep(0, expected_npartitions)
        )
      )
      entry$partitions <- max(entry$partitions, partitions, na.rm = TRUE)

      # 2 bytes in windows
      entry$seek_info[[partitions]] <- seek(con) - nchar(line, type = "bytes") - 2L

      manifest$set(data_name, entry)

      partition_size <- as_integer2(html_embed_attr_val(line, "data-parition-size"))

      if (isTRUE(partition_size > 0)) {
        # skip chars: be conservative
        skip_chars <- floor(partition_size / 3 * 4) - nchar(line)
        readChar(con, skip_chars)
      }

    }

  }

  class(manifest) <- c("ravepipeline_fastmap2", "fastmap2", "list")
  manifest
}

html_embed_read_manifest <- function(path) {
  stopifnot2(file.exists(path), msg = sprintf("`path` file not found: %s", path))

  con <- file(path, open = "rb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  manifest <- fastmap2()
  manifest$path <- path
  manifest$header <- html_embed_parse_manifest(con)
  manifest$content <- fastmap2()

  class(manifest) <- c("ravepipeline_html_embed_manifest", class(manifest))
  return(manifest)
}

#' @export
print.ravepipeline_html_embed_manifest <- function(x, ...) {

  dnames <- sort(names(x$header))
  loaded <- ifelse(dnames %in% names(x$content), " (loaded)", "")
  str <- c(
    "<HTML Embed Manifest>",
    sprintf("Path: %s", x$path),
    "Data Names:",
    sprintf("  `%s`%s", dnames, loaded),
    ""
  )
  cat(str, sep = "\n")
}

## decoder
#' @rdname html-embed
#' @export
html_embed_read <- function(path, name = NULL, parse_json = TRUE, update = FALSE) {

  # path <- "~/junk.html"

  # path can be path or manifest

  if (inherits(path, "ravepipeline_html_embed_manifest")) {
    manifest <- path
    path <- manifest$path
  } else {
    manifest <- html_embed_read_manifest(path)
  }

  if (is.null(name)) {
    return(manifest)
  }

  results <- manifest$content
  if (!inherits(results, "fastmap2")) {
    results <- fastmap2()
    manifest$content <- results
  }

  item_queue <- fastmap::fastqueue()

  fin <- file(path, open = "rb")
  on.exit({ try(silent = TRUE, { close(fin) }) })

  lapply(name, function(data_name) {
    # data_name <- "#cvs_avg35_inMNI152/surf/rh.sulc"

    if (!update && manifest$content$`@has`(data_name)) {
      return()
    }

    data_info <- manifest$header[[data_name]]
    if (!is.list(data_info) || !isTRUE(data_info$size > 0)) {
      return()
    }
    partitions <- data_info$partitions

    # no seek info
    if (!isTRUE(partitions > 0) && length(data_info$seek_info) < partitions) {
      return()
    }

    item_queue$reset()

    # the data can be sought
    partition_strs <- vapply(seq_len(partitions), function(partition) {
      sid <- data_info$seek_info[[partition]]
      seek(fin, origin = "start", where = sid)

      while (length(line <- readLines(fin, n = 1L)) > 0) {
        item_queue$add(line)
        if (grepl("</script>", line, ignore.case = TRUE)) {
          break
        }
      }

      partition_str <- paste(unlist(item_queue$as_list()), collapse = "")
      item_queue$reset()

      partition_str <- gsub(".*<script[^>]*>", "", partition_str, ignore.case = TRUE)
      gsub("</script>.*", "", partition_str, ignore.case = TRUE)
    }, "")

    # check type
    item <- base64enc::base64decode(what = partition_strs)

    if (startsWith(tolower(data_info$type), "text")) {
      # text
      item <- rawToChar(item)
    } else if (grepl("json", data_info$type, ignore.case = TRUE)) {
      # JSON
      item <- rawToChar(item)

      if (parse_json && endsWith(tolower(data_info$type), "json")) {
        item <- jsonlite::fromJSON(
          item,
          simplifyMatrix = FALSE,
          simplifyDataFrame = FALSE,
          simplifyVector = TRUE
        )
      }
    } else {
      # binary
    }

    results[[data_name]] <- item
    return()
  })

  return(manifest)
}


# ---- html writer -------------------------------------------------------

#' @rdname html-embed
#' @export
html_embed_write <- function(html_path,
                             json_string  = list(),
                             text_string  = list(),
                             binary_paths = list(),
                             missing_action = c("error", "warning", "ignore")) {
  missing_action <- match.arg(missing_action)
  chunk_size <- HTML_EMBED_CHUNK_SIZE
  if (file.exists(html_path)) {
    html_lines <- readLines(html_path, warn = FALSE)
  } else {
    switch(missing_action,
      "error"   = stop("`html_path` file not found: ", html_path),
      "warning" = warning("`html_path` file not found, creating new file: ", html_path)
    )
    html_lines <- character(0L)
  }

  # Split at last </body>; fall back to before </html>; else append.
  body_close_idx <- grep("</body>", html_lines, ignore.case = TRUE)
  if (length(body_close_idx) > 0L) {
    idx  <- body_close_idx[[length(body_close_idx)]]
    pre  <- html_lines[seq_len(idx)]
    post <- html_lines[seq.int(idx + 1L, length(html_lines))]
  } else {
    html_close_idx <- grep("</html>", html_lines, ignore.case = TRUE)
    if (length(html_close_idx) > 0L) {
      idx  <- html_close_idx[[length(html_close_idx)]]
      pre  <- html_lines[seq_len(idx - 1L)]
      post <- html_lines[seq.int(idx, length(html_lines))]
    } else {
      pre  <- html_lines
      post <- character(0L)
    }
  }

  # Combine json and text items — differ only in MIME type.
  mem_items <- c(
    lapply(names(json_string), function(nm) {
      list(nm = nm, x = json_string[[nm]], data_type = "application/json")
    }),
    lapply(names(text_string), function(nm) {
      list(nm = nm, x = text_string[[nm]], data_type = "text/plain")
    })
  )

  conn <- file(html_path, open = "w")
  on.exit(close(conn), add = TRUE)
  writeLines(pre, conn, sep = "\n")

  # ---- in-memory items (json + text) ----
  for (item in mem_items) {
    nm        <- item$nm
    raw_data  <- charToRaw(enc2utf8(item$x))
    fsize     <- as.numeric(length(raw_data))
    data_type <- item$data_type
    if (fsize == 0) {
      writeLines(sprintf(
        "<script type='text/plain;charset=UTF-8' data-for='%s' data-partition='0' data-type='%s' data-size='0' data-start='0' data-partition-size='0'></script>",
        nm, data_type
      ), conn, sep = "\n")
    } else {
      ii    <- 0L
      start <- 0L
      while (start < fsize) {
        chunk_end <- min(start + chunk_size, fsize)
        chunk     <- raw_data[seq.int(start + 1L, chunk_end)]
        n         <- length(chunk)
        encoded   <- base64enc::base64encode(what = chunk, linewidth = 72L)
        writeLines(c(
          sprintf(
            "<script type='text/plain;charset=UTF-8' data-for='%s' data-partition='%d' data-type='%s' data-size='%.0f' data-start='%.0f' data-partition-size='%.0f'>",
            nm, ii, data_type, fsize, start, n
          ),
          encoded, "</script>"
        ), conn, sep = "\n")
        start <- start + n
        ii    <- ii + 1L
      }
    }
  }

  # ---- binary files ----
  for (nm in names(binary_paths)) {
    x <- binary_paths[[nm]]
    if (!file.exists(x)) stop("`binary_paths[['", nm, "']]` file not found: ", x)
    fsize <- file.size(x)
    if (fsize == 0) {
      writeLines(sprintf(
        "<script type='text/plain;charset=UTF-8' data-for='%s' data-partition='0' data-type='application/octet-stream' data-size='0' data-start='0' data-partition-size='0'></script>",
        nm
      ), conn, sep = "\n")
    } else {
      local({
        fin <- file(x, open = "rb")
        on.exit(close(fin), add = TRUE)
        ii        <- 0L
        start     <- 0
        remaining <- fsize
        while (remaining > 0) {
          chunk   <- readBin(fin, what = "raw", n = min(remaining, chunk_size))
          n       <- length(chunk)
          encoded <- base64enc::base64encode(what = chunk, linewidth = 72L)
          writeLines(c(
            sprintf(
              "<script type='text/plain;charset=UTF-8' data-for='%s' data-partition='%d' data-type='application/octet-stream' data-size='%.0f' data-start='%.0f' data-partition-size='%.0f'>",
              nm, ii, fsize, start, n
            ),
            encoded, "</script>"
          ), conn, sep = "\n")
          start     <- start + n
          remaining <- remaining - n
          ii        <- ii + 1L
        }
      })
    }
  }

  writeLines(post, conn, sep = "\n")

  invisible(html_path)
}

