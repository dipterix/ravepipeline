str2lang_alt <- function (s) {
  s <- sprintf("quote(%s)", trimws(s))
  eval(parse(text = s))
}

str2lang <- function (s) {
  get0("str2lang", envir = baseenv(), ifnotfound = str2lang_alt)(s)
}

# These functions are available in R 4.0. However, to be backward compatible
deparse1 <- function(expr, collapse = ' '){
  paste(deparse(expr), collapse = collapse)
}

rand_string <- function(length = 51){
  pid <- as.integer(Sys.getpid())
  now <- as.numeric(Sys.time() - as.POSIXlt(Sys.Date()), units = "secs")
  now <- sprintf("%.24f", now)
  now <- strsplit(now, "\\.")[[1]]
  now2 <- strsplit(now[[2]], "")[[1]]
  now <- as.integer(c(
    paste(now2[c(1,5,9,13,17,21) + 3], collapse = ""),
    paste(now2[c(1,5,9,13,17,21) + 2], collapse = ""),
    paste(now2[c(1,5,9,13,17,21) + 1], collapse = ""),
    paste(now2[c(1,5,9,13,17,21)], collapse = ""),
    now[[1]]
  ))
  now <- rev(as.integer(now))

  dict0 <- digest::digest(paste(pid, now, "ravepipeline"), algo = "xxhash32", seed = pid)
  dict1 <- digest::digest(paste(pid, now, dict0), algo = "xxhash32", seed = now[[1]])
  dict2 <- digest::digest(paste(pid, now, dict1), algo = "murmur32", seed = sum(now))
  dict3 <- digest::digest(paste(pid, now, dict1, dict2), algo = "xxhash64",
                          seed = strtoi(sprintf("0x%s", substr(dict2, 1, 7))))

  dict <- strsplit(paste0(dict3, dict2, dict1), "")[[1]]
  # dict <- c(dict, letters, LETTERS, 0:9)

  paste(sample(dict, size = length, replace = TRUE), collapse = "")
  # c(dict1, dict2, dict3)
}

str_to_sentence <- function(x) {
  # Process character vectors element by element
  vapply(x, function(txt) {
    # Split into sentences using a naive pattern:
    #    look for . ? or !, followed by whitespace
    sentences <- strsplit(txt, "(?<=[.?!])\\s+", perl = TRUE)[[1]]

    # For each "sentence," do:
    #   1) force to lower case,
    #   2) uppercase just the first character,
    #   3) leave the rest as is
    sentences <- vapply(sentences, function(sent) {
      sent <- tolower(sent)
      if (nchar(sent) > 0) {
        # Replace first character with uppercase version
        substring(sent, 1, 1) <- toupper(substring(sent, 1, 1))
      }
      sent
    }, character(1))

    # Paste sentences back together with a space
    paste(sentences, collapse = " ")

  }, character(1))
}

cat2 <- function (..., level = "DEBUG", print_level = FALSE, file = "",
          sep = " ", fill = FALSE, labels = NULL, append = FALSE, end = "\n",
          pal = list(DEBUG = "grey60", INFO = "#1d9f34", WARNING = "#ec942c",
                     ERROR = "#f02c2c", FATAL = "#763053", DEFAULT = "grey60"),
          use_cli = TRUE, bullet = "auto")
{
  if (!level %in% names(pal)) {
    level <- "DEFAULT"
  }
  bullet_list <- list(DEBUG = "tick", INFO = "heart", WARNING = "warning",
                      ERROR = "cross", FATAL = "cross", DEFAULT = "arrow_right")
  .col <- pal[[level]]
  if (is.null(.col)) {
    .col <- "#000000"
  }
  if (bullet == "auto") {
    bullet <- bullet_list[[level]]
    if (is.null(bullet)) {
      bullet <- "arrow_right"
    }
  }
  if (interactive()) {
    if (use_cli) {
      cli::cat_bullet(..., col = .col, bullet = bullet)
    }
    else {
      col <- cli::make_ansi_style(.col)
      if (print_level) {
        cat("[", level, "]: ", sep = "")
      }
      cat(col(..., sep = sep), end = end, file = file,
                fill = fill, labels = labels, append = append)
    }
  }
  else {
    cat(..., end)
  }
  if (level == "FATAL") {
    err <- simpleError(paste(..., collapse = "", sep = ""),
                       call = NULL)
    trace <- rlang::trace_back()
    trace <- as.list(utils::capture.output(print(trace)))
    if (length(trace) > 1) {
      trace <- c("Full call trees:", trace)
      err$trace <- trace
      class(err) <- c("RAVEError", "dipsausError", "simpleError", "error",
                      "condition")
    }
    stop(err)
  }
  invisible()
}

catgl <- function(..., .envir = parent.frame(), level = 'DEBUG', .pal, .capture = FALSE){
  level <- toupper(level)
  opt_level <- raveio_getopt('verbose_level')
  args <- list(...)
  msg <- tryCatch({
    structure(glue::glue(..., .envir = .envir), log_level = level)
  }, error = function(...){
    s <- args
    if(length(names(s))){
      s <- s[names(s) %in% c('', 'sep', 'collapse')]
    }
    s[[length(s) + 1]] <- ''

    do.call('paste', s)
  })
  if(
    .capture || (
      sum(verbose_levels >= opt_level, na.rm = TRUE) <
      sum(verbose_levels >= level, na.rm = TRUE)
    )
  ) {
    # opt_level is too high, message is muffled. depending on level
    # return or stop
    if(level == 'FATAL'){
      stop(msg)
    }
    return(invisible(msg))
  }
  call <- match.call()
  call <- deparse1(call, collapse = '\n')

  # .envir = parent.frame(), level = 'DEBUG', .pal, .capture = FALSE
  if(package_installed('ravedash')){
    ravedash_loglevel <- switch (
      level,
      "DEFAULT" = "trace",
      "DEBUG" = "debug",
      "INFO" = "info",
      "WARNING" = "warning",
      'ERROR' = 'error',
      'FATAL' = 'fatal',
      { "trace" }
    )
    call_pkg_fun("ravedash", "logger", msg, level = ravedash_loglevel)
    if(level == 'FATAL') {
      stop(msg)
    }
  } else {
    if(missing(.pal)){
      cat2(msg, level = level)
    }else{
      cat2(msg, level = level, pal = .pal)
    }
  }

  return(invisible(msg))
}


deparse_svec <- function (
    nums,
    connect = "-",
    concatenate = TRUE,
    collapse = ",",
    max_lag = 1
)
{
  nums <- nums[is.finite(nums)]
  if (length(nums) == 0) {
    return("")
  }
  alag <- seq_len(max(1, max_lag))
  nums <- sort(unique(nums))
  lg <- c(NA, nums)[seq_len(length(nums))]
  ind <- nums - lg
  ind[1] <- 0
  ind2 <- c(ind[-1], -1)
  re <- apply(cbind(nums[!ind %in% alag], nums[!ind2 %in% alag]), 1, function(x) {
    if (x[1] == x[2]) {
      as.character(x[1])
    }
    else {
      paste(x, collapse = connect)
    }
  })
  if (concatenate) {
    re <- paste(re, collapse = collapse)
  }
  re
}


parse_svec <- function (text, sep = ",", connect = "-:|", sort = FALSE, unique = TRUE) {
  connect <- unique(unlist(strsplit(connect, "")))
  connect[connect %in% c("|", ":", "~")] <- paste0("\\", connect[connect %in% c("|", ":", "~")])
  if ("-" %in% connect) {
    connect <- c(connect[connect != "-"], "-")
  }
  connect <- paste(connect, collapse = "")
  if (length(text) != 1) {
    text <- paste(text, collapse = sep)
  }
  if (length(text) == 0 || !nzchar(trimws(text))) {
    return(NULL)
  }
  if (is.numeric(text)) {
    if (unique) {
      text <- unique(text)
    }
    if (sort) {
      text <- sort(text)
    }
    return(text)
  }
  s <- unlist(strsplit(text, sep, perl = TRUE))
  s <- trimws(s)
  s <- s[s != ""]
  s <- s[grepl(sprintf("^[0-9\\ %s]+$", connect), s)]
  re <- NULL
  for (ss in s) {
    if (grepl(sprintf("[%s]", connect), ss)) {
      ss <- unlist(strsplit(ss, sprintf("[%s]", connect),
                            perl = TRUE))
      ss <- trimws(ss)
      ss <- ss[grepl("^[0-9]+$", ss)]
      ss <- as.numeric(ss)
      ss <- ss[!is.na(ss)]
      if (length(ss) >= 2) {
        re <- c(re, (ss[1]:ss[2]))
      }
    }
    else {
      re <- c(re, as.numeric(ss))
    }
  }
  if (unique) {
    re <- unique(re)
  }
  if (sort) {
    re <- sort(re)
  }
  return(re)
}

# base64_urlinternal_encoder <- function (x) {
#   base64enc::base64encode(charToRaw(x))
# }

base64_urlencode <- function (x) {
  if (!length(x)) {
    return(character(x))
  }
  # re <- vapply(enc2utf8(as.character(x)), base64_urlinternal_encoder,
  #              "", USE.NAMES = FALSE)
  # re <- gsub("[=]{0,}$", "", re)
  # re <- gsub("[+]", "-", re)
  # gsub("/", "_", re)
  unname(base64url::base64_urlencode(x = as.character(x)))
}

safe_urlencode <- function (x) {
  re <- base64_urlencode(x)
  paste0("==", re)
}

# base64_urlinternal_decoder <- function (x) {
#   rawToChar(base64enc::base64decode(what = x), multiple = FALSE)
# }

base64_urldecode <- function (x) {
  # x <- gsub("-", "+", x)
  # x <- gsub("_", "/", x)
  # vapply(x, base64_urlinternal_decoder, "", USE.NAMES = FALSE)
  unname(base64url::base64_urldecode(x = x))
}

safe_urldecode <- function (x) {
  x <- gsub(x = as.character(x), pattern = "^==", replacement = "")
  base64_urldecode(x)
}
