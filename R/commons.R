stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    stop(msg)
  }
}


str2lang_alt <- function (s) {
  s <- sprintf("quote(%s)", trimws(s))
  eval(parse(text = s))
}

str2lang <- function (s) {
  get0("str2lang", envir = baseenv(), ifnotfound = str2lang_alt)(s)
}


append_el <- function(el, value, method = 'c'){
  el_expr <- substitute(el)
  value <- do.call(method, list(quote(el), quote(value)))
  do.call('<-', list(el_expr, value), envir = parent.frame())
}

# These functions are available in R 4.0. However, to be backward compatible
deparse1 <- function(expr, collapse = ' '){
  paste(deparse(expr), collapse = collapse)
}


require_package <- function(package, return_namespace = FALSE) {
  # if(system.file(package = package) == "") {
  #   stop(sprintf("Package [%s] is needed to run the script. Please install it first via\n  install.packages('%s')", package, package), call. = NULL)
  #
  # }
  targets::tar_assert_package(package)
  if( return_namespace ) {
    return(asNamespace(package))
  }
  return(invisible())
}


get_os <- function () {
  os <- R.version$os
  if (grepl("^darwin", os, ignore.case = TRUE)) {
    return("darwin")
  }
  if (grepl("^linux", os, ignore.case = TRUE)) {
    return("linux")
  }
  if (grepl("^solaris", os, ignore.case = TRUE)) {
    return("solaris")
  }
  if (grepl("^win", os, ignore.case = TRUE)) {
    return("windows")
  }
  if (grepl("^(emscr|wasm)", os, ignore.case = TRUE)) {
    return("emscripten")
  }
  return("unknown")
}

safe_system <- function(cmd, ..., intern = TRUE, ignore.stderr = TRUE,
                        minimized = TRUE, invisible = TRUE, show.output.on.console = TRUE){
  suppressWarnings({
    if(get_os() == 'windows'){
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr,
                    minimized = minimized, invisible = invisible,
                    show.output.on.console = show.output.on.console, ...)
    } else {
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr, ...)
    }
  })
  ret
}

safe_system2 <- function(cmd, args, ..., stdout = TRUE, stderr = FALSE, onFound = NULL, onNotFound = NA){

  if(Sys.which(cmd) == ""){
    return(onNotFound)
  }

  suppressWarnings({
    ret <- system2(cmd, args, ..., stdout = stdout, stderr = stderr)
  })
  if(is.function(onFound)){
    ret <- onFound(ret)
  }
  ret
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
  if(dipsaus::package_installed('ravedash')){
    ns <- do.call('asNamespace', list('ravedash'))
    ns$logger(msg, level = switch (
      level,
      "DEFAULT" = "trace",
      "DEBUG" = "debug",
      "INFO" = "info",
      "WARNING" = "warning",
      'ERROR' = 'error',
      'FATAL' = 'fatal',
      { "trace" }
    ))
    if(level == 'FATAL') {
      stop(msg)
    }
  } else {
    if(missing(.pal)){
      dipsaus::cat2(msg, level = level)
    }else{
      dipsaus::cat2(msg, level = level, pal = .pal)
    }
  }

  return(invisible(msg))
}


rand_string <- function(length = 50){
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

  dict0 <- dipsaus::digest(paste(pid, now), algo = "xxhash32", seed = pid)
  dict1 <- dipsaus::digest(paste(pid, now, dict0), algo = "xxhash32", seed = now[[1]])
  dict2 <- dipsaus::digest(paste(pid, now, dict1), algo = "murmur32", seed = sum(now))
  dict3 <- dipsaus::digest(paste(pid, now, dict1, dict2), algo = "xxhash64",
                           seed = strtoi(sprintf("0x%s", substr(dict2, 1, 7))))

  dict <- strsplit(paste0(dict3, dict2, dict1), "")[[1]]
  # dict <- c(dict, letters, LETTERS, 0:9)

  paste(sample(dict, size = length, replace = TRUE), collapse = "")
  # c(dict1, dict2, dict3)
}
