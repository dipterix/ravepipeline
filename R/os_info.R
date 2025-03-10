# This file is licensed under MIT by Zhengjia Wang

time_delta <- function (t1, t2, units = "secs") {
  as.numeric(t2 - t1, units = units)
}

to_ram_size <- function (s, kb_to_b = 1000) {
  base <- floor(log(max(abs(s), 1), kb_to_b))
  s <- s/(kb_to_b^(base))
  if (is.na(base)) {
    base <- 0
  }
  attr(s, "unit") <- c("B", "KB", "MB", "GB", "TB", "PB", "EB",
                       "ZB", "YB")[base + 1]
  class(s) <- c("ravepipeline_bytes", "dipsaus_bytes", class(s))
  s
}

get_ram_windows <- function () {
  cmd <- Sys.which("wmic")
  if (cmd == "") {
    windir <- Sys.getenv("windir")
    if (windir == "") {
      windir <- Sys.getenv("SystemRoot")
    }
    if (windir == "") {
      windir <- file.path(Sys.getenv("SystemDrive"), "WINDOWS",
                          fsep = "\\")
    }
    if (!dir.exists(windir)) {
      windir <- "C:\\WINDOWS"
    }
    cmd <- file.path(windir, "System32", "wbem", "wmic.exe",
                     fsep = "\\")
  }
  if (!file.exists(cmd)) {
    return(NA)
  }
  ram <- safe_system2(cmd, c("MemoryChip", "get", "Capacity"),
                      stdout = TRUE)
  if (length(ram) < 2) {
    return(NA)
  }
  suppressWarnings({
    ram <- as.numeric(ram[[2]])
    return(structure(as.numeric(ram), class = c("ravepipeline_bytes", "dipsaus_bytes"),
                     unit = "B"))
  })
}

get_ram_osx <- function () {
  cmd <- Sys.which("sysctl")
  if (cmd == "") {
    cmd <- "/usr/sbin/sysctl"
  }
  if (!file.exists(cmd)) {
    cmd <- "/sbin/sysctl"
  }
  if (!file.exists(cmd)) {
    return(NA)
  }
  ram <- safe_system2(cmd, "hw.memsize", stdout = TRUE, onFound = function(ram) {
    substring(ram, 13)
  })
  structure(as.numeric(ram), class = c("ravepipeline_bytes", "dipsaus_bytes"), unit = "B")
}

get_ram_linux <- function () {
  if (!file.exists("/proc/meminfo")) {
    return(NA)
  }
  s <- readLines("/proc/meminfo", n = 100)
  s <- s[startsWith(s, "MemTotal")]
  if (!length(s)) {
    return(NA)
  }
  match <- regexec("([0-9]+)([ kKmMgGtT]+)([bB])", text = s[[1]])[[1]]

  if(length(match) < 3) { return(NA) }

  s <- substring(s[[1]], match, match + attr(match, "match.length") - 1)
  # s <- stringr::str_match(s[[1]], "([0-9]+)([ kKmMgGtT]+)([bB])")
  unit <- tolower(trimws(s[[3]]))
  units <- c("", "k", "m", "g", "t")
  ram <- as.numeric(s[[2]]) * 1024^(which(units == unit) - 1)
  structure(as.numeric(ram), class = c("ravepipeline_bytes", "dipsaus_bytes"), unit = "B")
}

get_ram <- function () {
  os <- get_os()
  if (os == "windows") {
    return(get_ram_windows())
  }
  if (os == "darwin") {
    return(get_ram_osx())
  }
  if (os == "linux") {
    return(get_ram_linux())
  }
  return(NA)
}


#' @export
print.ravepipeline_bytes <- function (x, digit = 1, ...) {
  re <- as.character(x, digit = digit, ...)
  cat(re)
  invisible(re)
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


shiny_is_running <- function() {
  # I don't even want to even suggest shiny

  if(!package_installed("shiny")) { return(FALSE) }

  session <- call_pkg_fun("shiny", "getDefaultReactiveDomain", .if_missing = "none", .missing_default = NULL)
  if(is.null(session)) {
    return(FALSE)
  }

  return(TRUE)
}

get_shiny_session <- function() {
  if(!package_installed("shiny")) { return(NULL) }
  return(call_pkg_fun("shiny", "getDefaultReactiveDomain", .if_missing = "none", .missing_default = NULL))
}


session_uuid <- local({
  uuids <- list()

  function (pid = Sys.getpid(), attributes = FALSE) {

    pidstr <- as.character(pid)
    uuid <- uuids[[pidstr]]
    if (!is.null(uuid)) {
      if (!attributes) {
        attr(uuid, "source") <- NULL
      }
      return(uuid)
    }
    info <- Sys.info()
    host <- Sys.getenv(c("HOST", "HOSTNAME", "COMPUTERNAME"))
    host <- host[nzchar(host)]
    host <- if (length(host) == 0L) {
      info[["nodename"]]
    } else {
      host[1L]
    }

    info <- list(host = host, info = info, pid = pid, time = Sys.time(), package = "ravepipeline")
    # uuid <- digest::digest(info)
    uuid <- uuid::UUIDgenerate(use.time = TRUE, output = "string")
    # remove all dash
    uuid <- gsub("-", "", uuid, fixed = TRUE)

    attr(uuid, "source") <- info
    uuids[[pidstr]] <<- uuid
    if (!attributes) {
      attr(uuid, "source") <- NULL
    }
    uuid
  }
})
