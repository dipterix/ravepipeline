# This file is licensed under MIT by Zhengjia Wang

`%OF%` <- function(lhs, rhs) {
  if (length(rhs)) { de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if (!length(lhs)) { return(de) }
  sel <- lhs %in% rhs
  if (any(sel)) { return(lhs[sel][[1]]) }
  return(de)
}


stopifnot2 <- function(..., msg = "Condition not satisfied") {
  if (!all(c(...))) {
    stop(msg)
  }
}


append_el <- function(el, value, method = "c") {
  el_expr <- substitute(el)
  value <- do.call(method, list(quote(el), quote(value)))
  do.call("<-", list(el_expr, value), envir = parent.frame())
}

package_installed <- function(pkgs, all = FALSE) {
  re <- sapply(pkgs, function(p) {
    system.file("", package = p) != ""
  })
  if (all) {
    re <- all(re)
  }
  re
}

require_package <- function(package, return_namespace = FALSE) {
  # if(system.file(package = package) == "") {
  #   stop(sprintf("Package [%s] is needed to run the script. Please install it first via\n  install.packages('%s')", package, package), call. = NULL)
  #
  # }
  targets::tar_assert_package(package)
  if ( return_namespace ) {
    return(asNamespace(package))
  }
  return(invisible())
}

call_pkg_fun <- function(package, f_name, ...,
                         .if_missing = c("error", "warning", "none"),
                         .missing_default = NULL,
                         .call_pkg_function = TRUE) {

  stopifnot(length(package) == 1)

  if (!package_installed(package)) {
    .if_missing <- match.arg(.if_missing)
    switch(
      .if_missing,
      "error" = {
        stop("Package ", sQuote(package), " is missing.")
      },
      "warning" = {
        warning("Package ", sQuote(package), " is missing.")
      },
      {}
    )
    return(.missing_default)
  }

  ns <- asNamespace(package)
  fun <- ns[[f_name]]

  if ( .call_pkg_function ) {
    if (!is.function(fun)) {
      .if_missing <- match.arg(.if_missing)
      switch(
        .if_missing,
        "error" = {
          stop("Package ", sQuote(package), " does not have function ", sQuote(f_name))
        },
        "warning" = {
          warning("Package ", sQuote(package), " does not have function ", sQuote(f_name))
        },
        {}
      )
      return(.missing_default)
    }

    return(fun(...))
  } else {
    return(fun)
  }

}

call_ravecore_fun <- function(f_name, ...,
                            .if_missing = c("error", "warning", "none"),
                            .missing_default = NULL,
                            .call_pkg_function = TRUE) {
  .if_missing <- match.arg(.if_missing)
  if (!package_installed("ravecore")) {
    package <- "raveio"
  } else {
    package <- "ravecore"
  }
  call_pkg_fun(
    package = package,
    f_name = f_name,
    ...,
    .if_missing = .if_missing,
    .missing_default = .missing_default,
    .call_pkg_function = .call_pkg_function
  )
}

safe_system <- function(cmd, ..., intern = TRUE, ignore.stderr = TRUE,
                        minimized = TRUE, invisible = TRUE, show.output.on.console = TRUE) {
  suppressWarnings({
    if (get_os() == "windows") {
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr,
                    minimized = minimized, invisible = invisible,
                    show.output.on.console = show.output.on.console, ...)
    } else {
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr, ...)
    }
  })
  ret
}

safe_system2 <- function(cmd, args, ..., stdout = TRUE, stderr = FALSE, onFound = NULL, onNotFound = NA) {

  if (Sys.which(cmd) == "") {
    return(onNotFound)
  }

  suppressWarnings({
    ret <- system2(cmd, args, ..., stdout = stdout, stderr = stderr)
  })
  if (is.function(onFound)) {
    ret <- onFound(ret)
  }
  ret
}


new_function2 <- function(
    args = alist(), body = {},
    env = parent.frame(),
    quote_type = c("unquoted", "quote", "quo"),
    quasi_env = parent.frame()) {

  quote_type <- match.arg(quote_type)
  switch(quote_type, unquoted = {
    quo <- eval(as.call(list(quote(rlang::quo), substitute(body))),
                envir = quasi_env)
    body <- rlang::quo_squash(quo)
  }, quote = {
    quo <- eval(as.call(list(quote(rlang::quo), body)), envir = quasi_env)
    body <- rlang::quo_squash(quo)
  }, quo = {
    body <- rlang::quo_squash(quo)
  })
  f <- local({
    function() {
    }
  }, envir = env)
  formals(f) <- args
  body(f) <- body
  f

}


brew_program <- function(program) {

  os <- get_os()
  brew_bin <- switch (
    os,
    "darwin" = {
      res <- suppressWarnings({
        sanitized_path <- gsub("\\", "\\\\", Sys.getenv("PATH"), fixed = TRUE)
        sanitized_path <- gsub("\"", "\\\"", sanitized_path, fixed = TRUE)
        system(paste0("PATH=\"", sanitized_path, "\" /usr/bin/which ", "brew"), intern = TRUE)
      })
      if (length(res) == 0) {
        # brew is not in PATH
        # guess common places
        if (identical(R.version$arch, "aarch64")) {
          res <- "/opt/homebrew/bin/brew"
        } else {
          res <- "/usr/local/bin/brew"
        }
      }
      res
    },
    {
      res <- Sys.which("brew")
      if (!nzchar(res)) {
        if (file.exists()) {
          res <- "~/.linuxbrew/bin/brew"
        } else {
          res <- "/home/linuxbrew/.linuxbrew/bin/brew"
        }
      }
      res
    }
  )

  if (length(brew_bin) == 1 && !is.na(brew_bin) && nzchar(brew_bin) && file.exists(brew_bin)) {
    prefix <- suppressWarnings({
      system(sprintf("%s --prefix", shQuote(brew_bin)), intern = TRUE)
    })
    if (length(prefix) == 1) {
      path <- file.path(prefix, "bin", program, fsep = "/")
      if (file.exists(path)) {
        return(path)
      }
    }
  }
  return("")
}

find_program <- function(program) {
  os <- get_os()

  path <- switch(
    os,
    "darwin" = {
      res <- suppressWarnings({
        sanitized_path <- gsub("\\", "\\\\", Sys.getenv("PATH"), fixed = TRUE)
        sanitized_path <- gsub("\"", "\\\"", sanitized_path, fixed = TRUE)
        system(paste0("PATH=\"", sanitized_path, "\" /usr/bin/which ", program), intern = TRUE)
      })
      if (length(res) == 0) {
        # Check brew
        res <- brew_program(program)
      }
      res
    },
    "linux" = {
      res <- Sys.which(program)
      if (length(res) != 1 || !nzchar(res)) {
        res <- suppressWarnings({
          sanitized_path <- gsub("\\", "\\\\", Sys.getenv("PATH"), fixed = TRUE)
          sanitized_path <- gsub("\"", "\\\"", sanitized_path, fixed = TRUE)
          system(paste0("PATH=\"", sanitized_path, "\" /usr/bin/which ", program), intern = TRUE)
        })
        if (length(res) == 0) {
          # Check brew
          res <- brew_program(program)
        }
        res
      }
    },
    {
      Sys.which(program)
    }
  )

  if (length(path) == 0 || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return("")
  } else {
    return(path)
  }
}

with_pandoc_safe_environment <- function(code) {
  if (package_installed("rmarkdown")) {
    rmarkdown <- asNamespace("rmarkdown")
    if (is.function(rmarkdown$with_pandoc_safe_environment)) {
      try(
        silent = TRUE,
        {
          return(rmarkdown$with_pandoc_safe_environment(code))
        }
      )
    }
  }
  lc_all <- Sys.getenv("LC_ALL", unset = NA)
  if (!is.na(lc_all)) {
    Sys.unsetenv("LC_ALL")
    on.exit(Sys.setenv(LC_ALL = lc_all), add = TRUE)
  }
  lc_ctype <- Sys.getenv("LC_CTYPE", unset = NA)
  if (!is.na(lc_ctype)) {
    Sys.unsetenv("LC_CTYPE")
    on.exit(Sys.setenv(LC_CTYPE = lc_ctype), add = TRUE)
  }
  if (Sys.info()["sysname"] == "Linux" && is.na(Sys.getenv("HOME", unset = NA))) {
    stop("The 'HOME' environment variable must be set before running Pandoc.")
  }
  if (Sys.info()["sysname"] == "Linux" && is.na(Sys.getenv("LANG", unset = NA))) {
    locale_default <- "en_US.UTF-8"
    locale_util <- Sys.which("locale")
    if (nzchar(locale_util)) {
      locales <- system(paste(locale_util, "-a"), intern = TRUE)
      locales <- suppressWarnings(strsplit(locales, split = "\n", fixed = TRUE))
      if ("C.UTF-8" %in% locales) {
        locale_default <- "C.UTF-8"
      }
    }
    Sys.setenv(LANG = locale_default)
    on.exit(Sys.unsetenv("LANG"), add = TRUE)
  }
  if (Sys.info()["sysname"] == "Linux" && identical(Sys.getenv("LANG"), "en_US")) {
    Sys.setenv(LANG = "en_US.UTF-8")
    on.exit(Sys.setenv(LANG = "en_US"), add = TRUE)
  }
  force(code)
}

get_pandoc_version <- function(pandoc_dir) {
  path <- file.path(pandoc_dir, "pandoc")
  if (identical(get_os(), "windows")) {
    path <- paste0(path, ".exe")
  }
  if (!utils::file_test("-x", path)) {
    return(numeric_version("0"))
  }
  info <- with_pandoc_safe_environment(system(paste(shQuote(path), "--version"), intern = TRUE))
  version <- strsplit(info, "\n", useBytes = TRUE)[[1]][1]
  version <- strsplit(version, " ")[[1]][2]
  components <- strsplit(version, "-")[[1]]
  version <- components[1]
  nightly <- match("nightly", components)
  if (!is.na(nightly)) {
    version <- paste(c(version, grep("^[0-9]+$", components[-(1:nightly)],
                                     value = TRUE)), collapse = ".")
  }
  numeric_version(version)
}

register_pandoc <- function() {

  pandoc_dir <- raveio_getopt("pandoc_dir", default = "")

  sources <- c(Sys.getenv("RSTUDIO_PANDOC"), pandoc_dir, dirname(find_program("pandoc")), "~/opt/pandoc")
  sources <- path.expand(sources)
  versions <- lapply(sources, function(src) {
    if (dir.exists(src)) {
      try(silent = TRUE, {
        return(get_pandoc_version(src))
      })
    }
    numeric_version("0")
  })

  found_src <- NULL
  found_ver <- numeric_version("0")
  for (i in seq_along(sources)) {
    ver <- versions[[i]]
    if (ver > found_ver) {
      found_ver <- ver
      found_src <- sources[[i]]
    }
  }

  if (length(found_src) == 1) {
    # found pandoc
    Sys.setenv("RSTUDIO_PANDOC" = found_src)
    raveio_setopt("pandoc_dir", found_src)

    # Also update rmarkdown registry
    if (package_installed("rmarkdown")) {
      rmarkdown <- asNamespace("rmarkdown")
      rmarkdown$find_pandoc(cache = FALSE, dir = found_src)
    }
  }

  found_src
}
