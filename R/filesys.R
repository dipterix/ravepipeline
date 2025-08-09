# This file is licensed under MIT by Zhengjia Wang

backup_file <- function(path, remove = FALSE, quiet = FALSE) {

  if(length(path) != 1 || is.na(path)) {
    return(invisible(FALSE))
  }
  if(!file.exists(path)){ return(invisible(FALSE)) }

  path <- normalizePath(path, mustWork = TRUE, winslash = "/")

  is_dir <- dir.exists(path)

  # find the extension
  ext <- fileexts(path)

  bname <- basename(path)
  dname <- dirname(path)

  if(ext == '') {
    bname <- gsub("[/]+$", "", bname)
  } else {
    bname <- substr(bname, start = 1L, stop = nchar(bname) - nchar(ext) - 1)
  }

  # check if bname contains timestamp
  bname <- gsub("_\\[backup_[0-9]{8}_[0-9]{6}\\]$", "", x = bname)

  bname2 <- sprintf(
    "%s_[backup_%s]%s",
    bname,
    strftime(Sys.time(), "%Y%m%d_%H%M%S"),
    ifelse(ext == "", "", sprintf(".%s", ext))
  )
  if (!quiet) {
    catgl("{ifelse(remove, 'Moving', 'Copying')} {ifelse(is_dir, 'directory', 'file')} {basename(path)}\n  => {bname2}")
  }
  path2 <- file.path(dname, bname2)

  if( remove ) {
    file_move(from = path, to = path2)
  } else {
    if(is_dir) {
      dir_create2(path2)
      file.copy(
        from = list.files(
          path = path, all.files = TRUE, full.names = TRUE,
          recursive = FALSE, include.dirs = TRUE, no.. = TRUE
        ),
        to = path2, overwrite = TRUE, recursive = TRUE,
        copy.mode = TRUE, copy.date = TRUE
      )
    } else {
      file.copy(from = path, to = path2, overwrite = TRUE,
                copy.mode = TRUE, copy.date = TRUE, recursive = FALSE)
    }
  }

  return(invisible(path2))

}

#' @title Force creating directory with checks
#' @param x path to create
#' @param showWarnings,recursive,... passed to \code{\link{dir.create}}
#' @param check whether to check the directory after creation
#' @returns Normalized path
#'
#' @examples
#'
#' path <- file.path(tempfile(), 'a', 'b', 'c')
#'
#' # The following are equivalent
#' dir.create(path, showWarnings = FALSE, recursive = TRUE)
#'
#' dir_create2(path)
#'
#'
#' @export
dir_create2 <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    stop('Cannot create directory at ', shQuote(x))
  }
  invisible(normalizePath(x))
}

file_create2 <- function (x, showWarnings = FALSE, recursive = TRUE) {
  if (!file.exists(x)) {
    dir <- dirname(x)
    if (recursive && !dir.exists(dir)) {
      dir_create2(dir)
    }
    file.create(x, showWarnings = showWarnings)
  }
  invisible(normalizePath(x))
}

normalize_path <- function(path, must_work = NA) {
  path <- unlist(lapply(path, function(p) {
    if(!file.exists(p)) {
      dname <- dirname(p)
      dname <- normalizePath(dname, winslash = "/", mustWork = must_work)
      p <- file.path(dname, basename(p), fsep = "/")
    } else {
      p <- normalizePath(p, winslash = "/", mustWork = must_work)
    }
    p
  }))

  gsub("[/|\\\\]+", "/", path)
}

fileexts <- function(file){
  x <- basename(file)
  sapply(strsplit(file, ".", fixed = TRUE), function(x){
    l <- length(x)
    ifelse(l > 1, x[[l]], '')
  })
}


file_move <- function(from, to) {
  if(package_installed("fs")) {
    fs <- asNamespace("fs")
    impl <- fs$file_move
    if(is.function(impl)) {
      impl(path = from, new_path = to)
      return(invisible(to))
    }
  }
  file.rename(from = from, to = to)
  return(invisible(to))
}

remove_empty_dir <- function (path, all.files = TRUE, recursive = FALSE, verbose = FALSE) {
  if (!dir.exists(path)) {
    return()
  }
  sub_files <- list.files(path = path, recursive = FALSE, all.files = all.files,
                          include.dirs = TRUE, no.. = TRUE, full.names = TRUE)
  if (recursive) {
    if (length(sub_files) > 0L) {
      sub_dirs <- sub_files[dir.exists(sub_files)]
      for (dir in sub_dirs) {
        Recall(dir, all.files = all.files, recursive = recursive)
      }
      sub_files <- list.files(path = path, recursive = FALSE,
                              all.files = all.files, include.dirs = TRUE, no.. = TRUE,
                              full.names = TRUE)
    }
  }
  if (!length(sub_files)) {
    if (verbose) {
      message("Removing empty folder: ", path)
    }
    unlink(path, recursive = TRUE, force = TRUE)
  }
}


R_user_dir <- function (package, which = c("data", "config", "cache")) {
  stopifnot(is.character(package), length(package) == 1L)
  which <- match.arg(which)
  home <- normalizePath("~")
  path <- switch(which, data = {
    if (nzchar(p <- Sys.getenv("R_USER_DATA_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_DATA_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("APPDATA"), "R", "data")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Application Support", "org.R-project.R")
    else file.path(home, ".local", "share")
  }, config = {
    if (nzchar(p <- Sys.getenv("R_USER_CONFIG_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_CONFIG_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("APPDATA"), "R", "config")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Preferences", "org.R-project.R")
    else file.path(home, ".config")
  }, cache = {
    if (nzchar(p <- Sys.getenv("R_USER_CACHE_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_CACHE_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Caches", "org.R-project.R")
    else file.path(home, ".cache")
  })
  file.path(path, "R", package)
}

ravepipeline_data_dir <- function(...) {
  file.path(R_user_dir("raveio", "data"), ...)
}

ravepipeline_config_dir <- function(...) {
  file.path(R_user_dir("raveio", "config"), ...)
}

ravepipeline_cache_dir <- function(...) {
  file.path(R_user_dir("raveio", "cache"), ...)
}


temporary_session_root <- function(){
  d <- raveio_getopt('tensor_temp_path')
  if(!dir.exists(d)){
    d <- tempdir(check = TRUE)
  }
  d <- file.path(d, get('.session_string'))
  dir_create2(d)
  normalizePath(d)
}




cache_root <- function(check = FALSE){
  re <- raveio_getopt(key = 'tensor_temp_path', default = NULL)
  if(!length(re)){
    re <- '~/rave_data/cache_dir/'
    raveio_setopt(key = 'tensor_temp_path', value = re)
  }
  if(check){
    re <- dir_create2(re)
  }
  re
}


