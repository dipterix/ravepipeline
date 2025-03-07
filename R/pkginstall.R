guess_libpath <- function() {

  lib_path <- getOption("ravemanager.libpath", default = NULL)

  try(silent = TRUE, expr = {
    if(length(lib_path) == 1 && !is.na(lib_path) && is.character(lib_path) && dir.exists(lib_path)) {
      return(normalizePath(lib_path))
    }
  })

  lib_path <- Sys.getenv("RAVE_LIB_PATH", unset = Sys.getenv("R_LIBS_USER", unset = ""))

  ostype <- get_os()

  if(ostype == 'windows') {
    lib_path <- strsplit(lib_path, ";")[[1]]
  } else {
    lib_path <- strsplit(lib_path, ":")[[1]]
  }

  if(length(lib_path)) {
    return(lib_path[[1]])
  }

  return(.libPaths()[[1]])
}

install_deps <- function(root, upgrade = FALSE, force = FALSE, lib = guess_libpath(), ...) {
  if( !isTRUE(getOption("ravepipelines.install.yes_to_all", FALSE)) ) {
    # Installing packages should be under interactive session and should ask users
    if(!interactive()) {
      stop("`install_deps`: must run under interactive session")
    }
    ans <- utils::askYesNo("Installing dependencies... This might install additional packages. Proceed? ")
    if(!isTRUE(ans)) {
      stop("Abort.")
    }
    # suppress future question
    options("ravepipelines.install.yes_to_all" = TRUE)
  }

  remotes::install_deps(pkgdir = root, upgrade = upgrade, force = force, lib = lib, ...)
}

install_cran <- function(pkgs, upgrade = FALSE, lib = guess_libpath(), ...) {
  if( !isTRUE(getOption("ravepipelines.install.yes_to_all", FALSE)) ) {
    # Installing packages should be under interactive session and should ask users
    if(!interactive()) {
      stop("`install_deps`: must run under interactive session")
    }
    ans <- utils::askYesNo("Installing dependencies... This might install additional packages. Proceed? ")
    if(!isTRUE(ans)) {
      stop("Abort.")
    }
    # suppress future question
    options("ravepipelines.install.yes_to_all" = TRUE)
  }
  remotes::install_cran(pkgs, upgrade = ifelse(isTRUE(upgrade), "always", "never"),
                        lib = lib, ...)

}
