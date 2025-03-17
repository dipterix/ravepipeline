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
  # if( !isTRUE(getOption("ravepipelines.install.yes_to_all", FALSE)) ) {
  #   # Installing packages should be under interactive session and should ask users
  #   if(!interactive()) {
  #     stop("`install_deps`: must run under interactive session")
  #   }
  #   ans <- utils::askYesNo("Installing dependencies... This might install additional packages. Proceed? ")
  #   if(!isTRUE(ans)) {
  #     stop("Abort.")
  #   }
  #   # suppress future question
  #   old_opt <- options("ravepipelines.install.yes_to_all" = TRUE)
  #   on.exit({ options(old_opt) })
  # }

  install_deps <- get_remotes_fun("install_deps")
  install_deps(pkgdir = root, upgrade = upgrade, force = force, lib = lib, ...)
}

install_cran <- function(pkgs, upgrade = FALSE, lib = guess_libpath(), ...) {
  # if( !isTRUE(getOption("ravepipelines.install.yes_to_all", FALSE)) ) {
  #   # Installing packages should be under interactive session and should ask users
  #   if(!interactive()) {
  #     stop("`install_deps`: must run under interactive session")
  #   }
  #   ans <- utils::askYesNo("Installing dependencies... This might install additional packages. Proceed? ")
  #   if(!isTRUE(ans)) {
  #     stop("Abort.")
  #   }
  #   # suppress future question
  #   old_opt <- options("ravepipelines.install.yes_to_all" = TRUE)
  #   on.exit({ options(old_opt) })
  # }
  install_cran <- get_remotes_fun("install_cran")
  install_cran(pkgs, upgrade = ifelse(isTRUE(upgrade), "always", "never"), lib = lib, ...)

}

get_remotes_fun <- function(name) {
  # Make sure we do not install any packages during checking or testing
  not_cran_flag <- identical(toupper(as.character(Sys.getenv("NOT_CRAN", ""))), "TRUE")
  limit_core_flag <- identical(toupper(Sys.getenv("_R_CHECK_LIMIT_CORES_")), "TRUE")
  rave_testing_flag <- identical(toupper(Sys.getenv("RAVE_TESTING")), "TRUE")

  if ( rave_testing_flag || limit_core_flag || not_cran_flag ) {
    stop("Do NOT install R packages when checking")
  }

  remotes <- asNamespace("remotes")
  remotes[[name]]
}
