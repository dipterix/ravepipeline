
.onAttach <- function(libname, pkgname) {
  # check if rhdf5 has been installed
  s <- NULL

  pkg <- asNamespace(pkgname)
  if(length(pkg$.startup_msg)){
    s <- c(pkg$.startup_msg, "")
  }

  if(length(s)){
    s <- paste(s, collapse = "\n")
    packageStartupMessage(s)
  }

}

.onLoad <- function(libname, pkgname) {

  # Sys.unsetenv("RAVE_PIPELINE")

  pkg <- asNamespace(pkgname)
  sess_str <- rand_string(15)
  # .session_string <<- sess_str
  assign('.session_string', sess_str, envir = pkg)

  err_f <- function(e){
    assign('.startup_msg', sprintf("Issues loading `raveio`: %s\n", paste(e$message, collapse = "\n")), envir = pkg)
    NULL
  }
  s <- NULL
  tryCatch({
    suppressWarnings({
      s <- load_setting(reset_temp = TRUE)
    })
  }, error = err_f, warning = err_f)

  if( is.null(s) ){
    s <- default_settings()
  }

  .settings <<- s

  assign('.settings', s, envir = pkg)
  cenv <- environment(.subset2(s, 'reset'))

  assign(".target_formats", new.env(parent = emptyenv()), envir = pkg)
  assign(".locker_keys", fastmap::fastmap(), envir = pkg)

  target_format_register_onload()

  # .onUnload is suppose to work, but in RStudio environment
  # when users force restart rsession, .onUnload is ignored
  # and hence it's possible to leave massive amount of temporary files.
  # To clean these files, use reg.finalizer on settings, settings
  # map stays with current session. When
  # settings is gced, remove these files.
  reg.finalizer(cenv, function(cenv){
    try(expr = {
      if(is.function(cenv$get)) {
        tf_path <- cenv$get('tensor_temp_path')
        sess_str2 <- paste(sess_str, collapse = "")
        if(
          length(tf_path) == 1 && !is.na(tf_path) && is.character(tf_path) &&
          !trimws(tf_path) %in% c("", ".", "/") && file.exists(tf_path) &&
          !is.na(sess_str2) && nzchar(sess_str2)
        ) {
          ts_path <- file.path(tf_path, sess_str2)
          if(isTRUE(dir.exists(ts_path))){
            unlink(ts_path, recursive = TRUE)
          }
        }
      }
    })

  }, onexit = TRUE)

  # check if ravetools is installed
  # if(isNamespaceLoaded("ravetools") || system.file(package = "ravetools") != "") {
  #   options("raveio.use.ravetools" = TRUE)
  # }

}

.onUnload <- function(libpath){
  try({
    s <- load_setting(reset_temp = TRUE)
    sess_str <- get('.session_string')
    tf_path <- s[['tensor_temp_path']]
    if(
      length(tf_path) == 1 && !is.na(tf_path) && is.character(tf_path) &&
      !trimws(tf_path) %in% c("", ".", "/") && file.exists(tf_path) &&
      length(sess_str) == 1 && !is.na(sess_str) && nzchar(sess_str)
    ) {
      ts_path <- file.path(tf_path, sess_str)
      if(isTRUE(dir.exists(ts_path))){
        unlink(ts_path, recursive = TRUE)
      }
    }
  }, silent = TRUE)
}

