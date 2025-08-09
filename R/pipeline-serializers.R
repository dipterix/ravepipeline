# pipeline target formats

target_user_path <- function(target_export = "", check = FALSE) {
  user_dir <- file.path(targets::tar_config_get("store"), "user")
  if(check && !dir.exists(user_dir)) {
    dir.create(user_dir, showWarnings = FALSE, recursive = TRUE)
  }
  file.path(user_dir, target_export)
}

target_format <- function(name) {
  if(length(name) != 1) { return(targets::tar_option_get("format")) }
  flist <- get(".target_formats")
  re <- flist[[name]]
  if(length(re)) {
    return(re)
  }
  return(targets::tar_option_get("format"))
}

target_format_dynamic <- function(
    name, target_export = NULL,
    target_expr = NULL, target_depends = NULL) {

  if(length(name) != 1 || is.na(name) || identical(tolower(name), "rds")) {
    # pipeline-serializers-default.R
    return(target_format_default())
  }

  backup_format <- target_format(name)
  if(is.character(backup_format)) { return(backup_format) }

  read <- new_function2(
    args = alist(path = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ser <- asNamespace("ravepipeline")$target_format(.(name))
      ser$read(path, target_export = .(target_export),
               target_expr = quote(.(target_expr)),
               target_depends = .(target_depends))
    })
  )
  write <- new_function2(
    args = alist(object =, path = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ser <- asNamespace("ravepipeline")$target_format(.(name))
      ser$write(object = object, path = path,
                target_export = .(target_export))
    })
  )

  marshal <- new_function2(
    args = alist(object = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ser <- asNamespace("ravepipeline")$target_format(.(name))
      if(is.function(ser$marshal)) {
        ser$marshal(object, target_export = .(target_export), target_depends = .(target_depends))
      } else {
        object
      }
    })
  )

  unmarshal <- new_function2(
    args = alist(object = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ser <- asNamespace("ravepipeline")$target_format(.(name))
      if(is.function(ser$unmarshal)) {
        ser$unmarshal(object, target_export = .(target_export))
      } else {
        object
      }
    })
  )

  targets::tar_format(
    read = read,
    write = write,
    marshal = marshal,
    unmarshal = unmarshal
  )

}

target_format_register <- function(name, read, write, marshal = NULL, unmarshal = NULL) {
  stopifnot(length(name) == 1 && nzchar(name))
  flist <- get(".target_formats")
  flist[[name]] <- list(
    read = read,
    write = write,
    marshal = marshal,
    unmarshal = unmarshal
  )
  return(invisible(NULL))
}

target_format_unregister <- function(name) {
  stopifnot(length(name) == 1 && nzchar(name))
  flist <- get(".target_formats")
  if( exists(x = name, envir = flist) ) {
    rm(list = name, envir = flist, inherits = FALSE)
  }
  return(invisible(NULL))
}


# internally used at on load
target_format_register_onload <- function(verbose = TRUE) {

  on_exception <- function(e) {
    if(verbose) {
      warning(e)
    }
  }

  lapply(list(
    tfmtreg_filearray,
    tfmtreg_rave_brain,
    tfmtreg_user_defined_python,
    tfmtreg_user_defined_r
  ), function(tfmtreg_func) {
    tryCatch({
      tfmtreg_func()
    }, error = on_exception, warning = on_exception)
  })



}
