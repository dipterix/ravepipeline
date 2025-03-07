
rs_avail <- function (version_needed = "1.3", child_ok = FALSE, shiny_ok = FALSE) {
  if (!shiny_ok && shiny_is_running()) {
    return(FALSE)
  }

  if( package_installed("rstudioapi") ) {
    return(call_pkg_fun("rstudioapi", "isAvailable", version_needed = version_needed, child_ok = child_ok))
  }

  # rstudioapi is not available
  return(FALSE)
  if (!requireNamespace("rstudioapi")) {
    return(FALSE)
  }
}

rs_active_project <- function (...) {
  if (rs_avail(...)) {
    return(call_pkg_fun("rstudioapi", "getActiveProject"))
  }
  # rstudioapi is missing. However, there is still a hope
  # RStudio injects function `.rs.getProjectDirectory` to the environment
  # The environment name is `tools:rstudio`

  # get rstudio injected function from above globalenv, hence less change to
  # query the wrong ones
  rs_get_project_directory <- get0(
    ".rs.getProjectDirectory",
    envir = parent.env(globalenv()),
    inherits = TRUE,
    ifnotfound = NULL,
    mode = "function"
  )

  # if the function is missing, just return NA
  if(!is.function(rs_get_project_directory)) { return(NA_character_) }

  # if the namespace is tools:rstudio, run it
  env <- environment(rs_get_project_directory)
  if(identical(attr(env, "name"), "tools:rstudio")) {
    return(rs_get_project_directory())
  }

  NA_character_
}
