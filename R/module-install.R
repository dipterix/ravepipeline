
#' @title Install 'RAVE' modules
#' @description
#' Low-level function exported for down-stream 'RAVE' packages.
#'
#' @param modules a vector of characters, repository names; default is to
#' automatically determined from a public registry
#' @param dependencies whether to update dependent packages; default is false
#' @returns nothing
#' @export
install_modules <- function(modules, dependencies = FALSE) {

  # update registries
  regs <- get_modules_registries()

  if(missing(modules) || !length(modules)) {
    modules <- sapply(regs, '[[', 'repo')
    message('Found the following registries:\n  ', paste(modules, collapse = ", "))
  }

  for(repo in modules) {
    tryCatch({
      pipeline_install_github(
        repo = repo,
        to = "default",
        upgrade = dependencies
      )
    }, error = function(e) {
      # TODO: try to use the URL
      warning("Cannot install [", repo, "]. Reason: ", e$message)
    })

  }

  invisible()
}

