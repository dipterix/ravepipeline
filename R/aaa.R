#' @importFrom promises as.promise
#' @importFrom promises %...>%
#' @importFrom promises %...T>%
#' @importFrom promises %...!%
#' @importFrom promises %...T!%
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @importFrom R6 is.R6
#' @importFrom R6 is.R6Class
#' @importFrom digest digest
#' @importFrom remotes install_deps
NULL

#' @export
glue::glue

#' @export
promises::`%...>%`

#' @export
promises::`%...T>%`

#' @export
promises::`%...!%`

#' @export
promises::`%...T!%`

#' @export
promises::as.promise

#' @export
digest::digest


#' @name ravepipeline-constants
#' @title Constant variables used in 'RAVE' pipeline
#' @description
#' Regular expression \code{PIPELINE_FORK_PATTERN} defines the file matching
#' rules when forking a pipeline; see \code{\link{pipeline_fork}} for details.
#'
NULL


verbose_levels <-
  factor(
    c("DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", "FATAL"),
    levels = c("DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", "FATAL"),
    ordered = TRUE
  )

byte_size_lut <- list(
  "uint8" = 1, "int8" = 1,
  "uint16" = 2, "int16" = 2,
  "uint32" = 4, "int32" = 4,
  "uint64" = 8, "int64" = 8,
  "float" = 4, "double" = 8
)

r6_reserved_fields <- c('.__enclos_env__', 'clone', 'print', 'initialize', 'private')

# The following pipeline files will be copied
#' @rdname ravepipeline-constants
#' @export
PIPELINE_FORK_PATTERN <- "(^data|^R|^py|^preferences|\\.R$|\\.py$|\\.yaml$|\\.txt$|\\.csv$|\\.fst$|\\.conf$|\\.json$|\\.rds$|\\.Rmd$)"


#' @title Download 'RAVE' built-in pipelines and code snippets
#' @description
#' The official built-in pipeline repository is located at
#' \url{https://github.com/rave-ieeg/rave-pipelines};
#' The code snippet repository is located at
#' \url{https://github.com/rave-ieeg/rave-gists}.
#' @param upgrade rules to upgrade dependencies; default is to ask if needed
#' @param async whether to run in the background; ignore for now
#' @param ... ignored; reserved for external calls.
#' @returns A list built-in pipelines will be installed, the function itself
#' returns nothing.
#' @examples
#'
#' \dontrun{
#'
#' # This function requires connection to the Github, and must run
#' # under interactive session since an user prompt will be displayed
#'
#' ravepipeline_finalize_installation()
#'
#' }
#'
#' @export
ravepipeline_finalize_installation <- function(
    upgrade = c('ask', 'always', 'never', 'config-only', 'data-only'),
    async = FALSE, ...){

  upgrade <- match.arg(upgrade)

  template_path <- ravepipeline_data_dir("rave-pipelines")
  if(dir.exists(template_path)) {
    if(upgrade %in% c("never")) { return() }
    if(upgrade == "ask") {
      ans <- utils::askYesNo("Existing version of `rave-pipelines` is detected, upgrade? ")
      if(!isTRUE(ans)) { return() }
    }
  } else if (upgrade == "ask"){
    ans <- utils::askYesNo("Installing bultin `rave-pipelines`, proceed? ")
  }

  oldopt <- options("ravepipelines.install.yes_to_all" = TRUE)
  on.exit({
    options(oldopt)
  }, add = TRUE, after = TRUE)



  # update_sample_data <- FALSE
  # if(upgrade %in% c("always", "data-only")) {
  #   update_sample_data <- TRUE
  # }

  if(upgrade %in% c('always')) {
    upgrade <- TRUE
  } else {
    upgrade <- FALSE
  }

  repo_name <- 'rave-ieeg/rave-pipelines'
  if( getOption("ravemanager.nightly", FALSE) ) {
    repo_name <- 'rave-ieeg/rave-pipelines'
  }

  # Backup ravedash sessions since they might be too old now
  cache_path <- cache_root()

  if(file.exists(cache_path)) {
    fs <- list.dirs(cache_path, full.names = FALSE, recursive = FALSE)
    fs <- fs[grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", fs)]

    if(length(fs)) {
      for(path in file.path(cache_path, fs)) {
        backup_file(path, remove = TRUE, quiet = TRUE)
      }
    }
  }

  pipeline_install_github(
    repo = repo_name,
    to = "default", upgrade = upgrade
  )
  update_local_snippet(force = TRUE)

  invisible()

}

