#' Add new 'RAVE' (2.0) module to current project
#' @description
#' Creates a 'RAVE' pipeline with additional dashboard module from template.
#'
#' @param module_id module ID to create, must be unique; users cannot install
#' two modules with identical module ID. We recommend that
#' a module ID follows snake format, starting with lab name, for example,
#' \code{'beauchamplab_imaging_preprocess'}, \code{'karaslab_freez'}, or
#' \code{'upenn_ese25_fooof'}.
#' @param module_label a friendly label to display in the dashboard
#' @param path project root path; default is current directory
#' @param type template to choose, options are \code{'default'} and
#' \code{'bare'}
#' @param pipeline_name the pipeline name to create along with the module;
#' default is identical to \code{module_id} (strongly recommended); leave
#' it default unless you know what you are doing.
#' @param overwrite whether to overwrite existing module if module with
#' same ID exists; default is false
#' @param ... additional configurations to the module such as \code{'order'},
#' \code{'group'}, \code{'badge'}
#' @returns Nothing.
#' @examples
#'
#'
#' # For demonstrating this example only
#' project_root <- tempfile()
#' dir.create(project_root, showWarnings = FALSE, recursive = TRUE)
#'
#'
#' # Add a module
#' module_id <- "mylab_my_first_module"
#' module_add(
#'   module_id = module_id,
#'   module_label = "My Pipeline",
#'   path = project_root
#' )
#'
#'
#' # show the structure
#' cat(
#'   list.files(
#'     project_root,
#'     recursive = TRUE,
#'     full.names = FALSE,
#'     include.dirs = TRUE
#'   ),
#'   sep = "\n"
#' )
#'
#' unlink(project_root, recursive = TRUE)
#'
#'
#' @export
module_add <- function(
    module_id, module_label, path = ".", type = c("default", "bare", "scheduler", "python"), ...,
    pipeline_name = module_id, overwrite = FALSE
){

  force(module_id)
  force(module_label)
  path <- normalizePath(path)
  type <- match.arg(type)

  # path <- '~/Dropbox (Personal)/projects/rave-pipelines/'
  module_root <- file.path(path, "modules")
  module_path <- file.path(module_root, pipeline_name)

  # TODO: Add template-bare
  template_root <- system.file("rave-modules", "template", package = "ravepipeline")

  stopifnot(template_root != '')

  template_type <- switch (
    type,
    'bare' = "rmd-bare",
    'scheduler' = "rmd-scheduler",
    "python" = "rmd-python",
    "rmd"
  )

  pipeline_create_template(
    root_path = module_root,
    pipeline_name = pipeline_name,
    overwrite = overwrite,
    activate = FALSE,
    template_type = template_type
  )

  dir_create2(file.path(module_path, "R"))

  fs <- list.files(template_root, all.files = FALSE,
                   full.names = FALSE, recursive = TRUE,
                   include.dirs = FALSE, no.. = TRUE)

  for(f in fs) {
    s <- readLines(file.path(template_root, f))
    s <- gsub("\\{\\{[ ]*PIPELINE_NAME[ ]*\\}\\}", pipeline_name, s)
    s <- gsub("\\{\\{[ ]*MODULE_ID[ ]*\\}\\}", module_id, s)
    writeLines(s, file.path(module_path, f))
  }

  pipeline_build(pipe_dir = module_path)

  # add to module.yaml
  yaml <- file.path(path, "modules.yaml")
  if(file.exists(yaml)) {
    yaml_settings <- as.list(load_yaml(yaml))
    backup_file(yaml)
  } else {
    yaml_settings <- list(
      modules = list()
    )
  }

  item <- list(
    label = module_label,
    ...
  )
  item <- item[!names(item) %in% ""]
  yaml_settings$modules[[module_id]] <- item
  save_yaml(yaml_settings, yaml, sorted = TRUE)

  invisible()
}



module_dev_create <- function(
    path, ...
) {
  this_call <- match.call()
  catgl(deparse(this_call))
  args <- list(...)
  catgl("Creating RAVE 2.0 Repository -", path)

  template_path <- ravepipeline_data_dir("rave-pipelines")
  if(!dir.exists(template_path)) {
    ravepipeline_finalize_installation(upgrade = "never", async = FALSE)
  }

  fs <- list.files(template_path, recursive = FALSE, all.files = FALSE, no.. = TRUE, include.dirs = TRUE)
  fs <- fs[!grepl("\\.Rproj$", fs, perl = TRUE)]

  dir_create2(path)
  writeLines(c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^hello\\.R$",
               "^readme\\.md$", "adhoc/"),
             con = file.path(path, ".Rbuildignore"))
  file.copy(file.path(template_path, fs), path, recursive = TRUE)

  writeLines(
    con = file.path(path, '.Rprofile'),
    c(
      'message("Welcome to the Module Developement Project for RAVE 2.0.")',
      'message("  Your module has been created at `modules/` folder")',
      'message("  To preview the module, run")',
      'message("    ravedash::debug_modules()")'
    )
  )

  module_id <- args[["module_id"]]
  if(isTRUE(is.character(module_id)) && nchar(module_id) > 0) {
    module_add(path = path, ..., type = "bare", overwrite = TRUE)
  }


  invisible(path)

}

