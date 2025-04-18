
pipeline_install_directory <- function(
    directory, dest, upgrade = FALSE, force = FALSE, ...){

  directory <- normalizePath(directory, mustWork = TRUE)

  # find RAVE-CONFIG
  config_path <- file.path(directory, c("RAVE-CONFIG", "DESCRIPTION"))
  config_path <- config_path[file.exists(config_path)]

  if(!length(config_path)){
    stop("A RAVE pipeline must contains a RAVE-CONFIG or DESCRIPTION file")
  }
  config_path <- config_path[[1]]

  desc <- pipeline_description(config_path)

  if(!length(desc$Type)){
    stop("Cannot find `type` in the configuration file. ")
  }
  type <- desc$Type[[1]]
  tmp_dir <- tempfile()
  dir_create2(tmp_dir)
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)
  file.copy(config_path, file.path(tmp_dir, "DESCRIPTION"), overwrite = TRUE, recursive = FALSE)
  if(upgrade || force) {
    install_deps(root = tmp_dir, upgrade = upgrade, force = force, ...)
  } else {
    tryCatch({
      install_deps(root = tmp_dir, upgrade = upgrade, force = force, ...)
    }, error = function(e) {
      # Github might set a rate limit on the request
    })
  }


  if( length(desc$InteractiveModules) ) {
    modules <- strsplit(desc$InteractiveModules, "[,\n]+")[[1]]

    module_yaml <- file.path(directory, "modules.yaml")
    module_settings <- list()
    if(file.exists(module_yaml)){
      settings <- load_yaml(module_yaml)
      module_settings <- as.list(settings$modules)
    }

    modules <- lapply(modules, function(module){
      path <- file.path(directory, module)
      if(!dir.exists(path)){
        warning(glue("Unable to find module { module }."))
        return()
      }
      module_id <- basename(path)
      module_info <- as.list(module_settings[[module_id]])
      if(!length(module_info$label)){
        module_label <- gsub("[-_]+", " ", module_id)
        if(nchar(module_label)){
          substr(module_label, 1, 1) <- toupper(substr(module_label, 1, 1))
        }
        module_info$label <- module_label
      }
      catgl("Found interactive module: ", module_id, level = "DEFAULT")
      list(
        module_id = module_id,
        module_info = module_info,
        module_path = path
      )
    })
    # copy to shidashi template
    template_path <- ravepipeline_data_dir("shidashi_modules")
    module_path <- file.path(template_path, "modules")
    dir_create2(module_path)

    settings <- fastmap2()
    module_yaml <- file.path(template_path, "modules.yaml")
    if(file.exists(module_yaml)) {
      load_yaml(module_yaml, map = settings)
    }
    module_settings <- list()

    for(module in modules){

      module_target <- file.path(module_path, module$module_id)
      if(dir.exists(module_target)){
        unlink(module_target, recursive = TRUE)
      }
      file.copy(module$module_path, module_path, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)

      module_settings[[module$module_id]] <- module$module_info

      catgl("Installed interactive module: ", module$module_id, level = "DEFAULT")

    }

    settings$modules <- module_settings

    save_yaml(settings, module_yaml)
  }

  if( identical(desc$Type, "rave-pipeline-collection") ){
    # install sub-versions if possible
    if(length(desc$SubPipelines)){
      sub_pipes <- strsplit(desc$SubPipelines, "[,\n]+")[[1]]
      for(pname in sub_pipes){
        pdir <- file.path(directory, pname)

        catgl("Adding pipeline {pname}", level = "DEFAULT")
        pipeline_install_directory(pdir, dest, upgrade = upgrade, force = force, ...)
      }
    }
  } else {
    catgl("Adding pipeline {desc$Package}", level = "DEFAULT")
    pipeline_root <- file.path(dest, desc$Package, desc$Version)
    if(dir.exists(pipeline_root)){
      # if(!force){
      #   stop("Pipeline ", desc$Package, " - version ", desc$Version,
      #        ' already exists. Please use `force=TRUE` to force install')
      # }
      catgl("Removing previously installed {desc$Package}", level = "DEFAULT")
      unlink(pipeline_root, recursive = TRUE, force = TRUE)
    }
    dir_create2(pipeline_root)

    fs <- list.files(directory, all.files = TRUE, full.names = FALSE, recursive = FALSE, include.dirs = FALSE, no.. = TRUE)

    file.copy(file.path(directory, fs), pipeline_root, recursive = TRUE, copy.date = TRUE)

    # Update version file
    version_file <- file.path(dest, desc$Package, "versions.yaml")
    save_yaml(desc, version_file)
  }
  invisible()

}

#' @name pipeline_install
#' @title Install 'RAVE' pipelines
#' @param src pipeline directory
#' @param repo 'Github' repository in user-repository combination, for example,
#' \code{'rave-ieeg/rave-pipeline'}
#' @param to installation path; choices are \code{'default'}, \code{'custom'},
#' \code{'workdir'}, and \code{'tempdir'}. Please specify pipeline root path
#' via \code{\link{pipeline_root}} when \code{'custom'} is used.
#' @param upgrade whether to upgrade the dependence; default is \code{FALSE}
#' for stability, however, it is highly recommended to upgrade your
#' dependencies
#' @param set_default whether to set current pipeline module folder as the
#' default, will be automatically set when the pipeline is from the official
#' 'Github' repository.
#' @param force whether to force installing the pipelines
#' @param ... other parameters not used
#' @returns nothing
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' pipeline_install_github("rave-ieeg/pipelines")
#'
#'
#' # or download github.com/rave-ieeg/pipelines repository, extract
#' # to a folder, and call
#' pipeline_install_local("path/to/pipeline/folder")
#'
#' }
#'
#'
#' @export
pipeline_install_local <- function(
    src, to = c("default", "custom", "workdir", "tempdir"),
    upgrade = FALSE, force = FALSE, set_default = NA, ...
) {
  src <- normalizePath(src, mustWork = TRUE)
  stopifnot2(dir.exists(src), msg = "`pipeline_install_local`: `src` must be a valid directory")
  to <- match.arg(to)
  switch (
    to,
    "custom" = {
      dest <- pipeline_root()
      if(length(dest) > 1){
        dest <- dest[dest != "."]
      }
      dest <- normalizePath(dest[[1]])
    },
    "workdir" = {
      dest <- normalizePath(".")
    },
    "tempdir" = {
      dest <- normalizePath(tempdir(check = TRUE))
    },
    {
      dest <- normalizePath(ravepipeline_data_dir("pipelines"), mustWork = FALSE)
    }
  )

  if(isTRUE(set_default)) {
    fs <- list.files(src, recursive = FALSE, full.names = TRUE, all.files = TRUE)
    template_path <- ravepipeline_data_dir("rave-pipelines")

    if(dir.exists(template_path)) {
      try({
        unlink(template_path, recursive = TRUE)
      })
    }
    dir_create2(template_path)
    file.copy(from = fs, to = template_path, recursive = TRUE, copy.mode = FALSE, copy.date = TRUE, overwrite = TRUE)
  }

  pipeline_install_directory(directory = src, dest = dest, upgrade = upgrade, force = force, ...)

}

#' @rdname pipeline_install
#' @export
pipeline_install_github <- function(
    repo, to = c("default", "custom", "workdir", "tempdir"),
    upgrade = FALSE, force = FALSE, set_default = NA, ...
) {
  to <- match.arg(to)
  args <- list(...)

  # DIPSAUS DEBUG START
  # repo <- 'rave-ieeg/rave-pipelines'
  # to <- "default"
  # upgrade <- FALSE
  # force <- FALSE
  # args <- list()

  remote_argnames <- c("ref", "subdir", "auth_token", "sha", "host")
  remote_args <- args[names(args) %in% remote_argnames]
  remote_args$repo <- repo
  github_remote <- get_remotes_fun("github_remote")
  remote <- do.call(github_remote, remote_args)
  exdir <- tempfile()
  # CRAB policy requires resetting options
  timeout <- options(timeout = 3600)
  on.exit({ options(timeout) })

  res <- tryCatch({
    message("Trying to use Github API")
    remote_download <- get_remotes_fun("remote_download")
    tarball <- remote_download(remote)
    tarball_format <- "tar.gz"
  }, error = function(e) {
    message("Unable to download the tarball. You might have been using the wrong Github API token/privilege or incorrect repository name.")
    e
  })
  if(inherits(res, "error")) {
    message("Falling back to normal downloading method (branch information is ignored).")
    url <- sprintf("https://github.com/%s/%s/archive/refs/heads/main.zip",
                   remote$username, remote$repo)
    tarball <- tempfile(fileext = ".zip")
    utils::download.file(url, destfile = tarball, cacheOK = FALSE)
    tarball_format <- "zip"
  }

  on.exit({
    unlink(tarball)
    unlink(exdir, recursive = TRUE, force = TRUE)
  }, add = TRUE)


  utils::untar(tarball, exdir = exdir)

  src <- exdir
  conf_path <- file.path(src, c("RAVE-CONFIG", "DESCRIPTION"))
  if(!any(file.exists(conf_path))){
    srcs <- list.dirs(src, full.names = TRUE, recursive = FALSE)
    for(src in srcs){
      conf_path <- file.path(src, c("RAVE-CONFIG", "DESCRIPTION"))
      if(any(file.exists(conf_path))){
        break
      }
    }
  }
  conf_path <- conf_path[file.exists(conf_path)]
  if(length(conf_path)) {
    conf_path <- conf_path[[1]]
    repo0 <- gsub("@.*$", "", repo)
    reg <- module_registry2(repo0, conf_path)
    # get current registry
    all_regs <- get_modules_registries(update = FALSE)
    for(item in all_regs) {
      if(!identical(
        reg$maintainer$email,
        item$maintainer$email
      )) {
        dups <- item$modules[item$modules %in% reg$modules]
        if(length(dups)) {
          stop(sprintf("Cannot install modules from repository [%s]. The following module IDs have been registered by other repositories:\n  %s", repo, paste(dups, collapse = ", ")))
        }
      }

    }
    # conf <- as.list(as.data.frame(read.dcf(conf_path)))
  }

  if(is.na(set_default) && identical(repo, "rave-ieeg/rave-pipelines")) {
    set_default <- TRUE
  }
  # if(identical(repo, "rave-ieeg/rave-pipelines")) {
  #   fs <- list.files(src, recursive = FALSE, full.names = TRUE, all.files = TRUE)
  #   template_path <- ravepipeline_data_dir("rave-pipelines")
  #
  #   if(dir.exists(template_path)) {
  #     try({
  #       unlink(template_path, recursive = TRUE)
  #     })
  #   }
  #   dir_create2(template_path)
  #   file.copy(from = fs, to = template_path, recursive = TRUE, copy.mode = FALSE, copy.date = TRUE, overwrite = TRUE)
  # }


  args <- args[!names(args) %in% remote_argnames]
  args$src <- src
  args$to <- to
  args$upgrade <- upgrade
  args$force <- force
  args$set_default <- set_default
  do.call(pipeline_install_local, args)
}

#' @rdname rave-pipeline
#' @export
pipeline_root <- local({
  root <- NULL
  function(root_path, temporary = FALSE){
    re <- root
    if(!missing(root_path)){
      if(any(is.na(root_path))){ stop("pipeline root cannot be NA") }
      if('.' %in% root_path){
        root_path <- root_path[root_path != '.']
        re <- c(".", normalizePath(root_path, mustWork = FALSE))
        if(!temporary) {
          root <<- re
        }
      } else {
        re <- normalizePath(root_path, mustWork = FALSE)
        if(!temporary) {
          root <<- re
        }
      }
      if(!any(dir.exists(re))){
        warning("The following pipeline root directories do not exist: \n  |> ", paste(re, collapse = "\n  |> "))
      }
    } else {
      if(is.null(re)){
        re <- c(".", ravepipeline_data_dir("pipelines"))
        root <<- re
      }
    }
    unique(re)
  }
})

#' @rdname rave-pipeline
#' @export
pipeline_list <- function(root_path = pipeline_root()){
  names <-
    unlist(lapply(
      root_path,
      list.dirs,
      full.names = FALSE,
      recursive = FALSE
    ))
  names <- names[!grepl("^[.~_]", names)]
  names <- names[!names %in% c("R", "src", "inst", "man", "doc")]
  names <- names[vapply(names, function(nm){
    try({
      pipeline_find(nm, root_path = root_path)
      return(TRUE)
    }, silent = TRUE)
    return(FALSE)
  }, FALSE)]
  names
}

#' @rdname rave-pipeline
#' @export
pipeline_find <- function(name, root_path = pipeline_root()){

  paths <- file.path(root_path, name)
  paths <- paths[dir.exists(paths)]

  for(path in paths){
    path <- tryCatch({
      vpath <- file.path(path, "versions.yaml")
      if(file.exists(vpath)){
        # read version file
        v <- load_yaml(file.path(path, "versions.yaml"))
        spath <- file.path(path, v$Version)
        # copy the pipeline out immediate
        path <- file.path(temporary_session_root(), "pipelines", name)
        if(!file.exists(file.path(path, "common.R"))) {
          dir_create2(path)
          fs <- list.files(spath, all.files = FALSE, recursive = FALSE, include.dirs = TRUE, full.names = TRUE)
          for(f in fs) {
            file.copy(fs, path, overwrite = TRUE, recursive = TRUE)
          }
        }
      }
      path <- activate_pipeline(path)
      return(path)
    }, error = function(e){
      NULL
    })
    if(!is.null(path)){
      return(path)
    }
  }
  stop("Cannot find RAVE pipeline `", name, "`. Have you installed it?")
}

#' @rdname rave-pipeline
#' @export
pipeline_attach <- function(name, root_path = pipeline_root()){
  path <- pipeline_find(name, root_path)
  Sys.setenv("RAVE_PIPELINE" = path)
}

