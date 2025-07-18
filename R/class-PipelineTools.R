#' Class definition for 'RAVE' pipelines
#' @seealso \code{\link{pipeline}}
#' @export
PipelineTools <- R6::R6Class(
  classname = "PipelineTools",
  portable = TRUE,
  cloneable = TRUE,

  private = list(
    .pipeline_path = character(),
    .pipeline_name = character(),
    .settings_file = character(),
    .settings = NULL,
    .settings_external_inputs = list(),
    .description = NULL,
    .preferences = NULL
  ),

  public = list(

    #' @description construction function
    #' @param pipeline_name name of the pipeline, usually in the pipeline
    #' \code{'DESCRIPTION'} file, or pipeline folder name
    #' @param settings_file the file name of the settings file, where the
    #' user inputs are stored
    #' @param paths the paths to find the pipeline, usually the parent folder
    #' of the pipeline; default is \code{pipeline_root()}
    #' @param temporary whether not to save \code{paths} to current pipeline
    #' root registry. Set this to \code{TRUE} when importing pipelines
    #' from subject pipeline folders
    initialize = function(pipeline_name,
                          settings_file = "settings.yaml",
                          paths = pipeline_root(), temporary = FALSE) {

      default_paths <- c(".", ravepipeline_data_dir("pipelines"))

      paths <- c(paths[dir.exists(paths)], default_paths)

      private$.pipeline_path <- pipeline_find(pipeline_name, root_path = pipeline_root(paths, temporary = temporary))
      private$.pipeline_name <- attr(private$.pipeline_path, "target_name")
      private$.settings_file <- settings_file
      private$.preferences <- global_preferences(
        .prefix_whitelist = c("global", self$pipeline_name),
        # TODO: add type-explicit
        .type_whitelist = NULL
      )

      pipeline_settings_path <- file.path(
        private$.pipeline_path,
        private$.settings_file
      )

      settings <- load_yaml(pipeline_settings_path)
      lapply(names(settings), function(nm) {
        if(nm == "") { return() }
        opts <- resolve_pipeline_settings_opt(settings[[nm]], strict = FALSE)
        if(is.null(opts) || !is.list(opts)) { return() }

        opts$raw_input <- settings[[nm]]
        private$.settings_external_inputs[[nm]] <- opts
        settings[[nm]] <- resolve_pipeline_settings_value( settings[[nm]], pipe_dir = private$.pipeline_path )
      })
      private$.settings <- settings

      # also assign version
      descr <- tryCatch({
        get_module_description(private$.pipeline_path)
      }, error = function(...) {
        structure(
          list(
            Package = private$.pipeline_name,
            Type = "rave-pipeline",
            Title = "No Title",
            Version = "0.0.0.9000",
            Language = "en-US",
            Encoding = "UTF-8",
            License = "Unlicensed",
            Description = "No description found in this pipeline",
            Imports = "",
            Author = structure(list(
              list(
                given = "NA",
                family = "NA",
                role = c("aut", "cre"),
                email = "NA",
                comment = NULL
              )
            ), class = "person")
          ),
          class = "raveModuleDescription"
        )
      })

      private$.description <- descr

    },

    #' @description set inputs
    #' @param ...,.list named list of inputs; all inputs should be named,
    #' otherwise errors will be raised
    set_settings = function(..., .list = NULL) {
      args <- c(list(...), as.list(.list))
      argnames <- names(args)

      if(length(args)) {
        if(!length(argnames) || "" %in% argnames) {
          stop("`pipeline_set`: all input lists must have names")
        }

        external_inputs <- names(private$.settings_external_inputs)
        external_args <- argnames[argnames %in% external_inputs]
        internal_args <- argnames[!argnames %in% external_inputs]
        lapply(external_args, function(nm) {
          new_val <- args[[nm]]
          opts <- private$.settings_external_inputs[[nm]]

          pipeline_save_extdata(
            data = new_val,
            name = opts$name,
            format = opts$format,
            overwrite = TRUE,
            pipe_dir = private$.pipeline_path
          )
          cls <- class(new_val)
          if( !"raveio-pipeline-extdata" %in% cls ) {
            cls <- c("raveio-pipeline-extdata", cls)
          }
          private$.settings[[nm]] <- structure(
            new_val, class = cls,
            `raveio-pipeline-extdata-opts` = opts
          )
          return()
        })
        lapply(internal_args, function(nm) {
          private$.settings[[nm]] <- args[[nm]]
          return()
        })

        # TODO: check whether this should be put outside, i.e. save settings
        # no matter the settings have been changed or not
        pipeline_settings_path <- file.path(
          private$.pipeline_path,
          private$.settings_file
        )

        settings_copy <- as.list(private$.settings)
        if(length(external_inputs)) {
          settings_copy[external_inputs] <- lapply(private$.settings_external_inputs, "[[", "raw_input")
        }
        save_yaml(
          x = settings_copy,
          file = pipeline_settings_path,
          sorted = TRUE
        )
      }

      return(invisible(as.list(private$.settings)))
    },

    #' @description get current inputs
    #' @param key the input name; default is missing, i.e., to get all the
    #' settings
    #' @param default default value if not found
    #' @param constraint the constraint of the results; if input value is not
    #' from \code{constraint}, then only the first element of \code{constraint}
    #' will be returned.
    #' @returns The value of the inputs, or a list if \code{key} is missing
    get_settings = function(key, default = NULL, constraint) {
      if(missing(key)){
        return(as.list(private$.settings))
      }
      if(!private$.settings$`@has`(key)){
        re <- default
      } else {
        re <- private$.settings[[key]]
      }
      if(!missing(constraint)){
        re <- re %OF% constraint
      }
      re
    },

    #' @description read intermediate variables
    #' @param var_names the target names, can be obtained via
    #' \code{x$target_table} member; default is missing, i.e., to read
    #' all the intermediate variables
    #' @param ifnotfound variable default value if not found
    #' @param ... other parameters passing to \code{\link{pipeline_read}}
    #' @returns The values of the targets
    read = function(var_names, ifnotfound = NULL, ...) {

      # Check targets, make sure the `tar_runtime$store` is not identical to "shared"
      # targets ban users from read from pipeline when running, even from
      # another pipeline project
      # This is hack of course : )
      targets <- asNamespace("targets")
      tar_runtime <- targets$tar_runtime
      current_store <- tar_runtime$store
      needs_reset <- FALSE
      if( identical(tar_runtime$store, "shared") ) {
        needs_reset <- TRUE
        tar_runtime$store = '___'
        on.exit({
          if(!identical(tar_runtime$store, current_store)) {
            tar_runtime$store <- current_store
          }
        }, add = TRUE, after = FALSE)
      }

      if(missing(var_names)) {
        var_names <- pipeline_target_names(pipe_dir = private$.pipeline_path)
      } else {
        var_names_quoted <- substitute(var_names)
        if(typeof(var_names_quoted) == "language" &&
           identical(var_names_quoted[[1]], quote(`-`))) {
          all_names <- pipeline_target_names(pipe_dir = private$.pipeline_path)
          var_names <- all_names[!all_names %in% eval(var_names_quoted[[2]], envir = parent.frame())]
        }
      }

      re <- pipeline_read(var_names = var_names, pipe_dir = private$.pipeline_path,
                          ifnotfound = ifnotfound, ...)
      if( needs_reset ) {
        tar_runtime$store <- current_store
      }

      return(re)

    },

    #' @description run the pipeline
    #' @param names pipeline variable names to calculate; default is to
    #' calculate all the targets
    #' @param async whether to run asynchronous in another process
    #' @param as_promise whether to return a \code{\link{PipelineResult}}
    #' instance
    #' @param scheduler,type,envir,callr_function,return_values,debug,... passed
    #' to \code{\link{pipeline_run}} if \code{as_promise} is true, otherwise
    #' these arguments will be passed to \code{pipeline_run_bare}
    #' @returns A \code{\link{PipelineResult}} instance if \code{as_promise}
    #' or \code{async} is true; otherwise a list of values for input \code{names}
    run = function(names = NULL, async = FALSE, as_promise = async,
                   scheduler = c("none", "future", "clustermq"),
                   type = c("smart", "callr", "vanilla"),
                   envir = new.env(parent = globalenv()),
                   callr_function = NULL, return_values = TRUE,
                   debug = FALSE,
                   ...) {
      if(!as_promise && async) {
        stop("If you run the pipeline asynchronous, then the result must be a `promise` object")
      }
      if(missing(scheduler) && missing(type)) {
        py_module_exists <- tryCatch({
          self$python_module("exist")
        }, error = function(e) { FALSE })

        if( isTRUE(py_module_exists) ) {
          scheduler <- "future"
          type <- "callr"
        } else {
          scheduler <- match.arg(scheduler)
          type <- match.arg(type)
        }
      } else {
        scheduler <- match.arg(scheduler)
        type <- match.arg(type)
      }

      force(envir)
      force(callr_function)

      expr <- bquote(pipeline_run_bare(
        pipe_dir = .(private$.pipeline_path), scheduler = .(scheduler),
        type = .(type), envir = envir, callr_function = .(callr_function),
        names = .(names), return_values = .(return_values), debug = .(debug), ...))

      if( as_promise ) {
        expr[[1]] <- quote(pipeline_run)
        expr[["async"]] <- async
      }
      eval(expr)

    },

    #' @description run the pipeline in order; unlike \code{$run()}, this method
    #' does not use the \code{targets} infrastructure, hence the pipeline
    #' results will not be stored, and the order of \code{names} will be
    #' respected.
    #' @param names pipeline variable names to calculate; must be specified
    #' @param env environment to evaluate and store the results
    #' @param shortcut logical or characters; default is \code{FALSE}, meaning
    #' \code{names} and all the dependencies (if missing from \code{env})
    #' will be evaluated; set to \code{TRUE} if only \code{names} are to be
    #' evaluated. When \code{shortcut} is a character vector, it should be
    #' a list of targets (including their ancestors) whose values can be assumed
    #' to be up-to-date, and the evaluation of those targets can be skipped.
    #' @param clean whether to evaluate without polluting \code{env}
    #' @param ... passed to \code{\link{pipeline_eval}}
    eval = function(names, env = parent.frame(),
                    shortcut = FALSE, clean = TRUE, ...) {
      if(clean) {
        envir <- new.env(parent = env)
      } else {
        envir <- env
      }
      # shared_path <- file.path(private$.pipeline_path, "R")
      # shared_libs <- list.files(shared_path, pattern = "^shared-.*\\.R",
      #                           full.names = TRUE, ignore.case = TRUE)
      # shared_libs <- sort(shared_libs)
      #
      # lapply(shared_libs, function(f) {
      #   source(file = f, local = envir, chdir = TRUE)
      # })
      # list2env(self$get_settings(), envir = envir)
      if(missing(names)) {
        names <- self$target_table$Names
      }
      if(is.character(shortcut)) {
        # skip targets specified by `shortcut`

        # These targets need to update to get `names`
        queue_targets <- self$target_ancestors(names, skip_names = shortcut)

        # Update `names` so the `eval` is explicit
        names <- unique(c(queue_targets, names))

        # These targets are assumed up-to-date
        matured_targets <- attr(queue_targets, "skipped")

        # Pipeline shared environment with data loaded
        existing_names <- ls(env, all.names = TRUE, sorted = FALSE)
        missing_names <- matured_targets[!matured_targets %in% existing_names]
        if(length(missing_names)) {
          list2env(
            self$read(missing_names, simplify = FALSE),
            envir = envir
          )
        }
        shortcut <- TRUE
      }
      pipeline_eval(names = names, env = envir, pipe_dir = private$.pipeline_path,
                    settings_path = self$settings_path, shortcut = shortcut, ...)
    },

    #' @description run the pipeline shared library in scripts starting with
    #' path \code{R/shared}
    #' @param callr_function either \code{callr::r} or \code{NULL}; when
    #' \code{callr::r}, the environment will be loaded in isolated R session
    #' and serialized back to the main session to avoid contaminating the
    #' main session environment; when \code{NULL}, the code will be sourced
    #' directly in current environment.
    #' @returns An environment of shared variables
    shared_env = function(callr_function = callr::r) {
      env <- pipeline_shared(pipe_dir = private$.pipeline_path, callr_function = callr_function)
      return(env)
    },

    #' @description get 'Python' module embedded in the pipeline
    #' @param type return type, choices are \code{'info'} (get basic information
    #' such as module path, default), \code{'module'} (load module and return
    #' it), \code{'shared'} (load a shared sub-module from the module, which
    #' is shared also in report script), and \code{'exist'} (returns true
    #' or false on whether the module exists or not)
    #' @param must_work whether the module needs to be existed or not. If
    #' \code{TRUE}, the raise errors when the module does not exist; default
    #' is \code{TRUE}, ignored when \code{type} is \code{'exist'}.
    #' @returns See \code{type}
    python_module = function(type = c("info", "module", "shared", "exist"),
                             must_work = TRUE) {
      type <- match.arg(type)

      if(type == "exist") { must_work <- FALSE }

      re <- tryCatch({

        if( type == "module" ) {
          return(pipeline_py_module(
            pipe_dir = self$pipeline_path,
            must_work = must_work,
            convert = FALSE
          ))
        }

        minfo <- pipeline_py_info(pipe_dir = self$pipeline_path, must_work = must_work)
        switch(
          type,
          "info" = { return(minfo) },
          "exist" = {
            return(isTRUE(is.list(minfo)))
          },
          {
            if(!is.list(minfo)) { return(NULL) }
            pypath <- file.path(self$pipeline_path, "py")
            cwd <- getwd()

            setwd(pypath)
            on.exit({
              if(length(cwd) == 1) { setwd(cwd) }
            }, add = TRUE, after = FALSE)

            shared <- rpymat::import(sprintf("%s.shared", minfo$module_name),
                                     convert = FALSE, delay_load = FALSE)

            # set wd back so no need to wait for on.exit
            setwd(cwd)
            cwd <- NULL

            return(shared)
          }
        )

      }, error = function(e) {

        if(must_work) {
          stop(e)
        }
        NULL

      })


      return(re)


    },

    #' @description get progress of the pipeline
    #' @param method either \code{'summary'} or \code{'details'}
    #' @returns A table of the progress
    progress = function(method = c("summary", "details")) {
      method <- match.arg(method)
      pipeline_progress(pipe_dir = private$.pipeline_path, method = method)
    },

    #' @description attach pipeline tool to environment (internally used)
    #' @param env an environment
    attach = function(env) {
      env$pipeline_set <- self$set_settings
      env$pipeline_get <- self$get_settings
      env$pipeline_settings_path <- self$settings_path
      env$pipeline_path <- private$.pipeline_path
      list2env(as.list(self$get_settings()), envir = env)
      shared_env <- self$shared_env()
      list2env(as.list(shared_env), envir = env)
    },

    #' @description visualize pipeline target dependency graph
    #' @param glimpse whether to glimpse the graph network or render the state
    #' @param aspect_ratio controls node spacing
    #' @param node_size,label_size size of nodes and node labels
    #' @param ... passed to \code{\link{pipeline_visualize}}
    #' @returns Nothing
    visualize = function(glimpse = FALSE, aspect_ratio = 2, node_size = 30, label_size = 40, ...) {
      args <- list(pipe_dir = private$.pipeline_path, glimpse = glimpse, ...)
      tryCatch({
        widget <- pipeline_dependency_graph(
          glimpse = glimpse, pipeline_path = private$.pipeline_path,
          aspect_ratio = aspect_ratio, node_size = node_size, label_size = label_size, ...)
        asNamespace("htmlwidgets")
        print(widget)
      }, error = function(e) {
        re <- do.call(pipeline_visualize, args)
        if(inherits(re, "htmlwidget")) {
          print(re)
        }
      })
      return(invisible())
    },

    #' @description a helper function to get target ancestors
    #' @param names targets whose ancestor targets need to be queried
    #' @param skip_names targets that are assumed to be up-to-date, hence
    #' will be excluded, notice this exclusion is
    #' recursive, that means not only \code{skip_names} are excluded,
    #' but also their ancestors will be excluded from the result.
    #' @returns ancestor target names (including \code{names})
    target_ancestors = function(names, skip_names = NULL) {
      pipeline_dep_targets(names = names, skip_names = skip_names,
                           pipe_dir = private$.pipeline_path)
    },

    #' @description fork (copy) the current pipeline to a new directory
    #' @param path path to the new pipeline, a folder will be created there
    #' @param policy fork policy defined by module author, see text file
    #' 'fork-policy' under the pipeline directory; if missing, then default to
    #' avoid copying \code{main.html} and \code{shared} folder
    #' @returns A new pipeline object based on the path given
    fork = function(path, policy = "default") {
      pipeline_fork(
        src = self$pipeline_path,
        dest = path,
        policy = policy,
        activate = FALSE
      )
      pipeline(
        pipeline_name = basename(path),
        settings_file = basename(self$settings_path),
        paths = dirname(path)
      )
    },

    #' @description fork (copy) the current pipeline to a 'RAVE' subject
    #' @param subject subject ID or instance in which pipeline will be saved
    #' @param label pipeline label describing the pipeline
    #' @param policy fork policy defined by module author, see text file
    #' 'fork-policy' under the pipeline directory; if missing, then default to
    #' avoid copying \code{main.html} and \code{shared} folder
    #' @param delete_old whether to delete old pipelines with the same label
    #' default is false
    #' @param sanitize whether to sanitize the registry at save. This will
    #' remove missing folders and import manually copied pipelines to the
    #' registry (only for the pipelines with the same name)
    #' @returns A new pipeline object based on the path given
    fork_to_subject = function(subject, label = "NA", policy = "default",
                               delete_old = FALSE, sanitize = TRUE) {
      # subject <- restore_subject_instance(subject, strict = TRUE)
      subject <- call_pkg_fun(package = "raveio", f_name = "as_rave_subject", subject, strict = TRUE)
      label <- paste(label, collapse = "")
      label_cleaned <- gsub("[^a-zA-Z0-9_.-]+", "_", label)

      timestamp <- Sys.time()
      name <- sprintf(
        "%s-%s-%s",
        self$pipeline_name,
        label_cleaned,
        format(timestamp, "%Y%m%dT%H%M%S")
      )
      path <- file.path(
        subject$pipeline_path,
        self$pipeline_name,
        name
      )
      # make sure parent folder exists
      dir_create2(dirname(path))
      re <- self$fork(path = path, policy = policy)

      # register
      registry_path <- file.path(subject$pipeline_path, "pipeline-registry.csv")
      if(file.exists(registry_path)) {
        registry <- tryCatch({
          registry <- data.table::fread(registry_path, stringsAsFactors = FALSE, colClasses = c(
            project = "character",
            subject = "character",
            pipeline_name = "character",
            timestamp = "POSIXct",
            label = "character",
            directory = "character"
          ), na.strings = "n/a")
          nms <- names(registry)
          stopifnot(all(c("project", "subject", "pipeline_name", "timestamp", "label", "directory") %in% nms))
          if(!"policy" %in% nms) {
            registry$policy <- "default"
          }
          if(!"version" %in% nms) {
            registry$version <- "0.0.0.9000"
          }
          registry
        }, error = function(...) { NULL })
      } else {
        registry <- NULL
      }

      if( sanitize ) {
        # use subject's pipeline list
        new_registry <- subject$list_pipelines(pipeline_name = self$pipeline_name, cache = FALSE, check = TRUE, all = TRUE)
        if(length(registry)) {
          registry <- rbind(
            new_registry,
            registry[registry$pipeline_name != self$pipeline_name, ],
            fill = TRUE
          )
        } else {
          registry <- new_registry
        }

      } else {
        registry <- rbind(
          data.table::data.table(
            project = subject$project_name,
            subject = subject$subject_code,
            pipeline_name = self$pipeline_name,
            timestamp = timestamp,
            label = label,
            directory = name,
            policy = policy,
            version = self$description$Version,
            keep.rownames = FALSE, stringsAsFactors = FALSE
          ),
          registry
        )
      }

      registry$policy[is.na(registry$policy)] <- "default"
      registry$version[is.na(registry$version)] <- "0.0.0.9000"

      # also delete old pipelines
      if( delete_old ) {
        sel <- (
          registry$pipeline_name == self$pipeline_name &
            registry$label == label &
            registry$directory != name
        )
        if( any(sel) ) {
          sub <- registry[sel, ]
          dirs <- file.path(
            subject$pipeline_path,
            self$pipeline_name,
            sub$directory
          )
          lapply(dirs, unlink, recursive = TRUE)
          registry <- registry[!sel, ]
        }
      }

      tf <- tempfile()
      on.exit({ unlink(tf) })
      data.table::fwrite(registry, tf, row.names = FALSE, col.names = TRUE)
      file.copy(tf, to = registry_path, overwrite = TRUE, recursive = FALSE, copy.date = TRUE)
      re
    },

    #' @description run code with pipeline activated, some environment variables
    #' and function behaviors might change under such condition (for example,
    #' \code{targets} package functions)
    #' @param expr expression to evaluate
    #' @param quoted whether \code{expr} is quoted; default is false
    #' @param env environment to run \code{expr}
    with_activated = function(expr, quoted = FALSE, env = parent.frame()) {
      if(!quoted) {
        expr <- substitute(expr)
      }
      activate_pipeline(pipe_dir = private$.pipeline_path)
      # don't mess with self$eval
      basens <- baseenv()
      basens$eval(expr, envir = env)
    },


    #' @description clean all or part of the data store
    #' @param destroy,ask see \code{\link[targets]{tar_destroy}}
    clean = function(destroy = c(
      "all", "cloud", "local", "meta", "process", "preferences",
      "progress", "objects", "scratch", "workspaces"),
      ask = FALSE) {
      destroy <- match.arg(destroy)
      pipeline_clean(pipe_dir = private$.pipeline_path, ask = ask, destroy = destroy)
    },

    #' @description save data to pipeline data folder
    #' @param data R object
    #' @param name the name of the data to save, must start with letters
    #' @param format serialize format, choices are \code{'json'},
    #' \code{'yaml'}, \code{'csv'}, \code{'fst'}, \code{'rds'}; default is
    #' \code{'json'}. To save arbitrary objects such as functions or
    #' environments, use \code{'rds'}
    #' @param overwrite whether to overwrite existing files; default is no
    #' @param ... passed to saver functions
    #' @returns the saved file path
    save_data = function(data, name, format = c("json", "yaml", "csv", "fst", "rds"),
                         overwrite = FALSE, ...) {
      format <- match.arg(format)
      pipeline_save_extdata(
        data = data, name = name, format = format,
        overwrite = overwrite, pipe_dir = self$pipeline_path, ...)
    },

    #' @description load data from pipeline data folder
    #' @param name the name of the data
    #' @param error_if_missing whether to raise errors if the name is missing
    #' @param default_if_missing default values to return if the name is missing
    #' @param format the format of the data, default is automatically obtained
    #' from the file extension
    #' @param ... passed to loader functions
    #' @returns the data if file is found or a default value
    load_data = function(name, error_if_missing = TRUE, default_if_missing = NULL,
                         format = c("auto", "json", "yaml", "csv", "fst", "rds"), ...) {

      format <- match.arg(format)
      pipeline_load_extdata(name = name, format = format,
                            error_if_missing = error_if_missing,
                            default_if_missing = default_if_missing,
                            pipe_dir = self$pipeline_path, ...)
    },

    #' @description set persistent preferences from the pipeline.
    #' The preferences should not affect how pipeline is working, hence usually
    #' stores minor variables such as graphic options. Changing preferences
    #' will not invalidate pipeline cache.
    #' @param name preference name, must contain only letters, digits,
    #' underscore, and hyphen, will be coerced to lower case (case-insensitive)
    #' @param ...,.list key-value pairs of initial preference values. The keys
    #' must start with 'global' or the module ID, followed by dot and preference
    #' type and names. For example \code{'global.graphics.continuous_palette'}
    #' for setting palette colors for continuous heat-map; "global" means the
    #' settings should be applied to all 'RAVE' modules. The module-level
    #' preference, \code{'power_explorer.export.default_format'} sets the
    #' default format for power-explorer export dialogue.
    #' @returns A list of key-value pairs
    set_preferences = function(..., .list = NULL) {
      pipeline_set_preferences(..., .list = .list,
                               .preference_instance = private$.preferences)
    },

    #' @description get persistent preferences from the pipeline.
    #' @param keys characters to get the preferences
    #' @param simplify whether to simplify the results when length of key is 1;
    #' default is true; set to false to always return a list of preferences
    #' @param ifnotfound default value when the key is missing
    #' @param validator \code{NULL} or function to validate the values; see
    #' 'Examples'
    #' @param ... passed to \code{validator} if \code{validator} is a function
    #' @returns A list of the preferences. If \code{simplify} is true and length
    #' if keys is 1, then returns the value of that preference
    #' @examples
    #'
    #' library(ravepipeline)
    #' if(interactive() && length(pipeline_list()) > 0) {
    #'   pipeline <- pipeline("power_explorer")
    #'
    #'   # set dummy preference
    #'   pipeline$set_preferences("global.example.dummy_preference" = 1:3)
    #'
    #'   # get preference
    #'   pipeline$get_preferences("global.example.dummy_preference")
    #'
    #'   # get preference with validator to ensure the value length to be 1
    #'   pipeline$get_preferences(
    #'     "global.example.dummy_preference",
    #'     validator = function(value) {
    #'       stopifnot(length(value) == 1)
    #'     },
    #'     ifnotfound = 100
    #'   )
    #'
    #'   pipeline$has_preferences("global.example.dummy_preference")
    #' }
    #'
    get_preferences = function(keys, simplify = TRUE, ifnotfound = NULL,
                               validator = NULL, ...) {
      pipeline_get_preferences(
        keys = keys,
        simplify = simplify,
        ifnotfound = ifnotfound,
        validator = validator,
        ...,
        .preference_instance = private$.preferences
      )
    },

    #' @description whether pipeline has preference keys
    #' @param keys characters name of the preferences
    #' @param ... passed to internal methods
    #' @returns logical whether the keys exist
    has_preferences = function(keys, ...) {
      pipeline_has_preferences(keys = keys, ...,
                               .preference_instance = private$.preferences)
    },

    #' @description generate pipeline
    #' @param name report name, see field \code{'available_reports'}
    #' @param output_dir parent folder where output will be stored
    #' @param output_format output format
    #' @param clean whether to clean the output; default is false
    #' @param ... passed to \code{'rmarkdown'} render function
    #' @returns A job identification number, see \code{\link{resolve_job}} for
    #' querying job details
    generate_report = function(
      name, output_dir = NULL, output_format = "html_document",
      clean = FALSE, ...) {
      pipeline_report_generate(
        name = name, output_dir = output_dir,
        output_format = output_format, clean = clean,
        ..., pipe_dir = private$.pipeline_path)
    }

  ),
  active = list(

    #' @field description pipeline description
    description = function(){
      private$.description
    },

    #' @field settings_path absolute path to the settings file
    settings_path = function() {
      file.path(
        private$.pipeline_path,
        private$.settings_file
      )
    },

    #' @field extdata_path absolute path to the user-defined pipeline data folder
    extdata_path = function() {
      file.path(private$.pipeline_path, "data")
    },

    #' @field preference_path directory to the pipeline preference folder
    preference_path = function() {
      file.path(private$.pipeline_path, "preferences")
    },

    #' @field target_table table of target names and their descriptions
    target_table = function() {
      re <- pipeline_target_names(pipe_dir = private$.pipeline_path)
      des <- sapply(strsplit(names(re), "_"), function(x){
        x <- x[x != ""]
        if(!length(x)) { return("<No description>") }
        substr(x[[1]], start = 1, stop = 1) <- toupper(
          substr(x[[1]], start = 1, stop = 1)
        )
        paste(x, collapse = " ")
      })
      data.frame(
        Names = unname(re),
        Description = des
      )
    },

    #' @field result_table summary of the results, including
    #' signatures of data and commands
    result_table = function() {
      pipeline_vartable(pipe_dir = private$.pipeline_path)
    },

    #' @field pipeline_path the absolute path of the pipeline
    pipeline_path = function() {
      private$.pipeline_path
    },

    #' @field pipeline_name the code name of the pipeline
    pipeline_name = function() {
      private$.pipeline_name
    },

    #' @field available_reports available reports and their configurations
    available_reports = function() {
      reports <- as.list(pipeline_report_list(private$.pipeline_path))
      report_configurations <- reports$report_configurations
      re <- lapply(report_configurations, function(config) {
        name <- config$name
        structure(list(config), names = name)
      })
      unlist(re, recursive = FALSE, use.names = TRUE)
    }

  )
)

#' @title Creates 'RAVE' pipeline instance
#' @description Set pipeline inputs, execute, and read pipeline outputs
#' @param pipeline_name the name of the pipeline, usually title field in the
#' \code{'DESCRIPTION'} file, or the pipeline folder name (if description
#' file is missing)
#' @param settings_file the name of the settings file, usually stores user
#' inputs
#' @param temporary see \code{\link{pipeline_root}}
#' @param paths the paths to search for the pipeline, usually the parent
#' directory of the pipeline; default is \code{\link{pipeline_root}}, which
#' only search for pipelines that are installed or in current working directory.
#' @param path the pipeline folder
#' @returns A \code{\link{PipelineTools}} instance
#' @examples
#'
#' library(ravepipeline)
#'
#' if(interactive()) {
#'
#'   # ------------ Set up a bare minimal example pipeline ---------------
#'   root_path <- tempdir()
#'   pipeline_root_folder <- file.path(root_path, "modules")
#'
#'   # create pipeline folder
#'   pipeline_path <- pipeline_create_template(
#'     root_path = pipeline_root_folder, pipeline_name = "raveio_demo",
#'     overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")
#'
#'   # Set initial user inputs
#'   yaml::write_yaml(
#'     x = list(
#'       n = 100,
#'       pch = 16,
#'       col = "steelblue"
#'     ),
#'     file = file.path(pipeline_path, "settings.yaml")
#'   )
#'
#'   # build the pipeline for the first time
#'   # this is a one-time setup
#'   pipeline_build(pipeline_path)
#'
#'   # Temporarily redirect the pipeline project root
#'   # to `root_path`
#'   old_opt <- options("raveio.pipeline.project_root" = root_path)
#'   # Make sure the options are reset
#'   on.exit({ options(old_opt) })
#'
#'   # Compile the pipeline document
#'   pipeline_render(
#'     module_id = "raveio_demo",
#'     project_path = root_path
#'   )
#'
#'   \dontrun{
#'
#'     # Open web browser to see compiled report
#'     utils::browseURL(file.path(pipeline_path, "main.html"))
#'
#'   }
#'
#'   # --------------------- Example starts ------------------------
#'
#'   # Load pipeline
#'   pipeline <- pipeline(
#'     pipeline_name = "raveio_demo",
#'     paths = pipeline_root_folder,
#'     temporary = TRUE
#'   )
#'
#'   # Check which pipeline targets to run
#'   pipeline$target_table
#'
#'   # Run to `plot_data`, RAVE pipeline will automatically
#'   # calculate which up-stream targets need to be updated
#'   # and evaluate these targets
#'   pipeline$run("plot_data")
#'
#'   # Customize settings
#'   pipeline$set_settings(pch = 2)
#'
#'   # Run again with the new inputs, since input_data does not change,
#'   # the pipeline will skip that target automatically
#'   pipeline$run("plot_data")
#'
#'   # Read intermediate data
#'   head(pipeline$read("input_data"))
#'
#'   # or use `[]` to get results
#'   pipeline[c("n", "pch", "col")]
#'   pipeline[-c("input_data")]
#'
#'   # Check evaluating status
#'   pipeline$progress("details")
#'
#'   # result summary & cache table
#'   pipeline$result_table
#'
#'   # visualize the target dependency graph
#'   pipeline$visualize(glimpse = TRUE)
#'
#'   # --------------------- Clean up ------------------------
#'   unlink(pipeline_path, recursive = TRUE)
#' }
#'
#' @export
pipeline <- function(pipeline_name,
                     settings_file = "settings.yaml",
                     paths = pipeline_root(),
                     temporary = FALSE) {
  PipelineTools$new(pipeline_name, settings_file, paths, temporary = temporary)
}

#' @rdname pipeline
#' @export
pipeline_from_path <- function(path, settings_file = "settings.yaml") {
  if(!dir.exists(path)) { stop("pipeline_from_path: `path` is not a valid directory:\n  path=", path) }
  path <- normalize_path(path)
  root_path <- dirname(path)

  re <- pipeline(pipeline_name = basename(path), paths = root_path, temporary = TRUE, settings_file = settings_file)
  re
}

#' @export
`[.PipelineTools` <- function(x, ...) {
  # args <- deparse1(c(...))
  # as.call(quote(x$read), args)
  expr <- as.list(match.call(expand.dots = TRUE))
  expr[[1]] <- x$read
  expr[["x"]] <- NULL
  expr <- as.call(expr)
  eval(expr, envir = parent.frame())
}

#' @export
format.PipelineTools <- function(x, ...) {

  has_python <- x$python_module(type = "exist")

  citation_str <- NULL
  if(length(x$description$Citations)) {
    citation_str <- paste(format(x$description$Citations, bibtex = FALSE), collapse = "\n")
    citation_str <- c("", trimws(citation_str))
  }

  target_table <- x$target_table
  input_settings <- x$get_settings()
  input_names <- names(input_settings)

  all_targets <- x$with_activated({
    load_target("make-main.R")
  })
  target_names <- vapply(all_targets, get_target_name, "")

  types <- vapply(target_table$Names, function(nm) {
    switch(
      nm,
      "settings_path" = "[internal file]",
      "settings" = "[internal list]",
      {
        if(nm %in% input_names) {
          v <- input_settings[[nm]]
          s <- utils::capture.output({
            utils::str(
              v,
              max.level = 0L,
              nchar.max = 45,
              give.attr = FALSE,
              drop.deparse.attr = TRUE,
              give.head = FALSE,
              indent.str = "",
              comp.str = "",
              no.list = FALSE
            )
          })
          s <- paste(s, collapse = "")
          if(is.list(v)) {
            s <- sprintf("<%s>", s)
          }
          s
        } else {
          deps <- get_target_deps(all_targets[[which(target_names == nm)[[1]]]])
          sprintf("[dependency: %4d]", length(deps))
        }
      }
    )
  }, "")

  target_table <- data.frame(
    Target = target_table$Names,
    Snapshot = types,
    Description = target_table$Description
  )


  tbl_str <- utils::capture.output({
    print(target_table, row.names = FALSE)
    invisible()
  })

  str <- c(
    sprintf("Pipeline <%s>", x$pipeline_name),
    sprintf("  Title : %s", x$description$Title),
    sprintf("  Path  : %s", x$pipeline_path),
    sprintf("  Python: %s", ifelse(has_python, "yes", "no")),
    citation_str,
    "",
    "Runnable target table:",
    sprintf("  %s", tbl_str)
  )
  paste(str, collapse = "\n")
}
