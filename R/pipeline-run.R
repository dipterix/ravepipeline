#' @rdname rave-pipeline
#' @export
pipeline_run <- function(
    pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
    scheduler = c("none", "future", "clustermq"),
    type = c("smart", "callr", "vanilla"),
    envir = new.env(parent = globalenv()),
    callr_function = NULL,
    names = NULL,
    async = FALSE,
    check_interval = 0.5,
    progress_quiet = !async,
    progress_max = NA,
    progress_title = "Running pipeline",
    return_values = TRUE,
    debug = FALSE,
    ...){

  pipe_dir <- activate_pipeline(pipe_dir)

  type <- match.arg(type)
  scheduler <- match.arg(scheduler)
  callr_function <- substitute(callr_function)


  clustermq_scheduler <- getOption('clustermq.scheduler', NA)
  if(scheduler == "clustermq"){
    # if(!identical(clustermq_scheduler, "LOCAL")){
    #   callr_function <- NULL
    # }
    callr_function <- NULL
  }

  if(type == "vanilla"){
    callr_function <- NULL
  } else if (type == "callr") {
    callr_function <- quote(callr::r)
  }
  if(debug) {
    message("pipeline_run: running with scheduler [", scheduler, "] and type [", type, "].")
  }

  args <- list(
    names = names,
    envir = envir,
    callr_function = NULL,
    ...
  )

  subprocess <- TRUE
  fun <- function(subprocess = TRUE){}
  environment(fun) <- new.env(parent = globalenv())
  body(fun) <- bquote({

    # this is running in another R session so load `ravepipeline`
    # without violating the CRAN policy
    ns <- asNamespace("ravepipeline")
    callr_function <- eval(.(callr_function))
    args <- .(args)
    if(!is.null(callr_function)){
      args$callr_function <- callr_function
    }
    clustermq_scheduler <- .(clustermq_scheduler)

    all_names <- ns$pipeline_target_names(pipe_dir = .(pipe_dir))
    if(length(args$names)) {
      if(!is.character(args$names)){
        stop("pipeline_run: `names` must be NULL or characters")
      }
      missing_names <- args$names[!args$names %in% all_names]
      if(length(missing_names)) {
        stop("pipeline_run: the following `names` cannot be found: ", paste(missing_names, collapse = ", "))
      }
    } else {
      args$names <- NULL
    }

    if(subprocess) {
      old_opts <- options(
        "future.fork.enable" = FALSE,
        "dipsaus.no.fork" = TRUE,
        "dipsaus.cluster.backup" = "multisession"
      )
      on.exit({ options(old_opts) }, add = TRUE, after = FALSE)
    }

    # Load shared functions into envir
    args$envir$pipeline <- pipeline_from_path(.(pipe_dir))
    shared_libs <- list.files(file.path(.(pipe_dir), "R"), pattern = "^shared-.*\\.R",
                              full.names = TRUE, ignore.case = TRUE)
    lapply(sort(shared_libs), function(f) {
      if(.(debug)) {
        message("pipeline_run: loading script: ", f)
      }
      source(file = f, local = args$envir, chdir = TRUE)
      return()
    })

    # if(dir.exists(file.path(.(pipe_dir), "py"))) {
    #   pipeline_py_module(pipe_dir = .(pipe_dir),
    #                      convert = FALSE)
    # }

    if(.(type) == "smart"){
      local <- ns$with_future_parallel
    }

    if(.(debug)) {
      message("pipeline_run: targeting names: ", deparse1(args$names), "...")
    }

    make <- function(fun, use_local = TRUE) {

      Sys.setenv("RAVE_PIPELINE_ACTIVE" = "true")
      on.exit({ Sys.unsetenv("RAVE_PIPELINE_ACTIVE") }, add = TRUE, after = FALSE)

      suppressWarnings({

        # check if targets version is at least 1.11.1
        if(isTRUE(utils::compareVersion(as.character(utils::packageVersion("targets")), "1.11.1") >= 0)) {
          targets::tar_config_set(reporter_make = "terse", reporter_outdated = "terse")
        } else {
          targets::tar_config_unset(c("reporter_make", "reporter_outdated"))
        }

        # Make sure the temporary folder exists
        tryCatch(
          expr = {
            ns$pipeline_clean(destroy = "process")
            if( use_local ) {
              local({ do.call(fun, args) })
            } else {
              do.call(fun, args)
            }
          },
          `tar_condition_file` = function(e) {
            # destroy and try again, and throw all other errors
            targets::tar_destroy(ask = FALSE, destroy = "meta")
            if( use_local ) {
              local({ do.call(fun, args) })
            } else {
              do.call(fun, args)
            }
          },
          error = function( e ) {
            Sys.unsetenv("RAVE_PIPELINE_ACTIVE")
            stop(ns$sanitize_target_error(e))
          }
        )
      })

      warn_table <- as.data.frame(targets::tar_meta(fields = warnings, complete_only = TRUE))
      if( nrow(warn_table) ) {
        for(ii in seq_len(nrow(warn_table))) {
          msg <- sprintf("Caveat in target [%s]: %s", warn_table$name[[ii]], warn_table$warnings[[ii]])
          warning(msg, call. = FALSE)
        }
      }
      Sys.unsetenv("RAVE_PIPELINE_ACTIVE")
      return()
    }

    if("none" == .(scheduler)){
      if(.(debug)) {
        message("pipeline_run: using targets::tar_make")
      }
      make( targets::tar_make )
    } else if("future" == .(scheduler)){
      args$workers <- ns$raveio_getopt("max_worker", default = 1L)
      if(.(debug)) {
        message("pipeline_run: using targets::tar_make_future")
      }
      make( targets::tar_make_future )
      # local({ do.call(targets::tar_make_future, args) })
    } else {
      if(is.na(clustermq_scheduler)) {
        clustermq_scheduler <- "multiprocess"
      }
      old_opt <- options('clustermq.scheduler' = clustermq_scheduler)
      on.exit({ options(old_opt) }, add = TRUE)

      if(.(debug)) {
        message("pipeline_run: using targets::tar_make_clustermq with scheduler [", clustermq_scheduler, "]")
      }

      if(identical(clustermq_scheduler, "LOCAL")){
        make( targets::tar_make_clustermq )
        # local({ do.call(targets::tar_make_clustermq, args) })
      } else {
        args$workers <- ns$raveio_getopt("max_worker", default = 1L)
        # do.call(targets::tar_make_clustermq, args)
        make( targets::tar_make_clustermq, use_local = FALSE )
      }

    }

  })


  res <- PipelineResult$new(path = pipe_dir, verbose = TRUE)
  res$check_interval <- check_interval
  res$names <- names

  if(!progress_quiet && is.na(progress_max)){
    if(length(names)){
      progress_max <- length(names)
    } else {
      progress_max <- length(pipeline_target_names(pipe_dir = pipe_dir))
    }
  }
  res$progressor <- rave_progress(
    progress_title, max = progress_max, shiny_auto_close = !async,
    quiet = progress_quiet
  )

  if(async){


    res$run(
      async = TRUE,
      expr = {
        callr::r_bg(func = fun, args = list(subprocess = TRUE),
                    package = FALSE, poll_connection = TRUE,
                    supervise = TRUE, error = "error")
      }
    )
    res
  } else {
    res$run(
      async = FALSE,
      expr = {
        fun(subprocess = FALSE)
      }
    )
  }

  res
}

# Handles errors generated by package `targets`
sanitize_target_error <- function(e) {
  if(inherits(e, "tar_condition_run")) {
    # remove ANSI code
    msg <- trimws(cli::ansi_strip(e$message), which = "left")

    if(startsWith(msg, "Error running targets::tar_make")) {
      msg <- gsub("^Error running targets::tar_make.*help\\.html[\n \t]{0,}Last (error|error message):[\n \t]{0,1}", "", msg)
      msg <- gsub("Last error traceback:.*$", "", msg)
      e$message <- trimws(msg)
    } else {
      tar_warn <- Sys.getenv("TAR_WARN", unset = "N/A")
      Sys.setenv("TAR_WARN" = "false")
      if(!identical(tar_warn, "N/A")) {
        on.exit({
          Sys.setenv("TAR_WARN" = tar_warn)
        })
      }
      tryCatch({
        err_table <- targets::tar_meta(fields = "error", complete_only = TRUE)
        if(length(err_table$error)) {
          e$message <- sprintf("Possible issue: %s", paste(err_table$error, collapse = "; "))
        }
      }, error = function(...){})

    }
  }
  e
}

#' @rdname rave-pipeline
#' @export
pipeline_clean <- function(
    pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
    destroy = c("all", "cloud", "local", "meta", "process", "preferences",
                "progress", "objects", "scratch", "workspaces"),
    ask = FALSE
) {
  destroy <- match.arg(destroy)
  pipe_dir <- activate_pipeline(pipe_dir)
  if( destroy != "preferences" ) {
    targets::tar_destroy(ask = ask, destroy = destroy)
  }
  if( destroy %in% c("all", "preferences", "local") ) {
    pref_path <- file.path(pipe_dir, "preferences")
    if(dir.exists(pref_path)) {
      unlink(pref_path, recursive = TRUE, force = TRUE)
    }
  }
  invisible()
}

#' @rdname rave-pipeline
#' @export
pipeline_run_bare <- function(
    pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
    scheduler = c("none", "future", "clustermq"),
    type = c("smart", "callr", "vanilla"),
    envir = new.env(parent = globalenv()),
    callr_function = NULL,
    names = NULL, return_values = TRUE, debug = FALSE,
    ...) {
  pipe_dir <- activate_pipeline(pipe_dir)

  type <- match.arg(type)
  scheduler <- match.arg(scheduler)
  callr_function <- substitute(callr_function)


  clustermq_scheduler <- getOption('clustermq.scheduler', NA)
  if(scheduler == "clustermq"){
    # if(!identical(clustermq_scheduler, "LOCAL")){
    #   callr_function <- NULL
    # }
    callr_function <- NULL
  }

  if(type == "vanilla"){
    callr_function <- NULL
  } else if (type == "callr") {
    callr_function <- quote(callr::r)
  }
  if(debug) {
    message("pipeline_run_bare: running with scheduler [", scheduler, "] and type [", type, "].")
  }

  all_names <- pipeline_target_names(pipe_dir = pipe_dir)

  if(length(names)) {
    if(!is.character(names)){
      stop("pipeline_run_bare: `names` must be NULL or characters")
    }
    missing_names <- names[!names %in% all_names]
    if(length(missing_names)) {
      stop("pipeline_run_bare: the following `names` cannot be found: ", paste(missing_names, collapse = ", "))
    }
  } else {
    names <- NULL
  }

  if(type == "smart") {
    local <- with_future_parallel
  }

  if(debug) {
    message("pipeline_run_bare: targeting names: ", deparse1(names), "...")
  }

  args <- list(
    names = names,
    envir = envir,
    callr_function = callr_function,
    ...
  )

  # Load shared functions into envir
  args$envir$pipeline <- pipeline_from_path(pipe_dir)
  shared_libs <- list.files(file.path(pipe_dir, "R"), pattern = "^shared-.*\\.R",
                            full.names = TRUE, ignore.case = TRUE)
  lapply(sort(shared_libs), function(f) {
    if(debug) {
      message("pipeline_run_bare: loading script: ", f)
    }
    source(file = f, local = args$envir, chdir = TRUE)
    return()
  })

  # Python modules loaded from here will be null pointers in targets
  # if(dir.exists(file.path(pipe_dir, "py"))) {
  #   pipeline_py_module(pipe_dir = pipe_dir,
  #                      convert = FALSE)
  # }

  make <- function(fun, use_local = TRUE) {
    suppressWarnings({

      Sys.setenv("RAVE_PIPELINE_ACTIVE" = "true")
      on.exit({ Sys.unsetenv("RAVE_PIPELINE_ACTIVE") }, add = TRUE, after = FALSE)

      tryCatch(
        expr = {

          if(isTRUE(utils::compareVersion(as.character(utils::packageVersion("targets")), "1.11.1") >= 0)) {
            targets::tar_config_set(reporter_make = "terse", reporter_outdated = "terse")
          } else {
            targets::tar_config_unset(c("reporter_make", "reporter_outdated"))
          }

          targets::tar_destroy("process", ask = FALSE)
          if( use_local ) {
            local({ do.call(fun, args) })
          } else {
            do.call(fun, args)
          }
        },
        `tar_condition_file` = function(e) {
          # destroy and try again, and throw all other errors
          targets::tar_destroy(ask = FALSE, destroy = "meta")
          if( use_local ) {
            local({ do.call(fun, args) })
          } else {
            do.call(fun, args)
          }
        },
        error = function( e ) {
          Sys.unsetenv("RAVE_PIPELINE_ACTIVE")
          # if(debug) {
          #   g <- globalenv()
          #   g$.last_rave_pipeline_error <- e
          #   browser(condition = e)
          #   message("pipeline_run_bare: error instance has been saved to  `.last_rave_pipeline_error`")
          # }
          stop(sanitize_target_error(e))
        }
      )
    })
    warn_table <- as.data.frame(targets::tar_meta(fields = warnings, complete_only = TRUE))
    if( nrow(warn_table) ) {
      for(ii in seq_len(nrow(warn_table))) {
        msg <- sprintf("Caveat in target [%s]: %s", warn_table$name[[ii]], warn_table$warnings[[ii]])
        warning(msg, call. = FALSE)
      }
    }
    Sys.unsetenv("RAVE_PIPELINE_ACTIVE")
    return()
  }

  switch (
    scheduler,
    "none" = {
      if(debug) {
        message("pipeline_run_bare: using targets::tar_make")
      }
      make( targets::tar_make )
    },
    "future" = {
      args$workers <- raveio_getopt("max_worker", default = 1L)
      if(debug) {
        message("pipeline_run_bare: running targets in parallel")
      }
      make( targets::tar_make_future )
    },
    {
      if(is.na(clustermq_scheduler)) {
        clustermq_scheduler <- "multiprocess"
      }
      old_opt <- options('clustermq.scheduler' = clustermq_scheduler)
      on.exit({ options(old_opt) }, add = TRUE)

      if(debug) {
        message("pipeline_run_bare: running targets with clustermq scheduler: ", clustermq_scheduler)
      }

      if(identical(clustermq_scheduler, "LOCAL")){
        make( targets::tar_make_clustermq )
        # local({ do.call(targets::tar_make_clustermq, args) })
      } else {
        args$workers <- raveio_getopt("max_worker", default = 1L)
        # do.call(targets::tar_make_clustermq, args)
        make( targets::tar_make_clustermq, use_local = FALSE )
      }
    }
  )

  # Read in names
  if(!length(names)) {
    names <- pipeline_target_names(pipe_dir = pipe_dir)
  }

  if(debug) {
    message("pipeline_run_bare: done targets: ", deparse1(names))
  }

  if( return_values ) {
    if(debug) {
      message("pipeline_run_bare: reading back targets...")
    }
    return(pipeline_read(var_names = names, pipe_dir = pipe_dir))
  } else {
    return(invisible())
  }


}
