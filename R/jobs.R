new_job_id <- function(digest_key = NULL) {
  str <- digest(list(
    digest_key = digest_key,
    time = Sys.time(),
    pid = Sys.getpid(),
    uuid = uuid::UUIDgenerate(use.time = TRUE, output = "string")
  ))
  structure(str, class = "ravepipeline_jobid")
}

get_job_path <- function(job_id, check = TRUE) {
  path <- file.path(
    R_user_dir("ravepipeline", "cache"),
    "job_serialization",
    sprintf("jobID-%s.rds", job_id)
  )
  # path <- file.path(cache_root(), "ravepipeline-background-jobs", job_id)
  if(check) {
    path <- dir_create2(path)
  }
  path
}

clean_job_path <- function(job_id) {
  path <- get_job_path(job_id, check = FALSE)
  if(file.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
  }
  # dir_path <- dirname(path)
  return(invisible())
}

save_job_status <- function(status, path) {

  # If the job is deleted, then this function should not try to write to disk anymore
  # the process is stopped
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  tempfile(fileext = ".rds")
  dir <- dirname(path)
  tmp <- file.path(dir, sprintf("tmp-%s-%s.rds", basename(path), Sys.getpid()))
  saveRDS(status, tmp)

  ok <- FALSE
  for(retry in seq_len(5)) {
    suppressWarnings({ ok <- file.copy(tmp, path, overwrite = TRUE) })
    if(ok) {
      break
    }
    Sys.sleep(0.01)
  }
  unlink(tmp)
  if(!ok) {
    status$atomic_save <- FALSE
    # This
    saveRDS(status, path)
  }
  return()
}

prepare_job <- function(fun, fun_args = list(), packages = NULL, workdir = NULL,
                        digest_key = NULL, envvars = NULL) {

  job_id <- new_job_id(digest_key = digest_key)
  job_root <- get_job_path(job_id = job_id, check = TRUE)

  packages <- unique(c(packages, "ravepipeline"))
  shared_objects <- list(
    path = job_root,
    workdir = workdir,
    id = job_id,
    fun = list(
      formals = formals(fun),
      body = utils::removeSource(body(fun))
    ),
    args = as.list(fun_args),
    packages = packages
  )

  # Write initial state
  state_path <- file.path(job_root, "status.rds")

  prepare_time <- Sys.time()

  saveRDS(list(
    status = 0,
    # time at preparation
    prepare_time = prepare_time,

    # job init (before anything loaded)
    init_time = prepare_time,

    # loaded instructions
    loaded_instruction_time = prepare_time,

    # loaded globals
    loaded_globals_time = prepare_time,

    # job started (running)
    start_time = prepare_time,

    # value/error time
    current_time = prepare_time

  ), file = state_path)

  saveRDS(object = shared_objects, file = file.path(job_root, "globals.rds"), refhook = rave_serialize_refhook)

  # pass in current settings
  current_opts <- get(".settings", envir = asNamespace("ravepipeline"))
  current_opts <- as.list(current_opts)

  # Generate script for RStudio
  script <- bquote(local({

    preparation_ok <- FALSE
    job_status <- list(
      # 0: ready
      # 1: started
      # 2: running
      # 3: done
      # -1: error
      # -2: missing
      status = 1,
      prepare_time = .(prepare_time),
      init_time = Sys.time(),
      loaded_instruction_time = Sys.time(),
      loaded_globals_time = Sys.time(),
      start_time = Sys.time(),
      current_time = Sys.time()
    )

    tryCatch(
      {
        try(
          silent = TRUE,
          {
            envvars <- .(as.list(envvars))
            envvars$RAVE_JOB_SESSION <- "1"
            nms <- names(envvars)
            nms <- nms[!nms %in% ""]
            do.call(Sys.setenv, envvars[nms])
          }
        )

        opt <- options(rave.use_settings = .(current_opts))
        on.exit({ try({ options(opt) }, silent = TRUE) })

        packages <- .(packages)
        lapply(packages, function(pkg) {
          do.call("loadNamespace", list(package = pkg))
        })

        job_root <- .(job_root)
        status_path <- file.path(job_root, "status.rds")

        ravepipeline <- asNamespace("ravepipeline")
        save_job_status <- ravepipeline$save_job_status
        rave_unserialize_refhook <- ravepipeline$rave_unserialize_refhook
        rave_serialize_refhook <- ravepipeline$rave_serialize_refhook

        # Write initial state
        job_status$loaded_instruction_time <- Sys.time()

        save_job_status(job_status, status_path)

        preparation_ok <- TRUE
      },
      error = function(e) {
        preparation_ok <<- e
      }
    )

    tryCatch(
      {
        if(!isTRUE(preparation_ok)) {
          stop(preparation_ok)
        }
        # rave_unserialize_refhook <- ravepipeline$rave_unserialize_refhook
        # rave_serialize_refhook <- ravepipeline$rave_serialize_refhook

        globals_path <- file.path(job_root, "globals.rds")

        job_status$loaded_globals_time <- Sys.time()
        job_status$globals_size <- file.size(globals_path)

        shared_objects <- readRDS(globals_path, refhook = rave_unserialize_refhook)

        # job_id <- shared_objects$id

        fun <- function() {}
        formals(fun) <- shared_objects$fun$formals
        body(fun) <- shared_objects$fun$body
        environment(fun) <- new.env(parent = globalenv())

        args <- shared_objects$args
        packages <- shared_objects$packages
        workdir <- shared_objects$workdir

        for(pkg in packages) {
          do.call("loadNamespace", list(package = pkg))
        }


        job_status$start_time <- Sys.time()
        job_status$current_time <- Sys.time()
        job_status$status <- 2
        save_job_status(job_status, status_path)

        cwd <- getwd()
        if(length(workdir) == 1 && is.character(workdir) && !is.na(workdir)) {
          message("Working directory: ", workdir)
          setwd(workdir)
          on.exit({
            if(length(cwd) == 1 && dir.exists(cwd)) {
              try({ setwd(cwd) }, silent = TRUE)
            }
          })
        }

        result <- do.call(fun, args)
        result_path <- file.path(job_root, "results.rds")
        saveRDS(result, file = result_path, refhook = rave_serialize_refhook)

        job_status$current_time <- Sys.time()
        job_status$status <- 3
        job_status$result_size <- file.size(result_path)
        save_job_status(job_status, status_path)

        if(length(cwd) != 1 || !dir.exists(cwd)) {
          cwd <- tempdir()
        }

      },
      error = function(e) {
        job_status$current_time <- Sys.time()
        job_status$status <- -1
        job_status$error <- e
        save_job_status(job_status, status_path)
      }
    )

    NULL
  }))

  writeLines(
    text = format(script),
    con = file.path(job_root, "script.R")
  )

  stopifnot(file.exists(state_path))

  return(job_id)
}

get_job_status <- function(job_id) {
  job_root <- get_job_path(job_id = job_id, check = FALSE)
  status_path <- file.path(job_root, "status.rds")

  status <- list(
    status = -2,
    error = simpleError(sprintf("Job [%s] has not been prepared yet.", job_id))
  )

  if(file.exists(status_path)) {
    for(retry in seq_len(5)) {

      status_read <- tryCatch({
        readRDS(status_path)
      }, error = function(e) {
        NULL
      })

      if(!is.null(status_read)) {
        status <- status_read
        break
      }

      Sys.sleep(0.01)

    }
  }

  status$ID <- job_id

  structure(status, class = "ravepipeline_job_status")
}

start_job_rs <- function(fun, fun_args = list(), packages = NULL, workdir = NULL, name = NULL, digest_key = NULL, envvars = NULL) {
  if(isTRUE(package_installed("rstudioapi"))) {
    rstudioapi <- asNamespace("rstudioapi")

    rs_runScriptJob <- rstudioapi$jobRunScript
  } else {
    rs_runScriptJob <- get0(
      ".rs.api.runScriptJob",
      envir = parent.env(globalenv()),
      mode = "function",
      ifnotfound = NULL
    )
    if(!is.function(rs_runScriptJob) ||
       !identical(attr(parent.env(environment(rs_runScriptJob)), "name"), "tools:rstudio")) {
      stop("Unable to run RStudio/Positron job. Please make sure the IDE is RStudio/Workbench/Positron")
    }
  }

  job_id <- prepare_job(
    fun = fun,
    fun_args = fun_args,
    packages = packages,
    workdir = workdir,
    digest_key = digest_key,
    envvars = envvars
  )
  job_root <- get_job_path(job_id, check = FALSE)
  script_path <- file.path(job_root, "script.R")

  if(is.null(name)) {
    name <- sprintf("RAVE-JobID: %s", job_id)
  }
  rs_runScriptJob(path = script_path, name = name, workingDir = workdir)

  if (rs_avail(version_needed = "1.4", child_ok = FALSE, shiny_ok = TRUE)) {
    later::later(delay = 0.5, function() {
      try({
        rstudioapi::executeCommand("activateConsole", quiet = TRUE)
      }, silent = TRUE)
    })
  }
  return(structure(job_id, path = job_root))
}

start_job_callr <- function(fun, fun_args = list(), packages = NULL,
                            workdir = NULL, digest_key = NULL, envvars = NULL, ...) {

  job_id <- prepare_job(
    fun = fun,
    fun_args = fun_args,
    packages = packages,
    workdir = workdir,
    digest_key = digest_key,
    envvars = envvars
  )
  job_root <- get_job_path(job_id, check = FALSE)
  script_path <- file.path(job_root, "script.R")

  process <- callr::r_bg(
    func = function(script_path) {
      source(file = script_path,
             echo = FALSE,
             print.eval = FALSE)
    },
    args = list(
      # subprocess = TRUE,
      script_path = script_path
    ),
    package = FALSE,
    poll_connection = TRUE,

    # Do not supervise when during the checks as the opened supervisor
    # will trigger alerts
    supervise = !identical(Sys.getenv("RAVE_TESTING"), "TRUE"),
    error = "error"
  )

  return(structure(job_id, path = job_root, callr_process = process))
}

start_job_mirai <- function(fun, fun_args = list(), packages = NULL,
                            workdir = NULL, digest_key = NULL, envvars = NULL, ...) {

  mirai <- asNamespace('mirai')

  job_id <- prepare_job(
    fun = fun,
    fun_args = fun_args,
    packages = packages,
    workdir = workdir,
    digest_key = digest_key,
    envvars = envvars
  )
  job_root <- get_job_path(job_id, check = FALSE)
  script_path <- file.path(job_root, "script.R")

  m <- mirai$mirai(
    .expr = {
      source(file = script_path,
             echo = FALSE,
             print.eval = FALSE)
    },
    .timeout = NULL,
    .args = list(script_path = script_path)
  )

  return(structure(job_id, path = job_root, mirai_process = m))
}

#' @name rave-pipeline-jobs
#' @title Run a function (job) in another session
#' @param fun function to evaluate
#' @param fun_args list of function arguments
#' @param packages list of packages to load
#' @param workdir working directory; default is temporary path
#' @param method job type; choices are \code{'rs_job'} (only used in
#' \code{'RStudio'} environment), \code{'mirai'} (when package \code{'mirai'}
#' is installed), and \code{'callr'} (default).
#' @param name name of the job
#' @param job_id job identification number
#' @param timeout timeout in seconds before the resolve ends; jobs that
#' are still running are subject to \code{unresolved} policy
#' @param unresolved what to do if the job is still running after timing-out;
#' default is \code{'warning'} and return \code{NULL}, other choices are
#' \code{'error'} or \code{'silent'}
#' @param auto_remove whether to automatically remove the job if resolved;
#' default it true
#' @param ensure_init whether to make sure the job has been started; default
#' is true
#' @param digest_key a string that will affect how job ID is generated;
#' used internally
#' @param envvars additional environment variables to set; must be a named
#' list of environment variables
#' @param must_init whether the resolve should error out if the job is not
#' initialized: typically meaning the either the resolving occurs too soon
#' (only when \code{ensure_init=FALSE}) or the job files are corrupted;
#' default is true
#' @returns For \code{start_job}, a string of job identification number;
#' \code{check_job} returns the job status; \code{resolve_job} returns
#' the function result.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Basic use
#' job_id <- start_job(function() {
#'   Sys.sleep(1)
#'   Sys.getpid()
#' })
#'
#' check_job(job_id)
#'
#' result <- resolve_job(job_id)
#'
#'
#' # As promise
#' library(promises)
#' as.promise(
#'   start_job(function() {
#'     Sys.sleep(1)
#'     Sys.getpid()
#'   })
#' ) %...>%
#'   print()
#'
#' }
#'
#' @export
start_job <- function(
    fun,
    fun_args = list(),
    packages = NULL,
    workdir = NULL,
    method = c("callr", "rs_job", "mirai"),
    name = NULL,
    ensure_init = TRUE,
    digest_key = NULL,
    envvars = NULL
) {

  method <- match.arg(method)
  if(method == "rs_job") {
    if(!rs_avail(child_ok = TRUE, shiny_ok = TRUE)) {
      # not in rstudio
      method <- "callr"
    } else if(rs_avail(child_ok = FALSE, shiny_ok = TRUE)) {
      # in rstudio main session or positron
      if(!is.function(get0('.rs.api.runScriptJob', mode = "function", ifnotfound = NULL))) {
        # positron, no job available
        method <- "callr"
      }
    }
    # else it's in rstudio job
  }
  if(method == "mirai" && !package_installed("mirai")) {
    method <- "callr"
  }

  job_id <- switch (
    method,
    "rs_job" = {
      start_job_rs(
        fun = fun,
        fun_args = fun_args,
        packages = packages,
        workdir = workdir,
        name = name,
        digest_key = digest_key,
        envvars = envvars
      )
    },
    "mirai" = {
      start_job_mirai(
        fun = fun,
        fun_args = fun_args,
        packages = packages,
        workdir = workdir,
        digest_key = digest_key,
        envvars = envvars
      )
    },
    {
      start_job_callr(
        fun = fun,
        fun_args = fun_args,
        packages = packages,
        workdir = workdir,
        digest_key = digest_key,
        envvars = envvars
      )
    }
  )

  if( ensure_init ) {
    while({
      info <- as.list(check_job(job_id))
      isTRUE(info$status == 0)
    }) {
      Sys.sleep(0.01)
    }
  }


  job_id
}

#' @rdname rave-pipeline-jobs
#' @export
check_job <- function(job_id) {
  if(inherits(job_id, "ravepipeline_job_status")) {
    job_id <- job_id$ID
  }
  # job_id <- start_job(fun)
  status <- get_job_status(job_id)
  return(status)
}

#' @rdname rave-pipeline-jobs
#' @export
resolve_job <- function(
    job_id, timeout = Inf, auto_remove = TRUE, must_init = TRUE,
    unresolved = c("warning", "error", "silent")) {
  unresolved <- match.arg(unresolved)
  if(inherits(job_id, "ravepipeline_job_status")) {
    status <- job_id
    job_id <- status$ID
  } else {
    status <- check_job(job_id)
  }

  if(timeout < 0) {
    # Resolve or fail?
    timeout <- 0
  }
  start_time <- Sys.time()
  now <- start_time
  while(now <= (start_time + timeout)) {
    if(status$status < 0) {
      # -1: error
      # -2: missing
      error <- status$error
      if(auto_remove) {
        remove_job(job_id)
      }
      stop(error)
    } else if (status$status == 0) {
      # 0: ready
      if( must_init ) {
        stop("Job has not been started")
      }
    } else if (status$status == 3) {
      # 3: done
      job_root <- get_job_path(job_id = job_id, check = FALSE)
      result_path <- file.path(job_root, "results.rds")
      if(!file.exists(result_path)) {
        stop(sprintf("Job [%s] is done but no result file is available...", job_id))
      }
      results <- readRDS(result_path, refhook = rave_unserialize_refhook)
      if(auto_remove) {
        remove_job(job_id)
      }

      if(getOption("rave.job.profiling", FALSE)) {
        try(silent = TRUE, {
          message(sprintf(
            "Init=%.1f+load1=%.1f+load2=%.1f->overhead=%.1f, comp=%.1f, total=%.1f, globals=%.0f, res=%.0f",
            c(as.numeric(status$init_time     - status$prepare_time, units = "secs"), NA)[[1]],
            c(as.numeric(status$loaded_instruction_time - status$init_time, units = "secs"), NA)[[1]],
            c(as.numeric(status$loaded_globals_time - status$loaded_instruction_time, units = "secs"), NA)[[1]],
            c(as.numeric(status$start_time    - status$prepare_time, units = "secs"), NA)[[1]],
            c(as.numeric(status$current_time  - status$start_time, units = "secs"), NA)[[1]],
            c(as.numeric(Sys.time() - status$prepare_time, units = "secs"), NA)[[1]],
            c(status$globals_size, NA_real_)[[1]],
            c(status$result_size, NA_real_)[[1]]
          ))
        })
      }
      return(results)
    }

    elapsed <- as.numeric(now - status$start_time, units = "secs")
    time_left <- timeout - elapsed

    if(elapsed < 5) {
      wait_time <- min(time_left, 0.001)
    } else if(elapsed < 60) {
      wait_time <- min(time_left, 0.01)
    } else {
      wait_time <- min(time_left, 0.1)
    }

    Sys.sleep(max(wait_time, 0))
    status <- check_job(job_id)
    now <- Sys.time()
  }

  switch (
    unresolved,
    "warning" = {
      warning(sprintf("Job [%s] is still running", job_id))
    },
    "error" = {
      stop(sprintf("Job [%s] is still running", job_id))
    }, {

    }
  )

  return(invisible())
}

#' @rdname rave-pipeline-jobs
#' @export
remove_job <- function(job_id) {
  if(inherits(job_id, "ravepipeline_job_status")) {
    job_id <- job_id$ID
  }
  clean_job_path(job_id)
  callr_process <- attr(job_id, "callr_process")
  if(!is.null(callr_process)) {
    tryCatch(
      {
        callr_process$kill_tree()
      },
      error = function(e) {
        try({
          callr_process$kill()
        }, silent = TRUE)
      }
    )
  }
  invisible()
}

#' @export
print.ravepipeline_jobid <- function(x, ...) {
  cat(sprintf("RAVE job ID: %s\n", x))
}

#' @export
print.ravepipeline_job_status <- function(x, ...) {
  status_str <- switch (
    sprintf("%d", x$status),
    "0" = "initialized",
    "1" = "started",
    "2" = "running",
    "3" = "finished",
    "-1" = "errored",
    "-2" = "corrupted",
    "unknown"
  )
  cat(sprintf("RAVE job status:\n  ID: %s\n  Status: %d (%s)\n", x$ID, x$status, status_str))
}

#' @export
as.promise.ravepipeline_job_status <- function(x) {
  as.promise.ravepipeline_jobid(x$ID)
}

#' @export
as.promise.ravepipeline_jobid <- function(x) {

  job <- check_job(x)

  if( isTRUE(job$status %in% c(-1, 3)) ) {
    # resolved, either errored or finished
    promise <- promises::promise(function(resolve, reject) {
      tryCatch(
        {
          resolve(resolve_job(x))
        },
        error = function(e) {
          reject(e)
        }
      )
    })
  } else {
    promise <- promises::promise(function(resolve, reject) {

      poll_interval <- 0.1
      check <- function() {
        job <- check_job(x)
        if( isTRUE(job$status %in% c(-1, 3)) ) {
          tryCatch(
            {
              resolve(resolve_job(x))
            },
            error = function(e) {
              reject(e)
            }
          )
          return()
        }
        later::later(check, poll_interval)
      }

      check()

    })
  }

  promise
}
