new_job_id <- function() {
  uuid::UUIDgenerate(use.time = TRUE, output = "string")
}

get_job_path <- function(job_id, check = TRUE) {
  path <- file.path(cache_root(), "ravepipeline-background-jobs", job_id)
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

prepare_job <- function(fun, fun_args = list(), packages = NULL, workdir = NULL) {
  environment(fun) <- new.env(parent = globalenv())

  job_id <- new_job_id()
  job_root <- get_job_path(job_id = job_id, check = TRUE)

  shared_objects <- list(
    path = job_root,
    workdir = workdir,
    id = job_id,
    fun = fun,
    args = as.list(fun_args),
    packages = unique(c(packages, "ravepipeline"))
  )

  saveRDS(object = shared_objects, file = file.path(job_root, "globals.rds"))

  # Create

  # Generate script for RStudio
  script <- bquote(local({

    job_root <- .(job_root)
    job_status <- list(
      # 0: ready
      # 1: started
      # 2: running
      # 3: done
      # -1: error
      # -2: missing
      status = 1,
      current_time = Sys.time(),
      start_time = Sys.time()
    )
    # Write initial state
    saveRDS(job_status, file = file.path(job_root, "status.rds"))

    tryCatch(
      {
        shared_objects <- readRDS(file.path(job_root, "globals.rds"))

        job_id <- shared_objects$job_id
        fun <- shared_objects$fun
        args <- shared_objects$args
        packages <- shared_objects$packages
        workdir <- shared_objects$workdir

        for(pkg in packages) {
          do.call("loadNamespace", list(package = pkg))
        }
        environment(fun) <- new.env(parent = globalenv())

        job_status$current_time <- Sys.time()
        job_status$status <- 2
        saveRDS(job_status, file = file.path(job_root, "status.rds"))

        cwd <- getwd()
        if(length(workdir) == 1 && is.character(workdir) && !is.na(workdir)) {
          message("Working directory: ", workdir)
          setwd(workdir)
          on.exit({
            if(length(cwd) == 1 && dir.exists(cwd)) {
              setwd(cwd)
            }
          })
        }

        result <- do.call(fun, args)
        saveRDS(result, file = file.path(job_root, "results.rds"))

        job_status$current_time <- Sys.time()
        job_status$status <- 3
        saveRDS(job_status, file = file.path(job_root, "status.rds"))

        if(length(cwd) != 1 || !dir.exists(cwd)) {
          cwd <- tempdir()
        }

      },
      error = function(e) {
        job_status$current_time <- Sys.time()
        job_status$status <- -1
        job_status$error <- e
        saveRDS(job_status, file = file.path(job_root, "status.rds"))
      }
    )

    NULL
  }))

  writeLines(
    text = format(script),
    con = file.path(job_root, "script.R")
  )

  # Write initial state
  saveRDS(list(
    status = 0,
    current_time = Sys.time(),
    start_time = Sys.time()
  ), file = file.path(job_root, "status.rds"))

  return(job_id)
}

get_job_status <- function(job_id) {
  job_root <- get_job_path(job_id = job_id, check = FALSE)
  status_path <- file.path(job_root, "status.rds")

  status <- list(
    status = -2,
    error = simpleError(sprintf("Job [%s] has not not prepared yet.", job_id))
  )
  if(file.exists(status_path)) {
    tryCatch({
      status <- readRDS(status_path)
    }, error = function(e) {
    })
  }

  status$ID <- job_id

  structure(status, class = "ravepipeline_job_status")
}

start_job_rs <- function(fun, fun_args = list(), packages = NULL, workdir = NULL, name = NULL) {
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

  job_id <- prepare_job(fun = fun, fun_args = fun_args, packages = packages, workdir = workdir)
  job_root <- get_job_path(job_id, check = FALSE)
  script_path <- file.path(job_root, "script.R")

  if(is.null(name)) {
    name <- sprintf("RAVE-JobID: %s", job_id)
  }
  rs_runScriptJob(path = script_path, name = name, workingDir = workdir)

  if (rs_avail(version_needed = "1.4", child_ok = TRUE, shiny_ok = TRUE)) {
    Sys.sleep(0.5)
    try({
      rstudioapi::executeCommand("activateConsole", quiet = TRUE)
    }, silent = TRUE)
  }
  return(structure(job_id, path = job_root))
}

start_job_callr <- function(fun, fun_args = list(), packages = NULL, workdir = NULL, ...) {

  job_id <- prepare_job(fun = fun, fun_args = fun_args, packages = packages, workdir = workdir)
  job_root <- get_job_path(job_id, check = FALSE)
  script_path <- file.path(job_root, "script.R")

  callr::r_bg(
    func = function(script_path) {
      source(file = script_path,
             echo = FALSE,
             print.eval = FALSE)
    },
    args = list(script_path = script_path),
    package = FALSE,
    poll_connection = FALSE,
    supervise = TRUE,
    error = "error"
  )

  return(structure(job_id, path = job_root))
}

#' @name rave-pipeline-jobs
#' @title Run a function (job) in another session
#' @param fun function to evaluate
#' @param fun_args list of function arguments
#' @param packages list of packages to load
#' @param workdir working directory; default is temporary path
#' @param method job type; choices are \code{'rs_job'} and \code{'callr'}
#' @param name name of the job
#' @param job_id job identification number
#' @param timeout timeout in seconds before the resolve ends; jobs that
#' are still running are subject to \code{unresolved} policy
#' @param unresolved what to do if the job is still running after timing-out;
#' default is \code{'warning'} and return \code{NULL}, other choices are
#' \code{'error'} or \code{'silent'}
#' @param auto_remove whether to automatically remove the job if resolved;
#' default it true
#' @returns For \code{start_job}, a string of job identification number;
#' \code{check_job} returns the job status; \code{resolve_job} returns
#' the function result.
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' fun <- function() {
#'   Sys.sleep(2)
#'   Sys.getpid()
#' }
#'
#' job_id <- start_job(fun)
#'
#' check_job(job_id)
#'
#' result <- resolve_job(job_id)
#'
#' }
#'
#'
#' @export
start_job <- function(
    fun,
    fun_args = list(),
    packages = NULL,
    workdir = NULL,
    method = c("rs_job", "callr"),
    name = NULL
) {

  method <- match.arg(method)
  if(method == "rs_job") {
    if(!rs_avail(child_ok = TRUE, shiny_ok = TRUE)) {
      method <- "callr"
    }
  }

  job_id <- switch (
    method,
    "rs_job" = {
      start_job_rs(
        fun = fun,
        fun_args = fun_args,
        packages = packages,
        workdir = workdir,
        name = name
      )
    }, {
      start_job_callr(
        fun = fun,
        fun_args = fun_args,
        packages = packages,
        workdir = workdir
      )
    }
  )

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
    job_id, timeout = Inf, auto_remove = TRUE,
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
  while(timeout >= 0) {
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
      stop("Job has not been started")
    } else if (status$status == 3) {
      # 3: done
      job_root <- get_job_path(job_id = job_id, check = FALSE)
      result_path <- file.path(job_root, "results.rds")
      if(!file.exists(result_path)) {
        stop(sprintf("Job [%s] is done but no result file is available...", job_id))
      }
      results <- readRDS(result_path)
      if(auto_remove) {
        remove_job(job_id)
      }
      return(results)
    }

    # Still running
    wait_time <- max(min(1, timeout), 0.1)
    Sys.sleep(wait_time)
    timeout <- timeout - wait_time
    status <- check_job(job_id)
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
}
