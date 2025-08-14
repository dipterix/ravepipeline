on_rave_daemon <- function(type = c("worker", "pipeline")) {
  type <- match.arg(type)
  envvar <- sprintf("RAVE_DAEMON_%s", toupper(type))
  if(identical(Sys.getenv(envvar, ""), "true")) { return(TRUE) }
  if(!identical(Sys.getenv("RAVE_WITH_PARALLEL"), "true")) { return(TRUE) }
  return(FALSE)
}

#' @noRd
#' @returns Greater than 0 if async workers are desired, or 0 if no
#' async worker is allowed
calculate_workers <- function(workers = 0, always = FALSE) {
  if(on_rave_daemon()) { return(0L) }
  workers <- as.integer(workers)
  if(identical(workers, 1L) && !always) { return(0L) }

  max_workers <- raveio_getopt(key = "max_worker", default = 1L)
  max_workers2 <- getOption("rave.parallel.workers", max_workers)
  if(length(max_workers2) == 1 && !is.na(max_workers2) && is.numeric(max_workers2) &&
     max_workers2 < max_workers && max_workers2 > 0) {
    max_workers <- max_workers2
  }
  if(!isTRUE(workers >= 1 && workers <= max_workers)) {
    workers <- max_workers
  }

  if(workers > 1 || always) {
    return(workers)
  }
  return(0L)
}

initialize_rave_daemon <- function(type = c("worker", "pipeline")) {

  type <- match.arg(type)
  .envvar <- structure(
    list("true"),
    names = c(
      sprintf("RAVE_DAEMON_%s", toupper(type))
    )
  )
  do.call(Sys.setenv, .envvar)
  return()
}


#' @name with_rave_parallel
#' @title Internal parallel functions
#' @description
#' Experimental parallel functions, intended for internal use now. The goal
#' is to allow 'RAVE' functions to gain the potential benefit from parallel
#' computing, but allow users to control whether to do it.
#' @param expr expression to evaluate with parallel workers
#' @param workers number of workers
#' @param x a list, vector, array of R objects
#' @param fun function to apply to each element of \code{x}
#' @param ... additional arguments to be passed to \code{fun}
#' @param callback callback function, input is each element of \code{x} and
#' should return a string, for progress bar
#' @param .workers number of workers: note the actual numbers may differ,
#' depending on the options and number of input elements
#' @param .globals global variables to be serialized
#' @param .always whether always use workers, only considered when number of
#' workers is one; default is false, then run jobs in the main process when
#' only one worker is required
#'
#' @details
#' By default, \code{lapply_jobs} is almost identical to \code{\link{lapply}}.
#' It only runs in parallel when running inside of \code{with_rave_parallel}.
#'
#' The hard max-limit number of workers are determined by the 'RAVE' option
#' \code{raveio_getopt('max_worker')}. Users can lower this number for
#' memory-intensive tasks manually, via argument \code{.workers}.
#' The actual number of workers might be less than the requested ones, this
#' is often a result of sort input \code{x}. If the number of input iterations
#' has fewer than the max worker size, then the number of workers automatically
#' shrinks to the length of input list. All workers will be a child process
#' running separate from the main session, except for when only one worker
#' is needed and \code{.always=FALSE}: the only task will be executed in the
#' main session.
#'
#' Each worker session will run a completely isolated new process. There is
#' a ramp-up serialization that is needed for global objects (objects that
#' are defined elsewhere or outside of the function). Please make sure
#' the global objects are specified explicitly in \code{.globals}, a named list.
#' Unlike \code{future} package, users must specify the global objects.
#'
#' The global objects might be large to serialize. Please optimize the code
#' to avoid serializing big objects, especially environments or functions.
#' All objects inheriting \code{\link{RAVESerializable}} class with
#' \code{@marshal} and \code{@unmarshal} methods implemented correctly will
#' be serialized with reference hook \code{rave_serialize_refhook}, making
#' them extremely efficient.
#'
#'
#' @examples
#'
#'
#' # Run without `with_rave_parallel`
#' res <- lapply_jobs(1:5, function(x, ...) {
#'   c(child = Sys.getpid(), ...)
#' }, main = Sys.getpid())
#'
#' simplify2array(res)
#'
#' # Comparison
#' f <- function(n = 5, workers = 0) {
#'   system.time({
#'     ravepipeline::lapply_jobs(seq_len(n), function(x, ...) {
#'       Sys.sleep(1)
#'       c(child = Sys.getpid(), ...)
#'     }, main = Sys.getpid(), .workers = workers, callback = I)
#'   })
#' }
#'
#' \dontrun{
#'
#' # Without parallel
#' f()
#' #>    user  system elapsed
#' #>   0.022   0.019   5.010
#'
#'
#' # with parallel
#' with_rave_parallel({
#'   f()
#' })
#' #>    user  system elapsed
#' #>   0.729   0.190   1.460
#'
#' }
#'
#' @export
with_rave_parallel <- function(expr, .workers = 0) {
  if(!identical(Sys.getenv("RAVE_WITH_PARALLEL"), "true")) {
    workers <- as.integer(.workers)
    stopifnot(isTRUE(workers >= 0))

    Sys.setenv("RAVE_WITH_PARALLEL" = "true")
    if(workers > 0) {
      options("rave.parallel.workers" = workers)
    }

    on.exit({
      Sys.unsetenv("RAVE_WITH_PARALLEL")
      options("rave.parallel.workers" = NULL)
    }, add = TRUE, after = FALSE)
  }

  force(expr)
}

#' @rdname with_rave_parallel
#' @export
lapply_jobs <- function(x, fun, ..., .globals = list(), .workers = 0, .always = FALSE, callback = NULL) {

  if (length(x) == 0) return(vector("list", 0))

  workers <- 0L
  if(isFALSE(on_rave_daemon("worker"))) {
    workers <- calculate_workers(.workers, .always)
    if(workers > length(x)) { workers <- length(x) }
    if(workers == 1 && !.always) {
      workers <- 0L
    }
  }
  on_worker <- workers == 0

  has_callback <- is.function(callback)

  runtime_env <- environment()

  store <- fastmap2()
  store$finished <- 0
  store$errored <- FALSE
  n_total <- length(x)

  store$last_percentage <- 1
  step_size <- 1000 / (n_total + workers)

  default_progress_title <- ifelse(on_worker, "Running jobs", "Parallel jobs")
  progress <- rave_progress(
    title = default_progress_title,
    max = 1000,
    shiny_auto_close = FALSE,
    quiet = !has_callback
  )

  on.exit({
    progress$close()
  }, add = TRUE, after = TRUE)


  if(has_callback) {
    callback_impl <- function(x) {
      store$finished <- store$finished + step_size
      amount <- max(floor(store$finished) - store$last_percentage, 0)
      store$last_percentage <- store$last_percentage + amount

      msg <- tryCatch({
        paste(callback(x), collapse = "")
      }, error = function(e) {
        ""
      })

      msg <- paste(msg, collapse = "")
      msg <- strsplit(msg, "|", fixed = TRUE)[[1]]
      if(length(msg) > 1) {
        message <- msg[[1]]
        detail <- paste(msg[-1], collapse = "|")
      } else if(length(msg) == 1){
        message <- NULL
        detail <- msg[[1]]
      } else {
        message <- NULL
        detail <- ""
      }
      progress$inc(detail = trimws(detail), message = message, amount = amount)
    }
  } else {
    callback_impl <- function(x) {
      store$finished <- store$finished + step_size
    }
  }

  progress$inc("Initializing work...", message = default_progress_title)

  if( on_worker ) {

    # Simulate when the process is running
    runtime_env <- new.env(parent = parent.env(globalenv()))
    list2env(as.list(.globals), runtime_env)
    environment(fun) <- new.env(parent = runtime_env)

    results <- lapply(X = x, FUN = function(.x, ...) {
      re <- fun(.x, ...)
      callback_impl(.x)
      re
    }, ...)

    if(store$last_percentage < 1000) {
      progress$inc("Collecting work...", amount = 1000 - store$last_percentage)
    }
  } else {

    progress$inc("Preparing...", message = default_progress_title)

    job_ids <- fastmap2()
    job_results <- fastmap2()
    job_finished <- fastqueue2()

    # construct stuff that need to be serialized
    serialize_packet <- list(
      fun = list(
        formal = formals(fun),
        body = utils::removeSource(body(fun))
      ),
      args = list(...),
      globals = as.list(.globals),
      uuid = uuid::UUIDgenerate(use.time = TRUE, output = "string")
    )
    packet_digest <- digest(serialize_packet)
    cache_path <- file.path(
      R_user_dir("ravepipeline", "cache"),
      "job_serialization",
      sprintf("lapply_jobs-%s.rds", packet_digest)
    )

    while(file.exists(cache_path)) {
      packet_digest <- digest(list(packet_digest, Sys.time()))
      cache_path <- file.path(
        R_user_dir("ravepipeline", "cache"),
        "job_serialization",
        sprintf("lapply_jobs-%s.rds", packet_digest)
      )
    }

    dir_create2(dirname(cache_path))
    saveRDS(serialize_packet, cache_path, refhook = rave_serialize_refhook)

    on.exit({
      unlink(cache_path, force = TRUE)
      # clear up jobs, kill processes and remove resources
      job_ids_c <- unlist(c(job_finished$as_list(), as.list(job_ids)))
      lapply(job_ids_c, function(job_id) {
        try({
          remove_job(job_id)
        }, silent = TRUE)
      })
      try({
        job_finished$reset()
        job_ids$`@reset`()
      })
    }, add = TRUE, after = TRUE)
    cache_path <- normalizePath(cache_path, winslash = "/", mustWork = TRUE)

    job_fun <- new_function2(
      args = alist(.x = NULL),
      body = bquote({
        ravepipeline <- asNamespace("ravepipeline")
        ravepipeline$initialize_rave_daemon("worker")
        serialize_packet <- readRDS(.(cache_path), refhook = ravepipeline$rave_unserialize_refhook)

        fun <- function() {}
        formals(fun) <- serialize_packet$fun$formal
        body(fun) <- serialize_packet$fun$body

        runtime_env <- new.env(parent = globalenv())
        list2env(serialize_packet$globals, envir = runtime_env)
        environment(fun) <- new.env(parent = runtime_env)

        do.call(fun, c(list(.x), serialize_packet$args))
      }),
      quote_type = "quote",
      env = new.env(parent = parent.env(globalenv()))
    )
    cwd <- getwd()

    store$last_percentage <- 2


    wait_for_job <- function() {
      if(length(job_ids) < workers) { return(TRUE) }
      nms <- names(job_ids)
      for(nm in nms) {
        job_id <- job_ids[[nm]]
        status <- check_job(job_id)
        if(status$status < 0 || status$status >= 3) {
          res <- resolve_job(job_id, auto_remove = TRUE, must_init = FALSE)
          job_ids$`@remove`(nm)
          job_results[[nm]] <- res
          job_finished$add(job_id)
          return(TRUE)
        }
      }
      return(FALSE)
    }

    start_time <- Sys.time()

    lapply(seq_along(x), function(ii) {

      while(!wait_for_job()) {
        elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
        if(elapsed < 5) {
          Sys.sleep(0.01)
        } else if( elapsed < 60 ) {
          Sys.sleep(0.1)
        } else {
          Sys.sleep(0.5)
        }
      }

      .x <- x[[ii]]
      job_id <- start_job(
        job_fun,
        fun_args = list(.x = .x),
        workdir = cwd,
        method = "callr",
        ensure_init = FALSE,
        digest_key = ii
      )
      job_ids[[as.character(ii)]] <- job_id
      callback_impl(.x)
      return()
    })

    while(length(job_ids) > 0) {
      rest_size <- length(job_ids)
      store$finished <- store$finished + step_size
      amount <- max(floor(store$finished) - store$last_percentage, 0)
      # print(c(store$last_percentage, amount, store$finished))
      store$last_percentage <- store$last_percentage + amount
      progress$inc(detail = sprintf("Collecting work... (%d to go)", rest_size), amount = amount)

      nm <- names(job_ids)[[1]]
      job_id <- job_ids[[nm]]
      job_results[[nm]] <- resolve_job(job_id, auto_remove = FALSE, must_init = FALSE)
      job_ids$`@remove`(nm)
      job_finished$add(job_id)
    }

    results <- structure(
      lapply(seq_along(x), function(ii) {
        nm <- as.character(ii)
        re <- job_results[[nm]]
        job_results$`@remove`(nm)
        re
      }),
      names = names(x)
    )

    if(store$last_percentage < 1000) {
      progress$inc("Collecting work...", amount = 1000 - store$last_percentage)
    }

    results
  }


  return(results)
}
