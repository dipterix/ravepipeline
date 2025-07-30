

mirai_serialize_RAVESerializable <- function(x) {
  charToRaw(rave_serialize_refhook(x))
}

mirai_unserialize_RAVESerializable <- function(x) {
  rave_unserialize_refhook(rawToChar(x))
}

mirai_serialization_config <- function(config = list()) {

  nclasses <- length(config$class)
  stopifnot2(
    length(config$sfunc) == nclasses &&
      length(config$ufunc) == nclasses,
    msg = "Serialization configuration must be a list with names `class` (character vector), corresponding `sfunc` (list of serialization functions), and `ufunc` (list of unserialization functions)"
  )

  class <- config$class
  if(!"RAVESerializable" %in% config$class) {
    config$class <- c(config$class, "RAVESerializable")

    config$sfunc <- as.list(config$sfunc)
    config$sfunc[[length(config$sfunc) + 1]] <- mirai_serialize_RAVESerializable

    config$ufunc <- as.list(config$ufunc)
    config$ufunc[[length(config$ufunc) + 1]] <- mirai_unserialize_RAVESerializable

  }

  mirai::serial_config(
    class = config$class,
    sfunc = config$sfunc,
    ufunc = config$ufunc
  )
}

on_rave_daemon <- function(type = c("worker", "pipeline")) {
  type <- match.arg(type)
  envvar <- sprintf("RAVE_DAEMON_%s", toupper(type))
  if(identical(Sys.getenv(envvar, ""), "true")) { return(TRUE) }
  return(FALSE)
}

initialize_rave_daemon <- function(type = c("worker", "pipeline"), .globals = list()) {

  type <- match.arg(type)
  .envvar <- sprintf("RAVE_DAEMON_%s", toupper(type))
  .compute <- sprintf("rave_parallel_%s", type)

  mirai::everywhere({
    do.call(Sys.setenv, structure(names = .envvar, list('true')))
    if(length(.globals)) {
      list2env(as.list(.globals), envir = globalenv())
    }
  }, .compute = .compute, .args = list(.envvar = .envvar, .globals = .globals))

  return()
}

stop_mirai_with_trace <- function(error) {
  if(!mirai::is_mirai_error(error)) {
    stop(error)
  }
  traces <- error$stack.trace
  if(length(traces)) {
    traces <- utils::capture.output({
      traceback(traces)
    })
    on.exit({
      message(paste(c("\nTraceback:", traces), collapse = "\n"))
    })
  }
  cond <- simpleError(paste(error$message, collapse = "\n"), call = error$call)
  class(cond) <- c("RAVEAsyncWorker", class(cond))
  stop(cond)
}

#' @title Internal parallel functions
#' @description
#' Experimental parallel functions, intended for internal use now. The goal
#' is to allow 'RAVE' functions to gain the potential benefit from parallel
#' computing, but allow users to control whether to do it.
#' @param expr expression to evaluate with parallel workers
#' @param workers number of workers
#' @param globals,.globals global variables to be serialized
#' @param serialization_config serialization configurations
#' @param x a list, vector, array of R objects
#' @param fun function to apply to each elemnent of \code{x}
#' @param ... additional arguments to be passed to \code{fun}
#' @param callback callback function, input is each element of \code{x} and
#' should return a string, for progress bar
#'
#' @examples
#'
#'
#'
#' # Run without `with_mirai_parallel`
#' res <- lapply_jobs(1:5, function(x, ...) {
#'   c(child = Sys.getpid(), ...)
#' }, main = Sys.getpid())
#'
#' simplify2array(res)
#'
#' # When wrapped in `with_mirai_parallel`
#' with_mirai_parallel({
#'   res <- lapply_jobs(1:5, function(x, ...) {
#'     c(child = Sys.getpid(), ...)
#'   }, main = Sys.getpid())
#'   simplify2array(res)
#' }, workers = 1)
#'
#' # Comparison
#' f <- function() {
#'   system.time({
#'     lapply_jobs(1:5, function(x, ...) {
#'       Sys.sleep(1)
#'       c(child = Sys.getpid(), ...)
#'     }, main = Sys.getpid())
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
#' # with parallel
#' with_mirai_parallel(f(), worker = 5)
#' #>    user  system elapsed
#' #>   0.002   0.001   1.146
#'
#' }
#'
#'
#' @export
with_mirai_parallel <- function(expr, workers = 0, globals = list(),
                                serialization_config = list()) {

  if( !package_installed("mirai") || on_rave_daemon("worker") ) {
    re <- force(expr)
  } else {
    max_workers <- raveio_getopt(key = "max_worker", default = 1L)
    workers <- as.integer(workers)
    if(!isTRUE(workers >= 1 && workers <= max_workers)) {
      workers <- max_workers
    }

    # Make sure the daemons are reset
    mirai::daemons(0, .compute = "rave_parallel_worker")
    on.exit({ mirai::daemons(0, .compute = "rave_parallel_worker") }, add = TRUE, after = FALSE)

    re <- with(
      mirai::daemons(
        n = workers,
        serial = mirai_serialization_config(serialization_config),
        dispatcher = TRUE,
        autoexit = TRUE,
        cleanup = FALSE,
        output = TRUE,
        .compute = "rave_parallel_worker"
      ),
      {
        initialize_rave_daemon(type = "worker", .globals = globals)
        force(expr)
      }
    )
  }
  re
}

#' @rdname with_mirai_parallel
#' @export
lapply_jobs <- function(x, fun, ..., .globals = list(), callback = NULL) {

  has_callback <- is.function(callback)

  runtime_env <- environment()

  store <- fastmap2()
  store$finished <- 0
  store$errored <- FALSE
  n_total <- length(x)

  store$last_percentage <- 1
  step_size <- 1000 / n_total

  progress <- rave_progress(
    title = "Parallel jobs",
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
      } else {
        message <- NULL
        detail <- msg[[1]]
      }
      progress$inc(detail = trimws(detail), message = message, amount = amount)
    }
  } else {
    callback_impl <- function(x) {
      store$finished <- store$finished + step_size
    }
  }

  progress$inc("Initializing work...", message = "Parallel jobs")

  if( !package_installed("mirai") || on_rave_daemon("worker") ||
      !mirai::daemons_set(.compute = "rave_parallel_worker") ) {

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

    mirai_args <- c(
      list(
        .x = x,
        .f = fun,
        .args = list(...),
        .compute = "rave_parallel_worker",
        .promise = NULL
      ),
      as.list(.globals)
    )

    map <- do.call(mirai::mirai_map, mirai_args)

    progress$inc("Running...", message = "Parallel jobs")
    store$last_percentage <- 2

    results <- lapply(seq_len(n_total), function(ii) {
      if(store$errored) { return() }
      m <- mirai::call_mirai(map[[ii]])

      if(
        mirai::is_mirai_error(m$data) ||
        mirai::is_mirai_interrupt(m$data) ||
        mirai::is_error_value(m$data)
      ) {
        store$errored <- TRUE
        stop_mirai_with_trace(m$data)
      }
      callback_impl(x[[ii]])
      return()
    })

    if(store$last_percentage < 1000) {
      progress$inc("Collecting work...", amount = 1000 - store$last_percentage)
    }

    results <- map[]
  }


  return(results)
}
