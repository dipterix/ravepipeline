#' @title Class definition for 'RAVE' pipeline results
#' @export
PipelineResult <- R6::R6Class(
  classname = "PipelineResult",
  portable = TRUE,
  cloneable = TRUE,
  private = list(
    .path = character(0L),
    .state = character(0L),
    .process_type = character(0L),
    .process = NULL,
    .vartable = NULL,
    .invalidated = FALSE,
    .current_progress = NULL,
    finalize = function(...) {
      self$invalidate()
    },
    close_progressor = function() {
      try({
        if (length(self$progressor) && !self$progressor$is_closed()) {
          self$progressor$close()
          # self$progressor <- NULL
        }
      }, silent = !self$verbose)
    }
  ),
  public = list(

    #' @field progressor progress bar object, usually generated a progress
    #' instance
    progressor = NULL,

    #' @field promise a \code{\link[promises]{promise}} instance that monitors
    #' the pipeline progress
    promise = NULL,

    #' @field verbose whether to print warning messages
    verbose = FALSE,

    #' @field names names of the pipeline to build
    names = NULL,

    #' @field async_callback function callback to call in each check loop;
    #' only used when the pipeline is running in \code{async=TRUE} mode
    async_callback = NULL,

    #' @field check_interval used when \code{async=TRUE} in
    #' \code{\link{pipeline_run}}, interval in seconds to check the progress
    check_interval = 0.1,

    #' @description check if result is valid, raises errors when invalidated
    validate = function() {
      if (private$.invalidated) {
        stop("This result has been invalidated")
      }
      invisible()
    },

    #' @description invalidate the pipeline result
    invalidate = function() {
      private$.invalidated <- TRUE
      private$.state <- "invalidated"
      if (inherits(private$.process, "process")) {
        try({
          if (isTRUE(private$.process$is_alive())) {
            private$.process$kill()
          }
          private$.process <- NULL
        }, silent = !self$verbose)
      }
      private$close_progressor()
    },

    #' @description get pipeline progress
    get_progress = function() {
      self$validate()
      tbl <- pipeline_progress(pipe_dir = private$.path, method = "details")

      self$variables

      tbl <- merge(private$.vartable[, c("name", "description")], tbl, by = "name", all.x = TRUE, sort = FALSE)
      tbl$progress[is.na(tbl$progress)] <- "initialize"

      tbl_bk <- tbl
      on.exit({
        private$.vartable <- tbl_bk
      }, add = TRUE, after = FALSE)

      # tbl$progress[tbl$progress == "skipped"] <- "built"

      previous <- private$.vartable$progress %in% "started"
      # finished <- !tbl$progress %in% "initialize"
      started <- tbl$progress %in% "started"
      sel <- started & !previous
      if (any(sel)) {
        sel <- which(sel)
        sel <- sel[[length(sel)]]

        private$.current_progress <- sel

      } else {
        sel <- max(private$.current_progress, 1)
      }
      return(list(
        index = sel,
        name = tbl$name[[sel]],
        description = tbl$description[[sel]],
        progress = tbl$progress[[sel]]
      ))

    },

    #' @description constructor (internal)
    #' @param path pipeline path
    #' @param verbose whether to print warnings
    initialize = function(path = character(0L), verbose = FALSE) {
      private$.path <- path
      private$.current_progress <- 0
      private$.state <- "initialize"
      self$verbose <- isTRUE(as.logical(verbose))
    },

    #' @description run pipeline (internal)
    #' @param expr expression to evaluate
    #' @param env environment of \code{expr}
    #' @param quoted whether \code{expr} has been quoted
    #' @param async whether the process runs in other sessions
    #' @param process the process object inherits \code{\link[callr]{process}},
    #' will be inferred from \code{expr} if \code{process=NULL},
    #' and will raise errors if cannot be found
    run = function(expr, env = parent.frame(), quoted = FALSE,
                   async = FALSE, process = NULL) {
      if (!quoted) {
        expr <- substitute(expr)
      }
      # running, ready, errored
      private$.state <- "running"
      private$.vartable <- NULL
      # self$names <- names

      if (async) {
        private$.process_type <- "remote"

        self$promise <- promises::promise(
          function(resolve, reject) {
            process <- tryCatch({
              process <- eval(expr, env)
              if (inherits(process, "r_process")) {
                private$.process <- process
              } else {
                stop("`PipelineResult`: `expr` must return a callr::r_process instance")
              }
              process
            }, error = function(e) {
              private$.state <- "errored"
              private$close_progressor()
              reject(e)
              NULL
            })

            if (is.null(process)) { return() }

            run_async_callback <- function() {
              tryCatch({
                if (is.function(self$async_callback)) {
                  self$async_callback()
                }
              }, error = warning)
            }

            callback <- function() {

              continue <- tryCatch({
                if (private$.invalidated) {
                  private$.state <- "canceled"
                  self$invalidate()
                  e <- simpleCondition("Pipeline canceled")
                  run_async_callback()
                  reject(e)
                  return()
                }

                progress <- self$get_progress()

                if (!private$.process$is_alive()) {
                  private$.state <- "finished"
                  private$close_progressor()
                  private$.process$get_result()
                  resolve(private$.vartable)
                  return()
                }

                # show progress
                if (length(self$progressor)) {
                  old_val <- self$progressor$get_value()
                  increment <- progress$index - old_val
                  if (increment > 0) {
                    self$progressor$inc(
                      detail = progress$description,
                      amount = increment
                    )
                  }
                }

                # nrow(private$.vartable)

                TRUE
              }, error = function(e) {
                private$.state <- "errored"
                private$close_progressor()
                e
              })

              run_async_callback()

              if (isTRUE(continue)) {
                later::later(callback, delay = self$check_interval)
              } else {
                reject(callback)
                return()
              }
            }

            callback()

          }
        )

      } else {
        private$.process_type <- "native"
        self$promise <- promises::promise(
          function(resolve, reject) {
            tryCatch({
              eval(expr, env)
              private$.state <- "finished"
              # self$variables
              resolve(private$.vartable)
            }, error = function(e) {
              private$.state <- "errored"
              private$close_progressor()
              reject(e)
            })
          }
        )
      }

    },

    #' @description wait until some targets get finished
    #' @param names target names to wait, default is \code{NULL}, i.e. to
    #' wait for all targets that have been scheduled
    #' @param timeout maximum waiting time in seconds
    #' @returns \code{TRUE} if the target is finished, or \code{FALSE} if
    #' timeout is reached
    await = function(names = NULL, timeout = Inf) {
      if (!self$valid) { return(FALSE) }
      promise_impl <- attr(self$promise, "promise_impl")
      now <- Sys.time()
      if (length(names)) {
        missing_names <- names[!names %in% self$variables]
        if (length(missing_names)) {
          stop("Unable to watch the following names: ", paste(missing_names, collapse = ", "))
        }
      } else {
        names <- self$variables
      }
      sel <- which(private$.vartable$name %in% names)
      while (
        !promise_impl$status() %in% c("fulfilled", "rejected") &&
        !later::loop_empty()
      ) {
        later::run_now(0.1)

        if (private$.current_progress >= max(sel) &&
           !any(private$.vartable$progress %in% c("initialize", "started"))) {
          return(TRUE)
        }

        if (timeout <= as.numeric(Sys.time() - now, units = "secs")) {
          return(FALSE)
        }
      }
      return(TRUE)
    },


    #' @description print method
    print = function() {
      cat("<Pipeline result container> ")
      if (private$.invalidated) {
        cat("(Invalidated)\n")
      } else {
        cat("\nprocess:", private$.process_type)
        if (private$.state == "running") {
          cat(sprintf(
            "\nstatus: %s (%d of %d)\n",
            private$.state,
            private$.current_progress,
            length(self$variables)
          ))
        } else {
          cat(sprintf(
            "\nstatus: %s\n",
            private$.state
          ))
        }

      }
    },


    #' @description get results
    #' @param names the target names to read
    #' @param ... passed to \code{\link{pipeline_read}}
    get_values = function(names = NULL, ...) {
      self$validate()
      if (!length(names)) {
        names <- self$variables
      }
      pipeline_read(var_names = names, pipe_dir = private$.path, ...)
    }
  ),
  active = list(

    #' @field variables target variables of the pipeline
    variables = function() {
      if (!is.data.frame(private$.vartable)) {
        self$validate()
        variables <- pipeline_target_names(pipe_dir = private$.path)
        tarnames_readable <- names(variables)
        nvars <- length(variables)
        nactual <- length(tarnames_readable)
        if (nactual < nvars) {
          tarnames_readable <- c(tarnames_readable, rep("", nvars - nactual))
        }
        descr <- sapply(seq_len(nvars), function(ii) {
          nm <- tarnames_readable[[ii]]
          if (nm == "") {
            return(sprintf("Calculating `%s`", variables[[ii]]))
          } else {
            msg <- unlist(strsplit(nm, "[_-]+"))
            msg <- msg[msg != ""]
            msg <- paste(msg, collapse = " ")
            if (nchar(msg)) {
              msg <- sub("^[a-z]", toupper(substr(msg, 1, 1)), msg)
            }
            return(msg)
          }
        })
        tbl <- data.frame(
          name = unname(variables),
          description = descr,
          progress = "initialize",
          stringsAsFactors = FALSE
        )
        # tbl$included <- TRUE
        if (length(self$names)) {
          sel <- tbl$name %in% self$names
          if (any(sel)) {
            # tbl$included <- sel
            tbl <- tbl[tbl$name %in% self$names, ]
          }
        }
        private$.vartable <- tbl
      }
      private$.vartable$name
    },

    #' @field variable_descriptions readable descriptions of the target variables
    variable_descriptions = function() {
      self$variables
      private$.vartable$description
    },

    #' @field valid logical true or false whether the result instance hasn't
    #' been invalidated
    valid = function() {
      !private$.invalidated
    },

    #' @field status result status, possible status are \code{'initialize'},
    #' \code{'running'}, \code{'finished'}, \code{'canceled'},
    #' and \code{'errored'}. Note that \code{'finished'} only means the pipeline
    #' process has been finished.
    status = function() {
      private$.state
    },

    #' @field process (read-only) process object if the pipeline is running in
    #' \code{'async'} mode, or \code{NULL}; see \code{\link[callr]{r_bg}}.
    process = function() {
      private$.process
    }

  )
)

#' @export
as.promise.PipelineResult <- function(x) {
  x$promise
}

#' @export
plot.ravepipeline_plot_data <- function(
    x, callr_function = NULL, ...) {

  if (identical(Sys.getenv("RAVE_PIPELINE_ACTIVE"), "true")) {
    # Running within pipeline, plot generic should have been implemented
    return(NextMethod(plot))
  }

  # Check attribute
  pipeline_name <- attr(x, "pipeline_name")

  if (length(pipeline_name) != 1 || !is.character(pipeline_name) ||
      is.na(pipeline_name) || !nzchar(pipeline_name)) {
    return(NextMethod(plot))
  }

  # pipeline_name
  p <- tryCatch(
    {
      pipeline(pipeline_name)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(p)) {
    # Unable to find the pipeline
    return(NextMethod(plot))
  }

  env <- p$shared_env(callr_function = callr_function)

  # strip classes
  cls <- class(x)
  cls <- cls[!cls %in% c("ravepipeline_plot_data")]

  pipeline_plot_class <- attr(x, "pipeline_plot_class")
  if (length(pipeline_plot_class) == 1 &&
      is.character(pipeline_plot_class) &&
      !is.na(pipeline_plot_class) &&
      nzchar(pipeline_plot_class) &&
      !isTRUE(pipeline_plot_class %in% cls)) {
    cls <- c(pipeline_plot_class, cls)
  }

  ._plot_data <- structure(class = cls, x)
  env$._plot_data <- ._plot_data
  with(env, {
    plot(._plot_data)
  })
}

#' @export
print.ravepipeline_plot_data <- function(x, ...) {
  cls <- class(x)
  pipeline_name <- attr(x, "pipeline_name")
  pipeline_plot_class <- attr(x, "pipeline_plot_class")

  cat(c(
    "<RAVE plot data>",
    sprintf("Pipeline: %s; class: %s", pipeline_name, pipeline_plot_class),
    ""
  ), sep = "\n")

  class(x) <- cls[!cls %in% "ravepipeline_plot_data"]
  attr(x, "pipeline_name") <- NULL
  attr(x, "pipeline_plot_class") <- NULL

  NextMethod(print, object = x)
}

#' @title Create plot data from within pipeline make-file
#' @description
#' Tags an \R object so that calling \code{\link{plot}} on it outside the
#' pipeline can still dispatch the correct \verb{S3} method, even though that
#' method is only defined inside the pipeline's shared \R scripts.
#'
#' @section How plotting dispatch works:
#' A RAVE pipeline keeps its plot helpers in files whose names start with
#' \verb{shared} inside the pipeline's \verb{R/} folder (e.g.
#' \verb{R/shared-plots.R}).  Those files are sourced automatically every
#' time the pipeline runs, but they are \emph{not} available in an ordinary
#' interactive \R session.
#'
#' \code{pipeline_plot_data} bridges the two contexts by:
#' \enumerate{
#'   \item Inserting \code{name} and the sentinel class
#'         \code{"ravepipeline_plot_data"} to the class vector of \code{x}.
#'   \item Attaching the pipeline name as an attribute so the object can be
#'         re-associated with its pipeline later.
#' }
#'
#' When \code{plot()} is subsequently called:
#' \describe{
#'   \item{Inside the pipeline (during \code{pipeline_run})}{
#'     The environment variable \env{RAVE_PIPELINE_ACTIVE} is \code{"true"},
#'     the shared scripts have already been sourced, and \code{plot.<name>}
#'     is in scope.  \code{plot.ravepipeline_plot_data} simply calls
#'     \code{NextMethod()} so dispatch falls through to \code{plot.<name>}.
#'   }
#'   \item{Outside the pipeline (interactive session, report, Shiny app)}{
#'     \code{plot.ravepipeline_plot_data} locates the pipeline by
#'     \code{pipeline_name}, calls \code{$shared_env()} to source all
#'     \verb{R/shared*.R} files in an isolated environment, and then
#'     evaluates \code{plot(x)} inside that environment, where
#'     \code{plot.<name>} is now available.
#'   }
#' }
#'
#' @section Implementing a pipeline plot method:
#' \strong{Step 1 – define the \verb{S3} method} in any file whose name starts
#' with \verb{shared} inside the pipeline's \verb{R/} directory (e.g.
#' \verb{R/shared-plots.R}).  The function receives the original object \code{x}
#' with its user-defined class prepended, so standard \R dispatch applies:
#'
#' \preformatted{
#' # R/shared-plots.R  (inside the pipeline source tree)
#' plot.my_pipeline_result <- function(x, ...) {
#'   graphics::plot(
#'     x$time, x$signal,
#'     type = "l",
#'     xlab = "Time (s)",
#'     ylab = "Amplitude",
#'     main = x$title %||% ""
#'   )
#' }
#' }
#'
#' \strong{Step 2 – wrap the target} inside \verb{main.Rmd} (or any pipeline
#' make-file) by calling \code{pipeline_plot_data} with the same \code{name}
#' you used for the \verb{S3} method:
#'
#' \preformatted{
#' # main.Rmd  (pipeline make-file target block)
#' result_plot <- {
#'   ravepipeline::pipeline_plot_data(
#'     list(time = seq(0, 1, by = 0.01),
#'          signal = sin(2 * pi * 10 * seq(0, 1, by = 0.01)),
#'          title  = "10 Hz sine wave"),
#'     name = "my_pipeline_result"
#'   )
#' }
#' }
#'
#' \strong{Step 3 – call \code{plot()} anywhere:}
#'
#' \preformatted{
#' # Interactive session or report
#' p <- pipeline("my_pipeline")
#' result <- p$read("result_plot")
#' plot(result)   # sources R/shared-plots.R automatically, then calls
#'                # plot.my_pipeline_result(result)
#' }
#'
#' @param x R object to be used as plot data.
#' @param name \verb{S3} class name for which \code{plot.<name>} is implemented
#'   in the pipeline's \verb{R/shared*.R} files.  Must contain only ASCII
#'   letters, digits, dots, or underscores.  Defaults to the unevaluated
#'   expression passed as \code{x}.
#' @param strip_oldclasses if \code{TRUE} (default) and \code{x} already
#'   carries a \code{"ravepipeline_plot_data"} class from a previous call,
#'   the stale plot classes are stripped before re-tagging.  Set to
#'   \code{FALSE} to preserve the full original class vector.
#' @param pipe_dir path to the active pipeline directory.  Do not set this
#'   when calling from within a pipeline make-file; the default reads the
#'   \env{RAVE_PIPELINE} environment variable which is set automatically
#'   during \code{\link{pipeline_run}}.
#' @param pipeline_name character string overriding the pipeline name stored in
#'   the returned object.  When \code{NULL} (default) the name is inferred from
#'   \code{pipe_dir}.
#' @returns Object \code{x} with the class vector
#'   \code{c(name, "ravepipeline_plot_data", <original classes>)} and two
#'   extra attributes: \code{pipeline_name} and \code{pipeline_plot_class}.
#'
#' @examples
#'
#'
#' # 1.  R/shared-plots.R  -- define the S3 method
#' plot.toy_example <- function(x, ...) {
#'   graphics::plot(x$data,
#'                  xlab = "Index", ylab = "Value",
#'                  main = x$title %||% "")
#' }
#'
#' # 2.  main.Rmd target block -- wrap the data
#' plot_data <- ravepipeline::pipeline_plot_data(
#'   list(data = 1:10, title = "Toy example"),
#'   name = "toy_example",
#'   pipeline_name = "toy_pipeline"
#' )
#'
#' # 3.  Interactive session -- just call plot()
#' plot(plot_data)  # dispatches to plot.toy_example via shared_env
#'
#' @export
pipeline_plot_data <- function(x, name = substitute(x), strip_oldclasses = TRUE,
                               pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
                               pipeline_name = NULL) {

  if (is.null(pipeline_name)) {
    pipe_dir <- activate_pipeline(pipe_dir)
    pipeline_name <- attr(pipe_dir, "target_name")
  }

  name <- paste(as.character(name), collapse = "")

  if (!isTRUE(grepl("^[a-zA-Z0-9_.-]+$", name))) {
    stop("ravepipeline::pipeline_plot_data(x, name): `name` must only contain letters, digits, dots, underscores, not ", sQuote(name))
  }

  cls <- class(x)

  # Strip out the existing name/class
  if (strip_oldclasses && "ravepipeline_plot_data" %in% cls) {
    idx <- which(cls == "ravepipeline_plot_data")
    cls <- cls[-seq_len(idx[[length(idx)]])]
  }

  structure(
    class = c(name, "ravepipeline_plot_data", cls),
    pipeline_name = pipeline_name,
    pipeline_plot_class = name,
    x
  )
}
