# This file is licensed under MIT by Zhengjia Wang

fork_enabled <- function() {
  os <- get_os()
  if (os == "windows" || getOption("dipsaus.debug", FALSE) || getOption("dipsaus.no.fork", FALSE)) {
    return(FALSE)
  }
  return(TRUE)
}

make_forked_clusters <- function (
    workers = future::availableCores(),
    on_failure = getOption("dipsaus.cluster.backup", "sequential"), clean = FALSE, ...) {
  if (clean) {
    oplan <- future::plan("list")
    parent_frame <- parent.frame()
    expr <- rlang::quo_squash({
      rlang::quo({
        future::plan(!!oplan, substitute = FALSE, .call = NULL,
                     .cleanup = FALSE, .init = FALSE)
      })
    })
    do.call(on.exit, list(expr, add = TRUE, after = TRUE),
            envir = parent_frame)
  }

  if( fork_enabled() ) {
    suc <- tryCatch({
      future::plan(on_failure, .call = NULL, .cleanup = !clean,
                   .init = FALSE, workers = workers, ...)
      TRUE
    }, error = function(e) {
      FALSE
    }, warning = function(e) {
      FALSE
    })
    if (!suc) {
      future::plan(on_failure, .call = NULL, .cleanup = !clean,
                   .init = FALSE, ...)
    }
  } else {
    future::plan(future::multicore, workers = workers, .cleanup = !clean,
                 .init = FALSE, .call = NULL, ...)
  }
  invisible(future::plan())
}


with_future_parallel <- function(expr, env = parent.frame(), quoted = FALSE,
                                 on_failure = 'multisession', max_workers = NA,
                                 ...){
  if(!quoted){
    expr <- substitute(expr)
  }
  if(!is.na(max_workers) && max_workers >= 1){
    max_workers <- min(as.integer(max_workers), raveio_getopt("max_worker", 1L))
  } else {
    max_workers <- raveio_getopt("max_worker", 1L)
  }

  needs_reset <- TRUE
  fork_allowed <- fork_enabled()
  old_opts <- options("raveio.auto.parallel" = FALSE,
                      "future.fork.enable" = fork_allowed)
  on.exit({
    if( needs_reset ) {
      options(old_opts)
      future::plan("sequential")
    }
  }, add = TRUE, after = FALSE)

  make_forked_clusters(
    workers = max_workers,
    on_failure = on_failure,
    clean = FALSE, ...
  )

  re <- eval(expr, envir = env)

  # Make sure the options get reset before anything else happen
  # to avoid any possible side effects.
  # No need to reset because this IS the expression that resets the
  # options
  future::plan("sequential")
  options(old_opts)
  needs_reset <- FALSE

  re
}
