# This file is licensed under MIT by Zhengjia Wang


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
  os <- get_os()
  if (os == "windows" || getOption("dipsaus.debug", FALSE) || getOption("dipsaus.no.fork", FALSE)) {
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
  }
  else {
    options(future.fork.enable = TRUE)
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
  auto_parallel_old <- getOption("raveio.auto.parallel", default = TRUE)
  options("raveio.auto.parallel" = FALSE)
  make_forked_clusters(
    workers = max_workers,
    on_failure = on_failure,
    clean = FALSE, ...
  )

  needs_reset <- TRUE
  on.exit({
    if( needs_reset ) {
      future::plan("sequential")
      options("raveio.auto.parallel" = auto_parallel_old)
    }
  }, add = TRUE, after = FALSE)

  re <- eval(expr, envir = env)
  future::plan("sequential")
  options("raveio.auto.parallel" = auto_parallel_old)
  needs_reset <- FALSE

  re
}
