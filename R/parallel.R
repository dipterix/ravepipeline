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
  dipsaus::make_forked_clusters(
    workers = max_workers,
    on_failure = on_failure, clean = FALSE, ...
  )
  on.exit({
    asNamespace("future")$plan("sequential")
    options("raveio.auto.parallel" = auto_parallel_old)
  }, add = TRUE, after = FALSE)

  re <- eval(expr, envir = env)
  asNamespace("future")$plan("sequential")
  options("raveio.auto.parallel" = auto_parallel_old)

  re
}
