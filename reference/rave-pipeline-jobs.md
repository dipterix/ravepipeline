# Run a function (job) in another session

Run a function (job) in another session

## Usage

``` r
start_job(
  fun,
  fun_args = list(),
  packages = NULL,
  workdir = NULL,
  method = c("callr", "rs_job", "mirai"),
  name = NULL,
  ensure_init = TRUE,
  digest_key = NULL,
  envvars = NULL,
  log_path = NULL
)

check_job(job_id)

resolve_job(
  job_id,
  timeout = Inf,
  auto_remove = TRUE,
  must_init = TRUE,
  unresolved = c("warning", "error", "silent"),
  log_maxline = getOption("ravepipeline.log_maxline", 0L)
)

remove_job(job_id)
```

## Arguments

- fun:

  function to evaluate

- fun_args:

  list of function arguments

- packages:

  list of packages to load

- workdir:

  working directory; default is temporary path

- method:

  job type; choices are `'rs_job'` (only used in `'RStudio'`
  environment), `'mirai'` (when package `'mirai'` is installed), and
  `'callr'` (default).

- name:

  name of the job

- ensure_init:

  whether to make sure the job has been started; default is true

- digest_key:

  a string that will affect how job ID is generated; used internally

- envvars:

  additional environment variables to set; must be a named list of
  environment variables

- log_path:

  path to a log file for capturing both standard output and messages
  (stderr) from the job; default is `NULL` (no logging). Relative paths
  are resolved against `workdir`. The file is created at job preparation
  time; if creation fails or the path is a directory, logging is
  silently skipped.

- job_id:

  job identification number

- timeout:

  timeout in seconds before the resolve ends; jobs that are still
  running are subject to `unresolved` policy

- auto_remove:

  whether to automatically remove the job if resolved; default it true

- must_init:

  whether the resolve should error out if the job is not initialized:
  typically meaning the either the resolving occurs too soon (only when
  `ensure_init=FALSE`) or the job files are corrupted; default is true

- unresolved:

  what to do if the job is still running after timing-out; default is
  `'warning'` and return `NULL`, other choices are `'error'` or
  `'silent'`

- log_maxline:

  maximum number of log lines to read from the tail of the log file when
  resolving a job; default is
  `getOption("ravepipeline.log_maxline", 1000)`. The log lines are
  attached to the result as attribute `"rave_logs"` if non-empty.

## Value

For `start_job`, a string of job identification number; `check_job`
returns the job status; `resolve_job` returns the function result.

## Examples

``` r
if (FALSE) { # \dontrun{

# Basic use
job_id <- start_job(function() {
  Sys.sleep(1)
  Sys.getpid()
})

check_job(job_id)

result <- resolve_job(job_id)


# As promise
library(promises)
as.promise(
  start_job(function() {
    Sys.sleep(1)
    Sys.getpid()
  })
) %...>%
  print()

} # }
```
