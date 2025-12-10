# Internal parallel functions

Experimental parallel functions, intended for internal use now. The goal
is to allow 'RAVE' functions to gain the potential benefit from parallel
computing, but allow users to control whether to do it.

## Usage

``` r
with_rave_parallel(expr, .workers = 0)

lapply_jobs(
  x,
  fun,
  ...,
  .globals = list(),
  .workers = 0,
  .always = FALSE,
  callback = NULL
)
```

## Arguments

- expr:

  expression to evaluate with parallel workers

- .workers:

  number of workers: note the actual numbers may differ, depending on
  the options and number of input elements

- x:

  a list, vector, array of R objects

- fun:

  function to apply to each element of `x`

- ...:

  additional arguments to be passed to `fun`

- .globals:

  global variables to be serialized

- .always:

  whether always use workers, only considered when number of workers is
  one; default is false, then run jobs in the main process when only one
  worker is required

- callback:

  callback function, input is each element of `x` and should return a
  string, for progress bar

- workers:

  number of workers

## Details

By default, `lapply_jobs` is almost identical to
[`lapply`](https://rdrr.io/r/base/lapply.html). It only runs in parallel
when running inside of `with_rave_parallel`.

The hard max-limit number of workers are determined by the 'RAVE' option
`raveio_getopt('max_worker')`. Users can lower this number for
memory-intensive tasks manually, via argument `.workers`. The actual
number of workers might be less than the requested ones, this is often a
result of sort input `x`. If the number of input iterations has fewer
than the max worker size, then the number of workers automatically
shrinks to the length of input list. All workers will be a child process
running separate from the main session, except for when only one worker
is needed and `.always=FALSE`: the only task will be executed in the
main session.

Each worker session will run a completely isolated new process. There is
a ramp-up serialization that is needed for global objects (objects that
are defined elsewhere or outside of the function). Please make sure the
global objects are specified explicitly in `.globals`, a named list.
Unlike `future` package, users must specify the global objects.

The global objects might be large to serialize. Please optimize the code
to avoid serializing big objects, especially environments or functions.
All objects inheriting
[`RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
class with `@marshal` and `@unmarshal` methods implemented correctly
will be serialized with reference hook `rave_serialize_refhook`, making
them extremely efficient.

## Examples

``` r

# Run without `with_rave_parallel`
res <- lapply_jobs(1:5, function(x, ...) {
  c(child = Sys.getpid(), ...)
}, main = Sys.getpid())

simplify2array(res)
#>       [,1] [,2] [,3] [,4] [,5]
#> child 7058 7058 7058 7058 7058
#> main  7058 7058 7058 7058 7058

# Comparison
f <- function(n = 5, workers = 0) {
  system.time({
    ravepipeline::lapply_jobs(seq_len(n), function(x, ...) {
      Sys.sleep(1)
      c(child = Sys.getpid(), ...)
    }, main = Sys.getpid(), .workers = workers, callback = I)
  })
}

if (FALSE) { # \dontrun{

# Without parallel
f()
#>    user  system elapsed
#>   0.022   0.019   5.010


# with parallel
with_rave_parallel({
  f()
})
#>    user  system elapsed
#>   0.729   0.190   1.460

} # }
```
