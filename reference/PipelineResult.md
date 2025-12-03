# Class definition for 'RAVE' pipeline results

Class definition for 'RAVE' pipeline results

Class definition for 'RAVE' pipeline results

## Value

`TRUE` if the target is finished, or `FALSE` if timeout is reached

## Public fields

- `progressor`:

  progress bar object, usually generated a progress instance

- `promise`:

  a
  [`promise`](https://rstudio.github.io/promises/reference/promise.html)
  instance that monitors the pipeline progress

- `verbose`:

  whether to print warning messages

- `names`:

  names of the pipeline to build

- `async_callback`:

  function callback to call in each check loop; only used when the
  pipeline is running in `async=TRUE` mode

- `check_interval`:

  used when `async=TRUE` in
  [`pipeline_run`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md),
  interval in seconds to check the progress

## Active bindings

- `variables`:

  target variables of the pipeline

- `variable_descriptions`:

  readable descriptions of the target variables

- `valid`:

  logical true or false whether the result instance hasn't been
  invalidated

- `status`:

  result status, possible status are `'initialize'`, `'running'`,
  `'finished'`, `'canceled'`, and `'errored'`. Note that `'finished'`
  only means the pipeline process has been finished.

- `process`:

  (read-only) process object if the pipeline is running in `'async'`
  mode, or `NULL`; see
  [`r_bg`](https://callr.r-lib.org/reference/r_bg.html).

## Methods

### Public methods

- [`PipelineResult$validate()`](#method-PipelineResult-validate)

- [`PipelineResult$invalidate()`](#method-PipelineResult-invalidate)

- [`PipelineResult$get_progress()`](#method-PipelineResult-get_progress)

- [`PipelineResult$new()`](#method-PipelineResult-new)

- [`PipelineResult$run()`](#method-PipelineResult-run)

- [`PipelineResult$await()`](#method-PipelineResult-await)

- [`PipelineResult$print()`](#method-PipelineResult-print)

- [`PipelineResult$get_values()`](#method-PipelineResult-get_values)

- [`PipelineResult$clone()`](#method-PipelineResult-clone)

------------------------------------------------------------------------

### Method `validate()`

check if result is valid, raises errors when invalidated

#### Usage

    PipelineResult$validate()

------------------------------------------------------------------------

### Method `invalidate()`

invalidate the pipeline result

#### Usage

    PipelineResult$invalidate()

------------------------------------------------------------------------

### Method `get_progress()`

get pipeline progress

#### Usage

    PipelineResult$get_progress()

------------------------------------------------------------------------

### Method `new()`

constructor (internal)

#### Usage

    PipelineResult$new(path = character(0L), verbose = FALSE)

#### Arguments

- `path`:

  pipeline path

- `verbose`:

  whether to print warnings

------------------------------------------------------------------------

### Method `run()`

run pipeline (internal)

#### Usage

    PipelineResult$run(
      expr,
      env = parent.frame(),
      quoted = FALSE,
      async = FALSE,
      process = NULL
    )

#### Arguments

- `expr`:

  expression to evaluate

- `env`:

  environment of `expr`

- `quoted`:

  whether `expr` has been quoted

- `async`:

  whether the process runs in other sessions

- `process`:

  the process object inherits
  [`process`](http://processx.r-lib.org/reference/process.md), will be
  inferred from `expr` if `process=NULL`, and will raise errors if
  cannot be found

------------------------------------------------------------------------

### Method `await()`

wait until some targets get finished

#### Usage

    PipelineResult$await(names = NULL, timeout = Inf)

#### Arguments

- `names`:

  target names to wait, default is `NULL`, i.e. to wait for all targets
  that have been scheduled

- `timeout`:

  maximum waiting time in seconds

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print method

#### Usage

    PipelineResult$print()

------------------------------------------------------------------------

### Method `get_values()`

get results

#### Usage

    PipelineResult$get_values(names = NULL, ...)

#### Arguments

- `names`:

  the target names to read

- `...`:

  passed to
  [`pipeline_read`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipelineResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
