# Connect and schedule pipelines

Experimental, subject to change in the future.

## Value

A list containing

- `id`:

  the pipeline ID that can be used by `deps`

- `pipeline`:

  forked pipeline instance

- `target_names`:

  copy of `names`

- `depend_on`:

  copy of `deps`

- `cue`:

  copy of `cue`

- `standalone`:

  copy of `standalone`

## Public fields

- `verbose`:

  whether to verbose the build

## Active bindings

- `root_path`:

  path to the directory that contains pipelines and scheduler

- `collection_path`:

  path to the pipeline collections

- `pipeline_ids`:

  pipeline ID codes

## Methods

### Public methods

- [`PipelineCollections$new()`](#method-PipelineCollection-new)

- [`PipelineCollections$add_pipeline()`](#method-PipelineCollection-add_pipeline)

- [`PipelineCollections$build_pipelines()`](#method-PipelineCollection-build_pipelines)

- [`PipelineCollections$run()`](#method-PipelineCollection-run)

- [`PipelineCollections$get_scheduler()`](#method-PipelineCollection-get_scheduler)

------------------------------------------------------------------------

### Method `new()`

Constructor

#### Usage

    PipelineCollections$new(root_path = NULL, overwrite = FALSE)

#### Arguments

- `root_path`:

  where to store the pipelines and intermediate results

- `overwrite`:

  whether to overwrite if `root_path` exists

------------------------------------------------------------------------

### Method `add_pipeline()`

Add pipeline into the collection

#### Usage

    PipelineCollections$add_pipeline(
      x,
      names = NULL,
      deps = NULL,
      pre_hook = NULL,
      post_hook = NULL,
      cue = c("always", "thorough", "never"),
      search_paths = pipeline_root(),
      standalone = TRUE,
      hook_envir = parent.frame()
    )

#### Arguments

- `x`:

  a pipeline name (can be found via
  [`pipeline_list`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)),
  or a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)

- `names`:

  pipeline targets to execute

- `deps`:

  pipeline IDs to depend on; see 'Values' below

- `pre_hook`:

  function to run before the pipeline; the function needs two arguments:
  input map (can be edit in-place), and path to a directory that allows
  to store temporary files

- `post_hook`:

  function to run after the pipeline; the function needs two arguments:
  pipeline object, and path to a directory that allows to store
  intermediate results

- `cue`:

  whether to always run dependence

- `search_paths`:

  where to search for pipeline if `x` is a character; ignored when `x`
  is a pipeline object

- `standalone`:

  whether the pipeline should be standalone, set to `TRUE` if the same
  pipeline added multiple times should run independently; default is
  true

- `hook_envir`:

  where to look for global environments if `pre_hook` or `post_hook`
  contains global variables; default is the calling environment

------------------------------------------------------------------------

### Method `build_pipelines()`

Build pipelines and visualize

#### Usage

    PipelineCollections$build_pipelines(visualize = TRUE)

#### Arguments

- `visualize`:

  whether to visualize the pipeline; default is true

------------------------------------------------------------------------

### Method `run()`

Run the collection of pipelines

#### Usage

    PipelineCollections$run(
      error = c("error", "warning", "ignore"),
      .scheduler = c("none", "future", "clustermq"),
      .type = c("callr", "smart", "vanilla"),
      .as_promise = FALSE,
      .async = FALSE,
      rebuild = NA,
      ...
    )

#### Arguments

- `error`:

  what to do when error occurs; default is `'error'` throwing errors;
  other choices are `'warning'` and `'ignore'`

- `.scheduler, .type, .as_promise, .async, ...`:

  passed to
  [`pipeline_run`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)

- `rebuild`:

  whether to re-build the pipeline; default is `NA` ( if the pipeline
  has been built before, then do not rebuild)

------------------------------------------------------------------------

### Method `get_scheduler()`

Get `scheduler` object

#### Usage

    PipelineCollections$get_scheduler()
