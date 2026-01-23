# Class definition for 'RAVE' pipelines

Class definition for 'RAVE' pipelines

Class definition for 'RAVE' pipelines

## Value

The value of the inputs, or a list if `key` is missing

The values of the targets

A
[`PipelineResult`](http://dipterix.org/ravepipeline/reference/PipelineResult.md)
instance if `as_promise` or `async` is true; otherwise a list of values
for input `names`

An environment of shared variables

See `type`

A table of the progress

a list where the names are target names and values are the corresponding
dependence

ancestor target names (including `names`)

A new pipeline object based on the path given

A new pipeline object based on the path given

the saved file path

the data if file is found or a default value

A list of key-value pairs

A list of the preferences. If `simplify` is true and length if keys is
1, then returns the value of that preference

logical whether the keys exist

characters if the source document (`main.Rmd`) is found, otherwise
`NULL`

A job identification number, see
[`resolve_job`](http://dipterix.org/ravepipeline/reference/rave-pipeline-jobs.md)
for querying job details

## See also

[`pipeline`](http://dipterix.org/ravepipeline/reference/pipeline.md)

## Super class

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `PipelineTools`

## Active bindings

- `description`:

  pipeline description

- `settings_path`:

  absolute path to the settings file

- `extdata_path`:

  absolute path to the user-defined pipeline data folder

- `preference_path`:

  directory to the pipeline preference folder

- `target_table`:

  table of target names and their descriptions

- `result_table`:

  summary of the results, including signatures of data and commands

- `pipeline_path`:

  the absolute path of the pipeline

- `pipeline_name`:

  the code name of the pipeline

- `available_reports`:

  available reports and their configurations

## Methods

### Public methods

- [`PipelineTools$@marshal()`](#method-PipelineTools-@marshal)

- [`PipelineTools$@unmarshal()`](#method-PipelineTools-@unmarshal)

- [`PipelineTools$new()`](#method-PipelineTools-new)

- [`PipelineTools$set_settings()`](#method-PipelineTools-set_settings)

- [`PipelineTools$get_settings()`](#method-PipelineTools-get_settings)

- [`PipelineTools$read()`](#method-PipelineTools-read)

- [`PipelineTools$run()`](#method-PipelineTools-run)

- [`PipelineTools$eval()`](#method-PipelineTools-eval)

- [`PipelineTools$shared_env()`](#method-PipelineTools-shared_env)

- [`PipelineTools$python_module()`](#method-PipelineTools-python_module)

- [`PipelineTools$progress()`](#method-PipelineTools-progress)

- [`PipelineTools$attach()`](#method-PipelineTools-attach)

- [`PipelineTools$visualize()`](#method-PipelineTools-visualize)

- [`PipelineTools$target_ancestors()`](#method-PipelineTools-target_ancestors)

- [`PipelineTools$fork()`](#method-PipelineTools-fork)

- [`PipelineTools$fork_to_subject()`](#method-PipelineTools-fork_to_subject)

- [`PipelineTools$with_activated()`](#method-PipelineTools-with_activated)

- [`PipelineTools$clean()`](#method-PipelineTools-clean)

- [`PipelineTools$save_data()`](#method-PipelineTools-save_data)

- [`PipelineTools$load_data()`](#method-PipelineTools-load_data)

- [`PipelineTools$set_preferences()`](#method-PipelineTools-set_preferences)

- [`PipelineTools$get_preferences()`](#method-PipelineTools-get_preferences)

- [`PipelineTools$has_preferences()`](#method-PipelineTools-has_preferences)

- [`PipelineTools$source_document()`](#method-PipelineTools-source_document)

- [`PipelineTools$generate_report()`](#method-PipelineTools-generate_report)

- [`PipelineTools$clone()`](#method-PipelineTools-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)

------------------------------------------------------------------------

### Method `@marshal()`

Create an atomic list that can be serialized

#### Usage

    PipelineTools$@marshal(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `@unmarshal()`

Restore an object from an atomic list

#### Usage

    PipelineTools$@unmarshal(object, ...)

#### Arguments

- `object`:

  a list from `'@marshal'`

- `...`:

  ignored

------------------------------------------------------------------------

### Method `new()`

construction function

#### Usage

    PipelineTools$new(
      pipeline_name,
      settings_file = "settings.yaml",
      paths = pipeline_root(),
      temporary = FALSE
    )

#### Arguments

- `pipeline_name`:

  name of the pipeline, usually in the pipeline `'DESCRIPTION'` file, or
  pipeline folder name

- `settings_file`:

  the file name of the settings file, where the user inputs are stored

- `paths`:

  the paths to find the pipeline, usually the parent folder of the
  pipeline; default is
  [`pipeline_root()`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)

- `temporary`:

  whether not to save `paths` to current pipeline root registry. Set
  this to `TRUE` when importing pipelines from subject pipeline folders

------------------------------------------------------------------------

### Method `set_settings()`

set inputs

#### Usage

    PipelineTools$set_settings(..., .list = NULL)

#### Arguments

- `..., .list`:

  named list of inputs; all inputs should be named, otherwise errors
  will be raised

------------------------------------------------------------------------

### Method `get_settings()`

get current inputs

#### Usage

    PipelineTools$get_settings(key, default = NULL, constraint)

#### Arguments

- `key`:

  the input name; default is missing, i.e., to get all the settings

- `default`:

  default value if not found

- `constraint`:

  the constraint of the results; if input value is not from
  `constraint`, then only the first element of `constraint` will be
  returned.

------------------------------------------------------------------------

### Method `read()`

read intermediate variables

#### Usage

    PipelineTools$read(var_names, ifnotfound = NULL, ..., simplify = TRUE)

#### Arguments

- `var_names`:

  the target names, can be obtained via `x$target_table` member; default
  is missing, i.e., to read all the intermediate variables

- `ifnotfound`:

  variable default value if not found

- `simplify, ...`:

  other parameters passing to
  [`pipeline_read`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)

------------------------------------------------------------------------

### Method `run()`

run the pipeline

#### Usage

    PipelineTools$run(
      names = NULL,
      async = FALSE,
      as_promise = async,
      scheduler = c("none", "future", "clustermq"),
      type = c("smart", "callr", "vanilla"),
      envir = new.env(parent = globalenv()),
      callr_function = NULL,
      return_values = TRUE,
      debug = FALSE,
      ...
    )

#### Arguments

- `names`:

  pipeline variable names to calculate; default is to calculate all the
  targets

- `async`:

  whether to run asynchronous in another process

- `as_promise`:

  whether to return a
  [`PipelineResult`](http://dipterix.org/ravepipeline/reference/PipelineResult.md)
  instance

- `scheduler, type, envir, callr_function, return_values, debug, ...`:

  passed to
  [`pipeline_run`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)
  if `as_promise` is true, otherwise these arguments will be passed to
  `pipeline_run_bare`

------------------------------------------------------------------------

### Method [`eval()`](https://rdrr.io/r/base/eval.html)

run the pipeline in order; unlike `$run()`, this method does not use the
`targets` infrastructure, hence the pipeline results will not be stored,
and the order of `names` will be respected.

#### Usage

    PipelineTools$eval(
      names,
      env = parent.frame(),
      shortcut = FALSE,
      clean = TRUE,
      ...
    )

#### Arguments

- `names`:

  pipeline variable names to calculate; must be specified

- `env`:

  environment to evaluate and store the results

- `shortcut`:

  logical or characters; default is `FALSE`, meaning `names` and all the
  dependencies (if missing from `env`) will be evaluated; set to `TRUE`
  if only `names` are to be evaluated. When `shortcut` is a character
  vector, it should be a list of targets (including their ancestors)
  whose values can be assumed to be up-to-date, and the evaluation of
  those targets can be skipped.

- `clean`:

  whether to evaluate without polluting `env`

- `...`:

  passed to
  [`pipeline_eval`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)

------------------------------------------------------------------------

### Method `shared_env()`

run the pipeline shared library in scripts starting with path `R/shared`

#### Usage

    PipelineTools$shared_env(callr_function = callr::r)

#### Arguments

- `callr_function`:

  either [`callr::r`](https://callr.r-lib.org/reference/r.html) or
  `NULL`; when [`callr::r`](https://callr.r-lib.org/reference/r.html),
  the environment will be loaded in isolated R session and serialized
  back to the main session to avoid contaminating the main session
  environment; when `NULL`, the code will be sourced directly in current
  environment.

------------------------------------------------------------------------

### Method `python_module()`

get 'Python' module embedded in the pipeline

#### Usage

    PipelineTools$python_module(
      type = c("info", "module", "shared", "exist"),
      must_work = TRUE
    )

#### Arguments

- `type`:

  return type, choices are `'info'` (get basic information such as
  module path, default), `'module'` (load module and return it),
  `'shared'` (load a shared sub-module from the module, which is shared
  also in report script), and `'exist'` (returns true or false on
  whether the module exists or not)

- `must_work`:

  whether the module needs to be existed or not. If `TRUE`, the raise
  errors when the module does not exist; default is `TRUE`, ignored when
  `type` is `'exist'`.

------------------------------------------------------------------------

### Method `progress()`

get progress of the pipeline

#### Usage

    PipelineTools$progress(method = c("summary", "details"))

#### Arguments

- `method`:

  either `'summary'` or `'details'`

------------------------------------------------------------------------

### Method [`attach()`](https://rdrr.io/r/base/attach.html)

attach pipeline tool to environment (internally used)

#### Usage

    PipelineTools$attach(env)

#### Arguments

- `env`:

  an environment

------------------------------------------------------------------------

### Method `visualize()`

visualize pipeline target dependency graph

#### Usage

    PipelineTools$visualize(
      glimpse = FALSE,
      aspect_ratio = 2,
      node_size = 30,
      label_size = 40,
      ...
    )

#### Arguments

- `glimpse`:

  whether to glimpse the graph network or render the state

- `aspect_ratio`:

  controls node spacing

- `node_size, label_size`:

  size of nodes and node labels

- `...`:

  passed to
  [`pipeline_visualize`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)

------------------------------------------------------------------------

### Method `target_ancestors()`

a helper function to get target ancestors

#### Usage

    PipelineTools$target_ancestors(names, skip_names = NULL)

#### Arguments

- `names`:

  targets whose ancestor targets need to be queried

- `skip_names`:

  targets that are assumed to be up-to-date, hence will be excluded,
  notice this exclusion is recursive, that means not only `skip_names`
  are excluded, but also their ancestors will be excluded from the
  result.

------------------------------------------------------------------------

### Method `fork()`

fork (copy) the current pipeline to a new directory

#### Usage

    PipelineTools$fork(path, policy = "default")

#### Arguments

- `path`:

  path to the new pipeline, a folder will be created there

- `policy`:

  fork policy defined by module author, see text file 'fork-policy'
  under the pipeline directory; if missing, then default to avoid
  copying `main.html` and `shared` folder

------------------------------------------------------------------------

### Method `fork_to_subject()`

fork (copy) the current pipeline to a 'RAVE' subject

#### Usage

    PipelineTools$fork_to_subject(
      subject,
      label = "NA",
      policy = "default",
      delete_old = FALSE,
      sanitize = TRUE
    )

#### Arguments

- `subject`:

  subject ID or instance in which pipeline will be saved

- `label`:

  pipeline label describing the pipeline

- `policy`:

  fork policy defined by module author, see text file 'fork-policy'
  under the pipeline directory; if missing, then default to avoid
  copying `main.html` and `shared` folder

- `delete_old`:

  whether to delete old pipelines with the same label default is false

- `sanitize`:

  whether to sanitize the registry at save. This will remove missing
  folders and import manually copied pipelines to the registry (only for
  the pipelines with the same name)

------------------------------------------------------------------------

### Method `with_activated()`

run code with pipeline activated, some environment variables and
function behaviors might change under such condition (for example,
`targets` package functions)

#### Usage

    PipelineTools$with_activated(expr, quoted = FALSE, env = parent.frame())

#### Arguments

- `expr`:

  expression to evaluate

- `quoted`:

  whether `expr` is quoted; default is false

- `env`:

  environment to run `expr`

------------------------------------------------------------------------

### Method `clean()`

clean all or part of the data store

#### Usage

    PipelineTools$clean(
      destroy = c("all", "cloud", "local", "meta", "process", "preferences", "progress",
        "objects", "scratch", "workspaces"),
      ask = FALSE
    )

#### Arguments

- `destroy, ask`:

  see
  [`tar_destroy`](https://docs.ropensci.org/targets/reference/tar_destroy.html)

------------------------------------------------------------------------

### Method `save_data()`

save data to pipeline data folder

#### Usage

    PipelineTools$save_data(
      data,
      name,
      format = c("json", "yaml", "csv", "fst", "rds"),
      overwrite = FALSE,
      ...
    )

#### Arguments

- `data`:

  R object

- `name`:

  the name of the data to save, must start with letters

- `format`:

  serialize format, choices are `'json'`, `'yaml'`, `'csv'`, `'fst'`,
  `'rds'`; default is `'json'`. To save arbitrary objects such as
  functions or environments, use `'rds'`

- `overwrite`:

  whether to overwrite existing files; default is no

- `...`:

  passed to saver functions

------------------------------------------------------------------------

### Method `load_data()`

load data from pipeline data folder

#### Usage

    PipelineTools$load_data(
      name,
      error_if_missing = TRUE,
      default_if_missing = NULL,
      format = c("auto", "json", "yaml", "csv", "fst", "rds"),
      ...
    )

#### Arguments

- `name`:

  the name of the data

- `error_if_missing`:

  whether to raise errors if the name is missing

- `default_if_missing`:

  default values to return if the name is missing

- `format`:

  the format of the data, default is automatically obtained from the
  file extension

- `...`:

  passed to loader functions

------------------------------------------------------------------------

### Method `set_preferences()`

set persistent preferences from the pipeline. The preferences should not
affect how pipeline is working, hence usually stores minor variables
such as graphic options. Changing preferences will not invalidate
pipeline cache.

#### Usage

    PipelineTools$set_preferences(..., .list = NULL)

#### Arguments

- `..., .list`:

  key-value pairs of initial preference values. The keys must start with
  'global' or the module ID, followed by dot and preference type and
  names. For example `'global.graphics.continuous_palette'` for setting
  palette colors for continuous heat-map; "global" means the settings
  should be applied to all 'RAVE' modules. The module-level preference,
  `'power_explorer.export.default_format'` sets the default format for
  power-explorer export dialogue.

- `name`:

  preference name, must contain only letters, digits, underscore, and
  hyphen, will be coerced to lower case (case-insensitive)

------------------------------------------------------------------------

### Method `get_preferences()`

get persistent preferences from the pipeline.

#### Usage

    PipelineTools$get_preferences(
      keys,
      simplify = TRUE,
      ifnotfound = NULL,
      validator = NULL,
      ...
    )

#### Arguments

- `keys`:

  characters to get the preferences

- `simplify`:

  whether to simplify the results when length of key is 1; default is
  true; set to false to always return a list of preferences

- `ifnotfound`:

  default value when the key is missing

- `validator`:

  `NULL` or function to validate the values; see 'Examples'

- `...`:

  passed to `validator` if `validator` is a function

#### Examples

    library(ravepipeline)
    if(interactive() && length(pipeline_list()) > 0) {
      pipeline <- pipeline("power_explorer")

      # set dummy preference
      pipeline$set_preferences("global.example.dummy_preference" = 1:3)

      # get preference
      pipeline$get_preferences("global.example.dummy_preference")

      # get preference with validator to ensure the value length to be 1
      pipeline$get_preferences(
        "global.example.dummy_preference",
        validator = function(value) {
          stopifnot(length(value) == 1)
        },
        ifnotfound = 100
      )

      pipeline$has_preferences("global.example.dummy_preference")
    }

------------------------------------------------------------------------

### Method `has_preferences()`

whether pipeline has preference keys

#### Usage

    PipelineTools$has_preferences(keys, ...)

#### Arguments

- `keys`:

  characters name of the preferences

- `...`:

  passed to internal methods

------------------------------------------------------------------------

### Method `source_document()`

obtain the source document

#### Usage

    PipelineTools$source_document()

------------------------------------------------------------------------

### Method `generate_report()`

generate pipeline

#### Usage

    PipelineTools$generate_report(
      name,
      subject = NULL,
      output_dir = NULL,
      output_format = "auto",
      clean = FALSE,
      ...
    )

#### Arguments

- `name`:

  report name, see field `'available_reports'`

- `subject`:

  subject helps determine the `output_dir` and working directories

- `output_dir`:

  parent folder where output will be stored

- `output_format`:

  output format

- `clean`:

  whether to clean the output; default is false

- `...`:

  passed to `'rmarkdown'` render function

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipelineTools$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `PipelineTools$get_preferences`
## ------------------------------------------------


library(ravepipeline)
if(interactive() && length(pipeline_list()) > 0) {
  pipeline <- pipeline("power_explorer")

  # set dummy preference
  pipeline$set_preferences("global.example.dummy_preference" = 1:3)

  # get preference
  pipeline$get_preferences("global.example.dummy_preference")

  # get preference with validator to ensure the value length to be 1
  pipeline$get_preferences(
    "global.example.dummy_preference",
    validator = function(value) {
      stopifnot(length(value) == 1)
    },
    ifnotfound = 100
  )

  pipeline$has_preferences("global.example.dummy_preference")
}
```
