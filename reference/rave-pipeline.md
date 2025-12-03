# Low-level 'RAVE' pipeline functions

Utility functions for 'RAVE' pipelines, currently designed for internal
development use. The infrastructure will be deployed to 'RAVE' in the
future to facilitate the "self-expanding" aim. Please check the official
'RAVE' website.

## Usage

``` r
pipeline_root(root_path, temporary = FALSE)

pipeline_list(root_path = pipeline_root())

pipeline_find(name, root_path = pipeline_root())

pipeline_attach(name, root_path = pipeline_root())

pipeline_run(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  scheduler = c("none", "future", "clustermq"),
  type = c("smart", "callr", "vanilla"),
  envir = new.env(parent = globalenv()),
  callr_function = NULL,
  names = NULL,
  async = FALSE,
  check_interval = 0.5,
  progress_quiet = !async,
  progress_max = NA,
  progress_title = "Running pipeline",
  return_values = TRUE,
  debug = FALSE,
  ...
)

pipeline_clean(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  destroy = c("all", "cloud", "local", "meta", "process", "preferences", "progress",
    "objects", "scratch", "workspaces"),
  ask = FALSE
)

pipeline_run_bare(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  scheduler = c("none", "future", "clustermq"),
  type = c("smart", "callr", "vanilla"),
  envir = new.env(parent = globalenv()),
  callr_function = NULL,
  names = NULL,
  return_values = TRUE,
  debug = FALSE,
  ...
)

load_targets(..., env = NULL)

pipeline_target_names(pipe_dir = Sys.getenv("RAVE_PIPELINE", "."))

pipeline_debug(
  quick = TRUE,
  env = parent.frame(),
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  skip_names
)

pipeline_dep_targets(
  names,
  skip_names = NULL,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
)

pipeline_eval(
  names,
  env = new.env(parent = parent.frame()),
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  settings_path = file.path(pipe_dir, "settings.yaml"),
  shortcut = FALSE
)

pipeline_visualize(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  glimpse = FALSE,
  targets_only = TRUE,
  shortcut = FALSE,
  zoom_speed = 0.1,
  ...
)

pipeline_progress(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  method = c("summary", "details", "custom"),
  func = targets::tar_progress_summary
)

pipeline_fork(
  src = Sys.getenv("RAVE_PIPELINE", "."),
  dest = tempfile(pattern = "rave_pipeline_"),
  policy = "default",
  activate = FALSE,
  ...
)

pipeline_build(pipe_dir = Sys.getenv("RAVE_PIPELINE", "."))

pipeline_read(
  var_names,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  branches = NULL,
  ifnotfound = NULL,
  dependencies = c("none", "ancestors_only", "all"),
  simplify = TRUE,
  ...
)

pipeline_vartable(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  targets_only = TRUE,
  complete_only = FALSE,
  ...
)

pipeline_hasname(var_names, pipe_dir = Sys.getenv("RAVE_PIPELINE", "."))

pipeline_watch(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  targets_only = TRUE,
  ...
)

pipeline_create_template(
  root_path,
  pipeline_name,
  overwrite = FALSE,
  activate = TRUE,
  template_type = c("rmd", "r", "rmd-bare", "rmd-scheduler", "rmd-python")
)

pipeline_create_subject_pipeline(
  subject,
  pipeline_name,
  overwrite = FALSE,
  activate = TRUE,
  template_type = c("rmd", "r", "rmd-python")
)

pipeline_description(file)

pipeline_load_extdata(
  name,
  format = c("auto", "json", "yaml", "csv", "fst", "rds"),
  error_if_missing = TRUE,
  default_if_missing = NULL,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  ...
)

pipeline_save_extdata(
  data,
  name,
  format = c("json", "yaml", "csv", "fst", "rds"),
  overwrite = FALSE,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  ...
)

pipeline_shared(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  callr_function = callr::r
)

pipeline_set_preferences(
  ...,
  .list = NULL,
  .pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  .preference_instance = NULL
)

pipeline_get_preferences(
  keys,
  simplify = TRUE,
  ifnotfound = NULL,
  validator = NULL,
  ...,
  .preference_instance = NULL
)

pipeline_has_preferences(keys, ..., .preference_instance = NULL)
```

## Arguments

- root_path:

  the root directory for pipeline templates

- temporary:

  whether not to save `paths` to current pipeline root registry. Set
  this to `TRUE` when importing pipelines from subject pipeline folders

- name, pipeline_name:

  the pipeline name to create; usually also the folder

- pipe_dir, .pipe_dir:

  where the pipeline directory is; can be set via system environment
  `Sys.setenv("RAVE_PIPELINE"=...)`

- scheduler:

  how to schedule the target jobs: default is `'none'`, which is
  sequential. If you have multiple heavy-weighted jobs that can be
  scheduled at the same time, you can choose `'future'` or `'clustermq'`

- type:

  how the pipeline should be executed; current choices are `"smart"` to
  enable 'future' package if possible, `'callr'` to use
  [`r`](https://callr.r-lib.org/reference/r.html), or `'vanilla'` to run
  everything sequentially in the main session.

- callr_function:

  function that will be passed to
  [`tar_make`](https://docs.ropensci.org/targets/reference/tar_make.html);
  will be forced to be `NULL` if `type='vanilla'`, or
  [`r`](https://callr.r-lib.org/reference/r.html) if `type='callr'`

- names:

  the names of pipeline targets that are to be executed; default is
  `NULL`, which runs all targets; use `pipeline_target_names` to check
  all your available target names.

- async:

  whether to run pipeline without blocking the main session

- check_interval:

  when running in background (non-blocking mode), how often to check the
  pipeline

- progress_title, progress_max, progress_quiet:

  control the progress

- return_values:

  whether to return pipeline target values; default is true; only works
  in `pipeline_run_bare` and will be ignored by `pipeline_run`

- debug:

  whether to debug the process; default is false

- ..., .list:

  other parameters, targets, etc.

- destroy:

  what part of data repository needs to be cleaned

- ask:

  whether to ask

- env, envir:

  environment to execute the pipeline

- quick:

  whether to skip finished targets to save time

- skip_names:

  hint of target names to fast skip provided they are up-to-date; only
  used when `quick=TRUE`. If missing, then `skip_names` will be
  automatically determined

- settings_path:

  path to settings file name within subject's pipeline path

- shortcut:

  whether to display shortcut targets

- glimpse:

  whether to hide network status when visualizing the pipelines

- targets_only:

  whether to return the variable table for targets only; default is true

- zoom_speed:

  zoom speed when visualizing the pipeline dependence

- method:

  how the progress should be presented; choices are `"summary"`,
  `"details"`, `"custom"`. If custom method is chosen, then `func` will
  be called

- func:

  function to call when reading customized pipeline progress; default is
  [`tar_progress_summary`](https://docs.ropensci.org/targets/reference/tar_progress_summary.html)

- src, dest:

  pipeline folder to copy the pipeline script from and to

- policy:

  fork policy defined by module author, see text file 'fork-policy'
  under the pipeline directory; if missing, then default to avoid
  copying `main.html` and `shared` folder

- activate:

  whether to activate the new pipeline folder from `dest`; default is
  false

- var_names:

  variable name to fetch or to check

- branches:

  branch to read from; see
  [`tar_read`](https://docs.ropensci.org/targets/reference/tar_read.html)

- ifnotfound:

  default values to return if variable is not found

- dependencies:

  whether to load dependent targets, choices are `'none'` (default, only
  load targets specified by `names`), `'ancestors_only'` (load all but
  the ancestors targets), and `'all'` (both targets and ancestors)

- simplify:

  whether to simplify the output

- complete_only:

  whether only to show completed and up-to-date target variables;
  default is false

- overwrite:

  whether to overwrite existing pipeline; default is false so users can
  double-check; if true, then existing pipeline, including the data will
  be erased

- template_type:

  which template type to create; choices are `'r'` or `'rmd'`

- subject:

  character indicating valid 'RAVE' subject ID, or a `RAVESubject`
  instance

- file:

  path to the 'DESCRIPTION' file under the pipeline folder, or pipeline
  collection folder that contains the pipeline information, structures,
  dependencies, etc.

- format:

  format of the extended data, default is `'json'`, other choices are
  `'yaml'`, `'fst'`, `'csv'`, `'rds'`

- error_if_missing, default_if_missing:

  what to do if the extended data is not found

- data:

  extended data to be saved

- .preference_instance:

  internally used

- keys:

  preference keys

- validator:

  `NULL` or function to validate values

## Value

- `pipeline_root`:

  the root directories of the pipelines

- `pipeline_list`:

  the available pipeline names under `pipeline_root`

- `pipeline_find`:

  the path to the pipeline

- `pipeline_run`:

  a
  [`PipelineResult`](http://dipterix.org/ravepipeline/reference/PipelineResult.md)
  instance

- `load_targets`:

  a list of targets to build

- `pipeline_target_names`:

  a vector of characters indicating the pipeline target names

- `pipeline_visualize`:

  a widget visualizing the target dependence structure

- `pipeline_progress`:

  a table of building progress

- `pipeline_fork`:

  a normalized path of the forked pipeline directory

- `pipeline_read`:

  the value of corresponding `var_names`, or a named list if `var_names`
  has more than one element

- `pipeline_vartable`:

  a table of summaries of the variables; can raise errors if pipeline
  has never been executed

- `pipeline_hasname`:

  logical, whether the pipeline has variable built

- `pipeline_watch`:

  a basic shiny application to monitor the progress

- `pipeline_description`:

  the list of descriptions of the pipeline or pipeline collection
