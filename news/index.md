# Changelog

## ravepipeline 0.1.0

- Added `pipeline_plot_data` to help dispatch `plot` methods defined
  from within a pipeline, keeping plot logic encapsulated inside the
  pipeline script
- Added `pipeline_translate_settings`, `pipeline_export_wizard`, and
  `pipeline_import_wizard` for translating and migrating settings
  between pipeline versions or modules
- Added `import_settings` method to the `PipelineTools` class for
  importing external settings directly into a pipeline instance; fixed
  an edge case where the internal settings map was a `fastmap2` instead
  of a plain list
- Pipeline subset operator (`pipeline[[...]]`) now reads from
  `settings.yaml` when the pipeline has not yet been evaluated,
  providing access to default inputs before a run
- `pipeline_get_preferences` gains a `modes` argument for type-mode
  validation; stored values whose
  [`mode()`](https://rdrr.io/r/base/mode.html) does not match are
  treated as missing and `ifnotfound` is returned instead
- Added `html_embed_write` and `html_embed_read` to embed and recover
  arbitrary R objects as hidden data inside HTML documents
- JSON serialization sanitizes inputs before passing them to `jsonlite`,
  preventing conversion errors for types that `jsonlite` cannot encode
- `pandoc` discovery is now more robust and integrated into `RAVE`
  options; fixed `RSTUDIO_PANDOC` not being set in some environments;
  fixed `pandoc` not found on certain systems
- Pipeline reports no longer force `toc` and `code_folding`; these can
  now be customized by the caller; `distill` is used as an optional
  report style when the package is available, with `rmarkdown` as the
  fallback
- Added a `callback` argument to pipeline report generation for
  post-processing hooks; `start_job` gains log-streaming support so
  outputs always stream to the console; fixed a log-set bug when the job
  result is `NULL` and changed the default behavior to not attach the
  log file
- `lapply_jobs` now evaluates the worker function in the `runtime_env`
  environment so global helper functions defined in the same script can
  reference each other correctly
- `PipelineTools$run_as_task` enables running a pipeline as a `shiny`
  `ExtendedTask`, allowing non-blocking pipeline execution inside a
  Shiny app without blocking the session; pipelines also support early
  cancellation
- Added `MCP` helper utilities (`docs_package_help_topics`,
  `docs_help_page`, `docs_package_implementation`,
  `search_package_info`) for use with `rave-pipelines` repository
  tooling
- Pipeline templates are updated to match the latest `shidashi`
  framework changes; a timestamp is written on template updates; Python
  sub-module presence is verified before import
- `ravepipeline_finalize_installation` is now exported for post-install
  setup
- Miscellaneous fixes: corrected `detect_generic_land()` lookup,
  tightened embedding metadata accuracy, and removed obsolete vignettes

## ravepipeline 0.0.3

CRAN release: 2025-09-10

- Removed `raveio` from templates
- Internally, `rs_job` considers `Positron-IDE`
- Fixed incorrect path issue when the report working directory and
  output folder are identical
- Using `distill` for default report style, if available
- Added `SVG` plots to allow image overlays
- Added `base64URI`; using `base64enc` for more general `base64`
  converter
- Fixed a bug where `lapply_jobs` callback fail to run due to
  zero-length messages
- Job processes are killed by default when the job is removed
- `start_job` uses `callr` for maximized consistency by default
- During `@unmarshal`, the package is loaded by default
- Pipeline serialization has been implemented
- Stores process handlers in the job ID to avoid the process to be shut
  down accidentally by garbage collector
- Fixed pipeline paths, added behavior for subjects
- Pipeline report has more allowed tweaks (theme, style, parameter)
- Implemented serialization for brain object

## ravepipeline 0.0.2

- `with_rave_parallel` with one worker no longer runs in background,
  though users can force enabling this
- Added comparisons to `RAVESerializable` objects, allowing comparisons
  of two `R6` objects inheriting this class
- Added `RAVEFileArray` to wrap against `FileArray`, allowing
  file-arrays to be serialized faster when running with `mirai`
- Added `lapply_jobs`, allowing down-stream packages to use it for
  looping with similar behaviors to `lapply`, while giving users options
  to parallel computing the results with `with_rave_parallel`
- Added `RAVESerializable` class for `mirai`, allowing for fast
  serialization when running `lapply_jobs`
- `cat2` is no longer used as logging system and used `logger` instead
- Added `rave_progress` to replace `dipsaus`; removed dependencies on
  `dipsaus`
- Migrated `logger` from `ravedash` so this function can be used by
  `ravecore` and other depending packages
- Added print method to background jobs
- Added `mirai` as the optional background engine to run jobs
- Pipeline evaluation will have `RAVE_PIPELINE_ACTIVE` environment set
  to `"true"` to allow fast compile and debugging
- `pipeline$read` once disabled by `targets` during evaluation can now
  runs again with hacks
- Pipeline attach method also mounts shared environment for debugging
- Implemented pipeline reporting functions
- Added `start_job` to start background jobs, which can be converted
  into `promises`
- Make sure installing new snippets overwrites existing scripts
- Unset reporters for older versions of targets
- Fixed lazy evaluation issues
- Fixed a bug introduced by `targets` version `1.11.1` where
  use-after-free of `cli` progress crashes the pipeline run, the newer
  versions of that package has fixed it too
- Added optional functions to handle old `raveio` calls
- Removed questions once asked
- save `yaml` format now sort native lists with `sorted=TRUE`
- Removed `call_pkg_fun` for `rstudioapi` and use the package functions
  directly to avoid messing up with
  [`sys.call()`](https://rdrr.io/r/base/sys.parent.html)
- Fixed `entrace`
- Added pipeline collections

## ravepipeline 0.0.1

CRAN release: 2025-03-13

- Initial CRAN submission.
