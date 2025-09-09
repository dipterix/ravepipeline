# ravepipeline 0.0.3

* Removed `raveio` from templates
* Internally, `rs_job` considers `Positron-IDE`
* Fixed incorrect path issue when the report working directory and output folder are identical
* Using `distill` for default report style, if available
* Added `SVG` plots to allow image overlays
* Added `base64URI`; using `base64enc` for more general `base64` converter
* Fixed a bug where `lapply_jobs` callback fail to run due to zero-length messages
* Job processes are killed by default when the job is removed
* `start_job` uses `callr` for maximized consistency by default
* During `@unmarshal`, the package is loaded by default
* Pipeline serialization has been implemented
* Stores process handlers in the job ID to avoid the process to be shut down accidentally by garbage collector
* Fixed pipeline paths, added behavior for subjects
* Pipeline report has more allowed tweaks (theme, style, parameter)
* Implemented serialization for brain object

# ravepipeline 0.0.2

* `with_rave_parallel` with one worker no longer runs in background, though users can force enabling this
* Added comparisons to `RAVESerializable` objects, allowing comparisons of two `R6` objects inheriting this class
* Added `RAVEFileArray` to wrap against `FileArray`, allowing file-arrays to be serialized faster when running with `mirai`
* Added `lapply_jobs`, allowing down-stream packages to use it for looping with similar behaviors to `lapply`, while giving users options to parallel computing the results with `with_rave_parallel`
* Added `RAVESerializable` class for `mirai`, allowing for fast serialization when running `lapply_jobs`
* `cat2` is no longer used as logging system and used `logger` instead
* Added `rave_progress` to replace `dipsaus`; removed dependencies on `dipsaus`
* Migrated `logger` from `ravedash` so this function can be used by `ravecore` and other depending packages
* Added print method to background jobs
* Added `mirai` as the optional background engine to run jobs
* Pipeline evaluation will have `RAVE_PIPELINE_ACTIVE` environment set to `"true"` to allow fast compile and debugging
* `pipeline$read` once disabled by `targets` during evaluation can now runs again with hacks
* Pipeline attach method also mounts shared environment for debugging
* Implemented pipeline reporting functions
* Added `start_job` to start background jobs, which can be converted into `promises`
* Make sure installing new snippets overwrites existing scripts
* Unset reporters for older versions of targets
* Fixed lazy evaluation issues
* Fixed a bug introduced by `targets` version `1.11.1` where use-after-free of `cli` progress crashes the pipeline run, the newer versions of that package has fixed it too
* Added optional functions to handle old `raveio` calls 
* Removed questions once asked
* save `yaml` format now sort native lists with `sorted=TRUE`
* Removed `call_pkg_fun` for `rstudioapi` and use the package functions directly to avoid messing up with `sys.call()`
* Fixed `entrace`
* Added pipeline collections

# ravepipeline 0.0.1

* Initial CRAN submission.
