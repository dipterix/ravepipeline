# Using RAVE Pipelines Programmatically

``` r
library(ravepipeline)
```

## Overview

This vignette demonstrates how to programmatically interact with RAVE
pipelines using the `PipelineTools` R6 class, accessible via
[`ravepipeline::pipeline()`](http://dipterix.org/ravepipeline/reference/pipeline.md)
and
[`ravepipeline::pipeline_from_path()`](http://dipterix.org/ravepipeline/reference/pipeline.md).
This interface allows you to:

- Load and inspect pipelines
- Configure pipeline parameters (inputs, options)
- Execute pipelines with progress monitoring
- Retrieve and analyze results
- Generate reports

This document serves dual purposes:

1.  **User guide**: Learn how to use pipelines in scripts and workflows
2.  **AI assistant reference**: Instructions for AI agents to help users
    work with RAVE pipelines via the Model Context Protocol (MCP)

## Installation and Setup

The full RAVE suite installation provides built-in pipelines and demo
data. Please refer to
<https://rave.wiki/posts/installation/installation.html> on how to
install RAVE.

Once installed, the demo data is automatically available:

- **Project**: `"demo"`
- **Subject**: `"DemoSubject"`
- **Pipeline**: `"power_explorer"` (and others)

### Discovering Available Pipelines

List all installed pipelines:

``` r
# List all available pipelines
available_pipelines <- ravepipeline::pipeline_list()
print(available_pipelines)
```

## Creating a Pipeline Instance

The primary interface is the
[`pipeline()`](http://dipterix.org/ravepipeline/reference/pipeline.md)
function, which returns a `PipelineTools` object:

``` r
# Load the power_explorer pipeline
pipe <- ravepipeline::pipeline("power_explorer")

# Alternative: load from a specific path
# pipe <- ravepipeline::pipeline_from_path("/path/to/pipeline/folder")
```

## Inspecting Pipeline Structure

### Pipeline Metadata

View pipeline description and metadata:

``` r
# Get pipeline description
desc <- pipe$description
cat("Pipeline:", desc$Package, "\n")
cat("Version:", desc$Version, "\n")
cat("Title:", desc$Title, "\n")
```

### Available Targets

Examine what the pipeline computes:

``` r
# View all pipeline targets (computational steps)
target_table <- pipe$target_table
print(target_table)

# Target table shows:
# - Names: target (build variable) identifier
# - Description: description of the target
```

### Available Reports

Check what reports can be generated:

``` r
# List available report templates
reports <- pipe$available_reports
print(reports)
```

### Pipeline Settings

Get current pipeline settings:

``` r
# Retrieve all current settings
all_settings <- pipe$get_settings()
str(all_settings)
```

## Configuring Pipeline Parameters

### Setting Input Parameters

Use `$set_settings()` to configure pipeline inputs:

``` r
# Configure power_explorer for demo data
pipe$set_settings(
  project_name = "demo",
  subject_code = "DemoSubject",
  reference_name = "default",
  epoch_choice = "auditory_onset",
  epoch_choice__trial_starts = -1L,
  epoch_choice__trial_ends = 2L,
  second_condition_groupings = list(
    list(label = "ML", conditions = list()),
    list(label = "VL", conditions = list())
  ),
  omnibus_includes_all_electrodes = FALSE,
  loaded_electrodes = "13-16,24",
  first_condition_groupings = list(
    list(
      label = "audio_visual",
      conditions = c("known_av", "meant_av", "last_av", "drive_av")
    ),
    list(
      label = "auditory_only",
      conditions = c("last_a", "drive_a", "known_a", "meant_a")
    ),
    list(
      label = "visual_only",
      conditions = c("last_v", "drive_v", "known_v", "meant_v")
    )
  ),
  condition_variable = "Condition",
  baseline_settings = list(
    window = list(c(-1, -0.5)),
    scope = "Per frequency, trial, and electrode",
    unit_of_analysis = "% Change Power"
  ),
  analysis_settings = list(
    list(
      label = "VisStart",
      event = "Trial Onset",
      time = list(0L, 0.5),
      frequency_dd = "Select one",
      frequency = c(70L, 150L)
    )
  ),
  analysis_electrodes = "14"
)
```

### Retrieving Specific Settings

Get individual settings with defaults:

``` r
# Get a specific setting
project <- pipe$get_settings("project_name")
print(project)

# Get with default if missing
missing_val <- pipe$get_settings("nonexistent_key", default = "N/A")
print(missing_val)

# Get with constraint (ensures value is in allowed set)
reference <- pipe$get_settings(
  "reference_name",
  constraint = c("default", "CAR", "bipolar")
)
```

## Running the Pipeline

### Synchronous Execution

Run the pipeline and wait for completion:

``` r
# Run all targets: this will run all build targets (not recommended)
# pipe$run()

# Run specific targets only
pipe$run(names = c("repository", "baselined_power", "over_time_by_condition_data"))
```

> The pipeline will automatically determine which targets to update: for
> example, `"over_time_by_condition_data"` depends on target
> `"repository"` and `"baselined_power"`. Even if user does not specify
> to run `"repository"` or `"baselined_power"` explicitly, pipeline will
> still evaluate this target, hence

``` r
pipe$run(names = c("repository", "baselined_power", "over_time_by_condition_data"))
```

runs the same underlying code as

``` r
pipe$run(names = "over_time_by_condition_data")
```

The only difference is the first line returns the value of all three
targets, while the second line only returns target result for
`"over_time_by_condition_data"`

### Asynchronous Execution (experimental)

Run pipeline in background with progress tracking:

``` r
# Start pipeline execution as a background job
# This is the recommended way to run pipelines without blocking
job_id <- ravepipeline::start_job(
  name = "power_explorer_analysis",

  # Run this function as a background job
  fun = function(pipe) {
    pipe$run(names = c("repository", "over_time_by_condition_data"))
  },

  # expose `pipe` to background job
  fun_args = list(pipe = pipe)
)

# Check job status
job_results <- ravepipeline::resolve_job(job_id)
print(job_results)
```

### Execution Options

Control how the pipeline runs:

``` r
# Run with parallel processing
pipe$run(
  names = "over_time_by_condition_data",
  scheduler = "future",  # Use future for parallel execution
  return_values = FALSE  # Do not return values (default is TRUE)
)

# Run with each target with isolated clean environment (but slower)
pipe$run(
  names = "over_time_by_condition_data",
  type = "callr",
  return_values = FALSE
)

# Debug mode (more verbose output)
pipe$run(
  names = "over_time_by_condition_data",
  debug = TRUE,
  return_values = FALSE
)
```

## Monitoring Pipeline Execution

### Progress Summary

Check pipeline progress:

``` r
# Quick summary
pipe$progress("summary")

# Detailed progress for each target
pipe$progress("details")
```

`completed` means the target was executed; `skipped` means the target
has been executed before but skipped because the previous results can be
reused (typically the depending target results remain unchanged during
the session); `errored` means the target ran into an error.

### Result Table

View results with data signatures:

``` r
# Get table of results with metadata
results_summary <- pipe$result_table
str(results_summary, max.level = 2)

# tibble [30 × 18] (S3: tbl_df/tbl/data.frame)
#   $ name      : chr [1:30] "settings_path" "analysis_electrodes" ...
#   $ type      : chr [1:30] "stem" "stem" "stem" "stem" ...
#   $ data      : chr [1:30] "87464f5e9f50ed67" "ea945b95c2940f9e" ...
#   $ command   : chr [1:30] "0bc648d2b33e5acd" "7272b9187fc425a0" ...
#   $ depend    : chr [1:30] "2c530c1562a7fbd1" "7ac856221f002d65" ...
#   $ seed      : int [1:30] 853402860 -847753771 -444014686 1434286487 ...
#   $ path      :List of 30
#   $ time      : POSIXct[1:30], format: "2026-01-19 14:47:39" ...
#   $ size      : chr [1:30] "s1596b" "s55b" "s199b" "s62b" ...
#   $ bytes     : num [1:30] 1596 55 199 62 48 ...
#   $ format    : chr [1:30] "file" "rds" "rds" "rds" ...
#   $ repository: chr [1:30] "local" "local" "local" "local" ...
#   $ iteration : chr [1:30] "vector" "vector" "vector" "vector" ...
#   $ parent    : chr [1:30] NA NA NA NA ...
#   $ children  :List of 30
#   $ seconds   : num [1:30] 0 0 0 0 0 0 0 0 0 0 ...
#   $ warnings  : chr [1:30] NA NA NA NA ...
#   $ error     : chr [1:30] NA NA NA NA ...
```

### Dependency Visualization

Visualize pipeline structure: both method shows graph dependencies using
`visNetwork` package, with `glimpse = TRUE` only showing the dependency
while `glimpse = FALSE` (default) also shows the target node status.

``` r
# Interactive dependency graph with evaluation flow
pipe$visualize()

# Quick glimpse of interactive dependency graph without evaluations
pipe$visualize(glimpse = TRUE)
```

## Retrieving Pipeline Results

### Reading All Results

Get all computed results:

``` r
# Read all pipeline outputs
all_results <- pipe$read()
names(all_results)
```

### Reading Specific Results

Retrieve individual or multiple results:

``` r
# Read single result
repository <- pipe$read("repository")

# Read multiple results
results <- pipe$read(c("repository", "over_time_by_condition_data"))

# With default for missing results with a fallback `NULL`
safe_result <- pipe$read("maybe_missing", ifnotfound = NULL)
```

### Using Bracket Notation

Shorthand syntax for reading results:

``` r
# Equivalent to pipe$read()
power_data <- pipe["repository"]

# Multiple results
results <- pipe[c("repository", "over_time_by_condition_data")]

# Exclude specific results (get everything except these)
most_results <- pipe[-c("over_time_by_condition_data")]
```

### Reading with Dependencies

Include upstream dependencies: this is useful in getting all the data
used to generate the targets

``` r
# Read target and all its dependencies
full_result <- pipe$read(
  "repository",
  dependencies = "all"
)

# Read target and direct ancestors only
partial_result <- pipe$read(
  "repository",
  dependencies = "ancestors_only"
)
```

## Advanced Features

### Interactive Debugging with eval()

Evaluate targets step-by-step without using the cache:

``` r
# Run specific targets in order, get environment with results
env <- pipe$eval(
  names = c("repository", "baselined_power", "trial_details")
)

# Inspect intermediate results
ls(env)
head(env$trial_details)

# Use shortcut to skip already-loaded/executed dependencies
# Warning: the expired dependencies will not be evaluated so
# only use it for debugging or quick updating the target
env <- pipe$eval(
  names = "over_time_by_condition_data",
  shortcut = TRUE  # Skip deps already in environment
)
```

### Use Pipeline Runtime Helpers

Pipeline typically generates intermediate data. Users still need to use
the data for visualizations. For example

``` r
plot_data <- pipe$read("over_time_by_condition_data")
str(plot_data, max.level = 1)
# List of 3
#  $ audio_visual :List of 1
#  $ auditory_only:List of 1
#  $ visual_only  :List of 1
```

The data is a list of data. To visualize the data, pipelines may have
helpers that visualize the generated data. These helpers can be loaded
via

``` r
# Load runtime environment
runtime_env <- pipe$shared_env()

# Internal helpers
ls(runtime_env)

# Plot the over_time_by_condition_data
runtime_env$plot_over_time_by_condition(plot_data)
```

### External Data Management

RAVE pipelines use `YAML` format to store the input settings and
options. Some pipelines might require additional large data or data with
complicated structures.

``` r
additional_data <- array(rnorm(10000), dim = c(100, 100))

# Save large object to pipeline data folder
pipe$save_data(
  data = additional_data,
  name = "precomputed_matrix",
  format = "rds",  # Arbitrary R format
  overwrite = TRUE
)

# Load it back
matrix <- pipe$load_data("precomputed_matrix", format = "rds")

# Other formats: (for lists: "json", "yaml"), (for tables: "csv", "fst")
pipe$save_data(
  data = data.frame(
    a = letters
  ),
  name = "letters",
  format = "csv"
)
```

### Forking Pipelines

Create a copy of the pipeline:

``` r
# Copy pipeline to new location
# This example forks to temporary folder
new_pipe <- pipe$fork(
  path = file.path(tempfile(), pipe$pipeline_name),
  policy = "default"  # Copy policy
)

# The new pipeline is independent
new_pipe$pipeline_path
# [1] "..../RtmpCCl3Hf/file1565d24c91e48/power_explorer"
# attr(,"target_name")
# [1] "power_explorer"
# attr(,"target_script")
# [1] "make-power_explorer.R"
# attr(,"target_directory")
# [1] "shared"
```

### Preference Management (experimental)

*Not all built-in modules implement the same preference management
system. This depends on the module developers to implement. Here is the
guideline (recommended ways) to store preferences.*

Store `UI` preferences (non-analysis settings): the preference ID must
have syntax `[global/<module ID>].[type].[key]`. For example:
`global.graphics.ui_theme`, `power_explorer.graphics.plot_color`.
`Globals` are shared options across modules.

``` r
# Set persistent preferences (won't invalidate pipeline)
pipe$set_preferences(
  power_explorer.graphics.plot_color = "blue",
  global.graphics.ui_theme = "dark"
)

# Get preferences
color <- pipe$get_preferences(
  keys = "power_explorer.graphics.plot_color", 
  ifnotfound = "red")

# Check if preference exists
has_theme <- pipe$has_preferences(
  keys = "global.graphics.ui_theme"
)
```

### Report Generation

Generate analysis reports:

``` r
# List available reports
reports <- pipe$available_reports
print(reports)
# Example output for power_explorer:
# $univariatePower
#   $name: "univariatePower"
#   $label: "Univariate Power Spectrogram Analysis"
#   $entry: "report-univariate.Rmd"

# Generate a report
report_id <- pipe$generate_report(
  name = "univariatePower",  # Use the report name from available_reports
  output_dir = tempfile(),
  output_format = "html_document"
)
ravepipeline::resolve_job(report_id)
```

Because a report might take long to generate. The process typically runs
as a background job.

### Cleaning Pipeline Cache

Remove cached results:

``` r
# Clean cache (keep pipeline structure)
pipe$clean(destroy = "all")
```

## Understanding Pipeline Structure

RAVE pipelines typically use the
[`template-rmd`](https://github.com/rave-ieeg/rave-pipelines/tree/main/inst/rave-pipelines/template-rmd)
structure, where:

### Main Development File: `main.Rmd`

The `main.Rmd` file is where users write pipeline logic using R Markdown
with special `rave` code chunks:

```` markdown
```\{rave load_subject, export = "subject"\}
# This target loads subject instance
subject <- ravecore::new_rave_subject(
  project_name = project_name, 
  subject_code = subject_code
)
subject
```
````

Key features:

- **Chunk label**: becomes the target description (`load_subject` will
  be interpreted as `Load subject`) from the `pipeline$target_table`
- **`export` parameter**: becomes the target name, and also a variable
  to make available to subsequent chunks: this means the code chunk must
  create a variable `subject`.
- **`deps` parameter**: automatically detected or specified with `deps`
  parameter: this chunk requires variable `project_name` and
  `subject_code`. These variables are typically generated by previous
  chunks or defined in the settings file (see below). Upon building the
  file, RAVE automatically detects these dependencies and assigns
  `deps=c("project_name", "subject_code")`
- **`language` parameter**: the back-bone language of the block: RAVE
  supports running `R` and `python`. The default option is `"R"`, and
  `"python"` is experimental.

### Settings File: settings.yaml

Stores user inputs that configure the pipeline:

``` yaml
project_name: "demo"
subject_code: "DemoSubject"
loaded_electrodes: "14-16,24"
epoch_choice: "auditory_onset"
```

These settings are accessible as variables in the pipeline code.

### Configuration Files

- **`RAVE-CONFIG`** or **`DESCRIPTION`**: Pipeline metadata (name,
  version, dependencies)
- **`common.R`**: Shared initialization code internally used by the
  `ravepipeline` package

### Generated Scripts

When you compile `main.Rmd`, it generates:

- **`make-*.R`**: Target definitions for the `targets` package
- Pipeline builds these automatically when needed

### Shared Functions: R/shared-\*.R

Helper functions and targets configuration:

``` r
# R/shared-functions.R
my_helper <- function(x, y) {
  # Reusable pipeline function
}
```

Then function `my_helper` will be available during the runtime, and RAVE
pipeline chunks will have access to this function automatically.

### Compilation Workflow

1.  Edit `main.Rmd` with your analysis logic
2.  Knit the document (or call `ravepipeline::pipeline_build(pipe_dir)`)
3.  Generated `make-*.R` scripts define pipeline targets
4.  Use
    [`ravepipeline::pipeline_from_path()`](http://dipterix.org/ravepipeline/reference/pipeline.md)
    to load and execute
