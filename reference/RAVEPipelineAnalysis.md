# Modular analysis unit for 'RAVE' pipelines

A light-weight container that describes one analysis as a handful of
plain functions. The analysis never stores a pipeline: the 'RAVE'
dashboard (the controller) passes the pipeline, the shiny session, or
the values of the prerequisite pipeline targets to the methods that need
them. Methods whose names start with `@` are called by the controller
(the dashboard or the pipeline), not by analysis developers.

When a pipeline is compiled, every analysis defined at the top level of
a `R/shared-*.R` script becomes the pipeline target named
`results_target_name`, plus the target `analysis_cleaned_inputs_<name>`
when the analysis has a custom pipeline collector. The pipeline settings
file must contain the key `inputs_settings_name`; its value can start as
an empty list.

## Public fields

- `description`:

  a short text describing the analysis

## Active bindings

- `name`:

  analysis name, read-only

- `input_names`:

  names of the registered inputs, read-only

- `options`:

  named list of options passed to the analyze and visualize steps;
  assigning replaces the whole list and must be a named list
  ([`list()`](https://rdrr.io/r/base/list.html) clears it). Use
  `set_option` to change individual options

- `pipeline_targets`:

  names of the pipeline targets whose values the `preprocess` step
  receives, read-only; see `set_preprocess`

- `inputs_settings_name`:

  name of the pipeline settings that holds the saved input values,
  read-only

- `results_target_name`:

  name of the pipeline target that holds the analysis result, read-only

- `@ns`:

  shiny namespace function: `ns(id)` adds the namespace prefix to `id`,
  and `ns(NULL)` returns the prefix

## Methods

### Public methods

- [`RAVEPipelineAnalysis$set_option()`](#method-RAVEPipelineAnalysis-set_option)

- [`RAVEPipelineAnalysis$new()`](#method-RAVEPipelineAnalysis-initialize)

- [`RAVEPipelineAnalysis$get_id()`](#method-RAVEPipelineAnalysis-get_id)

- [`RAVEPipelineAnalysis$set_input_ui()`](#method-RAVEPipelineAnalysis-set_input_ui)

- [`RAVEPipelineAnalysis$@render_input()`](#method-RAVEPipelineAnalysis-@render_input)

- [`RAVEPipelineAnalysis$render_inputs()`](#method-RAVEPipelineAnalysis-render_inputs)

- [`RAVEPipelineAnalysis$set_collect_inputs_from_shiny()`](#method-RAVEPipelineAnalysis-set_collect_inputs_from_shiny)

- [`RAVEPipelineAnalysis$@collect_inputs_from_shiny()`](#method-RAVEPipelineAnalysis-@collect_inputs_from_shiny)

- [`RAVEPipelineAnalysis$set_collect_inputs_from_pipeline()`](#method-RAVEPipelineAnalysis-set_collect_inputs_from_pipeline)

- [`RAVEPipelineAnalysis$@collect_inputs_from_pipeline()`](#method-RAVEPipelineAnalysis-@collect_inputs_from_pipeline)

- [`RAVEPipelineAnalysis$set_store_inputs_to_pipeline()`](#method-RAVEPipelineAnalysis-set_store_inputs_to_pipeline)

- [`RAVEPipelineAnalysis$@store_inputs_to_pipeline()`](#method-RAVEPipelineAnalysis-@store_inputs_to_pipeline)

- [`RAVEPipelineAnalysis$@test_roundtrip_pipeline_inputs()`](#method-RAVEPipelineAnalysis-@test_roundtrip_pipeline_inputs)

- [`RAVEPipelineAnalysis$set_shiny_server()`](#method-RAVEPipelineAnalysis-set_shiny_server)

- [`RAVEPipelineAnalysis$@shiny_server()`](#method-RAVEPipelineAnalysis-@shiny_server)

- [`RAVEPipelineAnalysis$set_preprocess()`](#method-RAVEPipelineAnalysis-set_preprocess)

- [`RAVEPipelineAnalysis$@preprocess_data()`](#method-RAVEPipelineAnalysis-@preprocess_data)

- [`RAVEPipelineAnalysis$set_analyze()`](#method-RAVEPipelineAnalysis-set_analyze)

- [`RAVEPipelineAnalysis$@analyze_data()`](#method-RAVEPipelineAnalysis-@analyze_data)

- [`RAVEPipelineAnalysis$set_visualize()`](#method-RAVEPipelineAnalysis-set_visualize)

- [`RAVEPipelineAnalysis$@visualize_data()`](#method-RAVEPipelineAnalysis-@visualize_data)

- [`RAVEPipelineAnalysis$run()`](#method-RAVEPipelineAnalysis-run)

- [`RAVEPipelineAnalysis$run_as_task()`](#method-RAVEPipelineAnalysis-run_as_task)

- [`RAVEPipelineAnalysis$@build_targets()`](#method-RAVEPipelineAnalysis-@build_targets)

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_option()`

Set options one key at a time; this is how to change individual options,
since `analysis$options$key <- value` is not reliable on an active
binding

#### Usage

    RAVEPipelineAnalysis$set_option(..., .list = list(), .clear_first = FALSE)

#### Arguments

- `..., .list`:

  named options; each replaces the whole option of the same name (a
  nested list is not merged), and a `NULL` value is kept as `NULL`.
  `.list` takes precedence over `...` for the same name

- `.clear_first`:

  whether to remove all existing options first

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$new()`

Constructor

#### Usage

    RAVEPipelineAnalysis$new(
      name,
      namespace,
      description = gsub("[_]+", " ", name)
    )

#### Arguments

- `name`:

  analysis name, a single string of letters, digits, and underscores
  that starts with a letter; used as the prefix of the input identifiers

- `namespace`:

  shiny module namespace under which the inputs are rendered, usually
  the module ID; a single non-empty string

- `description`:

  a short text describing the analysis; default is the name with
  underscores replaced by spaces

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$get_id()`

Get the identifier of an input or output element

#### Usage

    RAVEPipelineAnalysis$get_id(id, with_namespace = FALSE)

#### Arguments

- `id`:

  input or output name, such as the `input_name` passed to
  `set_input_ui`

- `with_namespace`:

  whether to add the shiny namespace prefix; default is false, which
  gives the identifier used inside the module server (for example
  `input[[id]]` or `output[[id]]`)

#### Returns

A character vector of identifiers

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_input_ui()`

Register the function that renders an input

#### Usage

    RAVEPipelineAnalysis$set_input_ui(input_name, ui_func)

#### Arguments

- `input_name`:

  input name, a single string; the collected value uses this name

- `ui_func`:

  `function(inputId, ns, restored_inputs)` returning the input element,
  or `NULL` to remove the input. `inputId` is the identifier without the
  shiny namespace (the same as `get_id(input_name)`), and `ns` is the
  namespace function `@ns`: give the element the identifier
  `ns(inputId)`, and use `inputId` as is wherever the namespace is added
  separately

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@render_input()`

Render an input registered by `set_input_ui`

#### Usage

    RAVEPipelineAnalysis$@render_input(input_name, pipeline)

#### Arguments

- `input_name`:

  input name

- `pipeline`:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance from which the saved input values are restored, see
  `@collect_inputs_from_pipeline`

#### Returns

The value returned by the input function, which receives the identifier
without namespace, the namespace function `@ns`, and the restored input
values; `NULL` invisibly if the input is not registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$render_inputs()`

Render all registered inputs, each showing the input values saved in the
pipeline; requires htmltools

#### Usage

    RAVEPipelineAnalysis$render_inputs(pipeline)

#### Arguments

- `pipeline`:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance from which the saved input values are restored

#### Returns

An htmltools tag list of the rendered inputs

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_collect_inputs_from_shiny()`

Register the function that collects the input values from shiny

#### Usage

    RAVEPipelineAnalysis$set_collect_inputs_from_shiny(collect_func)

#### Arguments

- `collect_func`:

  `function(session)` returning the input values as a named list, where
  `session` is scoped to the analysis `namespace`; `NULL` restores the
  default, which reads the registered inputs within
  [`shiny::isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html). A
  custom function is called as is, so it should isolate its own reads if
  it may run outside of a reactive context

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@collect_inputs_from_shiny()`

Collect the input values from a shiny session

#### Usage

    RAVEPipelineAnalysis$@collect_inputs_from_shiny(session)

#### Arguments

- `session`:

  shiny session; any scope works, since the values are read under the
  analysis `namespace`

#### Returns

A named list of input values; by default one per registered input, which
is `NULL` if the session does not have it

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_collect_inputs_from_pipeline()`

Register the function that restores the input values from a pipeline

#### Usage

    RAVEPipelineAnalysis$set_collect_inputs_from_pipeline(collect_func)

#### Arguments

- `collect_func`:

  `function(pipeline_settings)` returning the input values as a named
  list, where `pipeline_settings` is the named list of pipeline
  settings; `NULL` restores the default, which reads the settings named
  `inputs_settings_name`. The settings are resolved in the dashboard and
  when the pipeline is compiled, but come straight from the settings
  file when the pipeline runs, so settings stored as external data
  differ between the two

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@collect_inputs_from_pipeline()`

Restore the input values saved in a pipeline

#### Usage

    RAVEPipelineAnalysis$@collect_inputs_from_pipeline(pipeline_settings)

#### Arguments

- `pipeline_settings`:

  named list of pipeline settings, or a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance whose settings are used

#### Returns

A named list of input values; by default the pipeline settings named
`inputs_settings_name`, or an empty list if no values have been saved

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_store_inputs_to_pipeline()`

Register the function that converts the input values before they are
saved to a pipeline

#### Usage

    RAVEPipelineAnalysis$set_store_inputs_to_pipeline(store_func)

#### Arguments

- `store_func`:

  `function(inputs, pipeline)` returning the named list to save as the
  pipeline settings named `inputs_settings_name`; its value is always
  saved. `NULL` restores the default, which saves the input values
  unchanged. This is the only step that receives the pipeline, so it may
  save some values as other settings; those are not part of the saved
  inputs, so list them in the `pipeline_targets` of `set_preprocess` if
  the analysis depends on them

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@store_inputs_to_pipeline()`

Save the input values to the pipeline settings named
`inputs_settings_name`

#### Usage

    RAVEPipelineAnalysis$@store_inputs_to_pipeline(inputs, pipeline)

#### Arguments

- `inputs`:

  input values, usually from `@collect_inputs_from_shiny`

- `pipeline`:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance

#### Returns

The saved value, which is the value returned by the store function (the
input values by default), invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@test_roundtrip_pipeline_inputs()`

Check that the input values survive being saved to a settings file and
read back; the check uses a temporary copy of the pipeline, so
`pipeline` is not changed

#### Usage

    RAVEPipelineAnalysis$@test_roundtrip_pipeline_inputs(pipeline)

#### Arguments

- `pipeline`:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance holding the input values to check

#### Returns

`TRUE` if the values read back are identical to the original ones,
otherwise `FALSE`

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_shiny_server()`

Register the shiny module server

#### Usage

    RAVEPipelineAnalysis$set_shiny_server(server_func)

#### Arguments

- `server_func`:

  `function(input, output, session)`, or `NULL` to remove the server

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@shiny_server()`

Start the shiny module server registered by `set_shiny_server`

#### Usage

    RAVEPipelineAnalysis$@shiny_server(session)

#### Arguments

- `session`:

  shiny session; any scope works, since the server always runs under the
  analysis `namespace`

#### Returns

The value returned by the server function; `NULL` invisibly if no server
is registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_preprocess()`

Register the `preprocess` step

#### Usage

    RAVEPipelineAnalysis$set_preprocess(preprocess_func, pipeline_targets = NULL)

#### Arguments

- `preprocess_func`:

  `function(value, pipeline_targets)` returning the processed values,
  which must hold everything the analyze step needs, or `NULL` to remove
  the step

- `pipeline_targets`:

  names of the pipeline targets that must be built before the
  `preprocess` step; their values are passed to the `preprocess` step
  only. Default is `NULL` (none)

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@preprocess_data()`

Process the collected input values before the analysis

#### Usage

    RAVEPipelineAnalysis$@preprocess_data(value, pipeline_targets = list())

#### Arguments

- `value`:

  input values, usually from `@collect_inputs_from_pipeline`

- `pipeline_targets`:

  named list of pipeline target values, which must include every target
  in the `pipeline_targets` field, for example
  `pipeline[analysis$pipeline_targets, simplify = FALSE]`

#### Returns

The processed values, or `value` if no `preprocess` step is registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_analyze()`

Register the analyze step

#### Usage

    RAVEPipelineAnalysis$set_analyze(analyze_func)

#### Arguments

- `analyze_func`:

  `function(value, options)` returning the analysis result, or `NULL` to
  remove the step

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@analyze_data()`

Run the analysis with the current `options`; this method does not call
`@preprocess_data`, so pass its result in. The analyze step receives
only the processed values and the options

#### Usage

    RAVEPipelineAnalysis$@analyze_data(value_processed)

#### Arguments

- `value_processed`:

  processed values, usually returned by `@preprocess_data`

#### Returns

An object of class `RAVEPipelineAnalysis_results`: a list with the
analysis name (`analysis_name`) and the analysis result (`results`),
which is `value_processed` if no analyze step is registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_visualize()`

Register the visualize step

#### Usage

    RAVEPipelineAnalysis$set_visualize(visualize_func)

#### Arguments

- `visualize_func`:

  `function(value, options)` that prints, plots, or writes text, or
  `NULL` to remove the step

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@visualize_data()`

Visualize the analysis result with the current `options`

#### Usage

    RAVEPipelineAnalysis$@visualize_data(value)

#### Arguments

- `value`:

  analysis result from `@analyze_data`, whose `results` are visualized;
  it must come from this analysis. A plain value that is not such a
  result is visualized as is

#### Returns

The value returned by the visualize function, visible or invisible as
that function returned it (so a returned plot object is printed at top
level or in a report chunk); `NULL` invisibly if no visualize step is
registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$run()`

Run the analysis with a pipeline, without the 'RAVE' dashboard

#### Usage

    RAVEPipelineAnalysis$run(
      pipeline,
      step = c("all", "inputs", "preprocess", "analyze", "visualize"),
      eval_method = c("run", "debug"),
      session = NULL,
      visualization_method = c("direct", "html"),
      ...
    )

#### Arguments

- `pipeline`:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance

- `step`:

  where to stop: `"inputs"` returns the collected input values,
  `"preprocess"` the processed values, and `"analyze"` the analysis
  result; `"all"` and `"visualize"` are the same, and also run the
  visualize step

- `eval_method`:

  `"run"` saves the input values to the pipeline and builds the target
  `results_target_name`, which must exist; `"debug"` computes the
  analysis in this session without that target, for analyses not yet
  compiled into the pipeline, and does not save the input values. In
  debug mode, and for `step` set to `"preprocess"`, the prerequisite
  targets are read from the pipeline, so they must have been built

- `session`:

  shiny session to collect the input values from; default is `NULL`,
  which restores them from the pipeline settings

- `visualization_method`:

  `"direct"` calls the visualize step; `"html"` renders it as an `HTML`
  fragment, which requires rmarkdown

- `...`:

  passed to the `run` method of the pipeline when `eval_method` is
  `"run"`

#### Returns

Depends on `step`: the input values, the processed values, the analysis
result, or the value returned by the visualize step (an htmltools `HTML`
fragment if `visualization_method` is `"html"`)

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$run_as_task()`

Save the input values to the pipeline, then build the target
`results_target_name` as a shiny extended task

#### Usage

    RAVEPipelineAnalysis$run_as_task(pipeline, session = NULL, ...)

#### Arguments

- `pipeline`:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance

- `session`:

  shiny session to collect the input values from; default is `NULL`,
  which restores them from the pipeline settings

- `...`:

  passed to the `run_as_task` method of the pipeline

#### Returns

The task returned by the `run_as_task` method of the pipeline

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$@build_targets()`

Create the pipeline target specifications for this analysis; called when
the pipeline is compiled

#### Usage

    RAVEPipelineAnalysis$@build_targets(varname, format = NULL, cue = "thorough")

#### Arguments

- `varname`:

  name of the variable that holds this analysis in the pipeline
  environment; the generated code refers to it

- `format, cue`:

  storage format and `targets` cue of the results target

#### Returns

A list of target specifications: the cleaned-inputs target (only with a
custom pipeline collector), then the results target

## Examples

``` r
if (FALSE) { # \dontrun{

# 1. R/shared-analysis.R -- define the analysis; adapted from the
#    streamline collision detection of the 'RAVE' 3D viewer module
analysis <- RAVEPipelineAnalysis$new(
  name = "streamline_collision_detection",
  namespace = "custom_3d_viewer",
  description = "Streamline collision detection"
)

# An input function receives `inputId` without the namespace, and gives
# the input `ns(inputId)`
analysis$set_input_ui("mode_x", function(inputId, ns, restored_inputs) {
  shiny::selectInput(
    inputId = ns(inputId),
    label = "Mode for ROI objects",
    choices = c("auto", "volume", "pointcloud", "surface"),
    selected = restored_inputs$mode_x %||% "auto"
  )
})
analysis$set_input_ui("radius", function(inputId, ns, restored_inputs) {
  shiny::tagList(
    shiny::numericInput(
      inputId = ns(inputId),
      label = "Radius (mm)",
      value = restored_inputs$radius %||% 0,
      min = 0,
      step = 0.1
    ),
    # `conditionalPanel()` adds the namespace itself, so its condition
    # refers to the input by `inputId`
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] > 0", inputId),
      ns = ns,
      shiny::helpText("ROI objects are expanded by this radius.")
    )
  )
})

# The module server uses the same identifiers
analysis$set_shiny_server(function(input, output, session) {
  shiny::observeEvent(input[[analysis$get_id("mode_x")]], {
    # a point cloud has no volume, so give it a positive radius
    if (input[[analysis$get_id("mode_x")]] == "pointcloud") {
      shiny::updateNumericInput(session, analysis$get_id("radius"), value = 1)
    }
  })
})

# The preprocess step gathers what the analysis needs, including the
# pipeline targets it declares
analysis$set_preprocess(
  pipeline_targets = c("loaded_brain_info", "analysis_objects"),
  preprocess_func = function(value, pipeline_targets) {
    list(
      brain = pipeline_targets$loaded_brain_info$brain,
      objects = pipeline_targets$analysis_objects,
      mode_x = value$mode_x %||% "auto",
      radius = value$radius %||% 0
    )
  }
)

# `detect_collision()` stands for the detection code, which lives in
# another `R/shared-*.R` script of the module
analysis$set_analyze(function(value, options) {
  detect_collision(value$brain, value$objects,
                   mode_x = value$mode_x, radius = value$radius)
})
analysis$set_visualize(function(value, options) {
  print(value)
})

# 2. settings.yaml -- the key that holds the saved inputs, empty at first
#    analysis_inputs_streamline_collision_detection: []

# 3. The dashboard -- render the inputs with the values saved in the
#    pipeline
pipeline <- pipeline("custom_3d_viewer")
analysis$render_inputs(pipeline)

# In the dashboard server function: start the analysis server; to run,
# save the inputs from the session and build the analysis result
analysis$`@shiny_server`(session)
analysis$run(pipeline, session = session, visualization_method = "html")

# Without the dashboard, run with the inputs saved in the pipeline
analysis$run(pipeline)
} # }
```
