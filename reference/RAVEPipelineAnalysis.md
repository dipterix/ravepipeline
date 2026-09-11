# Modular analysis unit for 'RAVE' pipelines

A light-weight container that lets module developers describe one
analysis as a handful of plain functions, while the 'RAVE' dashboard
(the controller) decides when to render the inputs, collect their
values, run the analysis, and show the results. Every step is optional,
and none of them requires shiny.

## Details

A controller runs an analysis in the following order; without registered
functions, `preprocess_data` and `analyze_data` return their input
unchanged, and `visualize_data` does nothing:


    value  <- analysis$collect_inputs(input)
    value  <- analysis$preprocess_data(value)
    result <- analysis$analyze_data(value)
    analysis$visualize_data(result)

Developers register the steps with the `set_*` methods. Each step
function must accept the arguments listed below, by name; a function
with a `...` formal argument accepts all of them.

- `set_input_ui`:

  `function(inputId, pipeline)`; returns whatever the dashboard renders,
  usually a shiny input whose identifier must be `inputId`

- `set_shiny_server`:

  `function(input, output, session)`; a shiny module server running in
  the analysis `namespace`

- `set_preprocess`:

  `function(value, pipeline)`; converts the collected input values into
  analysis parameters, and should call
  [`stop()`](https://rdrr.io/r/base/stop.html) to reject invalid values
  before the analysis runs

- `set_analyze`:

  `function(value, pipeline, options)`; runs synchronously on the output
  of `preprocess_data`, typically saving it with
  `pipeline$set_settings()` before running or reading pipeline targets,
  and returns the analysis result

- `set_visualize`:

  `function(value, pipeline, options)`; receives the analysis result and
  prints, plots, or writes text; the caller (for example an R Markdown
  chunk) captures the output

Options are a named list shared by the analyze and visualize steps. Set
the defaults with `analysis$options <- list(...)`. Extra named arguments
to `analyze_data` update the options and are kept for later calls, while
extra arguments to `visualize_data` apply to that call only. An option
whose name partially matches the first argument of these methods (for
example `val`) must be passed through `.list`.

Input identifiers are `"<name>__<input_name>"` (see `get_id`), placed
under the shiny module `namespace`, which defaults to the pipeline name;
standard 'RAVE' modules use the same name for the module and its
pipeline. Set `namespace` explicitly when they differ.

## Active bindings

- `name`:

  analysis name, read-only

- `input_names`:

  names of the registered inputs, read-only

- `options`:

  named list of options passed to the analyze and visualize steps;
  assigning replaces the whole list, and `NULL` clears it

- `pipeline`:

  the
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance that the analysis works with

- `ns`:

  shiny namespace function: `ns(id)` adds the namespace prefix to `id`,
  and `ns(NULL)` returns the prefix

## Methods

### Public methods

- [`RAVEPipelineAnalysis$new()`](#method-RAVEPipelineAnalysis-initialize)

- [`RAVEPipelineAnalysis$get_id()`](#method-RAVEPipelineAnalysis-get_id)

- [`RAVEPipelineAnalysis$set_input_ui()`](#method-RAVEPipelineAnalysis-set_input_ui)

- [`RAVEPipelineAnalysis$render_input()`](#method-RAVEPipelineAnalysis-render_input)

- [`RAVEPipelineAnalysis$collect_inputs()`](#method-RAVEPipelineAnalysis-collect_inputs)

- [`RAVEPipelineAnalysis$set_shiny_server()`](#method-RAVEPipelineAnalysis-set_shiny_server)

- [`RAVEPipelineAnalysis$shiny_server()`](#method-RAVEPipelineAnalysis-shiny_server)

- [`RAVEPipelineAnalysis$set_preprocess()`](#method-RAVEPipelineAnalysis-set_preprocess)

- [`RAVEPipelineAnalysis$preprocess_data()`](#method-RAVEPipelineAnalysis-preprocess_data)

- [`RAVEPipelineAnalysis$set_analyze()`](#method-RAVEPipelineAnalysis-set_analyze)

- [`RAVEPipelineAnalysis$analyze_data()`](#method-RAVEPipelineAnalysis-analyze_data)

- [`RAVEPipelineAnalysis$set_visualize()`](#method-RAVEPipelineAnalysis-set_visualize)

- [`RAVEPipelineAnalysis$visualize_data()`](#method-RAVEPipelineAnalysis-visualize_data)

- [`RAVEPipelineAnalysis$clone()`](#method-RAVEPipelineAnalysis-clone)

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$new()`

Constructor

#### Usage

    RAVEPipelineAnalysis$new(name, pipeline, namespace = pipeline$pipeline_name)

#### Arguments

- `name`:

  analysis name, a single string of letters, digits, and underscores
  that starts with a letter; used as the prefix of the input identifiers

- `pipeline`:

  a
  [`PipelineTools`](http://dipterix.org/ravepipeline/reference/PipelineTools.md)
  instance, see
  [`pipeline`](http://dipterix.org/ravepipeline/reference/pipeline.md)

- `namespace`:

  shiny module namespace under which the inputs are rendered; default is
  the pipeline name

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

  `function(inputId, pipeline)` returning the input element, or `NULL`
  to remove the input

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$render_input()`

Render an input registered by `set_input_ui`

#### Usage

    RAVEPipelineAnalysis$render_input(input_name)

#### Arguments

- `input_name`:

  input name

#### Returns

The value returned by the input function, which receives the identifier
with namespace; `NULL` invisibly if the input is not registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$collect_inputs()`

Collect the values of all registered inputs

#### Usage

    RAVEPipelineAnalysis$collect_inputs(input, with_namespace = FALSE)

#### Arguments

- `input`:

  a shiny `input` object, or any list, whose elements are the input
  values keyed by identifier

- `with_namespace`:

  whether the keys of `input` include the namespace; default is false,
  which matches the `input` inside the module server; set to true for
  the root session `input`

#### Returns

A named list of input values, one per registered input; a value is
`NULL` if `input` does not contain it

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

### `RAVEPipelineAnalysis$shiny_server()`

Start the shiny module server registered by `set_shiny_server`; must run
within a shiny session

#### Usage

    RAVEPipelineAnalysis$shiny_server(session = NULL)

#### Arguments

- `session`:

  shiny session; default is the current session. Any scope of the
  session works, since the server always runs under the analysis
  `namespace`

#### Returns

The value returned by the server function; `NULL` invisibly if no server
is registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_preprocess()`

Register the `preprocess` step

#### Usage

    RAVEPipelineAnalysis$set_preprocess(preprocess_func)

#### Arguments

- `preprocess_func`:

  `function(value, pipeline)` returning the processed values, or `NULL`
  to remove the step

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$preprocess_data()`

Process the collected input values before the analysis

#### Usage

    RAVEPipelineAnalysis$preprocess_data(value)

#### Arguments

- `value`:

  input values, usually from `collect_inputs`

#### Returns

The processed values, or `value` if no `preprocess` step is registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_analyze()`

Register the analyze step

#### Usage

    RAVEPipelineAnalysis$set_analyze(analyze_func)

#### Arguments

- `analyze_func`:

  `function(value, pipeline, options)` returning the analysis result, or
  `NULL` to remove the step

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$analyze_data()`

Run the analysis; this method does not call `preprocess_data`, so pass
its result in

#### Usage

    RAVEPipelineAnalysis$analyze_data(value_processed, ..., .list = list())

#### Arguments

- `value_processed`:

  processed values, usually returned by `preprocess_data`

- `..., .list`:

  named options to update and keep in `options` before the analysis
  runs; `.list` takes precedence over `...` for the same name

#### Returns

The analysis result, or `value_processed` if no analyze step is
registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$set_visualize()`

Register the visualize step

#### Usage

    RAVEPipelineAnalysis$set_visualize(visualize_func)

#### Arguments

- `visualize_func`:

  `function(value, pipeline, options)` that prints, plots, or writes
  text, or `NULL` to remove the step

#### Returns

The analysis object itself, invisibly

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$visualize_data()`

Visualize the analysis result

#### Usage

    RAVEPipelineAnalysis$visualize_data(value, ..., .list = list())

#### Arguments

- `value`:

  analysis result, usually from `analyze_data`

- `..., .list`:

  named options for this call only; `options` is restored afterwards.
  `.list` takes precedence over `...` for the same name

#### Returns

The value returned by the visualize function, visible or invisible as
that function returned it (so a returned plot object is printed at top
level or in a report chunk); `NULL` invisibly if no visualize step is
registered

------------------------------------------------------------------------

### `RAVEPipelineAnalysis$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVEPipelineAnalysis$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

if (FALSE) { # \dontrun{

# ---- A pipeline to work with ---------------------------------------
# Any 'RAVE' pipeline works; here is a bare template in a temporary folder
root_path <- tempfile()
pipeline_path <- pipeline_create_template(
  root_path = root_path, pipeline_name = "analysis_demo",
  overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")
pipe <- pipeline_from_path(pipeline_path)

# ---- Developer side: describe the analysis --------------------------
analysis <- RAVEPipelineAnalysis$new(name = "scatter", pipeline = pipe)

# Inputs return whatever the dashboard renders, usually shiny inputs;
# plain HTML strings here so the example does not need shiny
analysis$set_input_ui("n", function(inputId, pipeline) {
  n <- pipeline$get_settings("n", default = 100)
  paste0('<input id="', inputId, '" type="number" value="', n, '">')
})
analysis$set_input_ui("col", function(inputId, pipeline) {
  col <- pipeline$get_settings("col", default = "steelblue")
  paste0('<input id="', inputId, '" value="', col, '">')
})

# Pre-process: turn raw input values into analysis parameters
analysis$set_preprocess(function(value, pipeline) {
  value$n <- suppressWarnings(as.integer(value$n))
  if (is.na(value$n) || value$n < 2) {
    stop("`n` must be an integer greater than 1")
  }
  value
})

# Analyze: save the parameters to the pipeline, then compute
analysis$set_analyze(function(value, pipeline, options) {
  pipeline$set_settings(.list = value)
  if (length(options$seed)) {
    set.seed(options$seed)
  }
  x <- stats::rnorm(value$n)
  list(x = x, y = x + stats::rnorm(value$n), col = value$col)
})

# Visualize: any mix of printed text and plots
analysis$set_visualize(function(value, pipeline, options) {
  cat("Correlation:", round(stats::cor(value$x, value$y), 2), "\n")
  plot(value$x, value$y, col = value$col, pch = 16, main = options$main)
})

# Default options
analysis$options <- list(main = "Simulated data")

# ---- Controller side: what the dashboard does ----------------------
analysis$input_names
analysis$get_id("n")
analysis$get_id("n", with_namespace = TRUE)
analysis$render_input("n")

# `input` mimics shiny's `input`: values keyed by input identifier
input <- list(scatter__n = "50", scatter__col = "orange")

value <- analysis$collect_inputs(input)
value <- analysis$preprocess_data(value)

# `seed` is kept in `analysis$options` for later calls
result <- analysis$analyze_data(value, seed = 42)
analysis$options

# the analysis saved its parameters to the pipeline
pipe$get_settings("n")

# `main` applies to this call only
analysis$visualize_data(result, main = "One-off title")
analysis$options$main

# invalid input is rejected before the analysis runs
try(analysis$preprocess_data(list(n = "one", col = "red")))

# ---- Clean up -------------------------------------------------------
unlink(root_path, recursive = TRUE)

} # }
```
