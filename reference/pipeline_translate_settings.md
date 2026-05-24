# Translate pipeline settings between pipelines

Translate pipeline settings between pipelines using export and/or import
wizard functions defined in each pipeline's `R/import-export-wizard.R`
file. `pipeline_export_wizard` and `pipeline_import_wizard` register
those wizard functions.

## Usage

``` r
pipeline_translate_settings(src_pipeline, dst_pipeline, settings = NULL)

pipeline_export_wizard(fun, pipeline_name, env = parent.frame())

pipeline_import_wizard(fun, pipeline_name, env = parent.frame())
```

## Arguments

- src_pipeline:

  character name or a `PipelineTools` instance of the source pipeline
  whose settings are being translated.

- dst_pipeline:

  character name or a `PipelineTools` instance of the destination
  pipeline to translate the settings into.

- settings:

  named list of settings to translate. If `NULL` (default), the current
  settings of `src_pipeline` are read automatically.

- fun:

  a function with signature `function(settings)` that performs the
  settings translation and returns the modified settings list.

- pipeline_name:

  character; the pipeline name this wizard handles, with
  context-dependent meaning:

  - In `pipeline_export_wizard`: the **destination** pipeline name — the
    wizard is invoked when exporting the current pipeline's settings
    *to* `pipeline_name` (corresponds to `dst_pipeline_name` in
    `pipeline_translate_settings`).

  - In `pipeline_import_wizard`: the **source** pipeline name — the
    wizard is invoked when importing settings *from* `pipeline_name`
    into the current pipeline (corresponds to `src_pipeline_name` in
    `pipeline_translate_settings`).

- env:

  environment in which to register the wizard. Defaults to the calling
  frame, i.e. the sourced `import-export-wizard.R` environment.

## Value

- `pipeline_translate_settings`:

  A named list of translated settings compatible with
  `dst_pipeline_name`.

- `pipeline_export_wizard`, `pipeline_import_wizard`:

  `fun`, invisibly. Called for the side effect of registering the wizard
  in `env`.

## Details

Translation proceeds in up to two passes:

1.  **Export pass**: the source pipeline may declare an export wizard
    keyed by `dst_pipeline_name`; if present it converts the settings
    into the destination format.

2.  **Import pass**: the destination pipeline may declare an import
    wizard keyed by the (possibly already-converted) source pipeline
    name; if present it applies an additional filter. This also handles
    the case where the destination pipeline defines a self-filter
    applied after the export pass.

At least one wizard must exist; otherwise an error is raised.

## Examples

``` r


if (FALSE) { # \dontrun{

# Translate settings from "pipelineA" to "pipelineB"
new_settings <- pipeline_translate_settings(
  src_pipeline = "pipelineA",
  dst_pipeline = "pipelineB"
)

# To achieve this, you would define export and/or import wizards in the
# respective pipelines.

# Option 1: Inside the source pipeline (pipelineA):
# file `R/import-export-wizard.R`, define an export wizard for pipelineB:

pipeline_export_wizard(
  pipeline_name = "pipelineB",
  fun = function(settings) {
    # settings is the current settings list of pipelineA
    settings$frequency_range <- settings$freq_range
    settings$freq_range <- NULL
    settings
  }
)

# Option 2: Inside the destination pipeline (pipelineB):
# file `R/import-export-wizard.R`, define an import wizard for pipelineA:

pipeline_import_wizard(
  pipeline_name = "pipelineA",
  fun = function(settings) {
    # settings is the current settings list of pipelineA
    settings$frequency_range <- settings$freq_range
    settings$freq_range <- NULL
    settings
  }
)

} # }
```
