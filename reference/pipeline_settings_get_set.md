# Get or change pipeline input parameter settings

Get or change pipeline input parameter settings

## Usage

``` r
pipeline_settings_set(
  ...,
  pipeline_path = Sys.getenv("RAVE_PIPELINE", "."),
  pipeline_settings_path = file.path(pipeline_path, "settings.yaml")
)

pipeline_settings_get(
  key,
  default = NULL,
  constraint = NULL,
  pipeline_path = Sys.getenv("RAVE_PIPELINE", "."),
  pipeline_settings_path = file.path(pipeline_path, "settings.yaml")
)
```

## Arguments

- pipeline_path:

  the root directory of the pipeline

- pipeline_settings_path:

  the settings file of the pipeline, must be a 'yaml' file; default is
  `'settings.yaml'` in the current pipeline

- key, ...:

  the character key(s) to get or set

- default:

  the default value is key is missing

- constraint:

  the constraint of the resulting value; if not `NULL`, then result must
  be within the `constraint` values, otherwise the first element of
  `constraint` will be returned. This is useful to make sure the results
  stay within given options

## Value

`pipeline_settings_set` returns a list of all the settings.
`pipeline_settings_get` returns the value of given key.

## Examples

``` r


root_path <- tempfile()
pipeline_root_folder <- file.path(root_path, "modules")

# create pipeline folder
pipeline_path <- pipeline_create_template(
  root_path = pipeline_root_folder, pipeline_name = "raveio_demo",
  overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")

# Set initial user inputs
yaml::write_yaml(
  x = list(
    n = 100,
    pch = 16,
    col = "steelblue"
  ),
  file = file.path(pipeline_path, "settings.yaml")
)

# build the pipeline for the first time
# this is a one-time setup
pipeline_build(pipeline_path)
#> [1] TRUE

# get pipeline settings
pipeline_settings_get(
  key = "n",
  pipeline_path = pipeline_path
)
#> [1] 100

# get variable with default if missing
pipeline_settings_get(
  key = "missing_variable",
  default = "missing",
  pipeline_path = pipeline_path
)
#> [1] "missing"

pipeline_settings_set(
  missing_variable = "A",
  pipeline_path = pipeline_path
)
#> <Map, size=4, keys=[missing_variable, col, pch, n]>

pipeline_settings_get(
  key = "missing_variable",
  default = "missing",
  pipeline_path = pipeline_path
)
#> [1] "A"


unlink(root_path, recursive = TRUE)

```
