# Add new 'RAVE' (2.0) module to current project

Creates a 'RAVE' pipeline with additional dashboard module from
template.

## Usage

``` r
module_add(
  module_id,
  module_label,
  path = ".",
  type = c("default", "bare", "scheduler", "python"),
  ...,
  pipeline_name = module_id,
  overwrite = FALSE
)
```

## Arguments

- module_id:

  module ID to create, must be unique; users cannot install two modules
  with identical module ID. We recommend that a module ID follows snake
  format, starting with lab name, for example,
  `'beauchamplab_imaging_preprocess'`, `'karaslab_freez'`, or
  `'upenn_ese25_fooof'`.

- module_label:

  a friendly label to display in the dashboard

- path:

  project root path; default is current directory

- type:

  template to choose, options are `'default'` and `'bare'`

- ...:

  additional configurations to the module such as `'order'`, `'group'`,
  `'badge'`

- pipeline_name:

  the pipeline name to create along with the module; default is
  identical to `module_id` (strongly recommended); leave it default
  unless you know what you are doing.

- overwrite:

  whether to overwrite existing module if module with same ID exists;
  default is false

## Value

Nothing.

## Examples

``` r

# For demonstrating this example only
project_root <- tempfile()
dir.create(project_root, showWarnings = FALSE, recursive = TRUE)


# Add a module
module_id <- "mylab_my_first_module"
module_add(
  module_id = module_id,
  module_label = "My Pipeline",
  path = project_root
)


# show the structure
cat(
  list.files(
    project_root,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = TRUE
  ),
  sep = "\n"
)
#> modules
#> modules.yaml
#> modules/mylab_my_first_module
#> modules/mylab_my_first_module/DESCRIPTION
#> modules/mylab_my_first_module/R
#> modules/mylab_my_first_module/R/aa.R
#> modules/mylab_my_first_module/R/aaa-presets.R
#> modules/mylab_my_first_module/R/loader.R
#> modules/mylab_my_first_module/R/module_html.R
#> modules/mylab_my_first_module/R/module_server.R
#> modules/mylab_my_first_module/R/shared-scripts.R
#> modules/mylab_my_first_module/_targets.yaml
#> modules/mylab_my_first_module/common.R
#> modules/mylab_my_first_module/configure.R
#> modules/mylab_my_first_module/debug.R
#> modules/mylab_my_first_module/fork-policy
#> modules/mylab_my_first_module/main.Rmd
#> modules/mylab_my_first_module/make-main.R
#> modules/mylab_my_first_module/migrate.R
#> modules/mylab_my_first_module/module-ui.html
#> modules/mylab_my_first_module/server.R
#> modules/mylab_my_first_module/settings.yaml

unlink(project_root, recursive = TRUE)

```
