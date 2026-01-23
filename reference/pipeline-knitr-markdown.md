# Configure `'rmarkdown'` files to build 'RAVE' pipelines

Allows building 'RAVE' pipelines from `'rmarkdown'` files. Please use it
in `'rmarkdown'` scripts only. Use
[`pipeline_create_template`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)
to create an example.

## Usage

``` r
configure_knitr(languages = c("R", "python"))

pipeline_setup_rmd(
  module_id,
  env = parent.frame(),
  collapse = TRUE,
  comment = "#>",
  languages = c("R", "python"),
  project_path = getOption("raveio.pipeline.project_root", default =
    rs_active_project(child_ok = TRUE, shiny_ok = TRUE))
)

pipeline_render(
  module_id,
  ...,
  env = new.env(parent = parent.frame()),
  entry_file = "main.Rmd",
  project_path = getOption("raveio.pipeline.project_root", default =
    rs_active_project(child_ok = TRUE, shiny_ok = TRUE))
)
```

## Arguments

- languages:

  one or more programming languages to support; options are `'R'` and
  `'python'`

- module_id:

  the module ID, usually the name of direct parent folder containing the
  pipeline file

- env:

  environment to set up the pipeline translator

- collapse, comment:

  passed to `set` method of
  [`opts_chunk`](https://rdrr.io/pkg/knitr/man/opts_chunk.html)

- project_path:

  the project path containing all the pipeline folders, usually the
  active project folder

- ...:

  passed to internal function calls

- entry_file:

  the file to compile; default is `"main.Rmd"`

## Value

A function that is supposed to be called later that builds the pipeline
scripts

## Examples

``` r
configure_knitr("R")
#> function (make_file) 
#> {
#>     rave_knitr_build(targets, make_file)
#> }
#> <bytecode: 0x5608ad1e4d50>
#> <environment: 0x5608ad1e8800>

if (FALSE) { # \dontrun{

# Requires to configure Python
configure_knitr("python")

# This function must be called in an Rmd file setup block
# for example, see
# https://rave.wiki/posts/customize_modules/python_module_01.html

pipeline_setup_rmd("my_module_id")

} # }
```
