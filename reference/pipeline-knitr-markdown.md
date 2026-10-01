# Configure `'rmarkdown'` files to build 'RAVE' pipelines

Allows building 'RAVE' pipelines from `'rmarkdown'` files. Please use it
in `'rmarkdown'` scripts only. Use
[`pipeline_create_template`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)
to create an example. `pipeline_setup_rmd` also turns every
[`RAVEPipelineAnalysis`](http://dipterix.org/ravepipeline/reference/RAVEPipelineAnalysis.md)
defined at the top level of the module `R/shared-*.R` scripts into
pipeline targets.

## Usage

``` r
configure_knitr(languages = c("R", "python"), targets = fastqueue2())

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

- targets:

  internal queue that collects the pipeline target specifications;
  `pipeline_setup_rmd` passes its own queue so it can add the analysis
  targets. Leave it as the default, a new queue

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
#>     lapply(targets$as_list(), function(item) {
#>         if (isTRUE(item$is_delayed)) {
#>             message("Evaluating delayed target ", item$export, 
#>                 " [R]")
#>             force(env[[item$export]])
#>         }
#>         return()
#>     })
#>     rave_knitr_build(targets, make_file)
#> }
#> <bytecode: 0x5628a44fb1a8>
#> <environment: 0x5628a44fd250>

if (FALSE) { # \dontrun{

# Requires to configure Python
configure_knitr("python")

# This function must be called in an Rmd file setup block
# for example, see
# https://rave.wiki/posts/customize_modules/python_module_01.html

pipeline_setup_rmd("my_module_id")

} # }
```
