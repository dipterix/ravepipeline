# Install 'RAVE' pipelines

Install 'RAVE' pipelines

## Usage

``` r
pipeline_install_local(
  src,
  to = c("default", "custom", "workdir", "tempdir"),
  upgrade = FALSE,
  force = FALSE,
  set_default = NA,
  ...
)

pipeline_install_github(
  repo,
  to = c("default", "custom", "workdir", "tempdir"),
  upgrade = FALSE,
  force = FALSE,
  set_default = NA,
  ...
)
```

## Arguments

- src:

  pipeline directory

- to:

  installation path; choices are `'default'`, `'custom'`, `'workdir'`,
  and `'tempdir'`. Please specify pipeline root path via
  [`pipeline_root`](http://dipterix.org/ravepipeline/reference/rave-pipeline.md)
  when `'custom'` is used.

- upgrade:

  whether to upgrade the dependence; default is `FALSE` for stability,
  however, it is highly recommended to upgrade your dependencies

- force:

  whether to force installing the pipelines

- set_default:

  whether to set current pipeline module folder as the default, will be
  automatically set when the pipeline is from the official 'Github'
  repository.

- ...:

  other parameters not used

- repo:

  'Github' repository in user-repository combination, for example,
  `'rave-ieeg/rave-pipeline'`

## Value

nothing

## Examples

``` r
if (FALSE) { # \dontrun{


pipeline_install_github("rave-ieeg/pipelines")


# or download github.com/rave-ieeg/pipelines repository, extract
# to a folder, and call
pipeline_install_local("path/to/pipeline/folder")

} # }

```
